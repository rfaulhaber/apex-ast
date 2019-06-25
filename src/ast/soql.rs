use super::expr::Expr;
use super::identifier::Identifier;
use crate::parser::{GrammarParser, Rule};
use pest::iterators::{Pair, Pairs};

// TODO create general conditional expression for WHERE, WITH, HAVING, etc.

#[derive(Debug, Clone)]
pub struct SoqlQuery {
	pub select: Vec<QueryTerm>,
	pub from: FromClause,
	pub where_clause: Option<WhereClause>, // where is reserved by rust
	pub with: Option<WithClause>,
	pub group_by: Option<GroupByClause>,
	pub order_by: Option<OrderBy>,

	// trial and error testing determined the limit to a LIMIT clause is
	// around 2100000000
	pub limit: Option<u32>,
	pub offset: Option<u32>, // see above
	pub for_clause: Option<ForTerm>,
}

#[derive(Debug, Clone)]
pub struct QueryTerm {
	pub kind: QueryTermKind,
}

#[derive(Debug, Clone)]
pub enum QueryTermKind {
	/// an aggregate function, such as COUNT(Id)
	Func(String, Vec<String>),

	/// An object field, like Id or Name
	Field(String),

	SubQuery(SoqlSubQuery),
}

#[derive(Debug, Clone)]
pub struct FromClause {
	fields: Vec<String>,
	using_scope: Option<String>,
}

#[derive(Debug, Clone)]
pub struct WhereClause {
	terms: Box<WhereTerm>,
}

// TODO: rename?
/// Each WhereTerm is a single piece of a WHERE clause.
/// For example: `Id IN :ids` or `FirstName = 'John'`
#[derive(Debug, Clone)]
pub enum WhereTerm {
	/// A WHERE expression surrounded by parentheses.
	Braced(Box<WhereTerm>),

	/// A comparison, can be recursive to support complex Boolean logic.
	FieldExpr(WhereFieldOrFunction, SoqlCompOp, Box<WhereTerm>),

	/// An Apex expression, beginning with a colon, like `:ids`.
	ApexExpr(Expr),

	/// A subquery.
	SubQuery(SoqlSubQuery),

	/// A field literal, such as `'John'` or `3`.
	Literal(WhereLiteral),
}

/// The left-hand side of a WHERE field expression can be either a field or a
/// function.
#[derive(Debug, Clone)]
pub enum WhereFieldOrFunction {
	/// A field, such as `Foo__c`
	Field(String),

	/// A function, such as `CALENDAR_YEAR(CreatedDate)`
	Function(String, Vec<String>),
}

#[derive(Debug, Clone)]
pub enum SoqlCompOp {
	NotIn,
	In,
	Like,
	Ge,
	Le,
	Gt,
	Lt,
	NotEq,
	Eq,
}

#[derive(Debug, Clone)]
pub enum WhereLiteral {
	Integer(i64),
	Float(f64),
	String(String),
	Boolean(bool),
	Null,
}

#[derive(Debug, Clone)]
pub struct SoqlSubQuery {
	pub select: Vec<String>, // subqueries don't allow for aggregate functions
	pub from: FromClause,
	pub where_clause: Option<WhereClause>,
}

#[derive(Debug, Clone)]
pub enum SubQueryTerm {
	/// an aggregate function, such as COUNT(Id)
	Func(String, Vec<String>),

	/// An object field, like Id or Name
	Field(String),
}

#[derive(Debug, Clone)]
pub struct WithClause {
	pub kind: WithClauseKind,
}

#[derive(Debug, Clone)]
pub enum WithClauseKind {
	DataCategory(Vec<DataCategorySelection>),
	SecurityEnforced,
}

#[derive(Debug, Clone)]
pub struct DataCategorySelection(pub String, pub DataCategoryOp, pub String);

#[derive(Debug, Clone)]
pub enum DataCategoryOp {
	AboveOrBelow,
	Above,
	Below,
	At,
}

#[derive(Debug, Clone)]
pub struct GroupByClause {
	pub kind: GroupByKind,
	pub having: Option<Box<WhereTerm>>,
}

#[derive(Debug, Clone)]
pub enum GroupByKind {
	Func(GroupByFunc, Vec<String>),
	FieldList(Vec<String>),
}

impl<'a> From<Pair<'a, Rule>> for GroupByClause {
	// should come from soql_group_by_clause directly
	fn from(p: Pair<'a, Rule>) -> GroupByClause {
		let mut inner = p.into_inner();
		
		inner.next(); // discarding token

		let func_or_list = inner.next().unwrap();

		match func_or_list.as_rule() {
			Rule::soql_group_by_function => unimplemented!(),
			Rule::soql_field_list => unimplemented!(),
			_ => unreachable!(),
		}
	}
}

#[derive(Debug, Clone)]
pub enum GroupByFunc {
	Rollup,
	Cube,
}
impl<'a> From<Pair<'a, Rule>> for GroupByFunc {
	fn from(p: Pair<'a, Rule>) -> GroupByFunc {
		match p.as_rule() {
			Rule::ROLLUP => GroupByFunc::Rollup,
			Rule::CUBE => GroupByFunc::Cube,
			_ => unreachable!("expected a group by func, got {:?}", p.as_rule()),
		}
	}
}

#[derive(Debug, Clone)]
pub struct OrderBy {
	pub fields: Vec<Identifier>,
	pub order: Option<OrderOp>,
	pub nulls: Option<OrderNulls>,
}

impl<'a> From<Pair<'a, Rule>> for OrderBy {
	fn from(p: Pair<'a, Rule>) -> OrderBy {
		let mut inner = p.into_inner();

		// TODO we will probably need to save this for source mapping!
		inner.next();

		let field_list: Vec<Identifier> = inner
			.next()
			.unwrap()
			.into_inner()
			.map(Identifier::from)
			.collect();

		match inner.next() {
			Some(pair) => match pair.as_rule() {
				Rule::ASC | Rule::DESC => {
					let order_op: OrderOp = pair.into();

					match inner.next() {
						Some(inner_pair) => {
							let null_order = inner_pair.into();

							OrderBy {
								fields: field_list,
								order: Some(order_op),
								nulls: Some(null_order),
							}
						}
						None => OrderBy {
							fields: field_list,
							order: Some(order_op),
							nulls: None,
						},
					}
				}
				Rule::NULLS => {
					let null_order = inner.next().unwrap().into();

					OrderBy {
						fields: field_list,
						order: None,
						nulls: Some(null_order),
					}
				}
				_ => unreachable!("expected order by clause piece, got {:?}", pair.as_rule()),
			},
			None => OrderBy {
				fields: field_list,
				order: None,
				nulls: None,
			},
		}
	}
}

#[derive(Debug, Clone)]
pub enum OrderOp {
	Asc,
	Desc,
}

impl<'a> From<Pair<'a, Rule>> for OrderOp {
	fn from(p: Pair<'a, Rule>) -> OrderOp {
		match p.as_rule() {
			Rule::ASC => OrderOp::Asc,
			Rule::DESC => OrderOp::Desc,
			_ => unreachable!("expected order op (ASC | DESC), got {:?}", p.as_rule()),
		}
	}
}

#[derive(Debug, Clone)]
pub enum OrderNulls {
	First,
	Last,
}

impl<'a> From<Pair<'a, Rule>> for OrderNulls {
	fn from(p: Pair<'a, Rule>) -> OrderNulls {
		match p.as_rule() {
			Rule::FIRST => OrderNulls::First,
			Rule::LAST => OrderNulls::Last,
			_ => unreachable!("expected order nulls term, got {:?}", p.as_rule()),
		}
	}
}

#[derive(Debug, Clone)]
pub enum ForTerm {
	View,
	Reference,
	Update,
}

impl<'a> From<Pair<'a, Rule>> for ForTerm {
	fn from(p: Pair<'a, Rule>) -> ForTerm {
		match p.as_rule() {
			Rule::VIEW => ForTerm::View,
			Rule::REFERENCE => ForTerm::Reference,
			Rule::UPDATE => ForTerm::Update,
			_ => unreachable!("expected for term, got {:?}", p.as_rule()),
		}
	}
}

#[cfg(test)]
mod soql_tests {
	use super::*;
	use crate::parser::{GrammarParser, Rule};
	use pest::Parser;

	#[test]
	fn simple_soql_parses_correctly() {
		let soql = r#"
			SELECT Name, Id, Foo__c
			FROM Bar__c,
			WHERE Foo__c IN :listOfFoos
			ORDER BY Name ASC
		"#;

		let mut parsed = GrammarParser::parse(Rule::soql_query, soql).unwrap();

	}
}
