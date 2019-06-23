use super::expr::Expr;
use super::identifier::Identifier;

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
	pub for_clause: Option<For>,
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
	terms: Vec<WhereTerm>,
}

/// Each WhereTerm is a single piece of a WHERE clause.
/// For example: `Id IN :ids` or `FirstName = 'John'`
#[derive(Debug, Clone)]
pub enum WhereTerm {
	/// A WHERE expression surrounded by parentheses.
	Braced(Box<WhereTerm>),

	/// A comparison, can be recursive to support complex Boolean logic.
	FieldExpr(WhereFieldOrFunction, WhereOp, Box<WhereTerm>),

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
pub enum WhereOp {
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
pub struct DataCategorySelection(String, DataCategoryOp, String);

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
	pub having: Option<Having>,
}

#[derive(Debug, Clone)]
pub enum GroupByKind {
	Func(GroupByFunc, Vec<String>),
	FieldList(Vec<String>),
}

#[derive(Debug, Clone)]
pub enum GroupByFunc {
	Rollup,
	Cube,
}

#[derive(Debug, Clone)]
pub struct Having {}

#[derive(Debug, Clone)]
pub struct OrderBy;

#[derive(Debug, Clone)]
pub struct For;

#[cfg(test)]
mod soql_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;


}
