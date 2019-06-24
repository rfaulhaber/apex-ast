use super::identifier::Identifier;
use super::literal::*;
use super::ops::*;
use super::soql::*;
use super::sosl::*;

use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
	/// any expression surrounded by parentheses, like `(1 + 2)`
	Braced(Box<Expr>),

	/// a binary operation, like `x + 2`
	Binary(Box<Expr>, BinOp, Box<Expr>),

	/// a unary operation, like `!x`. Excludes increment or decrement expressions.
	Unary(UnOp, Box<Expr>),

	/// Either an increment or decrement expression.
	/// This has to be a different type of enum because they can either come
	/// before or after the expression.
	Postfix(Box<Expr>, PostfixOp),

	/// a literal, such as a number or string
	Literal(Literal),

	// an identifier
	Identifier(String),

	// a series of expressions joined by "."
	PropAccess(Vec<Expr>),

	// direct method call, like foo(2 + 3, x)
	Call(String, Option<Vec<Expr>>),

	// new object instantiation, like "new Foo(one, 2)" or "new Account(Name = 'Foo');"
	New(String, Option<Vec<Expr>>),

	// ternary expression, like the right-hand side of:
	// String foo = isTrue() ? 'Yes' : 'No';
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

	// a cast expression, like `(String) list.get(0)`
	CastExpr(String, Box<Expr>),

	// instanceof expressions, like: `x instanceof Account`
	InstanceOf(Identifier, Box<Expr>),

	// direct list array access, like `foo[2]`
	ListAccess(String, Box<Expr>),

	// a soql query expression
	SoqlQuery(SoqlQuery),

	// a sosl query expression
	SoslQuery(SoslQuery),
}

impl Into<Expr> for ExprKind {
	fn into(self) -> Expr {
		Expr { kind: self }
	}
}

impl<'a> From<Pair<'a, Rule>> for Expr {
	fn from(pair: Pair<Rule>) -> Expr {
		match pair.as_rule() {
			Rule::infix_expr => parse_infix_expr(pair),
			Rule::ternary_expr => parse_ternary_expr(pair),
			Rule::expr_inner => parse_expr_inner(pair),
			Rule::identifier => Expr {
				kind: ExprKind::Identifier(pair.as_str().into()),
			},
			Rule::string_literal => Expr {
				kind: ExprKind::Literal(pair.as_str().into()),
			},
			Rule::float_literal => Expr {
				kind: ExprKind::Literal(pair.as_str().parse::<f64>().unwrap().into()),
			},
			Rule::long_literal => Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Long(pair.as_str().parse::<i64>().unwrap()),
				}),
			},
			Rule::integer_literal => ExprKind::Literal(
				LiteralKind::Integer(pair.as_str().parse::<i64>().unwrap()).into(),
			)
			.into(),
			Rule::boolean_literal => {
				match pair.into_inner().next().unwrap().as_rule() {
					Rule::TRUE => ExprKind::Literal(LiteralKind::Boolean(true).into()).into(),
					Rule::FALSE => ExprKind::Literal(LiteralKind::Boolean(false).into()).into(),
					_ => unreachable!(), // we hope!
				}
			}
			Rule::NULL => Expr {
				kind: ExprKind::Literal(LiteralKind::Null.into()),
			},
			_ => unimplemented!(),
		}
	}
}

// parse pair after having gone into it already
fn parse_pair(pair: Pair<Rule>) -> Expr {
	match pair.as_rule() {
		Rule::identifier | Rule::literal => pair.into(),
		_ => unimplemented!(),
	}
}

fn parse_ternary_expr(pair: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_infix_expr(pair: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_expr_inner(pair: Pair<Rule>) -> Expr {
	let inner = pair.into_inner().next().unwrap();
	match inner.as_rule() {
		Rule::braced_expr => ExprKind::Braced(Box::new(inner.into())).into(),
		Rule::property_access => {
			ExprKind::PropAccess(inner.into_inner().map(|expr| expr.into()).collect()).into()
		}
		Rule::query_expression => unimplemented!(),
		Rule::new_instance_declaration => unimplemented!(),
		Rule::method_invocation => unimplemented!(),
		Rule::instanceof_expr => unimplemented!(),
		Rule::unary_expr => parse_unary_expr(inner),
		Rule::list_access => parse_list_access(inner),
		Rule::literal => inner.into(),
		Rule::identifier => inner.into(),
		_ => unreachable!(),
	}
}

fn parse_list_access(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();
	let id = String::from(inner.next().unwrap().as_str());
	let inner_expr = parse_pair(inner.next().unwrap());

	let kind = ExprKind::ListAccess(id, Box::new(inner_expr));

	Expr { kind }
}

fn parse_unary_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.clone().into_inner();
	let first = inner.next().unwrap();

	match first.as_rule() {
		Rule::inc_dec_prefix => parse_inc_dec_prefix(first),
		Rule::inc_dec_postfix => parse_inc_dec_postfix(first),
		Rule::unary_operator => {
			let unary_inner = first.into_inner();
			let op = UnOp::from(unary_inner.as_str());
			let expr = parse_pair(inner.next().unwrap());

			let kind = ExprKind::Unary(op, Box::new(expr));
			Expr { kind }
		}
		_ => unreachable!(),
	}

}

fn parse_inc_dec_prefix(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();
	let op = UnOp::from(inner.next().unwrap().as_str());

	let right_pair = inner.next().unwrap();

	let postfix = match right_pair.as_rule() {
		Rule::list_access => parse_list_access(right_pair),
		Rule::identifier => right_pair.into(),
		_ => unreachable!(),
	};

	ExprKind::Unary(op, Box::new(postfix)).into()
}

fn parse_inc_dec_postfix(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let right_pair = inner.next().unwrap();

	let postfix = match right_pair.as_rule() {
		Rule::list_access => parse_list_access(right_pair),
		Rule::identifier => right_pair.into(),
		_ => unreachable!(),
	};

	let op = PostfixOp::from(inner.next().unwrap().as_str());

	ExprKind::Postfix(Box::new(postfix), op).into()
}

fn parse_instanceof(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let id: Identifier = inner.next().unwrap().as_str().into();
	inner.next();

	unimplemented!();
}

#[cfg(test)]
mod expr_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

	#[test]
	fn from_pair_parses_literal_correctly() {
		let mut parsed = GrammarParser::parse(Rule::literal, "123").unwrap();

		let expr: Expr = parsed.next().unwrap().into();

		match expr.kind {
			ExprKind::Literal(lit) => match lit.kind {
				LiteralKind::Integer(int) => assert_eq!(123, int),
				_ => panic!("LiteralKind was not integer"),
			},
			_ => panic!("ExprKind was not literal"),
		}
	}

	#[test]
	fn from_pair_parses_identifier_correctly() {
		let mut parsed = GrammarParser::parse(Rule::identifier, "myVar").unwrap();
		let single_item = parsed.next().unwrap();

		let expr: Expr = single_item.clone().into();

		match expr.kind {
			ExprKind::Identifier(id) => assert_eq!(id, "myVar"),
			_ => panic!("invalid kind found: {:?}", single_item.as_rule()),
		}
	}

	#[test]
	fn simple_unary_op_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "!foo").unwrap();
		let item = parsed.next().unwrap();

		let expr: Expr = item.clone().into();

		// TODO this sucks, find a better way to handle this
		match expr.kind {
			ExprKind::Unary(op, expr) => match op {
				UnOp::Not => match expr.kind {
					ExprKind::Identifier(id) => assert_eq!(id, "foo"),
					_ => panic!("wrong exprkind found: {:?}", expr.kind),
				},
				_ => panic!("op was not correct"),
			},
			_ => panic!("was not unary expr"),
		}
	}

	#[test]
	fn simple_postfix_op_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo++").unwrap();
		let item = parsed.next().unwrap();

		let expr: Expr = item.clone().into();

		match expr.kind {
			ExprKind::Postfix(expr, op) => match op {
				PostfixOp::Inc => match expr.kind {
					ExprKind::Identifier(id) => assert_eq!(id, "foo"),
					_ => panic!("wrong exprkind found: {:?}", expr.kind),
				},
				_ => panic!("op was not correct"),
			},
			_ => panic!("was not unary expr"),
		}
	}
}
