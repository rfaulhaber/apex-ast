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
	/// any expression surrounded by parentheses, like (1 + 2)
	Braced(Box<Expr>),

	/// a binary operation, like `x + 2`
	Binary(Box<Expr>, BinOp, Box<Expr>),

	/// a unary operation, like `!x` or `x++`
	Unary(UnOp, Box<Expr>),

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

	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

	InstanceOf(Box<Expr>, Box<Expr>),

	ListAccess(String, Box<Expr>),

	SoqlQuery(SoqlQuery),

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
		Rule::property_access => ExprKind::PropAccess(inner.into_inner().map(|expr| expr.into()).collect()).into(),
		Rule::query_expression => unimplemented!(),
		Rule::new_instance_declaration => unimplemented!(),
		Rule::method_invocation => unimplemented!(),
		Rule::instanceof_expr => unimplemented!(),
		Rule::unary_expr => unimplemented!(),
		Rule::list_access => unimplemented!(),
		Rule::literal => inner.into(),
		Rule::identifier => inner.into(),
		_ => unreachable!(),
	}
}

fn parse_new_instance(pairs: Pairs<Rule>) -> Expr {
	let identifier = 
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
}
