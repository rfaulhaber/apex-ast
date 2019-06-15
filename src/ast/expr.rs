
use super::literal::*;
use super::ops::*;
use crate::pos::Span;
#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
	/// a binary operation, like `x + 2`
	Binary(Box<Expr>, BinOp, Box<Expr>),
	/// a unary operation, like `!x` or `x++`
	Unary(UnOp, Box<Expr>),
	/// a literal, such as a number or string
	Literal(Literal),
	Identifier(String),
	PropAccess,
	SoqlQuery,
	SoslQuery,
}