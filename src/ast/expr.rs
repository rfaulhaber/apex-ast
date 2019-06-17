use super::literal::*;
use super::ops::*;

#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
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
	// a series of expressions joined by "."
	PropAccess(Vec<Expr>),
	// direct method call, like foo(2 + 3, x)
	Call(String, Option<Vec<Expr>>),
	New(String, Option<Vec<Expr>>),
	InstanceOf(Box<Expr>, Box<Expr>),
	ListAccess(String, Box<Expr>),
	SoqlQuery(SoqlQuery),
	SoslQuery(SoslQuery),
}

#[derive(Debug, Clone)]
pub struct SoqlQuery;

#[derive(Debug, Clone)]
pub struct SoslQuery;