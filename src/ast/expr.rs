use super::identifier::Identifier;
use super::literal::*;
use super::ops::*;
use super::ty::*;
use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ExprKind {
	Infix(Box<Expr>, BinOp, Box<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	Assignment(Box<Expr>, AssignOp, Box<Expr>),
	Braced(Box<Expr>),
	PropertyAccess(Box<Expr>, Box<Expr>),
	/// List access, such as `foo[2]` or `list.get(0)[1]`.
	ListAccess(Box<Expr>, Box<Expr>),
	Query(Query),
	New(Ty, NewType),
	Call(Identifier, Option<Vec<Expr>>),
	Unary(UnOp, Box<Expr>),
	Prefix(IncDecOp, Box<Expr>),
	Postfix(Box<Expr>, IncDecOp),
	Instanceof(Box<Expr>, Ty),
	Cast(Ty, Box<Expr>),
	Type(Ty),
	Literal(Literal),
	Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum NewType {
	Map(Vec<(Expr, Expr)>),
	Collection(Vec<Expr>),
	Array(Vec<Expr>),
	Class(ClassArgs),
}

// this is necessary because we need a generic way of supporting both
// `new Foo(bar)` and `new Account(Name = 'foo')`.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ClassArgs {
	Basic(Option<Vec<Expr>>),
	SObject(Vec<(Identifier, Expr)>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Query {
	Soql(String),
	Sosl(String),
}
