use super::identifier::Identifier;
use super::literal::*;
use super::ops::*;
use super::ty::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
	pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
	Infix(Box<Expr>, BinOp, Box<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	Assignment(Box<Expr>, AssignOp, Box<Expr>),
	Braced(Box<Expr>),
	PropertyAccess(Box<Expr>, Box<Expr>),
	// for now, queries, once identified, are stored as a string
	Query(QueryKind, String),
	/// List access, such as `foo[2]` or `list.get(0)[1]`.
	ListAccess(Box<Expr>, Box<Expr>),
	New(Ty, NewType),
	Call(Identifier, Option<Vec<Expr>>),
	Unary(UnOp, Box<Expr>),
	Prefix(IncDecOp, Box<Expr>),
	Postfix(Box<Expr>, IncDecOp),
	Instanceof(Identifier, Ty),
	Cast(Ty, Box<Expr>),
	Type(Ty),
	Literal(Literal),
	Identifier(Identifier),
}

impl Into<Expr> for ExprKind {
	fn into(self) -> Expr {
		Expr { kind: self }
	}
}

impl From<Literal> for Expr {
	fn from(l: Literal) -> Expr {
		Expr {
			kind: ExprKind::Literal(l),
		}
	}
}

impl From<Identifier> for Expr {
	fn from(i: Identifier) -> Expr {
		Expr {
			kind: ExprKind::Identifier(i),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum NewType {
	Map(Vec<(Expr, Expr)>),
	Collection(Vec<Expr>),
	Array(Vec<Expr>),
	Class(ClassArgs),
}

// this is necessary because we need a generic way of supporting both
// `new Foo(bar)` and `new Account(Name = 'foo')`.
#[derive(Debug, Clone, PartialEq)]
pub enum ClassArgs {
	Basic(Option<Vec<Expr>>),
	SObject(Vec<(Identifier, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum QueryKind {
	Soql,
	Sosl,
}
