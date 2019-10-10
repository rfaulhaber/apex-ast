use super::annotation::*;
use super::expr::*;
use super::identifier::*;
use super::literal::*;
use super::ty::*;
use crate::source::Span;
use serde::Serialize;

pub type BlockRef = Box<Block>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Stmt {
	pub kind: StmtKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum StmtKind {
	For(ForStmt),
	DoWhile(BlockRef, Expr),
	While(Expr, BlockRef),
	// if condition, if block, optional else/if conditions and blocks, optional else block
	If(
		Expr,
		BlockRef,
		Option<Vec<(Expr, BlockRef)>>,
		Option<BlockRef>,
	),
	// test expr, when cases and their blocks, when else block
	Switch(Expr, Option<Vec<WhenCase>>, Option<Block>),
	// TODO make a type?
	TryCatch(
		BlockRef,
		(Ty, Identifier, Block),
		Option<Vec<(Ty, Identifier, Block)>>,
		Option<Block>,
	),
	Block(Block),
	Return(Option<Expr>),
	Dml(DmlOp, Expr),
	Throw(Expr),
	Break,
	Continue,
	StmtExpr(StmtExpr),
	Local(Local),
}

pub type WhenCase = (WhenCondition, Block);

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum WhenCondition {
	Type(Ty, Identifier),
	Value(Vec<WhenValue>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum WhenValue {
	Literal(Literal),
	Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Block {
	Body(Vec<Stmt>),
	Inline(Box<Stmt>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ForStmt {
	Basic(
		Option<Vec<StmtExpr>>,
		Option<Expr>,
		Option<StmtExpr>,
		BlockRef,
	),
	Enhanced(Ty, Identifier, Expr, BlockRef),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum StmtExpr {
	Expr(Expr),
	Local(Local),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Local {
	pub annotation: Option<Annotation>,
	pub is_final: bool,
	pub ty: Ty,
	pub id: Identifier,
	pub rhs: Option<Expr>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum DmlOp {
	Insert,
	Update,
	Upsert,
	Delete,
	Undelete,
	Merge,
}
