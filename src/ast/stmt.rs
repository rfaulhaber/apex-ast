use super::expr::Expr;
use super::identifier::Identifier;
use super::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
	pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
	// inits, condition, update
	ForIter(Vec<Local>, Option<Expr>, Vec<Expr>),
	ForEach(Ty, Identifier, Expr, BlockRef),
	DoWhile(BlockRef, Expr),
	While(BlockRef),
	// if block, else if blocks, else block
	If(BlockRef, Option<Vec<Block>>, Option<BlockRef>),
	// try block, catch block, any further catch blocks, finally block
	TryCatch(BlockRef, BlockRef, Option<Vec<Block>>, Option<Block>),
	// Switch(Expr, Vec<Expr>, ),
	Throw(Expr),
	Dml(DmlKind, Expr),
	Return(Expr),
	Continue(Expr),
	Break(Expr),
	Local(Local),
	Expr(Expr),
}

type BlockRef = Box<Block>;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
	kind: BlockKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockKind {
	Body(Vec<Stmt>),
	Inline(Box<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DmlKind {
	Insert,
	Update,
	Upsert,
	Delete,
	Undelete,
}

// Local assignment
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
	pub is_final: bool,
	// the "Integer" in `Integer foo = 22;`
	pub ty: Option<Ty>, // if None, reassignment
	pub identifier: Identifier,
	pub rhs: Option<Expr>,
}
