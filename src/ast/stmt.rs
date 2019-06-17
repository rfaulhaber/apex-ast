use super::expr::Expr;

#[derive(Debug, Clone)]
pub struct Stmt {
	pub kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
	// inits, condition, update
	ForIter(Vec<Local>, Option<Expr>, Vec<Expr>),
	ForEach(String, String, Expr, Block),
	DoWhile(Block, Expr),
	While(Block),
	// if block, else if blocks, else block
	If(Block, Option<Vec<Block>>, Option<Block>),
	// try block, catch block, any further catch blocks, finally block
	TryCatch(Block, Block, Option<Vec<Block>>, Option<Block>),
	// Switch(Expr, Vec<Expr>, ),
	Throw(Expr),
	Dml(DmlKind, Expr),
	Return(Expr),
	Continue(Expr),
	Break(Expr),
	Local(Local),
	Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum DmlKind {
	Insert,
	Update,
	Upsert,
	Delete,
	Undelete,
}

#[derive(Debug, Clone)]
pub struct Local {
	pub is_final: bool,
	pub return_type: Option<String>, // if None, reassignment
	pub identifier: String,
	pub rhs: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
	stmts: Vec<Stmt>,
}
