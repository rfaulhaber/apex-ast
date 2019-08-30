use super::annotation::*;
use super::expr::*;
use super::identifier::*;
use super::ty::*;
use super::*;

pub type BlockRef = Box<Block>;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
	pub kind: StmtKind,
}

impl Stmt {
	pub fn to_boxed(self) -> Box<Stmt> {
		Box::new(self)
	}
}

impl From<Local> for Stmt {
	fn from(l: Local) -> Stmt {
		Stmt {
			kind: StmtKind::StmtExpr(StmtExpr::Local(l)),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
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
	Switch(Expr, Vec<(Expr, Block)>, Block),
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

impl Into<Stmt> for StmtKind {
	fn into(self) -> Stmt {
		Stmt { kind: self }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Block {
	Body(Vec<Stmt>),
	Inline(Box<Stmt>),
}

impl From<Vec<Stmt>> for Block {
	fn from(v: Vec<Stmt>) -> Block {
		Block::Body(v)
	}
}

impl From<Stmt> for Block {
	fn from(s: Stmt) -> Block {
		Block::Inline(Box::new(s))
	}
}

impl Block {
	pub fn to_boxed(&self) -> Box<Self> {
		Box::new(self.clone())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForStmt {
	Basic(
		Option<Vec<StmtExpr>>,
		Option<Expr>,
		Option<StmtExpr>,
		BlockRef,
	),
	Enhanced(Ty, Identifier, Expr, BlockRef),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtExpr {
	Expr(Expr),
	Local(Local),
}

impl From<Expr> for StmtExpr {
	fn from(e: Expr) -> StmtExpr {
		StmtExpr::Expr(e)
	}
}

impl From<Local> for StmtExpr {
	fn from(l: Local) -> StmtExpr {
		StmtExpr::Local(l)
	}
}

impl From<StmtExpr> for Stmt {
	fn from(se: StmtExpr) -> Stmt {
		Stmt {
			kind: StmtKind::StmtExpr(se),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
	pub annotation: Option<Annotation>,
	pub is_final: bool,
	pub ty: Ty,
	pub id: Identifier,
	pub rhs: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DmlOp {
	Insert,
	Update,
	Upsert,
	Delete,
	Undelete,
	Merge,
}

impl DmlOp {
	pub fn from_str(s: &str) -> DmlOp {
		match s.to_lowercase().as_str() {
			"insert" => DmlOp::Insert,
			"update" => DmlOp::Update,
			"upsert" => DmlOp::Upsert,
			"delete" => DmlOp::Delete,
			"undelete" => DmlOp::Undelete,
			"merge" => DmlOp::Merge,
			_ => panic!("unexpected dml keyword: {}", s),
		}
	}
}
