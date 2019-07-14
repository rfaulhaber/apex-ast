use super::expr::Expr;
use super::identifier::Identifier;
use super::ty::Ty;

use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

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

impl<'a> From<Pair<'a, Rule>> for Stmt {
	fn from(pair: Pair<Rule>) -> Stmt {
		let inner = pair.into_inner().next().unwrap();
		match inner.as_rule() {
			Rule::for_statement => unimplemented!(),
			Rule::do_while_statement => unimplemented!(),
			Rule::while_statement => unimplemented!(),
			Rule::if_statement => unimplemented!(),
			Rule::try_catch_statement => unimplemented!(),
			Rule::throw_statement => unimplemented!(),
			Rule::dml_statement => unimplemented!(),
			Rule::code_block => unimplemented!(),
			Rule::return_statement => unimplemented!(),
			Rule::continue_statement => unimplemented!(),
			Rule::break_statement => unimplemented!(),
			Rule::local_assignment => unimplemented!(),
			_ => unreachable!("got {:?}", inner.as_rule()),
		}
	}
}
