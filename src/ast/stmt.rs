use super::expr::{Expr, ExprKind};
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
	ForIter(
		Option<Vec<Local>>,
		Option<Expr>,
		Option<Vec<Expr>>,
		BlockRef,
	),
	ForEach(Ty, Identifier, Expr, BlockRef),
	DoWhile(BlockRef, Expr),
	While(Expr, BlockRef),
	// if statement. condition, block, else if blocks, else block
	If(
		Expr,
		BlockRef,
		Option<Vec<(Expr, BlockRef)>>,
		Option<BlockRef>,
	),
	// try block, catch block, any further catch blocks, finally block
	TryCatch(BlockRef, BlockRef, Option<Vec<Block>>, Option<Block>),
	Switch(Expr, Vec<(Expr, Block)>),
	Throw(Expr), // NOTE should this be more granular?
	Dml(DmlKind, Expr),
	Return(Expr),
	// TODO: for continue and break, we'll want to store span in parent?
	Continue,
	Break,
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

// from either Rule::code_block or Rule::inline_code_block
impl<'a> From<Pair<'a, Rule>> for Block {
	fn from(pair: Pair<Rule>) -> Block {
		let inner = pair.into_inner().next().unwrap();
		match inner.as_rule() {
			Rule::code_block => {
				let stmts = inner.into_inner().map(Stmt::from).collect();

				Block {
					kind: BlockKind::Body(stmts),
				}
			}
			Rule::inline_code_block => {
				let stmt = Stmt::from(inner.into_inner().next().unwrap());

				Block {
					kind: BlockKind::Inline(Box::new(stmt)),
				}
			}
			_ => unreachable!("got {:?}", inner.as_rule()),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum DmlKind {
	Insert,
	Update,
	Upsert,
	Delete,
	Undelete,
}

// parses Rule::dml_action
impl<'a> From<Pair<'a, Rule>> for DmlKind {
	fn from(pair: Pair<Rule>) -> DmlKind {
		let inner = pair.into_inner().next().unwrap();

		match inner.as_rule() {
			Rule::INSERT => DmlKind::Insert,
			Rule::UPDATE => DmlKind::Update,
			Rule::UPSERT => DmlKind::Upsert,
			Rule::DELETE => DmlKind::Delete,
			Rule::UNDELETE => DmlKind::Undelete,
			_ => unreachable!("got {:?}", inner.as_rule()),
		}
	}
}

// Local assignment
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
	pub kind: LocalKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LocalKind {
	// whether or not it's final, type, identiifer, and rhs
	Assignment(bool, Option<Ty>, Identifier, Option<Expr>),
	Reassignment(Expr, Expr),
}

impl<'a> From<Pair<'a, Rule>> for Stmt {
	fn from(pair: Pair<Rule>) -> Stmt {
		let inner = pair.into_inner().next().unwrap();
		match inner.as_rule() {
			Rule::for_each_statement => unimplemented!(),
			Rule::for_iter_statement => unimplemented!(),
			Rule::do_while_statement => unimplemented!(),
			Rule::while_statement => unimplemented!(),
			Rule::if_statement => unimplemented!(),
			Rule::try_catch_statement => unimplemented!(),
			Rule::throw_statement => parse_throw_statement(inner),
			Rule::dml_statement => parse_dml_statement(inner),
			Rule::return_statement => parse_return_statement(inner),
			Rule::continue_statement => parse_continue_statement(inner),
			Rule::break_statement => parse_break_statement(inner),
			Rule::local_assignment => parse_local_assignment(inner),
			_ => unreachable!("got {:?}", inner.as_rule()),
		}
	}
}

fn parse_return_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();
	inner.next(); //discard "return", TODO: save for span

	let expr = Expr::from(inner.next().unwrap());

	Stmt {
		kind: StmtKind::Return(expr),
	}
}

fn parse_throw_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();
	inner.next(); //discard "throw", TODO: save for span

	let expr = Expr::from(inner.next().unwrap());

	Stmt {
		kind: StmtKind::Throw(expr),
	}
}

fn parse_continue_statement(pair: Pair<Rule>) -> Stmt {
	Stmt {
		kind: StmtKind::Continue,
	}
}

fn parse_break_statement(pair: Pair<Rule>) -> Stmt {
	Stmt {
		kind: StmtKind::Break,
	}
}

fn parse_dml_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();

	let action = DmlKind::from(inner.next().unwrap());
	let expr = Expr::from(inner.next().unwrap());

	Stmt {
		kind: StmtKind::Dml(action, expr),
	}
}

fn parse_local_assignment(pair: Pair<Rule>) -> Stmt {
	let dec_or_resassign = pair.into_inner().next().unwrap();

	match dec_or_resassign.as_rule() {
		Rule::local_variable_declaration => {
			let mut inner = dec_or_resassign.into_inner();

			let first = inner.next().unwrap();

			let is_final = first.as_rule() == Rule::FINAL;

			let ty = if is_final {
				Ty::from(inner.next().unwrap())
			} else {
				Ty::from(first)
			};

			let id = Identifier::from(inner.next().unwrap());

			let rhs = match inner.next() {
				Some(expr_pair) => Some(Expr::from(expr_pair)),
				None => None,
			};

			Stmt {
				kind: StmtKind::Local(Local {
					kind: LocalKind::Assignment(is_final, Some(ty), id, rhs),
				}),
			}
		}
		Rule::variable_reassignment => unimplemented!(),
		_ => unreachable!("got {:?}", dec_or_resassign.as_rule()),
	}
}

#[cfg(test)]
mod stmt_tests {
	use super::super::expr::*;
	use super::super::literal::*;
	use super::super::ty::*;
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

	// TODO: should these be one macro?
	// could "type"::from be abstracted?

	macro_rules! stmt_parse_correctly {
		($test_name:ident, $parse:literal, $expected:expr) => {
			#[test]
			fn $test_name() {
				let mut parsed = GrammarParser::parse(Rule::body_statement, $parse).unwrap();
				let item = parsed.next().unwrap();

				let result = Stmt::from(item);

				let expected = $expected;
				assert_eq!(expected, result);
			}
		};
	}

	macro_rules! block_parse_correctly {
		($test_name:ident, $parse:literal, $expected:expr) => {
			#[test]
			fn $test_name() {
				let mut parsed = GrammarParser::parse(Rule::body_statement, $parse).unwrap();
				let item = parsed.next().unwrap();

				let result = Block::from(item);

				let expected = $expected;
				assert_eq!(expected, result);
			}
		};
	}

	stmt_parse_correctly!(
		parse_return_parses_correctly,
		"return x;",
		Stmt {
			kind: StmtKind::Return(Expr {
				kind: ExprKind::Identifier(Identifier::from("x")),
			}),
		}
	);

	stmt_parse_correctly!(
		parse_continue_parses_correctly,
		"continue;",
		Stmt {
			kind: StmtKind::Continue,
		}
	);

	stmt_parse_correctly!(
		parse_break_parses_correctly,
		"break;",
		Stmt {
			kind: StmtKind::Break,
		}
	);

	stmt_parse_correctly!(
		parse_dml_parses_correctly,
		"insert list;",
		Stmt {
			kind: StmtKind::Dml(
				DmlKind::Insert,
				Expr {
					kind: ExprKind::Identifier(Identifier::from("list"))
				}
			)
		}
	);

	stmt_parse_correctly!(
		parse_dml_new_list_parses_correctly,
		"insert new List<Integer>{one, 2};",
		Stmt {
			kind: StmtKind::Dml(
				DmlKind::Insert,
				Expr {
					kind: ExprKind::New(
						Ty {
							kind: TyKind::Collection(Collection {
								kind: CollectionType::List(Box::new(Ty {
									kind: TyKind::Primitive(Primitive {
										kind: PrimitiveType::Integer,
									}),
									array: false,
								})),
							}),
							array: false,
						},
						Some(NewType::List(vec![
							Expr {
								kind: ExprKind::Identifier(Identifier::from("one")),
							},
							Expr {
								kind: ExprKind::Literal(Literal::from(2)),
							},
						]))
					),
				}
			)
		}
	);

	stmt_parse_correctly!(
		parse_throw_throwable_parses_correctly,
		"throw err;",
		Stmt {
			kind: StmtKind::Throw(Expr {
				kind: ExprKind::Identifier(Identifier::from("err"))
			})
		}
	);

	stmt_parse_correctly!(
		parse_throw_new_err_parses_correctly,
		"throw new TestException();",
		Stmt {
			kind: StmtKind::Throw(Expr {
				kind: ExprKind::New(
					Ty {
						array: false,
						kind: TyKind::ClassOrInterface(ClassOrInterface {
							kind: ClassOrInterfaceType::Class(Identifier::from("TestException"))
						})
					},
					None
				)
			})
		}
	);

	stmt_parse_correctly!(
		parse_local_assignment_simple_parses_correctly,
		"final Integer foo = 22;",
		Stmt {
			kind: StmtKind::Local(Local {
				kind: LocalKind::Assignment(
					true,
					Some(Ty {
						array: false,
						kind: TyKind::Primitive(Primitive {
							kind: PrimitiveType::Integer,
						})
					}),
					Identifier::from("foo"),
					Some(Expr {
						kind: ExprKind::Literal(Literal::from(22)),
					})
				)
			})
		}
	);

	block_parse_correctly!(
		parse_block_simple_parses_correctly,
		"{ final Integer foo = 22; return foo; }",
		Block {
			kind: BlockKind::Body(vec![
				Stmt {
					kind: StmtKind::Local(Local {
						kind: LocalKind::Assignment(
							true,
							Some(Ty {
								array: false,
								kind: TyKind::Primitive(Primitive {
									kind: PrimitiveType::Integer,
								})
							}),
							Identifier::from("foo"),
							Some(Expr {
								kind: ExprKind::Literal(Literal::from(22)),
							})
						)
					})
				},
				Stmt {
					kind: StmtKind::Return(Expr {
						kind: ExprKind::Identifier(Identifier::from("foo")),
					}),
				}
			])
		}
	);
}
