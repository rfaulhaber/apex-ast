use super::expr::{is_expr, Expr, ExprKind};
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
	ForIter(Option<Vec<Stmt>>, Option<Expr>, Option<Vec<Stmt>>, BlockRef),
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
	// TODO make this a type?
	TryCatch(
		BlockRef,
		(Ty, Identifier, Block),
		Option<Vec<(Ty, Identifier, Block)>>,
		Option<Block>,
	),
	Switch(Expr, Vec<(Expr, Block)>, Block),
	Throw(Expr), // NOTE should this be more granular?
	Dml(DmlKind, Expr),
	Return(Expr),
	// TODO: for continue and break, we'll want to store span in parent?
	Continue,
	Break,
	// TODO: box
	Local(Local),
	Block(Block),
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
		match pair.as_rule() {
			Rule::code_block => {
				let inner = pair.into_inner();
				let stmts = inner.map(Stmt::from).collect();

				Block {
					kind: BlockKind::Body(stmts),
				}
			}
			Rule::inline_code_block => {
				let mut inner = pair.into_inner();
				let stmt = Stmt::from(inner.next().unwrap());

				Block {
					kind: BlockKind::Inline(Box::new(stmt)),
				}
			}
			_ => unreachable!("got {:?}", pair.as_rule()),
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
	pub is_final: bool, // "final" is a Rust keyword
	pub ty: Ty,
	pub id: Identifier,
	pub expr: Option<Expr>,
}

impl<'a> From<Pair<'a, Rule>> for Local {
	fn from(pair: Pair<Rule>) -> Local {
		let mut inner = pair.into_inner();

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

		Local {
			is_final,
			ty,
			id,
			expr: rhs,
		}
	}
}

impl<'a> From<Pair<'a, Rule>> for Stmt {
	fn from(pair: Pair<Rule>) -> Stmt {
		let inner = pair.into_inner().next().unwrap();
		match inner.as_rule() {
			Rule::for_each_statement => parse_for_each_statement(inner),
			Rule::for_iter_statement => parse_for_iter_statement(inner),
			Rule::do_while_statement => parse_do_while_statement(inner),
			Rule::while_statement => parse_while_statement(inner),
			Rule::if_statement => unimplemented!(),
			Rule::try_catch_statement => unimplemented!(),
			Rule::switch_statement => unimplemented!(),
			Rule::throw_statement => parse_throw_statement(inner),
			Rule::dml_statement => parse_dml_statement(inner),
			Rule::code_block => parse_code_block(inner),
			Rule::return_statement => parse_return_statement(inner),
			Rule::continue_statement => parse_continue_statement(inner),
			Rule::break_statement => parse_break_statement(inner),
			Rule::local_variable_declaration => unimplemented!(),
			Rule::assignment_expr | Rule::property_access | Rule::method_invocation => {
				parse_expr_statement(inner)
			}
			_ => unreachable!("got {:?}", inner.as_rule()),
		}
	}
}

fn parse_for_each_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();

	inner.next(); // discard "FOR" token

	let ty = Ty::from(inner.next().unwrap());
	let id = Identifier::from(inner.next().unwrap());
	let expr = Expr::from(inner.next().unwrap());
	let block = Block::from(inner.next().unwrap());

	Stmt {
		kind: StmtKind::ForEach(ty, id, expr, Box::new(block)),
	}
}

fn parse_for_iter_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();

	inner.next(); // discard "FOR"

	let mut current = inner.next().unwrap();

	let for_init: Option<Vec<Stmt>> = if current.as_rule() == Rule::for_init {
		let current_inner = current.into_inner();
		let ret = Some(current_inner.map(Stmt::from).collect());

		current = inner.next().unwrap();

		ret
	} else {
		None
	};

	let for_expr: Option<Expr> = match current.as_rule() {
		Rule::ternary_expr | Rule::infix_expr | Rule::assignment_expr | Rule::expr_inner => {
			let expr = Expr::from(current);

			current = inner.next().unwrap();

			Some(expr)
		}
		_ => None,
	};

	let for_update: Option<Vec<Stmt>> = if current.as_rule() == Rule::for_init {
		let current_inner = current.into_inner();
		Some(current_inner.map(Stmt::from).collect())
	} else {
		None
	};

	let block = Box::new(Block::from(inner.next().unwrap()));

	Stmt {
		kind: StmtKind::ForIter(for_init, for_expr, for_update, block),
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

fn parse_code_block(pair: Pair<Rule>) -> Stmt {
	let block = Block::from(pair);

	Stmt {
		kind: StmtKind::Block(block),
	}
}

fn parse_local_assignment(pair: Pair<Rule>) -> Stmt {
	Stmt {
		kind: StmtKind::Local(Local::from(pair)),
	}
}

fn parse_expr_statement(pair: Pair<Rule>) -> Stmt {
	Stmt {
		kind: StmtKind::Expr(Expr::from(pair)),
	}
}

fn parse_while_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();

	inner.next(); // discard "WHILE"

	let expr = Expr::from(inner.next().unwrap());
	let block = Box::new(Block::from(inner.next().unwrap()));

	Stmt {
		kind: StmtKind::While(expr, block),
	}
}

fn parse_do_while_statement(pair: Pair<Rule>) -> Stmt {
	let mut inner = pair.into_inner();

	inner.next(); // discoard "DO"

	let block = Box::new(Block::from(inner.next().unwrap()));

	inner.next(); // discard "WHILE"

	let expr = Expr::from(inner.next().unwrap());

	Stmt {
		kind: StmtKind::DoWhile(block, expr),
	}
}

#[cfg(test)]
mod stmt_tests {
	use super::super::expr::*;
	use super::super::literal::*;
	use super::super::ops::*;
	use super::super::ty::*;
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;
	use pretty_assertions::{assert_eq, assert_ne};

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
				is_final: true,
				ty: Ty {
					array: false,
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveType::Integer,
					})
				},
				id: Identifier::from("foo"),
				expr: Some(Expr {
					kind: ExprKind::Literal(Literal::from(22)),
				})
			})
		}
	);

	stmt_parse_correctly!(
		parse_block_simple_parses_correctly,
		"{ final Integer foo = 22; return foo; }",
		Stmt {
			kind: StmtKind::Block(Block {
				kind: BlockKind::Body(vec![
					Stmt {
						kind: StmtKind::Local(Local {
							is_final: true,
							ty: Ty {
								array: false,
								kind: TyKind::Primitive(Primitive {
									kind: PrimitiveType::Integer,
								})
							},
							id: Identifier::from("foo"),
							expr: Some(Expr {
								kind: ExprKind::Literal(Literal::from(22)),
							})
						})
					},
					Stmt {
						kind: StmtKind::Return(Expr {
							kind: ExprKind::Identifier(Identifier::from("foo")),
						}),
					}
				])
			})
		}
	);

	stmt_parse_correctly!(
		parse_while_inline_parses_correctly,
		"while (x < 10) x++;",
		Stmt {
			kind: StmtKind::While(
				Expr {
					kind: ExprKind::Binary(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier::from("x"))
						}),
						BinOp::from("<"),
						Box::new(Expr {
							kind: ExprKind::Literal(Literal::from(10))
						})
					)
				},
				Box::new(Block {
					kind: BlockKind::Inline(Box::new(Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Postfix(
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("x"))
								}),
								PostfixOp::Inc
							))
						})
					}))
				})
			)
		}
	);

	stmt_parse_correctly!(
		parse_while_block_parses_correctly,
		"while (x < 10) { x++; }",
		Stmt {
			kind: StmtKind::While(
				Expr {
					kind: ExprKind::Binary(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier::from("x"))
						}),
						BinOp::from("<"),
						Box::new(Expr {
							kind: ExprKind::Literal(Literal::from(10))
						})
					)
				},
				Box::new(Block {
					kind: BlockKind::Body(vec![Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Postfix(
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("x"))
								}),
								PostfixOp::Inc
							))
						})
					}])
				})
			)
		}
	);

	stmt_parse_correctly!(
		parse_do_while_inline_parses_correctly,
		"do x++; while (x < 10);",
		Stmt {
			kind: StmtKind::DoWhile(
				Box::new(Block {
					kind: BlockKind::Inline(Box::new(Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Postfix(
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("x"))
								}),
								PostfixOp::Inc
							))
						})
					}))
				}),
				Expr {
					kind: ExprKind::Binary(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier::from("x"))
						}),
						BinOp::from("<"),
						Box::new(Expr {
							kind: ExprKind::Literal(Literal::from(10))
						})
					)
				}
			)
		}
	);

	stmt_parse_correctly!(
		parse_do_while_block_parses_correctly,
		"do { x++; } while (x < 10);",
		Stmt {
			kind: StmtKind::DoWhile(
				Box::new(Block {
					kind: BlockKind::Body(vec![Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Postfix(
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("x"))
								}),
								PostfixOp::Inc
							))
						})
					}])
				}),
				Expr {
					kind: ExprKind::Binary(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier::from("x"))
						}),
						BinOp::from("<"),
						Box::new(Expr {
							kind: ExprKind::Literal(Literal::from(10))
						})
					)
				}
			)
		}
	);

	stmt_parse_correctly!(
		parse_for_each_parses_correctly,
		r#"for (Integer i : ints) {
			sum += i;
		}"#,
		Stmt {
			kind: StmtKind::ForEach(
				Ty {
					array: false,
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveType::Integer,
					})
				},
				Identifier::from("i"),
				Expr {
					kind: ExprKind::Identifier(Identifier::from("ints"))
				},
				Box::new(Block {
					kind: BlockKind::Body(vec![Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Variable(
								Identifier::from("sum"),
								AssignOp::Add,
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("i"))
								})
							))
						})
					}])
				})
			)
		}
	);

	stmt_parse_correctly!(
		parse_for_iter_parses_correctly,
		r#"for (Integer i = 0; i < 100; i++) {
			sum += i;
		}"#,
		Stmt {
			kind: StmtKind::ForIter(
				Some(vec![Stmt {
					kind: StmtKind::Local(Local {
						is_final: true,
						ty: Ty {
							array: false,
							kind: TyKind::Primitive(Primitive {
								kind: PrimitiveType::Integer,
							})
						},
						id: Identifier::from("foo"),
						expr: Some(Expr {
							kind: ExprKind::Literal(Literal::from(22)),
						})
					})
				}]),
				Some(Expr {
					kind: ExprKind::Binary(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier::from("i"))
						}),
						BinOp::Le,
						Box::new(Expr {
							kind: ExprKind::Literal(Literal::from(100))
						})
					)
				}),
				Some(vec![Stmt {
					kind: StmtKind::Expr(Expr {
						kind: ExprKind::Assignment(Assignment::Postfix(
							Box::new(Expr {
								kind: ExprKind::Identifier(Identifier::from("i"))
							}),
							PostfixOp::Inc
						))
					})
				}]),
				Box::new(Block {
					kind: BlockKind::Body(vec![Stmt {
						kind: StmtKind::Expr(Expr {
							kind: ExprKind::Assignment(Assignment::Variable(
								Identifier::from("sum"),
								AssignOp::Add,
								Box::new(Expr {
									kind: ExprKind::Identifier(Identifier::from("i"))
								})
							))
						})
					}])
				})
			)
		}
	);
}
