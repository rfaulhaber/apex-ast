use super::identifier::Identifier;
use super::literal::*;
use super::ops::*;
use super::soql::*;
use super::sosl::*;
use super::ty::*;

use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

// TODO parse assignement expressions

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
	pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
	/// any expression surrounded by parentheses, like `(1 + 2)`
	Braced(Box<Expr>),

	/// a binary operation, like `x + 2`
	Binary(Box<Expr>, BinOp, Box<Expr>),

	/// a unary operation, like `!x`. Excludes increment or decrement expressions.
	Unary(UnOp, Box<Expr>),

	/// Either an increment or decrement expression.
	/// This has to be a different type of enum because they can either come
	/// before or after the expression.

	/// a literal, such as a number or string
	Literal(Literal),

	// an identifier
	Identifier(Identifier),

	// a series of expressions joined by "."
	PropAccess(Vec<Expr>),

	// direct method call, like foo(2 + 3, x)
	Call(Identifier, Option<Vec<Expr>>),

	// new object instantiation, like "new Foo(one, 2)" or "new Account(Name = 'Foo');"
	New(Ty, Option<NewType>),

	// ternary expression, like the right-hand side of:
	// String foo = isTrue() ? 'Yes' : 'No';
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

	// a cast expression, like `(String) list.get(0)`
	Cast(Ty, Box<Expr>),

	// instanceof expressions, like: `x instanceof Account`
	InstanceOf(Identifier, Ty),

	// direct list array access, like `foo[2]`
	ListAccess(Identifier, Box<Expr>),

	// this is "simple" assignment, anything that looks like `x = 2` or
	// `foo = new Foo()`
	Assignment(Assignment),

	// a soql query expression
	// TODO change to Box?
	SoqlQuery(SoqlQuery),

	// a sosl query expression
	// TODO change to Box?
	SoslQuery(SoslQuery),
}

impl Into<Expr> for ExprKind {
	fn into(self) -> Expr {
		Expr { kind: self }
	}
}

// this type is only constructed if there's any arguments passed,
// otherwise it's None
#[derive(Debug, Clone, PartialEq)]
pub enum NewType {
	// for Map literals, such as `new Map<Integer, String>{1 => 'one'}`
	Map(Vec<(Expr, Expr)>),
	// for List literals, such as `new List<Integer>{1, 2}`
	List(Vec<Expr>),
	// for Array literals, such as `new Integer[6]`
	Array(Vec<Expr>),
	// for everything else, including Lists, Sets, and Maps. ex. `new Foo(one, 2 + three)`
	Args(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assignment {
	Variable(Identifier, AssignOp, Box<Expr>),
	Array(Identifier, Box<Expr>, AssignOp, Box<Expr>),
	// NOTE: rename "PostfixOp"
	// TODO: can this be more precise?
	Prefix(PostfixOp, Box<Expr>),
	Postfix(Box<Expr>, PostfixOp),
}

pub fn is_expr(r: Rule) -> bool {
	r == Rule::expr_inner || r == Rule::infix_expr || r == Rule::ternary_expr
}

// needs to contain all forms of Rule::expression and children
impl<'a> From<Pair<'a, Rule>> for Expr {
	fn from(pair: Pair<Rule>) -> Expr {
		match pair.as_rule() {
			Rule::braced_expr => parse_braced_expr(pair),
			Rule::infix_expr => parse_infix_expr(pair),
			Rule::ternary_expr => parse_ternary_expr(pair),
			Rule::assignment_expr => parse_assignment_expr(pair),
			Rule::expr_inner => parse_expr_inner(pair),
			Rule::property_access => parse_property_access(pair),
			Rule::method_invocation => parse_method_invocation(pair),
			Rule::new_instance_declaration => parse_new_instance_declaration(pair),
			Rule::identifier => Expr {
				kind: ExprKind::Identifier(pair.as_str().into()),
			},
			Rule::string_literal => Expr {
				kind: ExprKind::Literal(pair.as_str().into()),
			},
			Rule::float_literal => Expr {
				kind: ExprKind::Literal(pair.as_str().parse::<f64>().unwrap().into()),
			},
			Rule::long_literal => Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Long(pair.as_str().parse::<i64>().unwrap()),
				}),
			},
			Rule::integer_literal => ExprKind::Literal(
				LiteralKind::Integer(pair.as_str().parse::<i64>().unwrap()).into(),
			)
			.into(),
			Rule::boolean_literal => {
				match pair.into_inner().next().unwrap().as_rule() {
					Rule::TRUE => ExprKind::Literal(LiteralKind::Boolean(true).into()).into(),
					Rule::FALSE => ExprKind::Literal(LiteralKind::Boolean(false).into()).into(),
					_ => unreachable!(), // we hope!
				}
			}
			Rule::query_expression => unimplemented!("query expressions are unimplemented"),
			Rule::NULL => Expr {
				kind: ExprKind::Literal(LiteralKind::Null.into()),
			},
			_ => unreachable!("got this rule: {:?}", pair.as_rule()),
		}
	}
}

// parse pair after having gone into it already
// TODO remove
fn parse_pair(pair: Pair<Rule>) -> Expr {
	match pair.as_rule() {
		Rule::identifier | Rule::literal => pair.into(),
		_ => unimplemented!(),
	}
}

fn parse_ternary_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let expr_test = Expr::from(inner.next().unwrap());
	let expr_true = Expr::from(inner.next().unwrap());
	let expr_false = Expr::from(inner.next().unwrap());

	Expr {
		kind: ExprKind::Ternary(
			Box::new(expr_test),
			Box::new(expr_true),
			Box::new(expr_false),
		),
	}
}

fn parse_assignment_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let first = inner.next().unwrap();

	match first.as_rule() {
		Rule::array_assignment_expr => {
			let mut arr_inner = first.into_inner();

			let id = Identifier::from(arr_inner.next().unwrap());
			let expr = Expr::from(arr_inner.next().unwrap());
			let assign_op = AssignOp::from(arr_inner.next().unwrap().as_str());
			let expr_rhs = Expr::from(arr_inner.next().unwrap());

			Expr {
				kind: ExprKind::Assignment(Assignment::Array(
					id,
					Box::new(expr),
					assign_op,
					Box::new(expr_rhs),
				)),
			}
		}
		Rule::variable_assignment_expr => {
			let mut var_inner = first.into_inner();

			let id = Identifier::from(var_inner.next().unwrap());
			let assign_op = AssignOp::from(var_inner.next().unwrap().as_str());
			let expr_rhs = Expr::from(var_inner.next().unwrap());

			Expr {
				kind: ExprKind::Assignment(Assignment::Variable(id, assign_op, Box::new(expr_rhs))),
			}
		}
		Rule::inc_dec_prefix => {
			let mut prefix_inner = first.into_inner();

			let op = PostfixOp::from(prefix_inner.next().unwrap().as_str());
			let expr = Expr::from(prefix_inner.next().unwrap());

			Expr {
				kind: ExprKind::Assignment(Assignment::Prefix(op, Box::new(expr))),
			}
		}
		Rule::inc_dec_postfix => {
			let mut postfix_inner = first.into_inner();

			let expr = Expr::from(postfix_inner.next().unwrap());
			let op = PostfixOp::from(postfix_inner.next().unwrap().as_str());

			Expr {
				kind: ExprKind::Assignment(Assignment::Postfix(Box::new(expr), op)),
			}
		}
		_ => unreachable!("unexpected rule, got {:?}", first.as_rule()),
	}
}

fn parse_infix_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let left = Expr::from(inner.next().unwrap());
	let op = BinOp::from(inner.next().unwrap().as_str());
	let right = Expr::from(inner.next().unwrap());

	Expr {
		kind: ExprKind::Binary(Box::new(left), op, Box::new(right)),
	}
}

fn parse_expr_inner(pair: Pair<Rule>) -> Expr {
	let inner = pair.into_inner().next().unwrap();
	match inner.as_rule() {
		Rule::braced_expr => parse_braced_expr(inner),
		Rule::property_access => parse_property_access(inner),
		Rule::cast_expr => parse_cast_expr(inner),
		Rule::query_expression => unimplemented!(),
		Rule::new_instance_declaration => parse_new_instance_declaration(inner),
		Rule::method_invocation => parse_method_invocation(inner),
		Rule::instanceof_expr => parse_instanceof(inner),
		Rule::unary_expr => parse_unary_expr(inner),
		Rule::list_access => parse_list_access(inner),
		Rule::literal => Expr::from(inner.into_inner().next().unwrap()),
		Rule::identifier => inner.into(),
		_ => unreachable!("got rule: {:?}", inner.as_rule()),
	}
}

fn parse_braced_expr(pair: Pair<Rule>) -> Expr {
	let inner = Expr::from(pair.into_inner().next().unwrap());

	Expr {
		kind: ExprKind::Braced(Box::new(inner)),
	}
}

fn parse_property_access(pair: Pair<Rule>) -> Expr {
	ExprKind::PropAccess(pair.into_inner().map(|expr| expr.into()).collect()).into()
}

fn parse_list_access(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();
	let id = Identifier::from(inner.next().unwrap());
	let inner_expr = Expr::from(inner.next().unwrap());

	let kind = ExprKind::ListAccess(id, Box::new(inner_expr));

	Expr { kind }
}

fn parse_unary_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.clone().into_inner();
	let first = inner.next().unwrap();

	match first.as_rule() {
		Rule::unary_operator => {
			let unary_inner = first.into_inner();
			let op = UnOp::from(unary_inner.as_str());
			let expr = parse_pair(inner.next().unwrap());

			let kind = ExprKind::Unary(op, Box::new(expr));
			Expr { kind }
		}
		_ => unreachable!("got unexpected rule: {:?}", first.as_rule()),
	}
}

pub fn parse_inc_dec_prefix(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();
	let op = UnOp::from(inner.next().unwrap().as_str());

	let right_pair = inner.next().unwrap();

	let postfix = match right_pair.as_rule() {
		Rule::list_access => parse_list_access(right_pair),
		Rule::identifier => right_pair.into(),
		_ => unreachable!(),
	};

	ExprKind::Unary(op, Box::new(postfix)).into()
}

fn parse_instanceof(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let id: Identifier = inner.next().unwrap().as_str().into();
	inner.next();

	let type_pair = inner.next().unwrap();

	let ty = Ty::from(type_pair);

	Expr {
		kind: ExprKind::InstanceOf(id, ty),
	}
}

fn parse_method_invocation(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let id = Identifier::from(inner.next().unwrap());

	let exprs: Vec<Expr> = inner.next().unwrap().into_inner().map(Expr::from).collect();

	let kind = ExprKind::Call(id, if exprs.is_empty() { None } else { Some(exprs) });

	Expr { kind }
}

fn parse_new_instance_declaration(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	// TODO keep for spans
	inner.next();

	let ty = Ty::from(inner.next().unwrap());

	let next = inner.next().unwrap();

	match next.as_rule() {
		Rule::new_map_literal => {
			let mappings: Vec<(Expr, Expr)> =
				next.into_inner().map(parse_map_literal_mapping).collect();

			Expr {
				kind: ExprKind::New(ty, Some(NewType::Map(mappings))),
			}
		}
		Rule::new_list_literal => {
			let exprs: Vec<Expr> = next.into_inner().map(Expr::from).collect();

			Expr {
				kind: ExprKind::New(ty, Some(NewType::List(exprs))),
			}
		}
		Rule::new_array_literal => {
			let exprs: Vec<Expr> = next.into_inner().map(Expr::from).collect();

			Expr {
				kind: ExprKind::New(ty, Some(NewType::Array(exprs))),
			}
		}
		Rule::call_arguments => {
			let exprs: Vec<Expr> = next.into_inner().map(Expr::from).collect();

			Expr {
				kind: ExprKind::New(
					ty,
					if exprs.is_empty() {
						None
					} else {
						Some(NewType::Args(exprs))
					},
				),
			}
		}
		_ => unreachable!("expected new argument form, got {:?}", next.as_rule()),
	}
}

fn parse_map_literal_mapping(p: Pair<Rule>) -> (Expr, Expr) {
	let mut inner = p.into_inner();

	// we have to parse Rule::literal and get the more specific rule
	let literal = Expr::from(inner.next().unwrap().into_inner().next().unwrap());
	let expr_mapping = Expr::from(inner.next().unwrap());

	(literal, expr_mapping)
}

fn parse_cast_expr(p: Pair<Rule>) -> Expr {
	let mut inner = p.into_inner();

	let ty = Ty::from(inner.next().unwrap().into_inner().next().unwrap());
	let expr = Expr::from(inner.next().unwrap());

	Expr {
		kind: ExprKind::Cast(ty, Box::new(expr)),
	}
}

#[cfg(test)]
mod expr_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;
	use pretty_assertions::{assert_eq, assert_ne};

	macro_rules! expr_parse_correctly {
		($test_name:ident, $expr_rule:ident, $parse:literal, $expected:expr) => {
			#[test]
			fn $test_name() {
				let mut parsed = GrammarParser::parse(Rule::$expr_rule, $parse).unwrap();
				let item = parsed.next().unwrap();

				let result = Expr::from(item);

				let expected = $expected;
				assert_eq!(expected, result);
			}
		};
	}

	#[test]
	fn from_pair_parses_literal_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "123").unwrap();

		let result = Expr::from(parsed.next().unwrap());

		let expected = Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(123),
			}),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn from_pair_parses_identifier_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "myVar").unwrap();
		let single_item = parsed.next().unwrap();

		let expr: Expr = single_item.clone().into();

		match expr.kind {
			ExprKind::Identifier(id) => assert_eq!(id.name, "myVar"),
			_ => panic!("invalid kind found: {:?}", single_item.as_rule()),
		}
	}

	expr_parse_correctly!(
		simple_unary_op_parses_correctly,
		expr_inner,
		"!foo",
		Expr {
			kind: ExprKind::Unary(
				UnOp::Not,
				Box::new(Expr {
					kind: ExprKind::Identifier(Identifier::from("foo"))
				})
			)
		}
	);

	expr_parse_correctly!(
		inc_dec_postfix_parses_correctly,
		assignment_expr,
		"i++",
		Expr {
			kind: ExprKind::Assignment(Assignment::Postfix(
				Box::new(Expr {
					kind: ExprKind::Identifier(Identifier::from("i"))
				}),
				PostfixOp::Inc
			))
		}
	);

	expr_parse_correctly!(
		inc_dec_prefix_parses_correctly,
		assignment_expr,
		"++i",
		Expr {
			kind: ExprKind::Assignment(Assignment::Prefix(
				PostfixOp::Inc,
				Box::new(Expr {
					kind: ExprKind::Identifier(Identifier::from("i"))
				})
			))
		}
	);

	expr_parse_correctly!(
		var_assignment_parses_correctly,
		assignment_expr,
		"x = 2",
		Expr {
			kind: ExprKind::Assignment(Assignment::Variable(
				Identifier::from("x"),
				AssignOp::Eq,
				Box::new(Expr {
					kind: ExprKind::Literal(Literal::from(2))
				})
			))
		}
	);

	expr_parse_correctly!(
		arr_assignment_parses_correctly,
		assignment_expr,
		"x[0] = 2",
		Expr {
			kind: ExprKind::Assignment(Assignment::Array(
				Identifier::from("x"),
				Box::new(Expr {
					kind: ExprKind::Literal(Literal::from(0))
				}),
				AssignOp::Eq,
				Box::new(Expr {
					kind: ExprKind::Literal(Literal::from(2))
				})
			))
		}
	);

	#[test]
	fn simple_instanceof_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "x instanceof Foo").unwrap();
		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());

		let expected = Expr {
			kind: ExprKind::InstanceOf(
				Identifier::from("x"),
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						kind: ClassOrInterfaceType::Class(Identifier::from("Foo")),
					}),
					array: false,
				},
			),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn simple_method_call_no_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo()").unwrap();
		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());

		let expected = Expr {
			kind: ExprKind::Call(Identifier::from("foo"), None),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn simple_method_call_simple_arg_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo(bar)").unwrap();
		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());

		let expected = Expr {
			kind: ExprKind::Call(
				Identifier::from("foo"),
				Some(vec![Expr {
					kind: ExprKind::Identifier(Identifier::from("bar")),
				}]),
			),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn simple_method_call_two_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo(bar, baz)").unwrap();
		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());

		let expected = Expr {
			kind: ExprKind::Call(
				Identifier::from("foo"),
				Some(vec![
					Expr {
						kind: ExprKind::Identifier(Identifier::from("bar")),
					},
					Expr {
						kind: ExprKind::Identifier(Identifier::from("baz")),
					},
				]),
			),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn new_instance_no_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "new Foo()").unwrap();
		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());

		let expected = Expr {
			kind: ExprKind::New(
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						kind: ClassOrInterfaceType::Class(Identifier::from("Foo")),
					}),
					array: false,
				},
				None,
			),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn new_instance_with_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "new Foo(one, 'two', 3)").unwrap();

		let item = parsed.next().unwrap();

		let expr = Expr::from(item.clone());
		let expected_arg_1 = Expr {
			kind: ExprKind::Identifier(Identifier::from("one")),
		};

		let expected_arg_2 = Expr {
			kind: ExprKind::Literal(Literal::from("\'two\'")),
		};

		let expected_arg_3 = Expr {
			kind: ExprKind::Literal(Literal::from(3)),
		};

		let new_type = NewType::Args(vec![expected_arg_1, expected_arg_2, expected_arg_3]);

		let expected = Expr {
			kind: ExprKind::New(
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						kind: ClassOrInterfaceType::Class(Identifier::from("Foo")),
					}),
					array: false,
				},
				Some(new_type),
			),
		};

		assert_eq!(expected, expr);
	}

	#[test]
	fn new_instance_list_type_no_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "new List<Foo>()").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let foo_ty = Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				kind: ClassOrInterfaceType::Class(Identifier::from("Foo")),
			}),
			array: false,
		};

		let expected_ty = Ty {
			kind: TyKind::Collection(Collection {
				kind: CollectionType::List(Box::new(foo_ty)),
			}),
			array: false,
		};

		let expected = Expr {
			kind: ExprKind::New(expected_ty, None),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn new_instance_list_type_with_arg_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "new List<Foo>(bar)").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let foo_ty = Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				kind: ClassOrInterfaceType::Class(Identifier::from("Foo")),
			}),
			array: false,
		};

		let expected_ty = Ty {
			kind: TyKind::Collection(Collection {
				kind: CollectionType::List(Box::new(foo_ty)),
			}),
			array: false,
		};

		let expected_newtype = NewType::Args(vec![Expr {
			kind: ExprKind::Identifier(Identifier::from("bar")),
		}]);

		let expected = Expr {
			kind: ExprKind::New(expected_ty, Some(expected_newtype)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn new_instance_list_type_literal_parses_correctly() {
		let mut parsed =
			GrammarParser::parse(Rule::expr_inner, "new List<Integer>{one, 2}").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let int_ty = Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveType::Integer,
			}),
			array: false,
		};

		let expected_ty = Ty {
			kind: TyKind::Collection(Collection {
				kind: CollectionType::List(Box::new(int_ty)),
			}),
			array: false,
		};

		let expected_args = vec![
			Expr {
				kind: ExprKind::Identifier(Identifier::from("one")),
			},
			Expr {
				kind: ExprKind::Literal(Literal::from(2)),
			},
		];

		let expected_newtype = NewType::List(expected_args);

		let expected = Expr {
			kind: ExprKind::New(expected_ty, Some(expected_newtype)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn new_instance_array_type_literal_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "new Integer[6]").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let expected_ty = Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveType::Integer,
			}),
			array: false,
		};

		let expected_args = vec![Expr {
			kind: ExprKind::Literal(Literal::from(6)),
		}];

		let expected_newtype = NewType::Array(expected_args);

		let expected = Expr {
			kind: ExprKind::New(expected_ty, Some(expected_newtype)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn new_instance_map_type_literal_parses_correctly() {
		let mut parsed = GrammarParser::parse(
			Rule::expr_inner,
			"new Map<Id, String>{1 => 'one', 2 => 'two'}",
		)
		.unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let id_type = Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveType::ID,
			}),
			array: false,
		};

		let string_type = Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveType::String,
			}),
			array: false,
		};

		let expected_ty = Ty {
			kind: TyKind::Collection(Collection {
				kind: CollectionType::Map(Box::new(id_type), Box::new(string_type)),
			}),
			array: false,
		};

		let expected_args = vec![
			(
				Expr {
					kind: ExprKind::Literal(Literal::from(1)),
				},
				Expr {
					kind: ExprKind::Literal(Literal::from("\'one\'")),
				},
			),
			(
				Expr {
					kind: ExprKind::Literal(Literal::from(2)),
				},
				Expr {
					kind: ExprKind::Literal(Literal::from("\'two\'")),
				},
			),
		];

		let expected_newtype = NewType::Map(expected_args);

		let expected = Expr {
			kind: ExprKind::New(expected_ty, Some(expected_newtype)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn infix_expr_simple_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::infix_expr, "x * 2").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let left = Expr {
			kind: ExprKind::Identifier(Identifier::from("x")),
		};

		let right = Expr {
			kind: ExprKind::Literal(Literal::from(2)),
		};

		let op = BinOp::Mul;

		let expected = Expr {
			kind: ExprKind::Binary(Box::new(left), op, Box::new(right)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn infix_expr_long_expr_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::infix_expr, "pi * Math.pow(r, 2)").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let left = Expr {
			kind: ExprKind::Identifier(Identifier::from("pi")),
		};

		let right = Expr {
			kind: ExprKind::PropAccess(vec![
				Expr {
					kind: ExprKind::Identifier(Identifier::from("Math")),
				},
				Expr {
					kind: ExprKind::Call(
						Identifier::from("pow"),
						Some(vec![
							Expr {
								kind: ExprKind::Identifier(Identifier::from("r")),
							},
							Expr {
								kind: ExprKind::Literal(Literal::from(2)),
							},
						]),
					),
				},
			]),
		};

		let expected = Expr {
			kind: ExprKind::Binary(Box::new(left), BinOp::Mul, Box::new(right)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn list_access_simple_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "list[2]").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let expected = Expr {
			kind: ExprKind::ListAccess(
				Identifier::from("list"),
				Box::new(Expr {
					kind: ExprKind::Literal(Literal::from(2)),
				}),
			),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn property_access_simple_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo.bar").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let expected = Expr {
			kind: ExprKind::PropAccess(vec![
				Expr {
					kind: ExprKind::Identifier(Identifier::from("foo")),
				},
				Expr {
					kind: ExprKind::Identifier(Identifier::from("bar")),
				},
			]),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn ternary_op_simple_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::ternary_expr, "foo ? 'bar' : 42").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let expected_test = Expr {
			kind: ExprKind::Identifier(Identifier::from("foo")),
		};

		let expected_true = Expr {
			kind: ExprKind::Literal(Literal::from("'bar'")),
		};

		let expected_false = Expr {
			kind: ExprKind::Literal(Literal::from(42)),
		};

		let ternary = ExprKind::Ternary(
			Box::new(expected_test),
			Box::new(expected_true),
			Box::new(expected_false),
		);

		let expected = Expr { kind: ternary };

		assert_eq!(expected, result);
	}

	#[test]
	fn cast_exprs_simple_parse_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "(String) obj.get('foo')").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let expected_ty = Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveType::String,
			}),
			array: false,
		};

		let expected_expr = Expr {
			kind: ExprKind::PropAccess(vec![
				Expr {
					kind: ExprKind::Identifier(Identifier::from("obj")),
				},
				Expr {
					kind: ExprKind::Call(
						Identifier::from("get"),
						Some(vec![Expr {
							kind: ExprKind::Literal(Literal::from("'foo'")),
						}]),
					),
				},
			]),
		};

		let expected = Expr {
			kind: ExprKind::Cast(expected_ty, Box::new(expected_expr)),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn simple_braced_expr_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "(x * 2)").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let left = Expr {
			kind: ExprKind::Identifier(Identifier::from("x")),
		};

		let right = Expr {
			kind: ExprKind::Literal(Literal::from(2)),
		};

		let op = BinOp::Mul;

		let inner = Box::new(Expr {
			kind: ExprKind::Binary(Box::new(left), op, Box::new(right)),
		});

		let expected = Expr {
			kind: ExprKind::Braced(inner),
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn nested_braced_expr_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expression, "x + (y * 2)").unwrap();
		let item = parsed.next().unwrap();

		let result = Expr::from(item);

		let left = Expr {
			kind: ExprKind::Identifier(Identifier::from("x")),
		};

		let left_inner = Expr {
			kind: ExprKind::Identifier(Identifier::from("y")),
		};

		let right_inner = Expr {
			kind: ExprKind::Literal(Literal::from(2)),
		};

		let op = BinOp::Mul;

		let right = Box::new(Expr {
			kind: ExprKind::Binary(Box::new(left_inner), op, Box::new(right_inner)),
		});

		let braced_right = Expr {
			kind: ExprKind::Braced(right),
		};

		let expected = Expr {
			kind: ExprKind::Binary(Box::new(left), BinOp::Add, Box::new(braced_right)),
		};

		assert_eq!(expected, result);
	}

	expr_parse_correctly!(
		assignment_parses_correctly,
		assignment_expr,
		"x = 2",
		Expr {
			kind: ExprKind::Assignment(Assignment::Variable(
				Identifier::from("x"),
				AssignOp::Eq,
				Box::new(Expr {
					kind: ExprKind::Literal(Literal::from(2))
				})
			))
		}
	);
}
