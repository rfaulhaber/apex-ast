use super::identifier::Identifier;
use super::literal::*;
use super::ops::*;
use super::soql::*;
use super::sosl::*;
use super::ty::*;

use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

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
	Postfix(Box<Expr>, PostfixOp),

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
	CastExpr(Ty, Box<Expr>),

	// instanceof expressions, like: `x instanceof Account`
	InstanceOf(Identifier, Ty),

	// direct list array access, like `foo[2]`
	ListAccess(Identifier, Box<Expr>),

	// a soql query expression
	SoqlQuery(SoqlQuery),

	// a sosl query expression
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

pub fn parse_expr(p: Pair<Rule>) -> Expr {
	p.into()
}

// needs to contain all forms of Rule::expression and children
impl<'a> From<Pair<'a, Rule>> for Expr {
	fn from(pair: Pair<Rule>) -> Expr {
		match pair.as_rule() {
			Rule::infix_expr => parse_infix_expr(pair),
			Rule::ternary_expr => parse_ternary_expr(pair),
			Rule::expr_inner => parse_expr_inner(pair),
			// TODO add all children of expr_inner, ternary_expr, and infix
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
			Rule::NULL => Expr {
				kind: ExprKind::Literal(LiteralKind::Null.into()),
			},
			_ => unimplemented!("got this rule: {:?}", pair.as_rule()),
		}
	}
}

// parse pair after having gone into it already
fn parse_pair(pair: Pair<Rule>) -> Expr {
	match pair.as_rule() {
		Rule::identifier | Rule::literal => pair.into(),
		_ => unimplemented!(),
	}
}

fn parse_ternary_expr(pair: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_infix_expr(pair: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_expr_inner(pair: Pair<Rule>) -> Expr {
	let inner = pair.into_inner().next().unwrap();
	match inner.as_rule() {
		Rule::braced_expr => ExprKind::Braced(Box::new(inner.into())).into(),
		Rule::property_access => {
			ExprKind::PropAccess(inner.into_inner().map(|expr| expr.into()).collect()).into()
		}
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

fn parse_list_access(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();
	let id = Identifier::from(inner.next().unwrap());
	let inner_expr = parse_pair(inner.next().unwrap());

	let kind = ExprKind::ListAccess(id, Box::new(inner_expr));

	Expr { kind }
}

fn parse_unary_expr(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.clone().into_inner();
	let first = inner.next().unwrap();

	match first.as_rule() {
		Rule::inc_dec_prefix => parse_inc_dec_prefix(first),
		Rule::inc_dec_postfix => parse_inc_dec_postfix(first),
		Rule::unary_operator => {
			let unary_inner = first.into_inner();
			let op = UnOp::from(unary_inner.as_str());
			let expr = parse_pair(inner.next().unwrap());

			let kind = ExprKind::Unary(op, Box::new(expr));
			Expr { kind }
		}
		_ => unreachable!(),
	}
}

fn parse_inc_dec_prefix(pair: Pair<Rule>) -> Expr {
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

fn parse_inc_dec_postfix(pair: Pair<Rule>) -> Expr {
	let mut inner = pair.into_inner();

	let right_pair = inner.next().unwrap();

	let postfix = match right_pair.as_rule() {
		Rule::list_access => parse_list_access(right_pair),
		Rule::identifier => right_pair.into(),
		_ => unreachable!(),
	};

	let op = PostfixOp::from(inner.next().unwrap().as_str());

	ExprKind::Postfix(Box::new(postfix), op).into()
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

#[cfg(test)]
mod expr_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

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

	#[test]
	fn simple_unary_op_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "!foo").unwrap();
		let item = parsed.next().unwrap();

		let expr: Expr = item.clone().into();

		// TODO this sucks, find a better way to handle this
		match expr.kind {
			ExprKind::Unary(op, expr) => match op {
				UnOp::Not => match expr.kind {
					ExprKind::Identifier(id) => assert_eq!(id.name, "foo"),
					_ => panic!("wrong exprkind found: {:?}", expr.kind),
				},
				_ => panic!("op was not correct"),
			},
			_ => panic!("was not unary expr"),
		}
	}

	#[test]
	fn simple_postfix_op_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::expr_inner, "foo++").unwrap();
		let item = parsed.next().unwrap();

		let expr: Expr = item.clone().into();

		match expr.kind {
			ExprKind::Postfix(expr, op) => match op {
				PostfixOp::Inc => match expr.kind {
					ExprKind::Identifier(id) => assert_eq!(id.name, "foo"),
					_ => panic!("wrong exprkind found: {:?}", expr.kind),
				},
				_ => panic!("op was not correct"),
			},
			_ => panic!("was not unary expr"),
		}
	}

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
}
