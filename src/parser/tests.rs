use super::parse::*;
use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::expr::*;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use crate::ast::ops::*;
use crate::ast::ty::*;
use pest::iterators::Pair;

use pretty_assertions::assert_eq;

fn test_parse<F, E>(rule: Rule, input: &str, parse: F, expected: E)
where
	F: FnOnce(Pair<Rule>) -> E,
	E: std::fmt::Debug + PartialEq,
{
	let mut parsed = GrammarParser::parse(rule, input).unwrap();
	let result = parse(parsed.next().unwrap());

	assert_eq!(expected, result);
}

#[test]
fn annotation_base_parses() {
	test_parse(
		Rule::annotation,
		"@AuraEnabled",
		parse_annotation,
		Annotation {
			name: Identifier {
				name: String::from("AuraEnabled"),
			},
			keypairs: None,
		},
	)
}

#[test]
fn annotation_with_attributes_parses() {
	test_parse(
		Rule::annotation,
		"@AuraEnabled(continuation=true cacheable=true)",
		parse_annotation,
		Annotation {
			name: Identifier {
				name: String::from("AuraEnabled"),
			},
			keypairs: Some(vec![
				(Identifier::from("continuation"), Literal::from(true)),
				(Identifier::from("cacheable"), Literal::from(true)),
			]),
		},
	)
}

#[test]
fn null_literal_parse_correctly() {
	test_parse(
		Rule::literal,
		"null",
		parse_literal,
		Literal {
			kind: LiteralKind::Null,
		},
	)
}

#[test]
fn integer_literal_parse_correctly() {
	test_parse(
		Rule::literal,
		"2",
		parse_literal,
		Literal {
			kind: LiteralKind::Integer(2),
		},
	)
}

#[test]
fn string_literal_parse_correctly() {
	let expected_str = "\'hello world\'";
	test_parse(
		Rule::literal,
		expected_str,
		parse_literal,
		Literal {
			kind: LiteralKind::String(String::from(expected_str)),
		},
	)
}

#[test]
fn identifier_parses_correctly() {
	test_parse(
		Rule::identifier,
		"x",
		parse_identifier,
		Identifier {
			name: String::from("x"),
		},
	);
}

#[test]
fn ty_primitive_parses() {
	test_parse(
		Rule::basic_type,
		"Integer",
		parse_ty,
		Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::Integer,
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_primitive_array_parses() {
	test_parse(
		Rule::basic_type,
		"Integer[]",
		parse_ty,
		Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::Integer,
				is_array: true,
			}),
		},
	)
}

#[test]
fn ty_class_parses() {
	test_parse(
		Rule::basic_type,
		"Foo",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: None,
				type_arguments: None,
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_class_array_parses() {
	test_parse(
		Rule::basic_type,
		"Foo[]",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: None,
				type_arguments: None,
				is_array: true,
			}),
		},
	)
}

#[test]
fn ty_subclass_parses() {
	test_parse(
		Rule::basic_type,
		"Foo.Bar",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: Some(Identifier::from("Bar")),
				type_arguments: None,
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_basic_generic_parses() {
	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Bar"),
			subclass: None,
			type_arguments: None,
			is_array: false,
		}),
	};

	test_parse(
		Rule::basic_type,
		"Foo<Bar>",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: None,
				type_arguments: Some((Box::new(subtype), None)),
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_generic_subtype_parses() {
	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Bar"),
			subclass: Some(Identifier::from("Baz")),
			type_arguments: None,
			is_array: false,
		}),
	};

	test_parse(
		Rule::basic_type,
		"Foo<Bar.Baz>",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: None,
				type_arguments: Some((Box::new(subtype), None)),
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_generic_subtype_array_parses() {
	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Bar"),
			subclass: Some(Identifier::from("Baz")),
			type_arguments: None,
			is_array: false,
		}),
	};

	test_parse(
		Rule::basic_type,
		"Foo<Bar.Baz>[]",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: None,
				type_arguments: Some((Box::new(subtype), None)),
				is_array: true,
			}),
		},
	)
}

#[test]
fn ty_subtype_generic_parses() {
	let gen_type = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Baz"),
			subclass: None,
			type_arguments: None,
			is_array: false,
		}),
	};

	let type_args = (Box::new(gen_type), None);

	test_parse(
		Rule::basic_type,
		"Foo.Bar<Baz>",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Foo"),
				subclass: Some(Identifier::from("Bar")),
				type_arguments: Some(type_args),
				is_array: false,
			}),
		},
	)
}

#[test]
fn ty_two_type_args_parses() {
	let id_type = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::ID,
			is_array: false,
		}),
	};

	let string_type = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::String,
			is_array: false,
		}),
	};

	test_parse(
		Rule::basic_type,
		"Map<Id, String>",
		parse_ty,
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier::from("Map"),
				subclass: None,
				type_arguments: Some((Box::new(id_type), Some(Box::new(string_type)))),
				is_array: false,
			}),
		},
	)
}

#[test]
fn expr_literal_parses() {
	test_parse(
		Rule::expression,
		"2",
		parse_expr,
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(2),
			}),
		},
	)
}

#[test]
fn expr_identifier_parses() {
	test_parse(
		Rule::expression,
		"foo",
		parse_expr,
		Expr {
			kind: ExprKind::Identifier(Identifier::from("foo")),
		},
	)
}

#[test]
fn expr_type_expr_parses() {
	test_parse(
		Rule::expression,
		"Foo.class",
		parse_expr,
		Expr {
			kind: ExprKind::Type(Ty {
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					name: Identifier::from("Foo"),
					is_array: false,
					subclass: None,
					type_arguments: None,
				}),
			}),
		},
	)
}

#[test]
fn instanceof_expr_parses() {
	let id = Identifier::from("foo");
	let ty: Ty = ClassOrInterface {
		name: Identifier::from("Foo"),
		subclass: None,
		type_arguments: None,
		is_array: false,
	}
	.into();

	test_parse(
		Rule::expression,
		"foo instanceof Foo",
		parse_expr,
		Expr {
			kind: ExprKind::Instanceof(id, ty),
		},
	)
}

#[test]
fn cast_expr_parses() {
	let ty: Ty = Primitive {
		kind: PrimitiveKind::String,
		is_array: false,
	}
	.into();

	let expr: Expr = ExprKind::Identifier(Identifier::from("foo")).into();

	test_parse(
		Rule::expression,
		"(String) foo",
		parse_expr,
		Expr {
			kind: ExprKind::Cast(ty, Box::new(expr)),
		},
	)
}

#[test]
fn prefix_expr_parses() {
	let inner: Expr = ExprKind::Identifier(Identifier::from("i")).into();

	test_parse(
		Rule::expression,
		"++i",
		parse_expr,
		Expr {
			kind: ExprKind::Prefix(IncDecOp::Inc, Box::new(inner)),
		},
	)
}

#[test]
fn postfix_expr_parses() {
	let inner: Expr = ExprKind::Identifier(Identifier::from("i")).into();

	test_parse(
		Rule::expression,
		"i++",
		parse_expr,
		Expr {
			kind: ExprKind::Postfix(Box::new(inner), IncDecOp::Inc),
		},
	)
}

#[test]
fn unary_expr_parses() {
	let inner: Expr = ExprKind::Identifier(Identifier::from("i")).into();

	test_parse(
		Rule::expression,
		"!i",
		parse_expr,
		Expr {
			kind: ExprKind::Unary(UnOp::Not, Box::new(inner)),
		},
	)
}

#[test]
fn method_call_no_args_parses() {
	test_parse(
		Rule::expression,
		"foo()",
		parse_expr,
		Expr {
			kind: ExprKind::Call(Identifier::from("foo"), None),
		},
	)
}

#[test]
fn method_call_one_arg_parses() {
	test_parse(
		Rule::expression,
		"foo(bar)",
		parse_expr,
		Expr {
			kind: ExprKind::Call(
				Identifier::from("foo"),
				Some(vec![ExprKind::Identifier(Identifier::from("bar")).into()]),
			),
		},
	)
}

#[test]
fn method_call_two_args_parses() {
	test_parse(
		Rule::expression,
		"foo(bar, \'baz\')",
		parse_expr,
		Expr {
			kind: ExprKind::Call(
				Identifier::from("foo"),
				Some(vec![
					ExprKind::Identifier(Identifier::from("bar")).into(),
					ExprKind::Literal(Literal::from("\'baz\'")).into(),
				]),
			),
		},
	)
}

#[test]
fn new_inst_array_literal_parses() {
	let ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
	};

	let literal_exprs = vec![
		Expr::from(Literal::from(1)),
		Expr::from(Literal::from(2)),
		Expr::from(Literal::from(3)),
	];

	test_parse(
		Rule::expression,
		"new Integer[1, 2, 3]",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Array(literal_exprs))),
		},
	)
}

#[test]
fn new_inst_collection_literal_parses() {
	let int_ty = Ty::from(PrimitiveKind::Integer);

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("List"),
			subclass: None,
			type_arguments: Some((Box::new(int_ty), None)),
			is_array: false,
		}),
	};

	let literal_exprs = vec![
		Expr::from(Literal::from(1)),
		Expr::from(Literal::from(2)),
		Expr::from(Literal::from(3)),
	];

	test_parse(
		Rule::expression,
		"new List<Integer>{1, 2, 3}",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Collection(literal_exprs))),
		},
	)
}

#[test]
fn new_inst_collection_literal_with_args_parses() {
	let int_ty = Ty::from(PrimitiveKind::Integer);

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("List"),
			subclass: None,
			type_arguments: Some((Box::new(int_ty), None)),
			is_array: false,
		}),
	};

	let args = Some(vec![Expr::from(Identifier::from("list"))]);

	test_parse(
		Rule::expression,
		"new List<Integer>(list)",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(args)))),
		},
	)
}

#[test]
fn new_inst_map_literal_parses() {
	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Map"),
			subclass: None,
			type_arguments: type_args!(
				Ty::from(PrimitiveKind::Integer),
				Ty::from(PrimitiveKind::String)
			),
			is_array: false,
		}),
	};

	let mapping = vec![
		(
			Expr::from(Literal::from(1)),
			Expr::from(Literal::from("'one'")),
		),
		(
			Expr::from(Literal::from(2)),
			Expr::from(Literal::from("'two'")),
		),
	];

	test_parse(
		Rule::expression,
		"new Map<Integer, String>{1 => 'one', 2 => 'two'}",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Map(mapping))),
		},
	)
}

#[test]
fn new_inst_map_args_parses() {
	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier::from("Map"),
			subclass: None,
			type_arguments: type_args!(
				Ty::from(PrimitiveKind::Integer),
				Ty::from(PrimitiveKind::String)
			),
			is_array: false,
		}),
	};

	let class_args = vec![Expr::from(Identifier::from("foo"))];

	test_parse(
		Rule::expression,
		"new Map<Integer, String>(foo)",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(Some(class_args))))),
		},
	)
}

#[test]
fn new_inst_class_parses() {
	let ty = Ty::from(Identifier::from("Foo"));

	test_parse(
		Rule::expression,
		"new Foo()",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(None)))),
		},
	);
}

#[test]
fn new_inst_class_sobject_argsparses() {
	let ty = Ty::from(Identifier::from("Account"));

	let args = vec![(Identifier::from("Name"), Expr::from(Literal::from("'foo'")))];

	test_parse(
		Rule::expression,
		"new Account(Name = 'foo')",
		parse_expr,
		Expr {
			kind: ExprKind::New(ty, Some(NewType::Class(ClassArgs::SObject(args)))),
		},
	);
}

#[test]
fn list_access_parses() {
	let accessible = Expr::from(Identifier::from("foo"));
	let access_expr = Expr::from(Literal::from(2));

	test_parse(
		Rule::expression,
		"foo[2]",
		parse_expr,
		Expr {
			kind: ExprKind::ListAccess(Box::new(accessible), Box::new(access_expr)),
		},
	);
}

#[test]
fn property_access_parses() {
	let accessible = Expr::from(Identifier::from("foo"));
	let selector = Expr {
		kind: ExprKind::Call(Identifier::from("bar"), None),
	};

	test_parse(
		Rule::expression,
		"foo.bar()",
		parse_expr,
		Expr {
			kind: ExprKind::PropertyAccess(Box::new(accessible), Box::new(selector)),
		},
	);
}

#[test]
fn simple_query_parses() {
	let query_str = "[ SELECT Foo FROM Bar ]";
	test_parse(
		Rule::expression,
		query_str,
		parse_expr,
		Expr {
			kind: ExprKind::Query(QueryKind::Soql, String::from("SELECT Foo FROM Bar")),
		},
	);
}
