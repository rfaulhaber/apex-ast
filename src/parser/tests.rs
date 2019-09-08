use super::parse::*;
use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::expr::*;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use crate::ast::ops::*;
use crate::ast::stmt::*;
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
fn for_basic_simple_parses() {
	let input = "for (Integer i = 0; i < 10; i++) sum += i;";

	let expected = Stmt {
		kind: StmtKind::For(ForStmt::Basic(
			Some(vec![StmtExpr::Local(Local {
				annotation: None,
				is_final: false,
				ty: Ty::from(PrimitiveKind::Integer),
				id: Identifier::from("i"),
				rhs: Some(Expr {
					kind: ExprKind::Literal(Literal::from(0)),
				}),
			})]),
			Some(Expr {
				kind: ExprKind::Infix(
					Box::new(Expr::from(Identifier::from("i"))),
					BinOp::Lt,
					Box::new(Expr::from(Literal::from(10))),
				),
			}),
			Some(StmtExpr::Expr(Expr {
				kind: ExprKind::Postfix(Box::new(Expr::from(Identifier::from("i"))), IncDecOp::Inc),
			})),
			Box::new(Block::Inline(Box::new(Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr::from(Identifier::from("sum"))),
						AssignOp::Add,
						Box::new(Expr::from(Identifier::from("i"))),
					),
				})),
			}))),
		)),
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn for_enhanced_parses() {
	let input = "for (Integer i : numbers) sum += i;";

	let expected = Stmt {
		kind: StmtKind::For(ForStmt::Enhanced(
			Ty::from(PrimitiveKind::Integer),
			Identifier::from("i"),
			Expr::from(Identifier::from("numbers")),
			Box::new(Block::Inline(Box::new(Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr::from(Identifier::from("sum"))),
						AssignOp::Add,
						Box::new(Expr::from(Identifier::from("i"))),
					),
				})),
			}))),
		)),
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn do_while_parses() {
	test_parse(
		Rule::statement,
		r#"do {
			return x;
		} while (true);"#,
		parse_stmt,
		Stmt {
			kind: StmtKind::DoWhile(
				Box::new(Block::from(vec![Stmt {
					kind: StmtKind::Return(Some(Expr::from(Identifier::from("x")))),
				}])),
				Expr::from(Literal::from(true)),
			),
		},
	);
}

#[test]
fn while_parses() {
	test_parse(
		Rule::statement,
		r#"while (true) {
		return x; 
	}"#,
		parse_stmt,
		Stmt {
			kind: StmtKind::While(
				Expr::from(Literal::from(true)),
				Box::new(Block::from(vec![Stmt {
					kind: StmtKind::Return(Some(Expr::from(Identifier::from("x")))),
				}])),
			),
		},
	)
}

#[test]
fn if_else_if_else_parses() {
	let input = r#"if (foo) {
		return bar;
	} else if (bar) {
		return baz;
	} else {
		return quux;
	}"#;

	let expected_kind = StmtKind::If(
		Expr::from(Identifier::from("foo")),
		Box::new(Block::from(vec![Stmt {
			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
		}])),
		Some(vec![(
			Expr::from(Identifier::from("bar")),
			Box::new(Block::from(vec![Stmt {
				kind: StmtKind::Return(Some(Expr::from(Identifier::from("baz")))),
			}])),
		)]),
		Some(Box::new(Block::from(vec![Stmt {
			kind: StmtKind::Return(Some(Expr::from(Identifier::from("quux")))),
		}]))),
	);

	test_parse(
		Rule::statement,
		input,
		parse_stmt,
		Stmt {
			kind: expected_kind,
		},
	);
}

#[test]
fn if_else_if_parses() {
	let input = r#"if (foo) {
		return bar;
	} else if (bar) {
		return baz;
	}"#;

	let expected_kind = StmtKind::If(
		Expr::from(Identifier::from("foo")),
		Box::new(Block::from(vec![Stmt {
			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
		}])),
		Some(vec![(
			Expr::from(Identifier::from("bar")),
			Box::new(Block::from(vec![Stmt {
				kind: StmtKind::Return(Some(Expr::from(Identifier::from("baz")))),
			}])),
		)]),
		None,
	);

	test_parse(
		Rule::statement,
		input,
		parse_stmt,
		Stmt {
			kind: expected_kind,
		},
	);
}

#[test]
fn basic_if_parses() {
	let input = r#"if (foo) {
		return bar;
	}"#;

	let expected_kind = StmtKind::If(
		Expr::from(Identifier::from("foo")),
		Box::new(Block::from(vec![Stmt {
			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
		}])),
		None,
		None,
	);

	test_parse(
		Rule::statement,
		input,
		parse_stmt,
		Stmt {
			kind: expected_kind,
		},
	);
}

#[test]
fn stmt_expr_postfix_parses() {
	let inner: Expr = ExprKind::Identifier(Identifier::from("i")).into();

	test_parse(
		Rule::statement,
		"i++;",
		parse_stmt,
		Stmt::from(StmtExpr::from(Expr {
			kind: ExprKind::Postfix(Box::new(inner), IncDecOp::Inc),
		})),
	)
}

#[test]
fn stmt_expr_expr_parses() {
	let lhs = Expr::from(Identifier::from("foo"));
	let rhs = Expr::from(Literal::from(22));

	test_parse(
		Rule::statement,
		"foo = 22;",
		parse_stmt,
		Stmt::from(StmtExpr::from(Expr {
			kind: ExprKind::Assignment(Box::new(lhs), AssignOp::Eq, Box::new(rhs)),
		})),
	);
}

#[test]
fn stmt_expr_local_parses() {
	let test_str = "Integer foo = 22;";

	let ty = Ty::from(PrimitiveKind::Integer);
	let id = Identifier::from("foo");
	let rhs = Expr::from(Literal::from(22));

	let expected = Stmt {
		kind: StmtKind::StmtExpr(StmtExpr::Local(Local {
			annotation: None,
			is_final: false,
			ty,
			id,
			rhs: Some(rhs),
		})),
	};

	test_parse(Rule::statement, test_str, parse_stmt, expected);
}

#[test]
fn try_catch_catch_finally_parses() {
	let test_str = r#"try {
		insert foo;
	} catch (Exception e) {
		return bar;
	} catch (DmlException de) {
		return baz;
	} finally {
		return quux;
	}"#;

	let try_block = vec![StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into()];

	let catch_clause = (
		Ty::from(Identifier::from("Exception")),
		Identifier::from("e"),
		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
			"bar",
		))))
		.into()]),
	);

	let opt_catch = Some(vec![(
		Ty::from(Identifier::from("DmlException")),
		Identifier::from("de"),
		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
			"baz",
		))))
		.into()]),
	)]);

	let finally = Some(Block::from(vec![StmtKind::Return(Some(Expr::from(
		Identifier::from("quux"),
	)))
	.into()]));

	let expected = StmtKind::TryCatch(
		Block::from(try_block).to_boxed(),
		catch_clause,
		opt_catch,
		finally,
	);

	test_parse(Rule::statement, test_str, parse_stmt, expected.into());
}

#[test]
fn try_catch_simple_parses() {
	let test_str = r#"try {
		insert foo;
	} catch (Exception e) {
		return bar;
	}"#;

	let try_block = vec![StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into()];

	let catch_clause = (
		Ty::from(Identifier::from("Exception")),
		Identifier::from("e"),
		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
			"bar",
		))))
		.into()]),
	);

	let expected = StmtKind::TryCatch(Block::from(try_block).to_boxed(), catch_clause, None, None);

	test_parse(Rule::statement, test_str, parse_stmt, expected.into());
}

#[test]
fn dml_stmt_parses() {
	test_parse(
		Rule::statement,
		"insert foo;",
		parse_stmt,
		StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into(),
	);
}

#[test]
fn throw_stmt_parses() {
	test_parse(
		Rule::statement,
		"throw new TestException();",
		parse_stmt,
		StmtKind::Throw(
			ExprKind::New(
				Ty::from(Identifier::from("TestException")),
				NewType::Class(ClassArgs::Basic(None)),
			)
			.into(),
		)
		.into(),
	);
}

#[test]
fn return_stmt_some_parses() {
	test_parse(
		Rule::statement,
		"return foo;",
		parse_stmt,
		StmtKind::Return(Some(Expr::from(Identifier::from("foo")))).into(),
	);
}

#[test]
fn return_stmt_none_parses() {
	test_parse(
		Rule::statement,
		"return;",
		parse_stmt,
		StmtKind::Return(None).into(),
	);
}

#[test]
fn break_stmt_parses() {
	test_parse(
		Rule::statement,
		"break;",
		parse_stmt,
		StmtKind::Break.into(),
	);
}

#[test]
fn continue_stmt_parses() {
	test_parse(
		Rule::statement,
		"continue;",
		parse_stmt,
		StmtKind::Continue.into(),
	);
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
			kind: ExprKind::New(ty, NewType::Array(literal_exprs)),
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
			kind: ExprKind::New(ty, NewType::Collection(literal_exprs)),
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
			kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(args))),
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
			kind: ExprKind::New(ty, NewType::Map(mapping)),
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
			kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(Some(class_args)))),
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
			kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(None))),
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
			kind: ExprKind::New(ty, NewType::Class(ClassArgs::SObject(args))),
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

#[test]
fn assignment_parses() {
	let lhs = Expr::from(Identifier::from("foo"));
	let rhs = Expr::from(Literal::from(22));

	test_parse(
		Rule::expression,
		"foo = 22",
		parse_expr,
		Expr {
			kind: ExprKind::Assignment(Box::new(lhs), AssignOp::Eq, Box::new(rhs)),
		},
	);
}

#[test]
fn ternary_parses() {
	let test = Expr::from(Identifier::from("isTrue"));
	let pos = Expr::from(Literal::from(true));
	let neg = Expr::from(Literal::from(false));

	test_parse(
		Rule::expression,
		"isTrue ? true : false",
		parse_expr,
		Expr {
			kind: ExprKind::Ternary(Box::new(test), Box::new(pos), Box::new(neg)),
		},
	);
}

#[test]
fn simple_infix_expr_parses() {
	let lhs = ExprKind::Call(Identifier::from("foo"), None).into();
	let op = BinOp::Eq;
	let rhs = Expr::from(Literal {
		kind: LiteralKind::Null,
	});

	test_parse(
		Rule::expression,
		"foo() == null",
		parse_expr,
		Expr {
			kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
		},
	);
}

#[test]
fn nested_infix_expr_parses() {
	let lhs = ExprKind::Call(Identifier::from("foo"), None).into();
	let op = BinOp::Eq;

	let rhs = Expr {
		kind: ExprKind::Infix(
			Box::new(Expr::from(Literal::from(2))),
			BinOp::Add,
			Box::new(Expr::from(Identifier::from("bar"))),
		),
	};

	test_parse(
		Rule::expression,
		"foo() == 2 + bar",
		parse_expr,
		Expr {
			kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
		},
	);
}
