use super::parse::*;
use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::class::*;
use crate::ast::expr::*;
use crate::ast::identifier::Identifier;
use crate::ast::interface::*;
use crate::ast::literal::*;
use crate::ast::method::*;
use crate::ast::modifier::*;
use crate::ast::ops::*;
use crate::ast::r#enum::Enum;
use crate::ast::stmt::*;
use crate::ast::trigger::*;
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
fn class_basic_parses() {
	let input = r#"public with sharing class FileWriter implements Writer {
		public Buffer buf;
		public static final String name;

		static {
			name = 'WRITER';
		}

		{
			buf = new Buffer();
		}

		public FileWriter() {}

		public Integer write(String output) {}
}"#;

	let expected = Class {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		sharing_or_impl_modifier: Some(ImplOrSharingMod::With),
		name: Identifier::from("FileWriter"),
		extension: None,
		implementations: vec![Ty::from(Identifier::from("Writer"))],
		body: vec![
			ClassBodyMember::Field(ClassField {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				instance_mod: None,
				is_final: false,
				ty: Ty::from(Identifier::from("Buffer")),
				id: Identifier::from("buf"),
				getter: None,
				setter: None,
				rhs: None,
			}),
			ClassBodyMember::Field(ClassField {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				instance_mod: Some(ClassInstanceModifier::Static),
				is_final: true,
				ty: Ty::from(PrimitiveKind::String),
				id: Identifier::from("name"),
				getter: None,
				setter: None,
				rhs: None,
			}),
			ClassBodyMember::StaticBlock(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::from(Expr::from(Identifier::from("name"))),
						AssignOp::Eq,
						Box::from(Expr::from(Literal::from("\'WRITER\'"))),
					),
				})),
			}])),
			ClassBodyMember::InstanceBlock(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::from(Expr::from(Identifier::from("buf"))),
						AssignOp::Eq,
						Box::from(Expr {
							kind: ExprKind::New(
								Ty::from(Identifier::from("Buffer")),
								NewType::Class(ClassArgs::Basic(None)),
							),
						}),
					),
				})),
			}])),
			ClassBodyMember::Constructor(ClassConstructor {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				identifier: Identifier::from("FileWriter"),
				params: Vec::new(),
				block: Block::Body(Vec::new()),
			}),
			ClassBodyMember::Method(ClassMethod {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				impl_mod: None,
				is_testmethod: false,
				return_type: Ty::from(PrimitiveKind::Integer),
				identifier: Identifier::from("write"),
				params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
				block: Some(Block::Body(Vec::new())),
			}),
		],
	};

	test_parse(Rule::class_declaration, input, parse_class, expected);
}

#[test]
fn class_constructor_parses() {
	let input = "public Foo(String bar) {}";

	let expected = ClassConstructor {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		identifier: Identifier::from("Foo"),
		params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("bar"))],
		block: Block::from(Vec::new()),
	};

	test_parse(
		Rule::class_constructor_definition,
		input,
		parse_class_constructor,
		expected,
	);
}

#[test]
fn class_constructor_no_access_mod_parses() {
	let input = "Foo() {}";

	let expected = ClassConstructor {
		annotation: None,
		access_mod: None,
		identifier: Identifier::from("Foo"),
		params: Vec::new(),
		block: Block::from(Vec::new()),
	};

	test_parse(
		Rule::class_constructor_definition,
		input,
		parse_class_constructor,
		expected,
	);
}

#[test]
fn abstract_class_method_parses() {
	let input = "abstract Integer abstractMethod();";

	let expected = ClassMethod {
		annotation: None,
		access_mod: None,
		impl_mod: Some(ImplModifier::Abstract),
		is_testmethod: false,
		return_type: Ty::from(PrimitiveKind::Integer),
		identifier: Identifier::from("abstractMethod"),
		params: Vec::new(),
		block: None,
	};

	test_parse(
		Rule::class_method_declaration,
		input,
		parse_class_method,
		expected,
	);
}

#[test]
fn interface_with_access_mod_parses() {
	let input = r#"public interface Writer extends Foo {
		Integer write(String output);
	}"#;

	let expected = Interface {
		access_mod: Some(AccessModifier::Public),
		is_virtual: false,
		name: Identifier::from("Writer"),
		extensions: vec![Ty::from(Identifier::from("Foo"))],
		methods: vec![ImplementableMethod {
			ty: Ty::from(PrimitiveKind::Integer),
			id: Identifier::from("write"),
			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
		}],
	};

	test_parse(
		Rule::interface_declaration,
		input,
		parse_interface,
		expected,
	);
}

#[test]
fn interface_parses() {
	let input = r#"interface Writer extends Foo {
		Integer write(String output);
	}"#;

	let expected = Interface {
		access_mod: None,
		is_virtual: false,
		name: Identifier::from("Writer"),
		extensions: vec![Ty::from(Identifier::from("Foo"))],
		methods: vec![ImplementableMethod {
			ty: Ty::from(PrimitiveKind::Integer),
			id: Identifier::from("write"),
			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
		}],
	};

	test_parse(
		Rule::interface_declaration,
		input,
		parse_interface,
		expected,
	);
}

#[test]
fn virtual_interface_parses() {
	let input = r#"public virtual interface Writer {
		Integer write(String output);
	}"#;

	let expected = Interface {
		access_mod: Some(AccessModifier::Public),
		is_virtual: true,
		name: Identifier::from("Writer"),
		extensions: Vec::new(),
		methods: vec![ImplementableMethod {
			ty: Ty::from(PrimitiveKind::Integer),
			id: Identifier::from("write"),
			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
		}],
	};

	test_parse(
		Rule::interface_declaration,
		input,
		parse_interface,
		expected,
	);
}

#[test]
fn trigger_parses() {
	let input = "trigger MyAccountTrigger on Account (before insert, before update) {}";

	let expected = Trigger {
		name: Identifier::from("MyAccountTrigger"),
		object: Ty::from(Identifier::from("Account")),
		events: vec![
			TriggerEvent(TriggerWhen::Before, DmlOp::Insert),
			TriggerEvent(TriggerWhen::Before, DmlOp::Update),
		],
		body: Block::Body(Vec::new()),
	};

	test_parse(Rule::trigger_declaration, input, parse_trigger, expected);
}

#[test]
fn enum_parses() {
	let input = "public enum Season {WINTER, SPRING, SUMMER, FALL}";

	let expected = Enum {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		name: Identifier::from("Season"),
		ids: vec![
			Identifier::from("WINTER"),
			Identifier::from("SPRING"),
			Identifier::from("SUMMER"),
			Identifier::from("FALL"),
		],
	};

	test_parse(Rule::enum_declaration, input, parse_enum, expected);
}

#[test]
fn class_field_no_rhs_parses() {
	let input = "public static String foo;";

	let expected = ClassField {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		instance_mod: Some(ClassInstanceModifier::Static),
		is_final: false,
		ty: Ty::from(PrimitiveKind::String),
		id: Identifier::from("foo"),
		getter: None,
		setter: None,
		rhs: None,
	};

	test_parse(
		Rule::class_field_declaration,
		input,
		parse_class_field,
		expected,
	);
}

#[test]
fn class_field_getter_setter_basic_parses() {
	let input = "public String name {get; set;}";

	let expected = ClassField {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		instance_mod: None,
		is_final: false,
		ty: Ty::from(PrimitiveKind::String),
		id: Identifier::from("name"),
		getter: Some(Property {
			access_mod: None,
			property_type: PropertyType::Get,
			body: None,
		}),
		setter: Some(Property {
			access_mod: None,
			property_type: PropertyType::Set,
			body: None,
		}),
		rhs: None,
	};

	test_parse(
		Rule::class_field_declaration,
		input,
		parse_class_field,
		expected,
	);
}

#[test]
fn class_field_rhs_parses() {
	let input = "public static Integer foo = 22;";

	let expected = ClassField {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		instance_mod: Some(ClassInstanceModifier::Static),
		is_final: false,
		ty: Ty::from(PrimitiveKind::Integer),
		id: Identifier::from("foo"),
		getter: None,
		setter: None,
		rhs: Some(Expr::from(Literal::from(22))),
	};

	test_parse(
		Rule::class_field_declaration,
		input,
		parse_class_field,
		expected,
	);
}

#[test]
fn class_field_getter_setter_maximal_parses() {
	let input = r#"public String name {
		public get {
			return 'foo';
		} 
		private set {
			name = value;
		}
	}"#;

	let expected = ClassField {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		instance_mod: None,
		is_final: false,
		ty: Ty::from(PrimitiveKind::String),
		id: Identifier::from("name"),
		getter: Some(Property {
			access_mod: Some(AccessModifier::Public),
			property_type: PropertyType::Get,
			body: Some(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr::from(Literal::from("\'foo\'")))),
			}])),
		}),
		setter: Some(Property {
			access_mod: Some(AccessModifier::Private),
			property_type: PropertyType::Set,
			body: Some(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr::from(Identifier::from("name"))),
						AssignOp::Eq,
						Box::new(Expr::from(Identifier::from("value"))),
					),
				})),
			}])),
		}),
		rhs: None,
	};

	test_parse(
		Rule::class_field_declaration,
		input,
		parse_class_field,
		expected,
	);
}

#[test]
fn class_method_maximal_parses() {
	let input = "@isTest public static testMethod Integer foo(Bar b, Baz bz) {}";

	let expected = ClassMethod {
		annotation: Some(Annotation::from("isTest")),
		access_mod: Some(AccessModifier::Public),
		impl_mod: Some(ImplModifier::Static),
		is_testmethod: true,
		return_type: Ty::from(PrimitiveKind::Integer),
		identifier: Identifier::from("foo"),
		params: vec![
			(Ty::from(Identifier::from("Bar")), Identifier::from("b")),
			(Ty::from(Identifier::from("Baz")), Identifier::from("bz")),
		],
		block: Some(Block::Body(Vec::new())),
	};

	test_parse(
		Rule::class_method_declaration,
		input,
		parse_class_method,
		expected,
	);
}

#[test]
fn class_method_basic_parses() {
	let input = "public static void foo() {}";

	let expected = ClassMethod {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		impl_mod: Some(ImplModifier::Static),
		is_testmethod: false,
		return_type: Ty::void(),
		identifier: Identifier::from("foo"),
		params: Vec::new(),
		block: Some(Block::Body(Vec::new())),
	};

	test_parse(
		Rule::class_method_declaration,
		input,
		parse_class_method,
		expected,
	);
}

#[test]
fn switch_basic_parses() {
	let input = r#"switch on i {}"#;

	let expr = Expr::from(Identifier::from("i"));

	let expected = Stmt {
		kind: StmtKind::Switch(expr, None, None),
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn switch_values_parses() {
	let input = r#"switch on i {
   when 2, 3, 4 {
       return 1;
   }
   when 5, 6 {
       return 2;
   }
   when 7 {
       return 3;
   }
   when else {
       return 4;
   }
}"#;

	let test_expr = Expr::from(Identifier::from("i"));

	let first_when_values = WhenCondition::Value(vec![
		WhenValue::Literal(Literal::from(2)),
		WhenValue::Literal(Literal::from(3)),
		WhenValue::Literal(Literal::from(4)),
	]);

	let first_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(1)))),
	}]);

	let second_when_values = WhenCondition::Value(vec![
		WhenValue::Literal(Literal::from(5)),
		WhenValue::Literal(Literal::from(6)),
	]);

	let second_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(2)))),
	}]);

	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal::from(7))]);

	let third_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(3)))),
	}]);

	let else_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(4)))),
	}]);

	test_parse(
		Rule::statement,
		input,
		parse_stmt,
		Stmt {
			kind: StmtKind::Switch(
				test_expr,
				Some(vec![
					(first_when_values, first_block),
					(second_when_values, second_block),
					(third_when_values, third_block),
				]),
				Some(else_block),
			),
		},
	)
}

#[test]
fn switch_types_parses() {
	let input = r#"switch on sobject {
   when Account a {
       return 1;
   }
   when Contact c {
       return 2;
   }
   when null {
       return 3;
   }
   when else {
       return 4;
   }
}"#;

	let test_expr = Expr::from(Identifier::from("sobject"));

	let first_when_values =
		WhenCondition::Type(Ty::from(Identifier::from("Account")), Identifier::from("a"));

	let first_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(1)))),
	}]);

	let second_when_values =
		WhenCondition::Type(Ty::from(Identifier::from("Contact")), Identifier::from("c"));

	let second_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(2)))),
	}]);

	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal {
		kind: LiteralKind::Null,
	})]);

	let third_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(3)))),
	}]);

	let else_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr::from(Literal::from(4)))),
	}]);

	test_parse(
		Rule::statement,
		input,
		parse_stmt,
		Stmt {
			kind: StmtKind::Switch(
				test_expr,
				Some(vec![
					(first_when_values, first_block),
					(second_when_values, second_block),
					(third_when_values, third_block),
				]),
				Some(else_block),
			),
		},
	)
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
fn ty_void_parses() {
	test_parse(Rule::basic_type, "void", parse_ty, Ty::void())
}

#[test]
fn ty_void_case_insensitive_parses() {
	test_parse(Rule::basic_type, "VOID", parse_ty, Ty::void())
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
fn soql_query_parses() {
	let query_str = "[ SELECT Foo FROM Bar ]";
	test_parse(
		Rule::expression,
		query_str,
		parse_expr,
		Expr {
			kind: ExprKind::Query(Query::Soql(String::from("SELECT Foo FROM Bar"))),
		},
	);
}

#[test]
fn sosl_query_parses() {
	let query_str = "[ FIND {foo} ]";
	test_parse(
		Rule::expression,
		query_str,
		parse_expr,
		Expr {
			kind: ExprKind::Query(Query::Sosl(String::from("FIND {foo}"))),
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
