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
use crate::source::*;
use pest::iterators::Pair;

use pretty_assertions::assert_eq;

fn test_parse<F, E>(rule: Rule, input: &str, parse: F, expected: E)
where
	F: FnOnce(Pair<Rule>) -> E,
	E: std::fmt::Debug + PartialEq,
{
	let mut parsed = ApexParser::parse(rule, input).unwrap();
	let result = parse(parsed.next().unwrap());

	assert_eq!(expected, result);
}

#[test]
fn file_parses() {
	let file = "public class Foo {}";

	let result = parse_file(file);

	assert!(result.is_ok());
}

// #[test]
// fn class_basic_parses() {
// 	let input = r#"public with sharing class FileWriter implements Writer {
// 		public Buffer buf;
// 		public static final String name;

// 		static {
// 			name = 'WRITER';
// 		}

// 		{
// 			buf = new Buffer();
// 		}

// 		public FileWriter() {}

// 		public Integer write(String output) {}
// }"#;

// 	let expected = Class {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		sharing_or_impl_modifier: Some(ImplOrSharingMod::With),
// 		name: Identifier {
// 			name: String::from("FileWriter"),
// 			span: Span::default(),
// 		}
// 		extension: None,
// 		implementations: vec![Ty::from(Identifier::from("Writer"))],
// 		body: vec![
// 			ClassBodyMember::Field(ClassField {
// 				annotation: None,
// 				access_mod: Some(AccessModifier::Public),
// 				instance_mod: None,
// 				is_final: false,
// 				ty: Ty::from(Identifier::from("Buffer")),
// 				id: Identifier::from("buf"),
// 				getter: None,
// 				setter: None,
// 				rhs: None,
// 				span: Span::default(),
// 				// span: Span {
// 				// 	start: 59,
// 				// 	end: 77,
// 				// 	start_pos: Position { line: 2, col: 3 },
// 				// 	end_pos: Position { line: 2, col: 21 },
// 				// },
// 			}),
// 			ClassBodyMember::Field(ClassField {
// 				annotation: None,
// 				access_mod: Some(AccessModifier::Public),
// 				instance_mod: Some(ClassInstanceModifier::Static),
// 				is_final: true,
// 				ty: Ty::from(PrimitiveKind::String),
// 				id: Identifier::from("name"),
// 				getter: None,
// 				setter: None,
// 				rhs: None,
// 				span: Span {
// 					start: 80,
// 					end: 112,
// 					start_pos: Position { line: 3, col: 3 },
// 					end_pos: Position { line: 3, col: 35 },
// 				},
// 			}),
// 			ClassBodyMember::StaticBlock(Block::Body(vec![Stmt {
// 				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
// 					kind: ExprKind::Assignment(
// 						Box::from(Expr::from(Identifier::from("name"))),
// 						AssignOp::Eq,
// 						Box::from(Expr::from(Literal::from("\'WRITER\'"))),
// 					),
// 				})),
// 			}])),
// 			ClassBodyMember::InstanceBlock(Block::Body(vec![Stmt {
// 				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
// 					kind: ExprKind::Assignment(
// 						Box::from(Expr::from(Identifier::from("buf"))),
// 						AssignOp::Eq,
// 						Box::from(Expr {
// 							kind: ExprKind::New(
// 								Ty::from(Identifier::from("Buffer")),
// 								NewType::Class(ClassArgs::Basic(None)),
// 							),
// 						}),
// 					),
// 				})),
// 			}])),
// 			ClassBodyMember::Constructor(ClassConstructor {
// 				annotation: None,
// 				access_mod: Some(AccessModifier::Public),
// 				identifier: Identifier::from("FileWriter"),
// 				params: Vec::new(),
// 				block: Block::Body(Vec::new()),
// 				span: Span {
// 					start: 184,
// 					end: 206,
// 					start_pos: Position { line: 13, col: 3 },
// 					end_pos: Position { line: 13, col: 25 },
// 				},
// 			}),
// 			ClassBodyMember::Method(ClassMethod {
// 				annotation: None,
// 				access_mod: Some(AccessModifier::Public),
// 				impl_mod: None,
// 				is_testmethod: false,
// 				return_type: Ty::from(PrimitiveKind::Integer),
// 				identifier: Identifier::from("write"),
// 				params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
// 				block: Some(Block::Body(Vec::new())),
// 				// span: Span::default(),
// 			}),
// 		],
// 		span: Span {
// 			start: 0,
// 			end: 250,
// 			start_pos: Position { line: 1, col: 1 },
// 			end_pos: Position { line: 16, col: 2 },
// 		},
// 	};

// 	test_parse(Rule::class_declaration, input, parse_class, expected);
// }

// #[test]
// fn class_constructor_parses() {
// 	let input = "public Foo(String bar) {}";

// 	let expected = ClassConstructor {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		identifier: Identifier::from("Foo"),
// 		params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("bar"))],
// 		block: Block::from(Vec::new()),
// 		span: Span {
// 			start: 0,
// 			end: 25,
// 			start_pos: Position { line: 1, col: 1 },
// 			end_pos: Position { line: 1, col: 26 },
// 		},
// 	};

// 	test_parse(
// 		Rule::class_constructor_definition,
// 		input,
// 		parse_class_constructor,
// 		expected,
// 	);
// }

// #[test]
// fn class_constructor_no_access_mod_parses() {
// 	let input = "Foo() {}";

// 	let expected = ClassConstructor {
// 		annotation: None,
// 		access_mod: None,
// 		identifier: Identifier::from("Foo"),
// 		params: Vec::new(),
// 		block: Block::from(Vec::new()),
// 		span: Span::default(),
// 	};

// 	test_parse(
// 		Rule::class_constructor_definition,
// 		input,
// 		parse_class_constructor,
// 		expected,
// 	);
// }

// #[test]
// fn abstract_class_method_parses() {
// 	let input = "abstract Integer abstractMethod();";

// 	let expected = ClassMethod {
// 		annotation: None,
// 		access_mod: None,
// 		impl_mod: Some(ImplModifier::Abstract),
// 		is_testmethod: false,
// 		return_type: Ty::from(PrimitiveKind::Integer),
// 		identifier: Identifier::from("abstractMethod"),
// 		params: Vec::new(),
// 		block: None,
// 	};

// 	test_parse(
// 		Rule::class_method_declaration,
// 		input,
// 		parse_class_method,
// 		expected,
// 	);
// }

// #[test]
// fn interface_with_access_mod_parses() {
// 	let input = r#"public interface Writer extends Foo {
// 		Integer write(String output);
// 	}"#;

// 	let expected = Interface {
// 		access_mod: Some(AccessModifier::Public),
// 		is_virtual: false,
// 		name: Identifier::from("Writer"),
// 		extensions: vec![Ty::from(Identifier::from("Foo"))],
// 		methods: vec![ImplementableMethod {
// 			ty: Ty::from(PrimitiveKind::Integer),
// 			id: Identifier::from("write"),
// 			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
// 		}],
// 	};

// 	test_parse(
// 		Rule::interface_declaration,
// 		input,
// 		parse_interface,
// 		expected,
// 	);
// }

// #[test]
// fn interface_parses() {
// 	let input = r#"interface Writer extends Foo {
// 		Integer write(String output);
// 	}"#;

// 	let expected = Interface {
// 		access_mod: None,
// 		is_virtual: false,
// 		name: Identifier::from("Writer"),
// 		extensions: vec![Ty::from(Identifier::from("Foo"))],
// 		methods: vec![ImplementableMethod {
// 			ty: Ty::from(PrimitiveKind::Integer),
// 			id: Identifier::from("write"),
// 			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
// 		}],
// 	};

// 	test_parse(
// 		Rule::interface_declaration,
// 		input,
// 		parse_interface,
// 		expected,
// 	);
// }

// #[test]
// fn virtual_interface_parses() {
// 	let input = r#"public virtual interface Writer {
// 		Integer write(String output);
// 	}"#;

// 	let expected = Interface {
// 		access_mod: Some(AccessModifier::Public),
// 		is_virtual: true,
// 		name: Identifier::from("Writer"),
// 		extensions: Vec::new(),
// 		methods: vec![ImplementableMethod {
// 			ty: Ty::from(PrimitiveKind::Integer),
// 			id: Identifier::from("write"),
// 			params: vec![(Ty::from(PrimitiveKind::String), Identifier::from("output"))],
// 		}],
// 	};

// 	test_parse(
// 		Rule::interface_declaration,
// 		input,
// 		parse_interface,
// 		expected,
// 	);
// }

// #[test]
// fn trigger_parses() {
// 	let input = "trigger MyAccountTrigger on Account (before insert, before update) {}";

// 	let expected = Trigger {
// 		name: Identifier::from("MyAccountTrigger"),
// 		object: Ty::from(Identifier::from("Account")),
// 		events: vec![
// 			TriggerEvent(TriggerWhen::Before, DmlOp::Insert),
// 			TriggerEvent(TriggerWhen::Before, DmlOp::Update),
// 		],
// 		body: Block::Body(Vec::new()),
// 	};

// 	test_parse(Rule::trigger_declaration, input, parse_trigger, expected);
// }

// #[test]
// fn enum_parses() {
// 	let input = "public enum Season {WINTER, SPRING, SUMMER, FALL}";

// 	let expected = Enum {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		name: Identifier::from("Season"),
// 		ids: vec![
// 			Identifier::from("WINTER"),
// 			Identifier::from("SPRING"),
// 			Identifier::from("SUMMER"),
// 			Identifier::from("FALL"),
// 		],
// 	};

// 	test_parse(Rule::enum_declaration, input, parse_enum, expected);
// }

// #[test]
// fn class_field_no_rhs_parses() {
// 	let input = "public static String foo;";

// 	let expected = ClassField {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		instance_mod: Some(ClassInstanceModifier::Static),
// 		is_final: false,
// 		ty: Ty::from(PrimitiveKind::String),
// 		id: Identifier::from("foo"),
// 		getter: None,
// 		setter: None,
// 		rhs: None,
// 		span: Span::default(),
// 	};

// 	test_parse(
// 		Rule::class_field_declaration,
// 		input,
// 		parse_class_field,
// 		expected,
// 	);
// }

// #[test]
// fn class_field_getter_setter_basic_parses() {
// 	let input = "public String name {get; set;}";

// 	let expected = ClassField {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		instance_mod: None,
// 		is_final: false,
// 		ty: Ty::from(PrimitiveKind::String),
// 		id: Identifier::from("name"),
// 		getter: Some(Property {
// 			access_mod: None,
// 			property_type: PropertyType::Get,
// 			body: None,
// 			span: Span::default(),
// 		}),
// 		setter: Some(Property {
// 			access_mod: None,
// 			property_type: PropertyType::Set,
// 			body: None,
// 			span: Span::default(),
// 		}),
// 		rhs: None,
// 		span: Span::default(),
// 	};

// 	test_parse(
// 		Rule::class_field_declaration,
// 		input,
// 		parse_class_field,
// 		expected,
// 	);
// }

// #[test]
// fn class_field_rhs_parses() {
// 	let input = "public static Integer foo = 22;";

// 	let expected = ClassField {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		instance_mod: Some(ClassInstanceModifier::Static),
// 		is_final: false,
// 		ty: Ty::from(PrimitiveKind::Integer),
// 		id: Identifier::from("foo"),
// 		getter: None,
// 		setter: None,
// 		rhs: Some(Expr::from(Literal::from(22))),
// 		span: Span::default(),
// 	};

// 	test_parse(
// 		Rule::class_field_declaration,
// 		input,
// 		parse_class_field,
// 		expected,
// 	);
// }

// #[test]
// fn class_field_getter_setter_maximal_parses() {
// 	let input = r#"public String name {
// 		public get {
// 			return 'foo';
// 		}
// 		private set {
// 			name = value;
// 		}
// 	}"#;

// 	let expected = ClassField {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		instance_mod: None,
// 		is_final: false,
// 		ty: Ty::from(PrimitiveKind::String),
// 		id: Identifier::from("name"),
// 		getter: Some(Property {
// 			access_mod: Some(AccessModifier::Public),
// 			property_type: PropertyType::Get,
// 			body: Some(Block::Body(vec![Stmt {
// 				kind: StmtKind::Return(Some(Expr::from(Literal::from("\'foo\'")))),
// 			}])),
// 			span: Span::default(),
// 		}),
// 		setter: Some(Property {
// 			access_mod: Some(AccessModifier::Private),
// 			property_type: PropertyType::Set,
// 			body: Some(Block::Body(vec![Stmt {
// 				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
// 					kind: ExprKind::Assignment(
// 						Box::new(Expr::from(Identifier::from("name"))),
// 						AssignOp::Eq,
// 						Box::new(Expr::from(Identifier::from("value"))),
// 					),
// 				})),
// 			}])),
// 			span: Span::default(),
// 		}),
// 		rhs: None,
// 		span: Span::default(),
// 	};

// 	test_parse(
// 		Rule::class_field_declaration,
// 		input,
// 		parse_class_field,
// 		expected,
// 	);
// }

// #[test]
// fn class_method_maximal_parses() {
// 	let input = "@isTest public static testMethod Integer foo(Bar b, Baz bz) {}";

// 	let expected = ClassMethod {
// 		annotation: Some(Annotation::from("isTest")),
// 		access_mod: Some(AccessModifier::Public),
// 		impl_mod: Some(ImplModifier::Static),
// 		is_testmethod: true,
// 		return_type: Ty::from(PrimitiveKind::Integer),
// 		identifier: Identifier::from("foo"),
// 		params: vec![
// 			(Ty::from(Identifier::from("Bar")), Identifier::from("b")),
// 			(Ty::from(Identifier::from("Baz")), Identifier::from("bz")),
// 		],
// 		block: Some(Block::Body(Vec::new())),
// 	};

// 	test_parse(
// 		Rule::class_method_declaration,
// 		input,
// 		parse_class_method,
// 		expected,
// 	);
// }

// #[test]
// fn class_method_basic_parses() {
// 	let input = "public static void foo() {}";

// 	let expected = ClassMethod {
// 		annotation: None,
// 		access_mod: Some(AccessModifier::Public),
// 		impl_mod: Some(ImplModifier::Static),
// 		is_testmethod: false,
// 		return_type: Ty::void(),
// 		identifier: Identifier::from("foo"),
// 		params: Vec::new(),
// 		block: Some(Block::Body(Vec::new())),
// 	};

// 	test_parse(
// 		Rule::class_method_declaration,
// 		input,
// 		parse_class_method,
// 		expected,
// 	);
// }

// #[test]
// fn switch_basic_parses() {
// 	let input = r#"switch on i {}"#;

// 	let expr = Expr::from(Identifier::from("i"));

// 	let expected = Stmt {
// 		kind: StmtKind::Switch(expr, None, None),
// 	};

// 	test_parse(Rule::statement, input, parse_stmt, expected);
// }

// #[test]
// fn switch_values_parses() {
// 	let input = r#"switch on i {
//    when 2, 3, 4 {
//        return 1;
//    }
//    when 5, 6 {
//        return 2;
//    }
//    when 7 {
//        return 3;
//    }
//    when else {
//        return 4;
//    }
// }"#;

// 	let test_expr = Expr::from(Identifier::from("i"));

// 	let first_when_values = WhenCondition::Value(vec![
// 		WhenValue::Literal(Literal::from(2)),
// 		WhenValue::Literal(Literal::from(3)),
// 		WhenValue::Literal(Literal::from(4)),
// 	]);

// 	let first_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(1)))),
// 	}]);

// 	let second_when_values = WhenCondition::Value(vec![
// 		WhenValue::Literal(Literal::from(5)),
// 		WhenValue::Literal(Literal::from(6)),
// 	]);

// 	let second_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(2)))),
// 	}]);

// 	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal::from(7))]);

// 	let third_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(3)))),
// 	}]);

// 	let else_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(4)))),
// 	}]);

// 	test_parse(
// 		Rule::statement,
// 		input,
// 		parse_stmt,
// 		Stmt {
// 			kind: StmtKind::Switch(
// 				test_expr,
// 				Some(vec![
// 					(first_when_values, first_block),
// 					(second_when_values, second_block),
// 					(third_when_values, third_block),
// 				]),
// 				Some(else_block),
// 			),
// 		},
// 	)
// }

// #[test]
// fn switch_types_parses() {
// 	let input = r#"switch on sobject {
//    when Account a {
//        return 1;
//    }
//    when Contact c {
//        return 2;
//    }
//    when null {
//        return 3;
//    }
//    when else {
//        return 4;
//    }
// }"#;

// 	let test_expr = Expr::from(Identifier::from("sobject"));

// 	let first_when_values =
// 		WhenCondition::Type(Ty::from(Identifier::from("Account")), Identifier::from("a"));

// 	let first_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(1)))),
// 	}]);

// 	let second_when_values =
// 		WhenCondition::Type(Ty::from(Identifier::from("Contact")), Identifier::from("c"));

// 	let second_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(2)))),
// 	}]);

// 	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal {
// 		kind: LiteralKind::Null,
// 	})]);

// 	let third_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(3)))),
// 	}]);

// 	let else_block = Block::Body(vec![Stmt {
// 		kind: StmtKind::Return(Some(Expr::from(Literal::from(4)))),
// 	}]);

// 	test_parse(
// 		Rule::statement,
// 		input,
// 		parse_stmt,
// 		Stmt {
// 			kind: StmtKind::Switch(
// 				test_expr,
// 				Some(vec![
// 					(first_when_values, first_block),
// 					(second_when_values, second_block),
// 					(third_when_values, third_block),
// 				]),
// 				Some(else_block),
// 			),
// 		},
// 	)
// }

// #[test]
// fn for_basic_simple_parses() {
// 	let input = "for (Integer i = 0; i < 10; i++) sum += i;";

// 	let expected = Stmt {
// 		kind: StmtKind::For(ForStmt::Basic(
// 			Some(vec![StmtExpr::Local(Local {
// 				annotation: None,
// 				is_final: false,
// 				ty: Ty::from(PrimitiveKind::Integer),
// 				id: Identifier::from("i"),
// 				rhs: Some(Expr {
// 					kind: ExprKind::Literal(Literal::from(0)),
// 				}),
// 			})]),
// 			Some(Expr {
// 				kind: ExprKind::Infix(
// 					Box::new(Expr::from(Identifier::from("i"))),
// 					BinOp::Lt,
// 					Box::new(Expr::from(Literal::from(10))),
// 				),
// 			}),
// 			Some(StmtExpr::Expr(Expr {
// 				kind: ExprKind::Postfix(Box::new(Expr::from(Identifier::from("i"))), IncDecOp::Inc),
// 			})),
// 			Box::new(Block::Inline(Box::new(Stmt {
// 				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
// 					kind: ExprKind::Assignment(
// 						Box::new(Expr::from(Identifier::from("sum"))),
// 						AssignOp::Add,
// 						Box::new(Expr::from(Identifier::from("i"))),
// 					),
// 				})),
// 			}))),
// 		)),
// 	};

// 	test_parse(Rule::statement, input, parse_stmt, expected);
// }

// #[test]
// fn for_enhanced_parses() {
// 	let input = "for (Integer i : numbers) sum += i;";

// 	let expected = Stmt {
// 		kind: StmtKind::For(ForStmt::Enhanced(
// 			Ty::from(PrimitiveKind::Integer),
// 			Identifier::from("i"),
// 			Expr::from(Identifier::from("numbers")),
// 			Box::new(Block::Inline(Box::new(Stmt {
// 				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
// 					kind: ExprKind::Assignment(
// 						Box::new(Expr::from(Identifier::from("sum"))),
// 						AssignOp::Add,
// 						Box::new(Expr::from(Identifier::from("i"))),
// 					),
// 				})),
// 			}))),
// 		)),
// 	};

// 	test_parse(Rule::statement, input, parse_stmt, expected);
// }

// #[test]
// fn do_while_parses() {
// 	test_parse(
// 		Rule::statement,
// 		r#"do {
// 			return x;
// 		} while (true);"#,
// 		parse_stmt,
// 		Stmt {
// 			kind: StmtKind::DoWhile(
// 				Box::new(Block::from(vec![Stmt {
// 					kind: StmtKind::Return(Some(Expr::from(Identifier::from("x")))),
// 				}])),
// 				Expr::from(Literal::from(true)),
// 			),
// 		},
// 	);
// }

// #[test]
// fn while_parses() {
// 	test_parse(
// 		Rule::statement,
// 		r#"while (true) {
// 		return x;
// 	}"#,
// 		parse_stmt,
// 		Stmt {
// 			kind: StmtKind::While(
// 				Expr::from(Literal::from(true)),
// 				Box::new(Block::from(vec![Stmt {
// 					kind: StmtKind::Return(Some(Expr::from(Identifier::from("x")))),
// 				}])),
// 			),
// 		},
// 	)
// }

// #[test]
// fn if_else_if_else_parses() {
// 	let input = r#"if (foo) {
// 		return bar;
// 	} else if (bar) {
// 		return baz;
// 	} else {
// 		return quux;
// 	}"#;

// 	let expected_kind = StmtKind::If(
// 		Expr::from(Identifier::from("foo")),
// 		Box::new(Block::from(vec![Stmt {
// 			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
// 		}])),
// 		Some(vec![(
// 			Expr::from(Identifier::from("bar")),
// 			Box::new(Block::from(vec![Stmt {
// 				kind: StmtKind::Return(Some(Expr::from(Identifier::from("baz")))),
// 			}])),
// 		)]),
// 		Some(Box::new(Block::from(vec![Stmt {
// 			kind: StmtKind::Return(Some(Expr::from(Identifier::from("quux")))),
// 		}]))),
// 	);

// 	test_parse(
// 		Rule::statement,
// 		input,
// 		parse_stmt,
// 		Stmt {
// 			kind: expected_kind,
// 		},
// 	);
// }

// #[test]
// fn if_else_if_parses() {
// 	let input = r#"if (foo) {
// 		return bar;
// 	} else if (bar) {
// 		return baz;
// 	}"#;

// 	let expected_kind = StmtKind::If(
// 		Expr::from(Identifier::from("foo")),
// 		Box::new(Block::from(vec![Stmt {
// 			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
// 		}])),
// 		Some(vec![(
// 			Expr::from(Identifier::from("bar")),
// 			Box::new(Block::from(vec![Stmt {
// 				kind: StmtKind::Return(Some(Expr::from(Identifier::from("baz")))),
// 			}])),
// 		)]),
// 		None,
// 	);

// 	test_parse(
// 		Rule::statement,
// 		input,
// 		parse_stmt,
// 		Stmt {
// 			kind: expected_kind,
// 		},
// 	);
// }

// #[test]
// fn basic_if_parses() {
// 	let input = r#"if (foo) {
// 		return bar;
// 	}"#;

// 	let expected_kind = StmtKind::If(
// 		Expr::from(Identifier::from("foo")),
// 		Box::new(Block::from(vec![Stmt {
// 			kind: StmtKind::Return(Some(Expr::from(Identifier::from("bar")))),
// 		}])),
// 		None,
// 		None,
// 	);

// 	test_parse(
// 		Rule::statement,
// 		input,
// 		parse_stmt,
// 		Stmt {
// 			kind: expected_kind,
// 		},
// 	);
// }

// #[test]
// fn stmt_expr_postfix_parses() {
// 	let inner: Expr = ExprKind::Identifier(Identifier::from("i")).into();

// 	test_parse(
// 		Rule::statement,
// 		"i++;",
// 		parse_stmt,
// 		Stmt::from(StmtExpr::from(Expr {
// 			kind: ExprKind::Postfix(Box::new(inner), IncDecOp::Inc),
// 		})),
// 	)
// }

// #[test]
// fn stmt_expr_expr_parses() {
// 	let lhs = Expr::from(Identifier::from("foo"));
// 	let rhs = Expr::from(Literal::from(22));

// 	test_parse(
// 		Rule::statement,
// 		"foo = 22;",
// 		parse_stmt,
// 		Stmt::from(StmtExpr::from(Expr {
// 			kind: ExprKind::Assignment(Box::new(lhs), AssignOp::Eq, Box::new(rhs)),
// 		})),
// 	);
// }

// #[test]
// fn stmt_expr_local_parses() {
// 	let test_str = "Integer foo = 22;";

// 	let ty = Ty::from(PrimitiveKind::Integer);
// 	let id = Identifier::from("foo");
// 	let rhs = Expr::from(Literal::from(22));

// 	let expected = Stmt {
// 		kind: StmtKind::StmtExpr(StmtExpr::Local(Local {
// 			annotation: None,
// 			is_final: false,
// 			ty,
// 			id,
// 			rhs: Some(rhs),
// 		})),
// 	};

// 	test_parse(Rule::statement, test_str, parse_stmt, expected);
// }

// #[test]
// fn try_catch_catch_finally_parses() {
// 	let test_str = r#"try {
// 		insert foo;
// 	} catch (Exception e) {
// 		return bar;
// 	} catch (DmlException de) {
// 		return baz;
// 	} finally {
// 		return quux;
// 	}"#;

// 	let try_block = vec![StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into()];

// 	let catch_clause = (
// 		Ty::from(Identifier::from("Exception")),
// 		Identifier::from("e"),
// 		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
// 			"bar",
// 		))))
// 		.into()]),
// 	);

// 	let opt_catch = Some(vec![(
// 		Ty::from(Identifier::from("DmlException")),
// 		Identifier::from("de"),
// 		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
// 			"baz",
// 		))))
// 		.into()]),
// 	)]);

// 	let finally = Some(Block::from(vec![StmtKind::Return(Some(Expr::from(
// 		Identifier::from("quux"),
// 	)))
// 	.into()]));

// 	let expected = StmtKind::TryCatch(
// 		Block::from(try_block).to_boxed(),
// 		catch_clause,
// 		opt_catch,
// 		finally,
// 	);

// 	test_parse(Rule::statement, test_str, parse_stmt, expected.into());
// }

// #[test]
// fn try_catch_simple_parses() {
// 	let test_str = r#"try {
// 		insert foo;
// 	} catch (Exception e) {
// 		return bar;
// 	}"#;

// 	let try_block = vec![StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into()];

// 	let catch_clause = (
// 		Ty::from(Identifier::from("Exception")),
// 		Identifier::from("e"),
// 		Block::from(vec![StmtKind::Return(Some(Expr::from(Identifier::from(
// 			"bar",
// 		))))
// 		.into()]),
// 	);

// 	let expected = StmtKind::TryCatch(Block::from(try_block).to_boxed(), catch_clause, None, None);

// 	test_parse(Rule::statement, test_str, parse_stmt, expected.into());
// }

// #[test]
// fn dml_stmt_parses() {
// 	test_parse(
// 		Rule::statement,
// 		"insert foo;",
// 		parse_stmt,
// 		StmtKind::Dml(DmlOp::Insert, Expr::from(Identifier::from("foo"))).into(),
// 	);
// }

// #[test]
// fn throw_stmt_parses() {
// 	test_parse(
// 		Rule::statement,
// 		"throw new TestException();",
// 		parse_stmt,
// 		StmtKind::Throw(
// 			ExprKind::New(
// 				Ty::from(Identifier::from("TestException")),
// 				NewType::Class(ClassArgs::Basic(None)),
// 			)
// 			.into(),
// 		)
// 		.into(),
// 	);
// }

#[test]
fn return_stmt_some_parses() {
	let input = "return foo;";

	let expected = Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Identifier(Identifier {
				name: String::from("foo"),
				span: Span {
					start: 7,
					end: 10,
					start_pos: Position { line: 1, col: 8 },
					end_pos: Position { line: 1, col: 11 },
				},
			}),
			span: Span {
				start: 7,
				end: 10,
				start_pos: Position { line: 1, col: 8 },
				end_pos: Position { line: 1, col: 11 },
			},
		})),
		span: Span {
			start: 0,
			end: 11,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected)
}

#[test]
fn return_stmt_none_parses() {
	let input = "return;";

	let expected = Stmt {
		kind: StmtKind::Return(None),
		span: Span {
			start: 0,
			end: 7,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn break_stmt_parses() {
	let input = "break;";

	let expected = Stmt {
		kind: StmtKind::Break,
		span: Span {
			start: 0,
			end: 6,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 7 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn continue_stmt_parses() {
	let input = "continue;";

	let expected = Stmt {
		kind: StmtKind::Continue,
		span: Span {
			start: 0,
			end: 9,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
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
				span: Span {
					start: 1,
					end: 12,
					start_pos: Position { line: 1, col: 2 },
					end_pos: Position { line: 1, col: 13 },
				},
			},
			keypairs: None,
			span: Span {
				start: 0,
				end: 12,
				start_pos: Position::default(),
				end_pos: Position { line: 1, col: 13 },
			},
		},
	)
}

#[test]
fn annotation_with_attributes_parses() {
	let input = "@AuraEnabled(continuation=true cacheable=true)";

	let expected = Annotation {
		name: Identifier {
			name: String::from("AuraEnabled"),
			span: Span {
				start: 1,
				end: 12,
				start_pos: Position { line: 1, col: 2 },
				end_pos: Position { line: 1, col: 13 },
			},
		},
		keypairs: Some(vec![
			(
				Identifier {
					name: String::from("continuation"),
					span: Span {
						start: 13,
						end: 25,
						start_pos: Position { line: 1, col: 14 },
						end_pos: Position { line: 1, col: 26 },
					},
				},
				Literal {
					kind: LiteralKind::Boolean(true),
					span: Span {
						start: 26,
						end: 30,
						start_pos: Position { line: 1, col: 27 },
						end_pos: Position { line: 1, col: 31 },
					},
				},
			),
			(
				Identifier {
					name: String::from("cacheable"),
					span: Span {
						start: 31,
						end: 40,
						start_pos: Position { line: 1, col: 32 },
						end_pos: Position { line: 1, col: 41 },
					},
				},
				Literal {
					kind: LiteralKind::Boolean(true),
					span: Span {
						start: 41,
						end: 45,
						start_pos: Position { line: 1, col: 42 },
						end_pos: Position { line: 1, col: 46 },
					},
				},
			),
		]),
		span: Span {
			start: 0,
			end: 46,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 47 },
		},
	};

	test_parse(Rule::annotation, input, parse_annotation, expected)
}

#[test]
fn null_literal_parse_correctly() {
	let input = "null";

	let expected = Literal {
		kind: LiteralKind::Null,
		span: Span {
			start: 0,
			end: 4,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 5 },
		},
	};

	test_parse(Rule::literal, input, parse_literal, expected);
}

#[test]
fn integer_literal_parse_correctly() {
	let input = "2";

	let expected = Literal {
		kind: LiteralKind::Integer(2),
		span: Span {
			start: 0,
			end: 1,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 2 },
		},
	};

	test_parse(Rule::literal, input, parse_literal, expected)
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
			span: Span {
				start: 0,
				end: 13,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 14 },
			},
		},
	)
}

#[test]
fn identifier_parses_correctly() {
	let input = "x";

	let expected = Identifier {
		name: String::from("x"),
		span: Span {
			start: 0,
			end: 1,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 2 },
		},
	};

	test_parse(Rule::identifier, input, parse_identifier, expected);
}

#[test]
fn ty_primitive_parses() {
	let input = "Integer";

	let expected = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
		span: Span {
			start: 0,
			end: 7,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_void_parses() {
	let input = "void";

	let expected = Ty {
		kind: TyKind::Void,
		span: Span {
			start: 0,
			end: 4,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 5 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_void_case_insensitive_parses() {
	let input = "VOID";
	let expected = Ty {
		kind: TyKind::Void,
		span: Span {
			start: 0,
			end: 4,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 5 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_primitive_array_parses() {
	let input = "Integer[]";
	let expected = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: true,
		}),
		span: Span {
			start: 0,
			end: 9,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_class_parses() {
	let input = "Foo";
	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_class_array_parses() {
	let input = "Foo[]";

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: true,
			span: Span {
				start: 0,
				end: 5,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 6 },
			},
		}),
		span: Span {
			start: 0,
			end: 5,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 6 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_subclass_parses() {
	let input = "Foo.Bar";

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: Some(Identifier {
				name: String::from("Bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			}),
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 0,
				end: 7,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 8 },
			},
		}),
		span: Span {
			start: 0,
			end: 7,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_basic_generic_parses() {
	let input = "Foo<Bar>";

	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 4,
				end: 7,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 8 },
			},
		}),
		span: Span {
			start: 4,
			end: 7,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(subtype), None)),
			is_array: false,
			span: Span {
				start: 0,
				end: 8,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 9 },
			},
		}),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_generic_subtype_parses() {
	let input = "Foo<Bar.Baz>";

	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: Some(Identifier {
				name: String::from("Baz"),
				span: Span {
					start: 8,
					end: 11,
					start_pos: Position { line: 1, col: 9 },
					end_pos: Position { line: 1, col: 12 },
				},
			}),
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 4,
				end: 11,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 4,
			end: 11,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(subtype), None)),
			is_array: false,
			span: Span {
				start: 0,
				end: 12,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 13 },
			},
		}),
		span: Span {
			start: 0,
			end: 12,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 13 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_generic_subtype_array_parses() {
	let input = "Foo<Bar.Baz>[]";

	let subtype = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: Some(Identifier {
				name: String::from("Baz"),
				span: Span {
					start: 8,
					end: 11,
					start_pos: Position { line: 1, col: 9 },
					end_pos: Position { line: 1, col: 12 },
				},
			}),
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 4,
				end: 11,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 4,
			end: 11,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(subtype), None)),
			is_array: true,
			span: Span {
				start: 0,
				end: 14,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 15 },
			},
		}),
		span: Span {
			start: 0,
			end: 14,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 15 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_subtype_generic_parses() {
	let input = "Foo.Bar<Baz>";
	let gen_type = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Baz"),
				span: Span {
					start: 8,
					end: 11,
					start_pos: Position { line: 1, col: 9 },
					end_pos: Position { line: 1, col: 12 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 8,
				end: 11,
				start_pos: Position { line: 1, col: 9 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 8,
			end: 11,
			start_pos: Position { line: 1, col: 9 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let type_args = (Box::new(gen_type), None);

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: Some(Identifier {
				name: String::from("Bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			}),
			type_arguments: Some(type_args),
			is_array: false,
			span: Span {
				start: 0,
				end: 12,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 13 },
			},
		}),
		span: Span {
			start: 0,
			end: 12,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 13 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn ty_two_type_args_parses() {
	let input = "Map<Id, String>";

	let id_type = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::ID,
			is_array: false,
		}),
		span: Span {
			start: 4,
			end: 6,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 7 },
		},
	};

	let string_type = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::String,
			is_array: false,
		}),
		span: Span {
			start: 8,
			end: 14,
			start_pos: Position { line: 1, col: 9 },
			end_pos: Position { line: 1, col: 15 },
		},
	};

	let expected = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Map"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(id_type), Some(Box::new(string_type)))),
			is_array: false,
			span: Span {
				start: 0,
				end: 15,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 16 },
			},
		}),
		span: Span {
			start: 0,
			end: 15,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 16 },
		},
	};

	test_parse(Rule::basic_type, input, parse_ty, expected);
}

#[test]
fn expr_literal_parses() {
	let input = "2";
	let expected = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Integer(2),
			span: Span {
				start: 0,
				end: 1,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 2 },
			},
		}),
		span: Span {
			start: 0,
			end: 1,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 2 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn expr_identifier_parses() {
	let input = "foo";

	let expected = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected)
}

#[test]
fn expr_type_expr_parses() {
	let input = "Foo.class";

	let expected = Expr {
		kind: ExprKind::Type(Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Foo"),
					span: Span {
						start: 0,
						end: 3,
						start_pos: Position { line: 1, col: 1 },
						end_pos: Position { line: 1, col: 4 },
					},
				},
				is_array: false,
				subclass: None,
				type_arguments: None,
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			}),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 9,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn instanceof_expr_parses() {
	let input = "foo instanceof Foo";

	let id = Identifier {
		name: String::from("foo"),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 15,
					end: 18,
					start_pos: Position { line: 1, col: 16 },
					end_pos: Position { line: 1, col: 19 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 15,
				end: 18,
				start_pos: Position { line: 1, col: 16 },
				end_pos: Position { line: 1, col: 19 },
			},
		}),
		span: Span {
			start: 15,
			end: 18,
			start_pos: Position { line: 1, col: 16 },
			end_pos: Position { line: 1, col: 19 },
		},
	};

	let expected = Expr {
		kind: ExprKind::Instanceof(id, ty),
		span: Span {
			start: 0,
			end: 18,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 19 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn cast_expr_parses() {
	let input = "(String) foo";

	let ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::String,
			is_array: false,
		}),
		span: Span {
			start: 1,
			end: 7,
			start_pos: Position { line: 1, col: 2 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	let expr = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 9,
				end: 12,
				start_pos: Position { line: 1, col: 10 },
				end_pos: Position { line: 1, col: 13 },
			},
		}),
		span: Span {
			start: 9,
			end: 12,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 13 },
		},
	};

	let expected = Expr {
		kind: ExprKind::Cast(ty, Box::new(expr)),
		span: Span {
			start: 0,
			end: 12,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 13 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn prefix_expr_parses() {
	let input = "++i";

	let inner = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("i"),
			span: Span {
				start: 2,
				end: 3,
				start_pos: Position { line: 1, col: 3 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 2,
			end: 3,
			start_pos: Position { line: 1, col: 3 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	let expected = Expr {
		kind: ExprKind::Prefix(IncDecOp::Inc, Box::new(inner)),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected)
}

#[test]
fn postfix_expr_parses() {
	let input = "i++";

	let inner = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("i"),
			span: Span {
				start: 0,
				end: 1,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 2 },
			},
		}),
		span: Span {
			start: 0,
			end: 1,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 2 },
		},
	};

	let expceted = Expr {
		kind: ExprKind::Postfix(Box::new(inner), IncDecOp::Inc),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expceted);
}

#[test]
fn unary_expr_parses() {
	let input = "!i";

	let inner = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("i"),
			span: Span {
				start: 1,
				end: 2,
				start_pos: Position { line: 1, col: 2 },
				end_pos: Position { line: 1, col: 3 },
			},
		}),
		span: Span {
			start: 1,
			end: 2,
			start_pos: Position { line: 1, col: 2 },
			end_pos: Position { line: 1, col: 3 },
		},
	};

	let expected = Expr {
		kind: ExprKind::Unary(UnOp::Not, Box::new(inner)),
		span: Span {
			start: 0,
			end: 2,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 3 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn method_call_no_args_parses() {
	let input = "foo()";

	let expected = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			None,
		),
		span: Span {
			start: 0,
			end: 5,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 6 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn method_call_one_arg_parses() {
	let input = "foo(bar)";

	let expected = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			Some(vec![Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("bar"),
					span: Span {
						start: 4,
						end: 7,
						start_pos: Position { line: 1, col: 5 },
						end_pos: Position { line: 1, col: 8 },
					},
				}),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			}]),
		),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn method_call_two_args_parses() {
	let input = "foo(bar, 'baz')";

	let expected = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			Some(vec![
				Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 4,
							end: 7,
							start_pos: Position { line: 1, col: 5 },
							end_pos: Position { line: 1, col: 8 },
						},
					}),
					span: Span {
						start: 4,
						end: 7,
						start_pos: Position { line: 1, col: 5 },
						end_pos: Position { line: 1, col: 8 },
					},
				},
				Expr {
					kind: ExprKind::Literal(Literal {
						kind: LiteralKind::String(String::from("'baz'")),
						span: Span {
							start: 9,
							end: 14,
							start_pos: Position { line: 1, col: 10 },
							end_pos: Position { line: 1, col: 15 },
						},
					}),
					span: Span {
						start: 9,
						end: 14,
						start_pos: Position { line: 1, col: 10 },
						end_pos: Position { line: 1, col: 15 },
					},
				},
			]),
		),
		span: Span {
			start: 0,
			end: 15,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 16 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_array_literal_parses() {
	let input = "new Integer[1, 2, 3]";

	let ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
		span: Span {
			start: 4,
			end: 11,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let literal_exprs = vec![
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(1),
				span: Span {
					start: 12,
					end: 13,
					start_pos: Position { line: 1, col: 13 },
					end_pos: Position { line: 1, col: 14 },
				},
			}),
			span: Span {
				start: 12,
				end: 13,
				start_pos: Position { line: 1, col: 13 },
				end_pos: Position { line: 1, col: 14 },
			},
		},
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(2),
				span: Span {
					start: 15,
					end: 16,
					start_pos: Position { line: 1, col: 16 },
					end_pos: Position { line: 1, col: 17 },
				},
			}),
			span: Span {
				start: 15,
				end: 16,
				start_pos: Position { line: 1, col: 16 },
				end_pos: Position { line: 1, col: 17 },
			},
		},
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(3),
				span: Span {
					start: 18,
					end: 19,
					start_pos: Position { line: 1, col: 19 },
					end_pos: Position { line: 1, col: 20 },
				},
			}),
			span: Span {
				start: 18,
				end: 19,
				start_pos: Position { line: 1, col: 19 },
				end_pos: Position { line: 1, col: 20 },
			},
		},
	];

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Array(literal_exprs)),
		span: Span {
			start: 4,
			end: 20,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 21 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_collection_literal_parses() {
	let input = "new List<Integer>{1, 2, 3}";

	let int_ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
		span: Span {
			start: 9,
			end: 16,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("List"),
				span: Span {
					start: 4,
					end: 8,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 9 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(int_ty), None)),
			is_array: false,
			span: Span {
				start: 4,
				end: 26,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 27 },
			},
		}),
		span: Span {
			start: 4,
			end: 26,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 27 },
		},
	};

	let literal_exprs = vec![
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(1),
				span: Span {
					start: 18,
					end: 19,
					start_pos: Position { line: 1, col: 19 },
					end_pos: Position { line: 1, col: 20 },
				},
			}),
			span: Span {
				start: 18,
				end: 19,
				start_pos: Position { line: 1, col: 19 },
				end_pos: Position { line: 1, col: 20 },
			},
		},
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(2),
				span: Span {
					start: 21,
					end: 22,
					start_pos: Position { line: 1, col: 22 },
					end_pos: Position { line: 1, col: 23 },
				},
			}),
			span: Span {
				start: 21,
				end: 22,
				start_pos: Position { line: 1, col: 22 },
				end_pos: Position { line: 1, col: 23 },
			},
		},
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(3),
				span: Span {
					start: 24,
					end: 25,
					start_pos: Position { line: 1, col: 25 },
					end_pos: Position { line: 1, col: 26 },
				},
			}),
			span: Span {
				start: 24,
				end: 25,
				start_pos: Position { line: 1, col: 25 },
				end_pos: Position { line: 1, col: 26 },
			},
		},
	];

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Collection(literal_exprs)),
		span: Span {
			start: 17,
			end: 26,
			start_pos: Position { line: 1, col: 18 },
			end_pos: Position { line: 1, col: 27 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_collection_literal_with_args_parses() {
	let input = "new List<Integer>(list)";

	let int_ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
		span: Span {
			start: 9,
			end: 16,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("List"),
				span: Span {
					start: 4,
					end: 8,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 9 },
				},
			},
			subclass: None,
			type_arguments: Some((Box::new(int_ty), None)),
			is_array: false,
			span: Span {
				start: 4,
				end: 23,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 24 },
			},
		}),
		span: Span {
			start: 4,
			end: 23,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 24 },
		},
	};

	let args = Some(vec![Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("list"),
			span: Span {
				start: 18,
				end: 22,
				start_pos: Position { line: 1, col: 19 },
				end_pos: Position { line: 1, col: 23 },
			},
		}),
		span: Span {
			start: 18,
			end: 22,
			start_pos: Position { line: 1, col: 19 },
			end_pos: Position { line: 1, col: 23 },
		},
	}]);

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(args))),
		span: Span {
			start: 17,
			end: 23,
			start_pos: Position { line: 1, col: 18 },
			end_pos: Position { line: 1, col: 24 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_map_literal_parses() {
	let input = "new Map<Integer, String>{1 => 'one', 2 => 'two'}";

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Map"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: None,
			type_arguments: type_args!(
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::Integer,
						is_array: false,
					}),
					span: Span {
						start: 8,
						end: 15,
						start_pos: Position { line: 1, col: 9 },
						end_pos: Position { line: 1, col: 16 },
					},
				},
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 17,
						end: 23,
						start_pos: Position { line: 1, col: 18 },
						end_pos: Position { line: 1, col: 24 },
					},
				}
			),
			is_array: false,
			span: Span {
				start: 4,
				end: 48,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 49 },
			},
		}),
		span: Span {
			start: 4,
			end: 48,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 49 },
		},
	};

	let mapping = vec![
		(
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Integer(1),
					span: Span {
						start: 25,
						end: 26,
						start_pos: Position { line: 1, col: 26 },
						end_pos: Position { line: 1, col: 27 },
					},
				}),
				span: Span {
					start: 25,
					end: 26,
					start_pos: Position { line: 1, col: 26 },
					end_pos: Position { line: 1, col: 27 },
				},
			},
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::String(String::from("'one'")),
					span: Span {
						start: 30,
						end: 35,
						start_pos: Position { line: 1, col: 31 },
						end_pos: Position { line: 1, col: 36 },
					},
				}),
				span: Span {
					start: 30,
					end: 35,
					start_pos: Position { line: 1, col: 31 },
					end_pos: Position { line: 1, col: 36 },
				},
			},
		),
		(
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Integer(2),
					span: Span {
						start: 37,
						end: 38,
						start_pos: Position { line: 1, col: 38 },
						end_pos: Position { line: 1, col: 39 },
					},
				}),
				span: Span {
					start: 37,
					end: 38,
					start_pos: Position { line: 1, col: 38 },
					end_pos: Position { line: 1, col: 39 },
				},
			},
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::String(String::from("'two'")),
					span: Span {
						start: 42,
						end: 47,
						start_pos: Position { line: 1, col: 43 },
						end_pos: Position { line: 1, col: 48 },
					},
				}),
				span: Span {
					start: 42,
					end: 47,
					start_pos: Position { line: 1, col: 43 },
					end_pos: Position { line: 1, col: 48 },
				},
			},
		),
	];

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Map(mapping)),
		span: Span {
			start: 24,
			end: 48,
			start_pos: Position { line: 1, col: 25 },
			end_pos: Position { line: 1, col: 49 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_map_args_parses() {
	let input = "new Map<Integer, String>(foo)";

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Map"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: None,
			type_arguments: type_args!(
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::Integer,
						is_array: false,
					}),
					span: Span {
						start: 8,
						end: 15,
						start_pos: Position { line: 1, col: 9 },
						end_pos: Position { line: 1, col: 16 },
					},
				},
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 17,
						end: 23,
						start_pos: Position { line: 1, col: 18 },
						end_pos: Position { line: 1, col: 24 },
					},
				}
			),
			is_array: false,
			span: Span {
				start: 4,
				end: 29,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 30 },
			},
		}),
		span: Span {
			start: 4,
			end: 29,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 30 },
		},
	};

	let class_args = vec![Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 25,
				end: 28,
				start_pos: Position { line: 1, col: 26 },
				end_pos: Position { line: 1, col: 29 },
			},
		}),
		span: Span {
			start: 25,
			end: 28,
			start_pos: Position { line: 1, col: 26 },
			end_pos: Position { line: 1, col: 29 },
		},
	}];

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(Some(class_args)))),
		span: Span {
			start: 24,
			end: 29,
			start_pos: Position { line: 1, col: 25 },
			end_pos: Position { line: 1, col: 30 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_class_parses() {
	let input = "new Foo()";

	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Foo"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 4,
				end: 7,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 8 },
			},
		}),
		span: Span {
			start: 4,
			end: 7,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 8 },
		},
	};

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Class(ClassArgs::Basic(None))),
		span: Span {
			start: 7,
			end: 9,
			start_pos: Position { line: 1, col: 8 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn new_inst_class_sobject_argsparses() {
	let input = "new Account(Name = 'foo')";
	let ty = Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			name: Identifier {
				name: String::from("Account"),
				span: Span {
					start: 4,
					end: 11,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 12 },
				},
			},
			subclass: None,
			type_arguments: None,
			is_array: false,
			span: Span {
				start: 4,
				end: 11,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 4,
			end: 11,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let args = vec![(
		Identifier {
			name: String::from("Name"),
			span: Span {
				start: 12,
				end: 16,
				start_pos: Position { line: 1, col: 13 },
				end_pos: Position { line: 1, col: 17 },
			},
		},
		Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::String(String::from("\'foo\'")),
				span: Span {
					start: 19,
					end: 24,
					start_pos: Position { line: 1, col: 20 },
					end_pos: Position { line: 1, col: 25 },
				},
			}),
			span: Span {
				start: 19,
				end: 24,
				start_pos: Position { line: 1, col: 20 },
				end_pos: Position { line: 1, col: 25 },
			},
		},
	)];

	let expected = Expr {
		kind: ExprKind::New(ty, NewType::Class(ClassArgs::SObject(args))),
		span: Span {
			start: 11,
			end: 25,
			start_pos: Position { line: 1, col: 12 },
			end_pos: Position { line: 1, col: 26 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn list_access_parses() {
	let input = "foo[2]";
	let accessible = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	let access_expr = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Integer(2),
			span: Span {
				start: 4,
				end: 5,
				start_pos: Position { line: 1, col: 5 },
				end_pos: Position { line: 1, col: 6 },
			},
		}),
		span: Span {
			start: 4,
			end: 5,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 6 },
		},
	};

	let expected = Expr {
		kind: ExprKind::ListAccess(Box::new(accessible), Box::new(access_expr)),
		span: Span {
			start: 0,
			end: 6,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 7 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn property_access_parses() {
	let input = "foo.bar()";

	let accessible = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	let selector = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("bar"),
				span: Span {
					start: 4,
					end: 7,
					start_pos: Position { line: 1, col: 5 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			None,
		),

		span: Span {
			start: 4,
			end: 9,
			start_pos: Position { line: 1, col: 5 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	let expected = Expr {
		kind: ExprKind::PropertyAccess(Box::new(accessible), Box::new(selector)),
		span: Span {
			start: 0,
			end: 9,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 10 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}

#[test]
fn soql_query_parses() {
	let query_str = "[ SELECT Foo FROM Bar ]";

	let expected = Expr {
		kind: ExprKind::Query(Query::Soql(String::from("SELECT Foo FROM Bar"))),
		span: Span {
			start: 0,
			end: 23,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 24 },
		},
	};

	test_parse(Rule::expression, query_str, parse_expr, expected);
}

#[test]
fn sosl_query_parses() {
	let query_str = "[ FIND {foo} ]";

	let expected = Expr {
		kind: ExprKind::Query(Query::Sosl(String::from("FIND {foo}"))),
		span: Span {
			start: 0,
			end: 14,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 15 },
		},
	};

	test_parse(Rule::expression, query_str, parse_expr, expected);
}

#[test]
fn assignment_parses() {
	let input = "foo = 22";

	let lhs = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("foo"),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		}),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	let rhs = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Integer(22),
			span: Span {
				start: 6,
				end: 8,
				start_pos: Position { line: 1, col: 7 },
				end_pos: Position { line: 1, col: 9 },
			},
		}),
		span: Span {
			start: 6,
			end: 8,
			start_pos: Position { line: 1, col: 7 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	let expcted = Expr {
		kind: ExprKind::Assignment(Box::new(lhs), AssignOp::Eq, Box::new(rhs)),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expcted);
}

#[test]
fn ternary_parses() {
	let input = "isTrue ? true : false";

	let test = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("isTrue"),
			span: Span {
				start: 0,
				end: 6,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 7 },
			},
		}),
		span: Span {
			start: 0,
			end: 6,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 7 },
		},
	};

	let pos = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Boolean(true),
			span: Span {
				start: 9,
				end: 13,
				start_pos: Position { line: 1, col: 10 },
				end_pos: Position { line: 1, col: 14 },
			},
		}),
		span: Span {
			start: 9,
			end: 13,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 14 },
		},
	};

	let neg = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Boolean(false),
			span: Span {
				start: 16,
				end: 21,
				start_pos: Position { line: 1, col: 17 },
				end_pos: Position { line: 1, col: 22 },
			},
		}),
		span: Span {
			start: 16,
			end: 21,
			start_pos: Position { line: 1, col: 17 },
			end_pos: Position { line: 1, col: 22 },
		},
	};

	test_parse(
		Rule::expression,
		input,
		parse_expr,
		Expr {
			kind: ExprKind::Ternary(Box::new(test), Box::new(pos), Box::new(neg)),
			span: Span {
				start: 0,
				end: 21,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 22 },
			},
		},
	);
}

#[test]
fn simple_infix_expr_parses() {
	let input = "foo() == null";

	let lhs = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			None,
		),
		span: Span {
			start: 0,
			end: 5,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 6 },
		},
	};

	let op = BinOp::Eq;

	let rhs = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Null,
			span: Span {
				start: 9,
				end: 13,
				start_pos: Position { line: 1, col: 10 },
				end_pos: Position { line: 1, col: 14 },
			},
		}),
		span: Span {
			start: 9,
			end: 13,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 14 },
		},
	};

	test_parse(
		Rule::expression,
		input,
		parse_expr,
		Expr {
			kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
			span: Span {
				start: 0,
				end: 13,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 14 },
			},
		},
	);
}

#[test]
fn nested_infix_expr_parses() {
	let input = "foo() == 2 + bar";

	let lhs = Expr {
		kind: ExprKind::Call(
			Identifier {
				name: String::from("foo"),
				span: Span {
					start: 0,
					end: 3,
					start_pos: Position { line: 1, col: 1 },
					end_pos: Position { line: 1, col: 4 },
				},
			},
			None,
		),
		span: Span {
			start: 0,
			end: 5,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 6 },
		},
	};

	let op = BinOp::Eq;

	let rhs = Expr {
		kind: ExprKind::Infix(
			Box::new(Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Integer(2),
					span: Span {
						start: 9,
						end: 10,
						start_pos: Position { line: 1, col: 10 },
						end_pos: Position { line: 1, col: 11 },
					},
				}),
				span: Span {
					start: 9,
					end: 10,
					start_pos: Position { line: 1, col: 10 },
					end_pos: Position { line: 1, col: 11 },
				},
			}),
			BinOp::Add,
			Box::new(Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("bar"),
					span: Span {
						start: 13,
						end: 16,
						start_pos: Position { line: 1, col: 14 },
						end_pos: Position { line: 1, col: 17 },
					},
				}),
				span: Span {
					start: 13,
					end: 16,
					start_pos: Position { line: 1, col: 14 },
					end_pos: Position { line: 1, col: 17 },
				},
			}),
		),
		span: Span {
			start: 9,
			end: 16,
			start_pos: Position { line: 1, col: 10 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	let expected = Expr {
		kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
		span: Span {
			start: 0,
			end: 16,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	test_parse(Rule::expression, input, parse_expr, expected);
}
