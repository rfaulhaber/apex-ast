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

#[test]
fn virtual_class_parses() {
	let input = "public virtual with sharing class Foo {}";

	let expected = Class {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		impl_mod: Some(ClassImplMod::Virtual),
		sharing_mod: Some(ClassSharingMod::With),
		name: Identifier {
			name: String::from("Foo"),
			span: Span {
				start: 34,
				end: 37,
				start_pos: Position { line: 1, col: 35 },
				end_pos: Position { line: 1, col: 38 },
			},
		},
		extension: None,
		implementations: Vec::new(),
		body: Vec::new(),
		span: Span {
			start: 0,
			end: 40,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 41 },
		},
	};

	test_parse(Rule::class_declaration, input, parse_class, expected);
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
		impl_mod: None,
		sharing_mod: Some(ClassSharingMod::With),
		name: Identifier {
			name: String::from("FileWriter"),
			span: Span {
				start: 26,
				end: 36,
				start_pos: Position { line: 1, col: 27 },
				end_pos: Position { line: 1, col: 37 },
			},
		},
		extension: None,
		implementations: vec![Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Writer"),
					span: Span {
						start: 48,
						end: 54,
						start_pos: Position { line: 1, col: 49 },
						end_pos: Position { line: 1, col: 55 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 48,
					end: 55,
					start_pos: Position { line: 1, col: 49 },
					end_pos: Position { line: 1, col: 56 },
				},
			}),
			span: Span {
				start: 48,
				end: 55,
				start_pos: Position { line: 1, col: 49 },
				end_pos: Position { line: 1, col: 56 },
			},
		}],
		body: vec![
			ClassBodyMember::Field(ClassField {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				instance_mod: None,
				is_final: false,
				ty: Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						name: Identifier {
							name: String::from("Buffer"),
							span: Span {
								start: 66,
								end: 72,
								start_pos: Position { line: 2, col: 10 },
								end_pos: Position { line: 2, col: 16 },
							},
						},
						subclass: None,
						type_arguments: None,
						is_array: false,
						span: Span {
							start: 66,
							end: 73,
							start_pos: Position { line: 2, col: 10 },
							end_pos: Position { line: 2, col: 17 },
						},
					}),
					span: Span {
						start: 66,
						end: 73,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 17 },
					},
				},
				id: Identifier {
					name: String::from("buf"),
					span: Span {
						start: 73,
						end: 76,
						start_pos: Position { line: 2, col: 17 },
						end_pos: Position { line: 2, col: 20 },
					},
				},
				getter: None,
				setter: None,
				rhs: None,
				span: Span {
					start: 59,
					end: 77,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 21 },
				},
			}),
			ClassBodyMember::Field(ClassField {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				instance_mod: Some(ClassInstanceModifier::Static),
				is_final: true,
				ty: Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 100,
						end: 107,
						start_pos: Position { line: 3, col: 23 },
						end_pos: Position { line: 3, col: 30 },
					},
				},
				id: Identifier {
					name: String::from("name"),
					span: Span {
						start: 107,
						end: 111,
						start_pos: Position { line: 3, col: 30 },
						end_pos: Position { line: 3, col: 34 },
					},
				},
				getter: None,
				setter: None,
				rhs: None,
				span: Span {
					start: 80,
					end: 112,
					start_pos: Position { line: 3, col: 3 },
					end_pos: Position { line: 3, col: 35 },
				},
			}),
			ClassBodyMember::StaticBlock(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("name"),
								span: Span {
									start: 128,
									end: 132,
									start_pos: Position { line: 6, col: 4 },
									end_pos: Position { line: 6, col: 8 },
								},
							}),
							span: Span {
								start: 128,
								end: 132,
								start_pos: Position { line: 6, col: 4 },
								end_pos: Position { line: 6, col: 8 },
							},
						}),
						AssignOp::Eq,
						Box::new(Expr {
							kind: ExprKind::Literal(Literal {
								kind: LiteralKind::String(String::from("'WRITER'")),
								span: Span {
									start: 135,
									end: 143,
									start_pos: Position { line: 6, col: 11 },
									end_pos: Position { line: 6, col: 19 },
								},
							}),
							span: Span {
								start: 135,
								end: 143,
								start_pos: Position { line: 6, col: 11 },
								end_pos: Position { line: 6, col: 19 },
							},
						}),
					),
					span: Span {
						start: 128,
						end: 143,
						start_pos: Position { line: 6, col: 4 },
						end_pos: Position { line: 6, col: 19 },
					},
				})),
				span: Span {
					start: 128,
					end: 143,
					start_pos: Position { line: 6, col: 4 },
					end_pos: Position { line: 6, col: 19 },
				},
			}])),
			ClassBodyMember::InstanceBlock(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("buf"),
								span: Span {
									start: 157,
									end: 160,
									start_pos: Position { line: 10, col: 4 },
									end_pos: Position { line: 10, col: 7 },
								},
							}),
							span: Span {
								start: 157,
								end: 160,
								start_pos: Position { line: 10, col: 4 },
								end_pos: Position { line: 10, col: 7 },
							},
						}),
						AssignOp::Eq,
						Box::from(Expr {
							kind: ExprKind::New(
								Ty {
									kind: TyKind::ClassOrInterface(ClassOrInterface {
										name: Identifier {
											name: String::from("Buffer"),
											span: Span {
												start: 167,
												end: 173,
												start_pos: Position { line: 10, col: 14 },
												end_pos: Position { line: 10, col: 20 },
											},
										},
										subclass: None,
										type_arguments: None,
										is_array: false,
										span: Span {
											start: 167,
											end: 173,
											start_pos: Position { line: 10, col: 14 },
											end_pos: Position { line: 10, col: 20 },
										},
									}),
									span: Span {
										start: 167,
										end: 173,
										start_pos: Position { line: 10, col: 14 },
										end_pos: Position { line: 10, col: 20 },
									},
								},
								NewType::Class(ClassArgs::Basic(None)),
							),
							span: Span {
								start: 163,
								end: 175,
								start_pos: Position { line: 10, col: 10 },
								end_pos: Position { line: 10, col: 22 },
							},
						}),
					),
					span: Span {
						start: 157,
						end: 175,
						start_pos: Position { line: 10, col: 4 },
						end_pos: Position { line: 10, col: 22 },
					},
				})),
				span: Span {
					start: 157,
					end: 175,
					start_pos: Position { line: 10, col: 4 },
					end_pos: Position { line: 10, col: 22 },
				},
			}])),
			ClassBodyMember::Constructor(ClassConstructor {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				identifier: Identifier {
					name: String::from("FileWriter"),
					span: Span {
						start: 191,
						end: 201,
						start_pos: Position { line: 13, col: 10 },
						end_pos: Position { line: 13, col: 20 },
					},
				},
				params: Vec::new(),
				block: Block::Body(Vec::new()),
				span: Span {
					start: 184,
					end: 206,
					start_pos: Position { line: 13, col: 3 },
					end_pos: Position { line: 13, col: 25 },
				},
			}),
			ClassBodyMember::Method(ClassMethod {
				annotation: None,
				access_mod: Some(AccessModifier::Public),
				impl_mod: None,
				is_testmethod: false,
				return_type: Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::Integer,
						is_array: false,
					}),
					span: Span {
						start: 217,
						end: 225,
						start_pos: Position { line: 15, col: 10 },
						end_pos: Position { line: 15, col: 18 },
					},
				},
				identifier: Identifier {
					name: String::from("write"),
					span: Span {
						start: 225,
						end: 230,
						start_pos: Position { line: 15, col: 18 },
						end_pos: Position { line: 15, col: 23 },
					},
				},
				params: vec![(
					Ty {
						kind: TyKind::Primitive(Primitive {
							kind: PrimitiveKind::String,
							is_array: false,
						}),
						span: Span {
							start: 231,
							end: 238,
							start_pos: Position { line: 15, col: 24 },
							end_pos: Position { line: 15, col: 31 },
						},
					},
					Identifier {
						name: String::from("output"),
						span: Span {
							start: 238,
							end: 244,
							start_pos: Position { line: 15, col: 31 },
							end_pos: Position { line: 15, col: 37 },
						},
					},
				)],
				block: Some(Block::Body(Vec::new())),
				span: Span {
					start: 210,
					end: 248,
					start_pos: Position { line: 15, col: 3 },
					end_pos: Position { line: 15, col: 41 },
				},
			}),
		],
		span: Span {
			start: 0,
			end: 250,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 16, col: 2 },
		},
	};

	test_parse(Rule::class_declaration, input, parse_class, expected);
}

#[test]
fn class_constructor_parses() {
	let input = "public Foo(String bar) {}";

	let expected = ClassConstructor {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		identifier: Identifier {
			name: String::from("Foo"),
			span: Span {
				start: 7,
				end: 10,
				start_pos: Position { line: 1, col: 8 },
				end_pos: Position { line: 1, col: 11 },
			},
		},
		params: vec![(
			Ty {
				kind: TyKind::Primitive(Primitive {
					kind: PrimitiveKind::String,
					is_array: false,
				}),
				span: Span {
					start: 11,
					end: 18,
					start_pos: Position { line: 1, col: 12 },
					end_pos: Position { line: 1, col: 19 },
				},
			},
			Identifier {
				name: String::from("bar"),
				span: Span {
					start: 18,
					end: 21,
					start_pos: Position { line: 1, col: 19 },
					end_pos: Position { line: 1, col: 22 },
				},
			},
		)],
		block: Block::Body(Vec::new()),
		span: Span {
			start: 0,
			end: 25,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 26 },
		},
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
		identifier: Identifier {
			name: String::from("Foo"),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		},
		params: Vec::new(),
		block: Block::Body(Vec::new()),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
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
		return_type: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::Integer,
				is_array: false,
			}),
			span: Span {
				start: 9,
				end: 17,
				start_pos: Position { line: 1, col: 10 },
				end_pos: Position { line: 1, col: 18 },
			},
		},
		identifier: Identifier {
			name: String::from("abstractMethod"),
			span: Span {
				start: 17,
				end: 31,
				start_pos: Position { line: 1, col: 18 },
				end_pos: Position { line: 1, col: 32 },
			},
		},
		params: Vec::new(),
		block: None,
		span: Span {
			start: 0,
			end: 34,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 35 },
		},
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
		name: Identifier {
			name: String::from("Writer"),
			span: Span {
				start: 17,
				end: 23,
				start_pos: Position { line: 1, col: 18 },
				end_pos: Position { line: 1, col: 24 },
			},
		},
		extensions: vec![Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Foo"),
					span: Span {
						start: 32,
						end: 35,
						start_pos: Position { line: 1, col: 33 },
						end_pos: Position { line: 1, col: 36 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 32,
					end: 36,
					start_pos: Position { line: 1, col: 33 },
					end_pos: Position { line: 1, col: 37 },
				},
			}),
			span: Span {
				start: 32,
				end: 36,
				start_pos: Position { line: 1, col: 33 },
				end_pos: Position { line: 1, col: 37 },
			},
		}],
		methods: vec![ImplementableMethod {
			ty: Ty {
				kind: TyKind::Primitive(Primitive {
					kind: PrimitiveKind::Integer,
					is_array: false,
				}),
				span: Span {
					start: 40,
					end: 48,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 11 },
				},
			},
			id: Identifier {
				name: String::from("write"),
				span: Span {
					start: 48,
					end: 53,
					start_pos: Position { line: 2, col: 11 },
					end_pos: Position { line: 2, col: 16 },
				},
			},
			params: vec![(
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 54,
						end: 61,
						start_pos: Position { line: 2, col: 17 },
						end_pos: Position { line: 2, col: 24 },
					},
				},
				Identifier {
					name: String::from("output"),
					span: Span {
						start: 61,
						end: 67,
						start_pos: Position { line: 2, col: 24 },
						end_pos: Position { line: 2, col: 30 },
					},
				},
			)],
			span: Span {
				start: 40,
				end: 69,
				start_pos: Position { line: 2, col: 3 },
				end_pos: Position { line: 2, col: 32 },
			},
		}],
		span: Span {
			start: 0,
			end: 72,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 3 },
		},
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
		name: Identifier {
			name: String::from("Writer"),
			span: Span {
				start: 10,
				end: 16,
				start_pos: Position { line: 1, col: 11 },
				end_pos: Position { line: 1, col: 17 },
			},
		},
		extensions: vec![Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Foo"),
					span: Span {
						start: 25,
						end: 28,
						start_pos: Position { line: 1, col: 26 },
						end_pos: Position { line: 1, col: 29 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 25,
					end: 29,
					start_pos: Position { line: 1, col: 26 },
					end_pos: Position { line: 1, col: 30 },
				},
			}),
			span: Span {
				start: 25,
				end: 29,
				start_pos: Position { line: 1, col: 26 },
				end_pos: Position { line: 1, col: 30 },
			},
		}],
		methods: vec![ImplementableMethod {
			ty: Ty {
				kind: TyKind::Primitive(Primitive {
					kind: PrimitiveKind::Integer,
					is_array: false,
				}),
				span: Span {
					start: 33,
					end: 41,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 11 },
				},
			},
			id: Identifier {
				name: String::from("write"),
				span: Span {
					start: 41,
					end: 46,
					start_pos: Position { line: 2, col: 11 },
					end_pos: Position { line: 2, col: 16 },
				},
			},
			params: vec![(
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 47,
						end: 54,
						start_pos: Position { line: 2, col: 17 },
						end_pos: Position { line: 2, col: 24 },
					},
				},
				Identifier {
					name: String::from("output"),
					span: Span {
						start: 54,
						end: 60,
						start_pos: Position { line: 2, col: 24 },
						end_pos: Position { line: 2, col: 30 },
					},
				},
			)],
			span: Span {
				start: 33,
				end: 62,
				start_pos: Position { line: 2, col: 3 },
				end_pos: Position { line: 2, col: 32 },
			},
		}],
		span: Span {
			start: 0,
			end: 65,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 3 },
		},
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
		name: Identifier {
			name: String::from("Writer"),
			span: Span {
				start: 25,
				end: 31,
				start_pos: Position { line: 1, col: 26 },
				end_pos: Position { line: 1, col: 32 },
			},
		},
		extensions: Vec::new(),
		methods: vec![ImplementableMethod {
			ty: Ty {
				kind: TyKind::Primitive(Primitive {
					kind: PrimitiveKind::Integer,
					is_array: false,
				}),
				span: Span {
					start: 36,
					end: 44,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 11 },
				},
			},
			id: Identifier {
				name: String::from("write"),
				span: Span {
					start: 44,
					end: 49,
					start_pos: Position { line: 2, col: 11 },
					end_pos: Position { line: 2, col: 16 },
				},
			},
			params: vec![(
				Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::String,
						is_array: false,
					}),
					span: Span {
						start: 50,
						end: 57,
						start_pos: Position { line: 2, col: 17 },
						end_pos: Position { line: 2, col: 24 },
					},
				},
				Identifier {
					name: String::from("output"),
					span: Span {
						start: 57,
						end: 63,
						start_pos: Position { line: 2, col: 24 },
						end_pos: Position { line: 2, col: 30 },
					},
				},
			)],
			span: Span {
				start: 36,
				end: 65,
				start_pos: Position { line: 2, col: 3 },
				end_pos: Position { line: 2, col: 32 },
			},
		}],
		span: Span {
			start: 0,
			end: 68,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 3 },
		},
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
		name: Identifier {
			name: String::from("MyAccountTrigger"),
			span: Span {
				start: 8,
				end: 24,
				start_pos: Position { line: 1, col: 9 },
				end_pos: Position { line: 1, col: 25 },
			},
		},
		object: Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Account"),
					span: Span {
						start: 28,
						end: 35,
						start_pos: Position { line: 1, col: 29 },
						end_pos: Position { line: 1, col: 36 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 28,
					end: 36,
					start_pos: Position { line: 1, col: 29 },
					end_pos: Position { line: 1, col: 37 },
				},
			}),
			span: Span {
				start: 28,
				end: 36,
				start_pos: Position { line: 1, col: 29 },
				end_pos: Position { line: 1, col: 37 },
			},
		},
		events: vec![
			TriggerEvent {
				when: TriggerWhen::Before,
				op: DmlOp::Insert,
			},
			TriggerEvent {
				when: TriggerWhen::Before,
				op: DmlOp::Update,
			},
		],
		body: Block::Body(Vec::new()),
		span: Span {
			start: 0,
			end: 69,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 70 },
		},
	};

	test_parse(Rule::trigger_declaration, input, parse_trigger, expected);
}

#[test]
fn enum_parses() {
	let input = "public enum Season {WINTER, SPRING, SUMMER, FALL}";

	let expected = Enum {
		annotation: None,
		access_mod: Some(AccessModifier::Public),
		name: Identifier {
			name: String::from("Season"),
			span: Span {
				start: 12,
				end: 18,
				start_pos: Position { line: 1, col: 13 },
				end_pos: Position { line: 1, col: 19 },
			},
		},
		ids: vec![
			Identifier {
				name: String::from("WINTER"),
				span: Span {
					start: 20,
					end: 26,
					start_pos: Position { line: 1, col: 21 },
					end_pos: Position { line: 1, col: 27 },
				},
			},
			Identifier {
				name: String::from("SPRING"),
				span: Span {
					start: 28,
					end: 34,
					start_pos: Position { line: 1, col: 29 },
					end_pos: Position { line: 1, col: 35 },
				},
			},
			Identifier {
				name: String::from("SUMMER"),
				span: Span {
					start: 36,
					end: 42,
					start_pos: Position { line: 1, col: 37 },
					end_pos: Position { line: 1, col: 43 },
				},
			},
			Identifier {
				name: String::from("FALL"),
				span: Span {
					start: 44,
					end: 48,
					start_pos: Position { line: 1, col: 45 },
					end_pos: Position { line: 1, col: 49 },
				},
			},
		],
		span: Span {
			start: 0,
			end: 49,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 50 },
		},
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
		ty: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::String,
				is_array: false,
			}),
			span: Span {
				start: 14,
				end: 21,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 22 },
			},
		},
		id: Identifier {
			name: String::from("foo"),
			span: Span {
				start: 21,
				end: 24,
				start_pos: Position { line: 1, col: 22 },
				end_pos: Position { line: 1, col: 25 },
			},
		},
		getter: None,
		setter: None,
		rhs: None,
		span: Span {
			start: 0,
			end: 25,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 26 },
		},
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
		ty: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::String,
				is_array: false,
			}),
			span: Span {
				start: 7,
				end: 14,
				start_pos: Position { line: 1, col: 8 },
				end_pos: Position { line: 1, col: 15 },
			},
		},
		id: Identifier {
			name: String::from("name"),
			span: Span {
				start: 14,
				end: 18,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 19 },
			},
		},
		getter: Some(Property {
			access_mod: None,
			property_type: PropertyType::Get,
			body: None,
			span: Span {
				start: 20,
				end: 24,
				start_pos: Position { line: 1, col: 21 },
				end_pos: Position { line: 1, col: 25 },
			},
		}),
		setter: Some(Property {
			access_mod: None,
			property_type: PropertyType::Set,
			body: None,
			span: Span {
				start: 25,
				end: 29,
				start_pos: Position { line: 1, col: 26 },
				end_pos: Position { line: 1, col: 30 },
			},
		}),
		rhs: None,
		span: Span {
			start: 0,
			end: 30,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 31 },
		},
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
		ty: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::Integer,
				is_array: false,
			}),
			span: Span {
				start: 14,
				end: 22,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 23 },
			},
		},
		id: Identifier {
			name: String::from("foo"),
			span: Span {
				start: 22,
				end: 25,
				start_pos: Position { line: 1, col: 23 },
				end_pos: Position { line: 1, col: 26 },
			},
		},
		getter: None,
		setter: None,
		rhs: Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(22),
				span: Span {
					start: 28,
					end: 30,
					start_pos: Position { line: 1, col: 29 },
					end_pos: Position { line: 1, col: 31 },
				},
			}),
			span: Span {
				start: 28,
				end: 30,
				start_pos: Position { line: 1, col: 29 },
				end_pos: Position { line: 1, col: 31 },
			},
		}),
		span: Span {
			start: 0,
			end: 31,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 32 },
		},
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
		ty: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::String,
				is_array: false,
			}),
			span: Span {
				start: 7,
				end: 14,
				start_pos: Position { line: 1, col: 8 },
				end_pos: Position { line: 1, col: 15 },
			},
		},
		id: Identifier {
			name: String::from("name"),
			span: Span {
				start: 14,
				end: 18,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 19 },
			},
		},
		getter: Some(Property {
			access_mod: Some(AccessModifier::Public),
			property_type: PropertyType::Get,
			body: Some(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Literal(Literal {
						kind: LiteralKind::String(String::from("'foo'")),
						span: Span {
							start: 46,
							end: 51,
							start_pos: Position { line: 3, col: 11 },
							end_pos: Position { line: 3, col: 16 },
						},
					}),
					span: Span {
						start: 46,
						end: 51,
						start_pos: Position { line: 3, col: 11 },
						end_pos: Position { line: 3, col: 16 },
					},
				})),
				span: Span {
					start: 39,
					end: 52,
					start_pos: Position { line: 3, col: 4 },
					end_pos: Position { line: 3, col: 17 },
				},
			}])),
			span: Span {
				start: 23,
				end: 56,
				start_pos: Position { line: 2, col: 3 },
				end_pos: Position { line: 4, col: 4 },
			},
		}),
		setter: Some(Property {
			access_mod: Some(AccessModifier::Private),
			property_type: PropertyType::Set,
			body: Some(Block::Body(vec![Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("name"),
								span: Span {
									start: 76,
									end: 80,
									start_pos: Position { line: 6, col: 4 },
									end_pos: Position { line: 6, col: 8 },
								},
							}),
							span: Span {
								start: 76,
								end: 80,
								start_pos: Position { line: 6, col: 4 },
								end_pos: Position { line: 6, col: 8 },
							},
						}),
						AssignOp::Eq,
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("value"),
								span: Span {
									start: 83,
									end: 88,
									start_pos: Position { line: 6, col: 11 },
									end_pos: Position { line: 6, col: 16 },
								},
							}),
							span: Span {
								start: 83,
								end: 88,
								start_pos: Position { line: 6, col: 11 },
								end_pos: Position { line: 6, col: 16 },
							},
						}),
					),
					span: Span {
						start: 76,
						end: 88,
						start_pos: Position { line: 6, col: 4 },
						end_pos: Position { line: 6, col: 16 },
					},
				})),
				span: Span {
					start: 76,
					end: 88,
					start_pos: Position { line: 6, col: 4 },
					end_pos: Position { line: 6, col: 16 },
				},
			}])),
			span: Span {
				start: 59,
				end: 93,
				start_pos: Position { line: 5, col: 3 },
				end_pos: Position { line: 7, col: 4 },
			},
		}),
		rhs: None,
		span: Span {
			start: 0,
			end: 96,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 8, col: 3 },
		},
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
		annotation: Some(Annotation {
			name: Identifier {
				name: String::from("isTest"),
				span: Span {
					start: 1,
					end: 7,
					start_pos: Position { line: 1, col: 2 },
					end_pos: Position { line: 1, col: 8 },
				},
			},
			keypairs: None,
			span: Span {
				start: 0,
				end: 8,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 9 },
			},
		}),
		access_mod: Some(AccessModifier::Public),
		impl_mod: Some(ImplModifier::Static),
		is_testmethod: true,
		return_type: Ty {
			kind: TyKind::Primitive(Primitive {
				kind: PrimitiveKind::Integer,
				is_array: false,
			}),
			span: Span {
				start: 33,
				end: 41,
				start_pos: Position { line: 1, col: 34 },
				end_pos: Position { line: 1, col: 42 },
			},
		},
		identifier: Identifier {
			name: String::from("foo"),
			span: Span {
				start: 41,
				end: 44,
				start_pos: Position { line: 1, col: 42 },
				end_pos: Position { line: 1, col: 45 },
			},
		},
		params: vec![
			(
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						name: Identifier {
							name: String::from("Bar"),
							span: Span {
								start: 45,
								end: 48,
								start_pos: Position { line: 1, col: 46 },
								end_pos: Position { line: 1, col: 49 },
							},
						},
						subclass: None,
						type_arguments: None,
						is_array: false,
						span: Span {
							start: 45,
							end: 49,
							start_pos: Position { line: 1, col: 46 },
							end_pos: Position { line: 1, col: 50 },
						},
					}),
					span: Span {
						start: 45,
						end: 49,
						start_pos: Position { line: 1, col: 46 },
						end_pos: Position { line: 1, col: 50 },
					},
				},
				Identifier {
					name: String::from("b"),
					span: Span {
						start: 49,
						end: 50,
						start_pos: Position { line: 1, col: 50 },
						end_pos: Position { line: 1, col: 51 },
					},
				},
			),
			(
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						name: Identifier {
							name: String::from("Baz"),
							span: Span {
								start: 52,
								end: 55,
								start_pos: Position { line: 1, col: 53 },
								end_pos: Position { line: 1, col: 56 },
							},
						},
						subclass: None,
						type_arguments: None,
						is_array: false,
						span: Span {
							start: 52,
							end: 56,
							start_pos: Position { line: 1, col: 53 },
							end_pos: Position { line: 1, col: 57 },
						},
					}),
					span: Span {
						start: 52,
						end: 56,
						start_pos: Position { line: 1, col: 53 },
						end_pos: Position { line: 1, col: 57 },
					},
				},
				Identifier {
					name: String::from("bz"),
					span: Span {
						start: 56,
						end: 58,
						start_pos: Position { line: 1, col: 57 },
						end_pos: Position { line: 1, col: 59 },
					},
				},
			),
		],
		block: Some(Block::Body(Vec::new())),
		span: Span {
			start: 0,
			end: 62,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 63 },
		},
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
		return_type: Ty {
			kind: TyKind::Void,
			span: Span {
				start: 14,
				end: 19,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 20 },
			},
		},
		identifier: Identifier {
			name: String::from("foo"),
			span: Span {
				start: 19,
				end: 22,
				start_pos: Position { line: 1, col: 20 },
				end_pos: Position { line: 1, col: 23 },
			},
		},
		params: Vec::new(),
		block: Some(Block::Body(Vec::new())),
		span: Span {
			start: 0,
			end: 27,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 28 },
		},
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

	let expr = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("i"),
			span: Span {
				start: 10,
				end: 11,
				start_pos: Position { line: 1, col: 11 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 10,
			end: 11,
			start_pos: Position { line: 1, col: 11 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let expected = Stmt {
		kind: StmtKind::Switch(expr, None, None),
		span: Span {
			start: 0,
			end: 14,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 15 },
		},
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

	let test_expr = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("i"),
			span: Span {
				start: 10,
				end: 11,
				start_pos: Position { line: 1, col: 11 },
				end_pos: Position { line: 1, col: 12 },
			},
		}),
		span: Span {
			start: 10,
			end: 11,
			start_pos: Position { line: 1, col: 11 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let first_when_values = WhenCondition::Value(vec![
		WhenValue::Literal(Literal {
			kind: LiteralKind::Integer(2),
			span: Span {
				start: 22,
				end: 23,
				start_pos: Position { line: 2, col: 9 },
				end_pos: Position { line: 2, col: 10 },
			},
		}),
		WhenValue::Literal(Literal {
			kind: LiteralKind::Integer(3),
			span: Span {
				start: 25,
				end: 26,
				start_pos: Position { line: 2, col: 12 },
				end_pos: Position { line: 2, col: 13 },
			},
		}),
		WhenValue::Literal(Literal {
			kind: LiteralKind::Integer(4),
			span: Span {
				start: 28,
				end: 29,
				start_pos: Position { line: 2, col: 15 },
				end_pos: Position { line: 2, col: 16 },
			},
		}),
	]);

	let first_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(1),
				span: Span {
					start: 46,
					end: 47,
					start_pos: Position { line: 3, col: 15 },
					end_pos: Position { line: 3, col: 16 },
				},
			}),
			span: Span {
				start: 46,
				end: 47,
				start_pos: Position { line: 3, col: 15 },
				end_pos: Position { line: 3, col: 16 },
			},
		})),
		span: Span {
			start: 39,
			end: 48,
			start_pos: Position { line: 3, col: 8 },
			end_pos: Position { line: 3, col: 17 },
		},
	}]);

	let second_when_values = WhenCondition::Value(vec![
		WhenValue::Literal(Literal {
			kind: LiteralKind::Integer(5),
			span: Span {
				start: 62,
				end: 63,
				start_pos: Position { line: 5, col: 9 },
				end_pos: Position { line: 5, col: 10 },
			},
		}),
		WhenValue::Literal(Literal {
			kind: LiteralKind::Integer(6),
			span: Span {
				start: 65,
				end: 66,
				start_pos: Position { line: 5, col: 12 },
				end_pos: Position { line: 5, col: 13 },
			},
		}),
	]);

	let second_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(2),
				span: Span {
					start: 83,
					end: 84,
					start_pos: Position { line: 6, col: 15 },
					end_pos: Position { line: 6, col: 16 },
				},
			}),
			span: Span {
				start: 83,
				end: 84,
				start_pos: Position { line: 6, col: 15 },
				end_pos: Position { line: 6, col: 16 },
			},
		})),
		span: Span {
			start: 76,
			end: 85,
			start_pos: Position { line: 6, col: 8 },
			end_pos: Position { line: 6, col: 17 },
		},
	}]);

	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal {
		kind: LiteralKind::Integer(7),
		span: Span {
			start: 99,
			end: 100,
			start_pos: Position { line: 8, col: 9 },
			end_pos: Position { line: 8, col: 10 },
		},
	})]);

	let third_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(3),
				span: Span {
					start: 117,
					end: 118,
					start_pos: Position { line: 9, col: 15 },
					end_pos: Position { line: 9, col: 16 },
				},
			}),
			span: Span {
				start: 117,
				end: 118,
				start_pos: Position { line: 9, col: 15 },
				end_pos: Position { line: 9, col: 16 },
			},
		})),
		span: Span {
			start: 110,
			end: 119,
			start_pos: Position { line: 9, col: 8 },
			end_pos: Position { line: 9, col: 17 },
		},
	}]);

	let else_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(4),
				span: Span {
					start: 154,
					end: 155,
					start_pos: Position { line: 12, col: 15 },
					end_pos: Position { line: 12, col: 16 },
				},
			}),
			span: Span {
				start: 154,
				end: 155,
				start_pos: Position { line: 12, col: 15 },
				end_pos: Position { line: 12, col: 16 },
			},
		})),
		span: Span {
			start: 147,
			end: 156,
			start_pos: Position { line: 12, col: 8 },
			end_pos: Position { line: 12, col: 17 },
		},
	}]);

	let expected = Stmt {
		kind: StmtKind::Switch(
			test_expr,
			Some(vec![
				(first_when_values, first_block),
				(second_when_values, second_block),
				(third_when_values, third_block),
			]),
			Some(else_block),
		),
		span: Span {
			start: 0,
			end: 163,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 14, col: 2 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
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

	let test_expr = Expr {
		kind: ExprKind::Identifier(Identifier {
			name: String::from("sobject"),
			span: Span {
				start: 10,
				end: 17,
				start_pos: Position { line: 1, col: 11 },
				end_pos: Position { line: 1, col: 18 },
			},
		}),
		span: Span {
			start: 10,
			end: 17,
			start_pos: Position { line: 1, col: 11 },
			end_pos: Position { line: 1, col: 18 },
		},
	};

	let first_when_values = WhenCondition::Type(
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Account"),
					span: Span {
						start: 28,
						end: 35,
						start_pos: Position { line: 2, col: 9 },
						end_pos: Position { line: 2, col: 16 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 28,
					end: 36,
					start_pos: Position { line: 2, col: 9 },
					end_pos: Position { line: 2, col: 17 },
				},
			}),
			span: Span {
				start: 28,
				end: 36,
				start_pos: Position { line: 2, col: 9 },
				end_pos: Position { line: 2, col: 17 },
			},
		},
		Identifier {
			name: String::from("a"),
			span: Span {
				start: 36,
				end: 37,
				start_pos: Position { line: 2, col: 17 },
				end_pos: Position { line: 2, col: 18 },
			},
		},
	);

	let first_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(1),
				span: Span {
					start: 54,
					end: 55,
					start_pos: Position { line: 3, col: 15 },
					end_pos: Position { line: 3, col: 16 },
				},
			}),
			span: Span {
				start: 54,
				end: 55,
				start_pos: Position { line: 3, col: 15 },
				end_pos: Position { line: 3, col: 16 },
			},
		})),
		span: Span {
			start: 47,
			end: 56,
			start_pos: Position { line: 3, col: 8 },
			end_pos: Position { line: 3, col: 17 },
		},
	}]);

	let second_when_values = WhenCondition::Type(
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Contact"),
					span: Span {
						start: 70,
						end: 77,
						start_pos: Position { line: 5, col: 9 },
						end_pos: Position { line: 5, col: 16 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 70,
					end: 78,
					start_pos: Position { line: 5, col: 9 },
					end_pos: Position { line: 5, col: 17 },
				},
			}),
			span: Span {
				start: 70,
				end: 78,
				start_pos: Position { line: 5, col: 9 },
				end_pos: Position { line: 5, col: 17 },
			},
		},
		Identifier {
			name: String::from("c"),
			span: Span {
				start: 78,
				end: 79,
				start_pos: Position { line: 5, col: 17 },
				end_pos: Position { line: 5, col: 18 },
			},
		},
	);

	let second_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(2),
				span: Span {
					start: 96,
					end: 97,
					start_pos: Position { line: 6, col: 15 },
					end_pos: Position { line: 6, col: 16 },
				},
			}),
			span: Span {
				start: 96,
				end: 97,
				start_pos: Position { line: 6, col: 15 },
				end_pos: Position { line: 6, col: 16 },
			},
		})),
		span: Span {
			start: 89,
			end: 98,
			start_pos: Position { line: 6, col: 8 },
			end_pos: Position { line: 6, col: 17 },
		},
	}]);

	let third_when_values = WhenCondition::Value(vec![WhenValue::Literal(Literal {
		kind: LiteralKind::Null,
		span: Span {
			start: 112,
			end: 116,
			start_pos: Position { line: 8, col: 9 },
			end_pos: Position { line: 8, col: 13 },
		},
	})]);

	let third_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(3),
				span: Span {
					start: 133,
					end: 134,
					start_pos: Position { line: 9, col: 15 },
					end_pos: Position { line: 9, col: 16 },
				},
			}),
			span: Span {
				start: 133,
				end: 134,
				start_pos: Position { line: 9, col: 15 },
				end_pos: Position { line: 9, col: 16 },
			},
		})),
		span: Span {
			start: 126,
			end: 135,
			start_pos: Position { line: 9, col: 8 },
			end_pos: Position { line: 9, col: 17 },
		},
	}]);

	let else_block = Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Literal(Literal {
				kind: LiteralKind::Integer(4),
				span: Span {
					start: 170,
					end: 171,
					start_pos: Position { line: 12, col: 15 },
					end_pos: Position { line: 12, col: 16 },
				},
			}),
			span: Span {
				start: 170,
				end: 171,
				start_pos: Position { line: 12, col: 15 },
				end_pos: Position { line: 12, col: 16 },
			},
		})),
		span: Span {
			start: 163,
			end: 172,
			start_pos: Position { line: 12, col: 8 },
			end_pos: Position { line: 12, col: 17 },
		},
	}]);

	let expected = Stmt {
		kind: StmtKind::Switch(
			test_expr,
			Some(vec![
				(first_when_values, first_block),
				(second_when_values, second_block),
				(third_when_values, third_block),
			]),
			Some(else_block),
		),
		span: Span {
			start: 0,
			end: 179,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 14, col: 2 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn for_basic_simple_parses() {
	let input = "for (Integer i = 0; i < 10; i++) sum += i;";

	let expected = Stmt {
		kind: StmtKind::For(ForStmt::Basic(
			Some(vec![StmtExpr::Local(Local {
				annotation: None,
				is_final: false,
				ty: Ty {
					kind: TyKind::Primitive(Primitive {
						kind: PrimitiveKind::Integer,
						is_array: false,
					}),
					span: Span {
						start: 5,
						end: 13,
						start_pos: Position { line: 1, col: 6 },
						end_pos: Position { line: 1, col: 14 },
					},
				},
				id: Identifier {
					name: String::from("i"),
					span: Span {
						start: 13,
						end: 14,
						start_pos: Position { line: 1, col: 14 },
						end_pos: Position { line: 1, col: 15 },
					},
				},
				rhs: Some(Expr {
					kind: ExprKind::Literal(Literal {
						kind: LiteralKind::Integer(0),
						span: Span {
							start: 17,
							end: 18,
							start_pos: Position { line: 1, col: 18 },
							end_pos: Position { line: 1, col: 19 },
						},
					}),
					span: Span {
						start: 17,
						end: 18,
						start_pos: Position { line: 1, col: 18 },
						end_pos: Position { line: 1, col: 19 },
					},
				}),
				span: Span {
					start: 5,
					end: 18,
					start_pos: Position { line: 1, col: 6 },
					end_pos: Position { line: 1, col: 19 },
				},
			})]),
			Some(Expr {
				kind: ExprKind::Infix(
					Box::new(Expr {
						kind: ExprKind::Identifier(Identifier {
							name: String::from("i"),
							span: Span {
								start: 20,
								end: 21,
								start_pos: Position { line: 1, col: 21 },
								end_pos: Position { line: 1, col: 22 },
							},
						}),
						span: Span {
							start: 20,
							end: 21,
							start_pos: Position { line: 1, col: 21 },
							end_pos: Position { line: 1, col: 22 },
						},
					}),
					BinOp::Lt,
					Box::new(Expr {
						kind: ExprKind::Literal(Literal {
							kind: LiteralKind::Integer(10),
							span: Span {
								start: 24,
								end: 26,
								start_pos: Position { line: 1, col: 25 },
								end_pos: Position { line: 1, col: 27 },
							},
						}),
						span: Span {
							start: 24,
							end: 26,
							start_pos: Position { line: 1, col: 25 },
							end_pos: Position { line: 1, col: 27 },
						},
					}),
				),
				span: Span {
					start: 20,
					end: 26,
					start_pos: Position { line: 1, col: 21 },
					end_pos: Position { line: 1, col: 27 },
				},
			}),
			Some(StmtExpr::Expr(Expr {
				kind: ExprKind::Postfix(
					Box::new(Expr {
						kind: ExprKind::Identifier(Identifier {
							name: String::from("i"),
							span: Span {
								start: 28,
								end: 29,
								start_pos: Position { line: 1, col: 29 },
								end_pos: Position { line: 1, col: 30 },
							},
						}),
						span: Span {
							start: 28,
							end: 29,
							start_pos: Position { line: 1, col: 29 },
							end_pos: Position { line: 1, col: 30 },
						},
					}),
					IncDecOp::Inc,
				),
				span: Span {
					start: 28,
					end: 31,
					start_pos: Position { line: 1, col: 29 },
					end_pos: Position { line: 1, col: 32 },
				},
			})),
			Box::new(Block::Inline(Box::new(Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("sum"),
								span: Span {
									start: 33,
									end: 36,
									start_pos: Position { line: 1, col: 34 },
									end_pos: Position { line: 1, col: 37 },
								},
							}),
							span: Span {
								start: 33,
								end: 36,
								start_pos: Position { line: 1, col: 34 },
								end_pos: Position { line: 1, col: 37 },
							},
						}),
						AssignOp::Add,
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("i"),
								span: Span {
									start: 40,
									end: 41,
									start_pos: Position { line: 1, col: 41 },
									end_pos: Position { line: 1, col: 42 },
								},
							}),
							span: Span {
								start: 40,
								end: 41,
								start_pos: Position { line: 1, col: 41 },
								end_pos: Position { line: 1, col: 42 },
							},
						}),
					),
					span: Span {
						start: 33,
						end: 41,
						start_pos: Position { line: 1, col: 34 },
						end_pos: Position { line: 1, col: 42 },
					},
				})),
				span: Span {
					start: 33,
					end: 41,
					start_pos: Position { line: 1, col: 34 },
					end_pos: Position { line: 1, col: 42 },
				},
			}))),
		)),
		span: Span {
			start: 0,
			end: 42,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 43 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn for_enhanced_parses() {
	let input = "for (Integer i : numbers) sum += i;";

	let expected = Stmt {
		kind: StmtKind::For(ForStmt::Enhanced(
			Ty {
				kind: TyKind::Primitive(Primitive {
					kind: PrimitiveKind::Integer,
					is_array: false,
				}),
				span: Span {
					start: 5,
					end: 13,
					start_pos: Position { line: 1, col: 6 },
					end_pos: Position { line: 1, col: 14 },
				},
			},
			Identifier {
				name: String::from("i"),
				span: Span {
					start: 13,
					end: 14,
					start_pos: Position { line: 1, col: 14 },
					end_pos: Position { line: 1, col: 15 },
				},
			},
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("numbers"),
					span: Span {
						start: 17,
						end: 24,
						start_pos: Position { line: 1, col: 18 },
						end_pos: Position { line: 1, col: 25 },
					},
				}),
				span: Span {
					start: 17,
					end: 24,
					start_pos: Position { line: 1, col: 18 },
					end_pos: Position { line: 1, col: 25 },
				},
			},
			Box::new(Block::Inline(Box::new(Stmt {
				kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
					kind: ExprKind::Assignment(
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("sum"),
								span: Span {
									start: 26,
									end: 29,
									start_pos: Position { line: 1, col: 27 },
									end_pos: Position { line: 1, col: 30 },
								},
							}),
							span: Span {
								start: 26,
								end: 29,
								start_pos: Position { line: 1, col: 27 },
								end_pos: Position { line: 1, col: 30 },
							},
						}),
						AssignOp::Add,
						Box::new(Expr {
							kind: ExprKind::Identifier(Identifier {
								name: String::from("i"),
								span: Span {
									start: 33,
									end: 34,
									start_pos: Position { line: 1, col: 34 },
									end_pos: Position { line: 1, col: 35 },
								},
							}),
							span: Span {
								start: 33,
								end: 34,
								start_pos: Position { line: 1, col: 34 },
								end_pos: Position { line: 1, col: 35 },
							},
						}),
					),
					span: Span {
						start: 26,
						end: 34,
						start_pos: Position { line: 1, col: 27 },
						end_pos: Position { line: 1, col: 35 },
					},
				})),
				span: Span {
					start: 26,
					end: 34,
					start_pos: Position { line: 1, col: 27 },
					end_pos: Position { line: 1, col: 35 },
				},
			}))),
		)),
		span: Span {
			start: 0,
			end: 35,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 36 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn do_while_parses() {
	let input = r#"do {
			return x;
		} while (true);"#;

	let expected = Stmt {
		kind: StmtKind::DoWhile(
			Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("x"),
						span: Span {
							start: 15,
							end: 16,
							start_pos: Position { line: 2, col: 11 },
							end_pos: Position { line: 2, col: 12 },
						},
					}),
					span: Span {
						start: 15,
						end: 16,
						start_pos: Position { line: 2, col: 11 },
						end_pos: Position { line: 2, col: 12 },
					},
				})),
				span: Span {
					start: 8,
					end: 17,
					start_pos: Position { line: 2, col: 4 },
					end_pos: Position { line: 2, col: 13 },
				},
			}])),
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Boolean(true),
					span: Span {
						start: 29,
						end: 33,
						start_pos: Position { line: 3, col: 12 },
						end_pos: Position { line: 3, col: 16 },
					},
				}),
				span: Span {
					start: 29,
					end: 33,
					start_pos: Position { line: 3, col: 12 },
					end_pos: Position { line: 3, col: 16 },
				},
			},
		),
		span: Span {
			start: 0,
			end: 35,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 18 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn while_parses() {
	let input = r#"while (true) {
		return x;
	}"#;

	let expected = Stmt {
		kind: StmtKind::While(
			Expr {
				kind: ExprKind::Literal(Literal {
					kind: LiteralKind::Boolean(true),
					span: Span {
						start: 7,
						end: 11,
						start_pos: Position { line: 1, col: 8 },
						end_pos: Position { line: 1, col: 12 },
					},
				}),
				span: Span {
					start: 7,
					end: 11,
					start_pos: Position { line: 1, col: 8 },
					end_pos: Position { line: 1, col: 12 },
				},
			},
			Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("x"),
						span: Span {
							start: 24,
							end: 25,
							start_pos: Position { line: 2, col: 10 },
							end_pos: Position { line: 2, col: 11 },
						},
					}),
					span: Span {
						start: 24,
						end: 25,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 11 },
					},
				})),
				span: Span {
					start: 17,
					end: 26,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 12 },
				},
			}])),
		),
		span: Span {
			start: 0,
			end: 29,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 3 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
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

	let expected = Stmt {
		kind: StmtKind::If(
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("foo"),
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
			Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 20,
							end: 23,
							start_pos: Position { line: 2, col: 10 },
							end_pos: Position { line: 2, col: 13 },
						},
					}),
					span: Span {
						start: 20,
						end: 23,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 13 },
					},
				})),
				span: Span {
					start: 13,
					end: 24,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 14 },
				},
			}])),
			Some(vec![(
				Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 37,
							end: 40,
							start_pos: Position { line: 3, col: 13 },
							end_pos: Position { line: 3, col: 16 },
						},
					}),
					span: Span {
						start: 37,
						end: 40,
						start_pos: Position { line: 3, col: 13 },
						end_pos: Position { line: 3, col: 16 },
					},
				},
				Box::new(Block::Body(vec![Stmt {
					kind: StmtKind::Return(Some(Expr {
						kind: ExprKind::Identifier(Identifier {
							name: String::from("baz"),
							span: Span {
								start: 53,
								end: 56,
								start_pos: Position { line: 4, col: 10 },
								end_pos: Position { line: 4, col: 13 },
							},
						}),
						span: Span {
							start: 53,
							end: 56,
							start_pos: Position { line: 4, col: 10 },
							end_pos: Position { line: 4, col: 13 },
						},
					})),
					span: Span {
						start: 46,
						end: 57,
						start_pos: Position { line: 4, col: 3 },
						end_pos: Position { line: 4, col: 14 },
					},
				}])),
			)]),
			Some(Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("quux"),
						span: Span {
							start: 77,
							end: 81,
							start_pos: Position { line: 6, col: 10 },
							end_pos: Position { line: 6, col: 14 },
						},
					}),
					span: Span {
						start: 77,
						end: 81,
						start_pos: Position { line: 6, col: 10 },
						end_pos: Position { line: 6, col: 14 },
					},
				})),
				span: Span {
					start: 70,
					end: 82,
					start_pos: Position { line: 6, col: 3 },
					end_pos: Position { line: 6, col: 15 },
				},
			}]))),
		),
		span: Span {
			start: 0,
			end: 85,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 7, col: 3 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn if_else_if_parses() {
	let input = r#"if (foo) {
		return bar;
	} else if (bar) {
		return baz;
	}"#;

	let expected = Stmt {
		kind: StmtKind::If(
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("foo"),
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
			Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 20,
							end: 23,
							start_pos: Position { line: 2, col: 10 },
							end_pos: Position { line: 2, col: 13 },
						},
					}),
					span: Span {
						start: 20,
						end: 23,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 13 },
					},
				})),
				span: Span {
					start: 13,
					end: 24,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 14 },
				},
			}])),
			Some(vec![(
				Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 37,
							end: 40,
							start_pos: Position { line: 3, col: 13 },
							end_pos: Position { line: 3, col: 16 },
						},
					}),
					span: Span {
						start: 37,
						end: 40,
						start_pos: Position { line: 3, col: 13 },
						end_pos: Position { line: 3, col: 16 },
					},
				},
				Box::new(Block::Body(vec![Stmt {
					kind: StmtKind::Return(Some(Expr {
						kind: ExprKind::Identifier(Identifier {
							name: String::from("baz"),
							span: Span {
								start: 53,
								end: 56,
								start_pos: Position { line: 4, col: 10 },
								end_pos: Position { line: 4, col: 13 },
							},
						}),
						span: Span {
							start: 53,
							end: 56,
							start_pos: Position { line: 4, col: 10 },
							end_pos: Position { line: 4, col: 13 },
						},
					})),
					span: Span {
						start: 46,
						end: 57,
						start_pos: Position { line: 4, col: 3 },
						end_pos: Position { line: 4, col: 14 },
					},
				}])),
			)]),
			None,
		),
		span: Span {
			start: 0,
			end: 60,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 5, col: 3 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn basic_if_parses() {
	let input = r#"if (foo) {
		return bar;
	}"#;

	let expected = Stmt {
		kind: StmtKind::If(
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("foo"),
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
			Box::new(Block::Body(vec![Stmt {
				kind: StmtKind::Return(Some(Expr {
					kind: ExprKind::Identifier(Identifier {
						name: String::from("bar"),
						span: Span {
							start: 20,
							end: 23,
							start_pos: Position { line: 2, col: 10 },
							end_pos: Position { line: 2, col: 13 },
						},
					}),
					span: Span {
						start: 20,
						end: 23,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 13 },
					},
				})),
				span: Span {
					start: 13,
					end: 24,
					start_pos: Position { line: 2, col: 3 },
					end_pos: Position { line: 2, col: 14 },
				},
			}])),
			None,
			None,
		),
		span: Span {
			start: 0,
			end: 27,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 3, col: 3 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn stmt_expr_postfix_parses() {
	let input = "i++;";

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

	let expected = Stmt {
		kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
			kind: ExprKind::Postfix(Box::new(inner), IncDecOp::Inc),
			span: Span {
				start: 0,
				end: 3,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 4 },
			},
		})),
		span: Span {
			start: 0,
			end: 3,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 4 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn stmt_expr_expr_parses() {
	let input = "foo = 22;";

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

	let expected = Stmt {
		kind: StmtKind::StmtExpr(StmtExpr::Expr(Expr {
			kind: ExprKind::Assignment(Box::new(lhs), AssignOp::Eq, Box::new(rhs)),
			span: Span {
				start: 0,
				end: 8,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 9 },
			},
		})),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn stmt_expr_local_parses() {
	let input = "Integer foo = 22;";

	let ty = Ty {
		kind: TyKind::Primitive(Primitive {
			kind: PrimitiveKind::Integer,
			is_array: false,
		}),
		span: Span {
			start: 0,
			end: 8,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 9 },
		},
	};

	let id = Identifier {
		name: String::from("foo"),
		span: Span {
			start: 8,
			end: 11,
			start_pos: Position { line: 1, col: 9 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	let rhs = Expr {
		kind: ExprKind::Literal(Literal {
			kind: LiteralKind::Integer(22),
			span: Span {
				start: 14,
				end: 16,
				start_pos: Position { line: 1, col: 15 },
				end_pos: Position { line: 1, col: 17 },
			},
		}),
		span: Span {
			start: 14,
			end: 16,
			start_pos: Position { line: 1, col: 15 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	let expected = Stmt {
		kind: StmtKind::StmtExpr(StmtExpr::Local(Local {
			annotation: None,
			is_final: false,
			ty,
			id,
			rhs: Some(rhs),
			span: Span {
				start: 0,
				end: 16,
				start_pos: Position { line: 1, col: 1 },
				end_pos: Position { line: 1, col: 17 },
			},
		})),
		span: Span {
			start: 0,
			end: 16,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 17 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
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

	let try_block = Box::new(Block::Body(vec![Stmt {
		kind: StmtKind::Dml(
			DmlOp::Insert,
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("foo"),
					span: Span {
						start: 15,
						end: 18,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 13 },
					},
				}),
				span: Span {
					start: 15,
					end: 18,
					start_pos: Position { line: 2, col: 10 },
					end_pos: Position { line: 2, col: 13 },
				},
			},
		),
		span: Span {
			start: 8,
			end: 19,
			start_pos: Position { line: 2, col: 3 },
			end_pos: Position { line: 2, col: 14 },
		},
	}]));

	let catch_clause = (
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Exception"),
					span: Span {
						start: 30,
						end: 39,
						start_pos: Position { line: 3, col: 11 },
						end_pos: Position { line: 3, col: 20 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 30,
					end: 40,
					start_pos: Position { line: 3, col: 11 },
					end_pos: Position { line: 3, col: 21 },
				},
			}),
			span: Span {
				start: 30,
				end: 40,
				start_pos: Position { line: 3, col: 11 },
				end_pos: Position { line: 3, col: 21 },
			},
		},
		Identifier {
			name: String::from("e"),
			span: Span {
				start: 40,
				end: 41,
				start_pos: Position { line: 3, col: 21 },
				end_pos: Position { line: 3, col: 22 },
			},
		},
		Block::Body(vec![Stmt {
			kind: StmtKind::Return(Some(Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("bar"),
					span: Span {
						start: 54,
						end: 57,
						start_pos: Position { line: 4, col: 10 },
						end_pos: Position { line: 4, col: 13 },
					},
				}),
				span: Span {
					start: 54,
					end: 57,
					start_pos: Position { line: 4, col: 10 },
					end_pos: Position { line: 4, col: 13 },
				},
			})),
			span: Span {
				start: 47,
				end: 58,
				start_pos: Position { line: 4, col: 3 },
				end_pos: Position { line: 4, col: 14 },
			},
		}]),
	);

	let opt_catch = Some(vec![(
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("DmlException"),
					span: Span {
						start: 69,
						end: 81,
						start_pos: Position { line: 5, col: 11 },
						end_pos: Position { line: 5, col: 23 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 69,
					end: 82,
					start_pos: Position { line: 5, col: 11 },
					end_pos: Position { line: 5, col: 24 },
				},
			}),
			span: Span {
				start: 69,
				end: 82,
				start_pos: Position { line: 5, col: 11 },
				end_pos: Position { line: 5, col: 24 },
			},
		},
		Identifier {
			name: String::from("de"),
			span: Span {
				start: 82,
				end: 84,
				start_pos: Position { line: 5, col: 24 },
				end_pos: Position { line: 5, col: 26 },
			},
		},
		Block::Body(vec![Stmt {
			kind: StmtKind::Return(Some(Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("baz"),
					span: Span {
						start: 97,
						end: 100,
						start_pos: Position { line: 6, col: 10 },
						end_pos: Position { line: 6, col: 13 },
					},
				}),
				span: Span {
					start: 97,
					end: 100,
					start_pos: Position { line: 6, col: 10 },
					end_pos: Position { line: 6, col: 13 },
				},
			})),
			span: Span {
				start: 90,
				end: 101,
				start_pos: Position { line: 6, col: 3 },
				end_pos: Position { line: 6, col: 14 },
			},
		}]),
	)]);

	let finally = Some(Block::Body(vec![Stmt {
		kind: StmtKind::Return(Some(Expr {
			kind: ExprKind::Identifier(Identifier {
				name: String::from("quux"),
				span: Span {
					start: 124,
					end: 128,
					start_pos: Position { line: 8, col: 10 },
					end_pos: Position { line: 8, col: 14 },
				},
			}),
			span: Span {
				start: 124,
				end: 128,
				start_pos: Position { line: 8, col: 10 },
				end_pos: Position { line: 8, col: 14 },
			},
		})),
		span: Span {
			start: 117,
			end: 129,
			start_pos: Position { line: 8, col: 3 },
			end_pos: Position { line: 8, col: 15 },
		},
	}]));

	let expected = Stmt {
		kind: StmtKind::TryCatch(try_block, catch_clause, opt_catch, finally),
		span: Span {
			start: 0,
			end: 132,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 9, col: 3 },
		},
	};

	test_parse(Rule::statement, test_str, parse_stmt, expected);
}

#[test]
fn try_catch_simple_parses() {
	let input = r#"try {
		insert foo;
	} catch (Exception e) {
		return bar;
	}"#;

	let try_block = Box::new(Block::Body(vec![Stmt {
		kind: StmtKind::Dml(
			DmlOp::Insert,
			Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("foo"),
					span: Span {
						start: 15,
						end: 18,
						start_pos: Position { line: 2, col: 10 },
						end_pos: Position { line: 2, col: 13 },
					},
				}),
				span: Span {
					start: 15,
					end: 18,
					start_pos: Position { line: 2, col: 10 },
					end_pos: Position { line: 2, col: 13 },
				},
			},
		),
		span: Span {
			start: 8,
			end: 19,
			start_pos: Position { line: 2, col: 3 },
			end_pos: Position { line: 2, col: 14 },
		},
	}]));

	let catch_clause = (
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: Identifier {
					name: String::from("Exception"),
					span: Span {
						start: 30,
						end: 39,
						start_pos: Position { line: 3, col: 11 },
						end_pos: Position { line: 3, col: 20 },
					},
				},
				subclass: None,
				type_arguments: None,
				is_array: false,
				span: Span {
					start: 30,
					end: 40,
					start_pos: Position { line: 3, col: 11 },
					end_pos: Position { line: 3, col: 21 },
				},
			}),
			span: Span {
				start: 30,
				end: 40,
				start_pos: Position { line: 3, col: 11 },
				end_pos: Position { line: 3, col: 21 },
			},
		},
		Identifier {
			name: String::from("e"),
			span: Span {
				start: 40,
				end: 41,
				start_pos: Position { line: 3, col: 21 },
				end_pos: Position { line: 3, col: 22 },
			},
		},
		Block::Body(vec![Stmt {
			kind: StmtKind::Return(Some(Expr {
				kind: ExprKind::Identifier(Identifier {
					name: String::from("bar"),
					span: Span {
						start: 54,
						end: 57,
						start_pos: Position { line: 4, col: 10 },
						end_pos: Position { line: 4, col: 13 },
					},
				}),
				span: Span {
					start: 54,
					end: 57,
					start_pos: Position { line: 4, col: 10 },
					end_pos: Position { line: 4, col: 13 },
				},
			})),
			span: Span {
				start: 47,
				end: 58,
				start_pos: Position { line: 4, col: 3 },
				end_pos: Position { line: 4, col: 14 },
			},
		}]),
	);

	let expected = Stmt {
		kind: StmtKind::TryCatch(try_block, catch_clause, None, None),
		span: Span {
			start: 0,
			end: 61,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 5, col: 3 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn dml_stmt_parses() {
	let input = "insert foo;";

	let expected = Stmt {
		kind: StmtKind::Dml(
			DmlOp::Insert,
			Expr {
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
			},
		),
		span: Span {
			start: 0,
			end: 11,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 12 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

#[test]
fn throw_stmt_parses() {
	let input = "throw new TestException();";

	let expected = Stmt {
		kind: StmtKind::Throw(Expr {
			kind: ExprKind::New(
				Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						name: Identifier {
							name: String::from("TestException"),
							span: Span {
								start: 10,
								end: 23,
								start_pos: Position { line: 1, col: 11 },
								end_pos: Position { line: 1, col: 24 },
							},
						},
						subclass: None,
						type_arguments: None,
						is_array: false,
						span: Span {
							start: 10,
							end: 23,
							start_pos: Position { line: 1, col: 11 },
							end_pos: Position { line: 1, col: 24 },
						},
					}),
					span: Span {
						start: 10,
						end: 23,
						start_pos: Position { line: 1, col: 11 },
						end_pos: Position { line: 1, col: 24 },
					},
				},
				NewType::Class(ClassArgs::Basic(None)),
			),
			span: Span {
				start: 6,
				end: 25,
				start_pos: Position { line: 1, col: 7 },
				end_pos: Position { line: 1, col: 26 },
			},
		}),
		span: Span {
			start: 0,
			end: 26,
			start_pos: Position { line: 1, col: 1 },
			end_pos: Position { line: 1, col: 27 },
		},
	};

	test_parse(Rule::statement, input, parse_stmt, expected);
}

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
fn instanceof_expr_parses() {
	let input = "foo instanceof Foo";

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
		kind: ExprKind::Instanceof(Box::new(lhs), ty),
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
			start: 0,
			end: 20,
			start_pos: Position { line: 1, col: 1 },
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
			start: 0,
			end: 26,
			start_pos: Position { line: 1, col: 1 },
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
			start: 0,
			end: 48,
			start_pos: Position { line: 1, col: 1 },
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
			start: 0,
			end: 29,
			start_pos: Position { line: 1, col: 1 },
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
			start: 0,
			end: 9,
			start_pos: Position { line: 1, col: 1 },
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
