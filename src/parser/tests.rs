use super::fns::*;
use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use crate::ast::ty::*;

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
