use super::identifier::Identifier;
use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

// "Ty" corresponds to the "basic_type" rule
#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
	// does it contain array brackets?
	// sets, lists, and maps proper are always false
	pub array: bool,
	pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
	Collection(Collection),
	Primitive(Primitive),
	ClassOrInterface(ClassOrInterface),
}

impl From<Collection> for TyKind {
	fn from(c: Collection) -> TyKind {
		TyKind::Collection(c)
	}
}

impl From<Primitive> for TyKind {
	fn from(p: Primitive) -> TyKind {
		TyKind::Primitive(p)
	}
}

impl From<ClassOrInterface> for TyKind {
	fn from(coi: ClassOrInterface) -> TyKind {
		TyKind::ClassOrInterface(coi)
	}
}

// this should be called when we encounter Rule::basic_type
impl<'a> From<Pair<'a, Rule>> for Ty {
	fn from(p: Pair<'a, Rule>) -> Ty {
		let rule = p.as_rule();

		let mut inner = p.into_inner();

		let first = inner.next().unwrap();

		let contains_brakets = inner.next().is_some();

		match first.as_rule() {
			Rule::collection_type => parse_collection_type(first),
			Rule::primitive_type => parse_primitive_type(first, contains_brakets),
			Rule::class_or_interface_type => parse_class_or_interface_type(first, contains_brakets),
			Rule::typed_type => parse_typed_type(first),
			_ => unreachable!("expected basic_type, got {:?}", rule),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Collection {
	pub kind: CollectionType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CollectionType {
	Map(Box<Ty>, Box<Ty>),
	Set(Box<Ty>),
	List(Box<Ty>),
}

// we assume this gets called when we come across Rule::collection_type
impl<'a> From<Pair<'a, Rule>> for Collection {
	fn from(p: Pair<'a, Rule>) -> Collection {
		let inner = p.into_inner().next().unwrap();

		match inner.as_rule() {
			Rule::set_type => unimplemented!(),
			Rule::list_type => unimplemented!(),
			Rule::map_type => unimplemented!(),
			_ => unreachable!("expected a valid collection type"),
		}
	}
}

impl From<CollectionType> for Collection {
	fn from(ct: CollectionType) -> Collection {
		Collection { kind: ct }
	}
}

// "primitive" in terms of Apex just means "built-in" since it technically does
// not have primitives

#[derive(Debug, Clone, PartialEq)]
pub struct Primitive {
	pub kind: PrimitiveType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
	Blob,
	Boolean,
	Datetime,
	Date,
	Decimal,
	Double,
	ID,
	Integer,
	Long,
	Object,
	String,
	Time,
}

impl From<PrimitiveType> for Primitive {
	fn from(pt: PrimitiveType) -> Primitive {
		Primitive { kind: pt }
	}
}

impl From<&str> for PrimitiveType {
	fn from(s: &str) -> PrimitiveType {
		match s.to_lowercase().as_str() {
			"blob" => PrimitiveType::Blob,
			"boolean" => PrimitiveType::Boolean,
			"datetime" => PrimitiveType::Datetime,
			"date" => PrimitiveType::Date,
			"decimal" => PrimitiveType::Decimal,
			"double" => PrimitiveType::Double,
			"id" => PrimitiveType::ID,
			"integer" => PrimitiveType::Integer,
			"long" => PrimitiveType::Long,
			"object" => PrimitiveType::Object,
			"string" => PrimitiveType::String,
			"time" => PrimitiveType::Time,
			_ => unreachable!("expected primitive as string"),
		}
	}
}

impl<'a> From<Pair<'a, Rule>> for Primitive {
	fn from(p: Pair<'a, Rule>) -> Primitive {
		Primitive {
			kind: p.as_str().into(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassOrInterface {
	pub kind: ClassOrInterfaceType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassOrInterfaceType {
	// e.g. Foo
	Class(Identifier),

	// e.g. Foo<Bar>
	Generic(Identifier, Box<Ty>),

	// e.g. Foo.Bar, Foo.Bar<Baz> or some combination
	Inner(Identifier, Box<Ty>),
}

// parsed when pair is Rule::colection_type
fn parse_collection_type(p: Pair<Rule>) -> Ty {
	let outer_rule = p.as_rule();
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::set_type => parse_set_type(inner),
		Rule::list_type => parse_list_type(inner),
		Rule::map_type => parse_map_type(inner),
		_ => unreachable!("expected collection_type, got {:?}", outer_rule),
	}
}

fn parse_set_type(p: Pair<Rule>) -> Ty {
	let collection = Collection {
		kind: CollectionType::Set(Box::new(Ty::from(p.into_inner().next().unwrap()))),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

fn parse_list_type(p: Pair<Rule>) -> Ty {
	let collection = Collection {
		kind: CollectionType::List(Box::new(Ty::from(p.into_inner().next().unwrap()))),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

fn parse_map_type(p: Pair<Rule>) -> Ty {
	let mut inner = p.into_inner();

	let inner_types = (
		Ty::from(inner.next().unwrap()),
		Ty::from(inner.next().unwrap()),
	);
	let collection = Collection {
		kind: CollectionType::Map(Box::new(inner_types.0), Box::new(inner_types.1)),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

fn parse_primitive_type(pair: Pair<Rule>, is_array: bool) -> Ty {
	// NOTE this will have to be reworked for spans
	let kind_str = pair.as_str();

	let kind = PrimitiveType::from(kind_str);

	Ty {
		kind: TyKind::Primitive(kind.into()),
		array: is_array,
	}
}

fn parse_class_or_interface_type(pair: Pair<Rule>, is_array: bool) -> Ty {
	let mut inner = pair.into_inner();

	println!("parse_class_or_interface: {:?}", inner);

	let class: Identifier = inner.next().unwrap().into();

	match inner.next() {
		Some(p) => match p.into_inner().next().unwrap().as_rule() {
			Rule::typed_type => unimplemented!(),
			Rule::identifier => unimplemented!(),
			_ => unreachable!("expected typed type"),
		},
		None => Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				kind: ClassOrInterfaceType::Class(class),
			}),
			array: is_array,
		},
	}
}

fn parse_typed_type(pair: Pair<Rule>) -> Ty {
	let mut inner = pair.into_inner();

	let class = Identifier::from(inner.next().unwrap());

	let inner_type = Box::new(Ty::from(inner.next().unwrap()));

	Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface {
			kind: ClassOrInterfaceType::Generic(class, inner_type),
		}),
		array: false,
	}
}

fn parse_subclass(pair: Pair<Rule>) -> ClassOrInterface {
	let mut inner = pair.into_inner();

	let first = inner.next().unwrap();

	match first.as_rule() {
		Rule::typed_type => ClassOrInterface {
			kind: ClassOrInterfaceType::Generic(
				Identifier::from(first),
				Box::new(Ty::from(inner.next().unwrap())),
			),
		},
		Rule::identifier => ClassOrInterface {
			kind: ClassOrInterfaceType::Class(Identifier::from(first)),
		},
		_ => unreachable!("expected Rule::subclass"),
	}
}

#[cfg(test)]
mod expr_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

	#[test]
	fn primitive_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Integer")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::Primitive(prim) = ty.kind {
			assert_eq!(PrimitiveType::Integer, prim.kind);
		} else {
			panic!("unexpected type kind found");
		}
	}


	#[test]
	fn class_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Foo")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::ClassOrInterface(coi) = ty.kind {
			assert_eq!(ClassOrInterfaceType::Class("Foo".into()), coi.kind);
		} else {
			panic!("unexpected type kind found");
		}
	}

	#[test]
	fn class_type_array_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Foo[]")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(ty.array);

		if let TyKind::ClassOrInterface(coi) = ty.kind {
			assert_eq!(ClassOrInterfaceType::Class("Foo".into()), coi.kind);
		} else {
			panic!("unexpected type kind found");
		}
	}

	#[test]
	fn composite_class_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Foo.Bar")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::ClassOrInterface(coi) = ty.kind {
			let expected_inner = Ty {
				array: false,
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					kind: ClassOrInterfaceType::Class("Bar".into()),
				}),
			};
			let expected = ClassOrInterface {
				kind: ClassOrInterfaceType::Inner("Foo".into(), Box::new(expected_inner)),
			};

			assert_eq!(expected, coi);
		} else {
			panic!("unexpected type kind found");
		}
	}

	#[test]
	fn composite_typed_class_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Foo.Bar<Baz>")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::ClassOrInterface(coi) = ty.kind {
			let expected_gen_type = Ty {
				array: false,
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					kind: ClassOrInterfaceType::Class("Baz".into()),
				}),
			};

			let expected_gen = Ty {
				array: false,
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					kind: ClassOrInterfaceType::Generic("Bar".into(), Box::new(expected_gen_type)),
				}),
			};


			let expected = ClassOrInterface {
				kind: ClassOrInterfaceType::Inner("Foo".into(), Box::new(expected_gen)),
			};

			assert_eq!(expected, coi);
		} else {
			panic!("unexpected type kind found");
		}
	}


	#[test]
	fn collection_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "List<String>")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::Collection(coll) = ty.kind {
			match coll.kind {
				CollectionType::List(inner_ty) => {
					if let TyKind::Primitive(prim) = inner_ty.kind {
						assert_eq!(PrimitiveType::String, prim.kind);
					} else {
						panic!("expected String, got {:?}", inner_ty.kind);
					}
				}
				_ => panic!("expected list, got {:?}", coll.kind),
			}
		} else {
			panic!("unexpected type kind found");
		}
	}


	// TODO
	#[test]
	fn generic_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Foo<Bar>")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::ClassOrInterface(coi) = ty.kind {
			let expected_inner = Ty {
				array: false,
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					kind: ClassOrInterfaceType::Class("Bar".into()),
				}),
			};

			let expected = ClassOrInterface {
				kind: ClassOrInterfaceType::Generic("Foo".into(), Box::new(expected_inner)),
			};

			assert_eq!(expected, coi);
		} else {
			panic!("unexpected type kind found");
		}
	}

	#[test]
	fn map_type_should_parse_correctly() {
		let parsed = GrammarParser::parse(Rule::basic_type, "Map<Id, Case>")
			.unwrap()
			.next()
			.unwrap();

		let ty = Ty::from(parsed);

		assert!(!ty.array);

		if let TyKind::Collection(coll) = ty.kind {
			match coll.kind {
				CollectionType::Map(key_ty, val_ty) => match (key_ty.kind, val_ty.kind) {
					(TyKind::Primitive(prim), TyKind::ClassOrInterface(coi)) => {
						assert_eq!(prim.kind, PrimitiveType::ID);

						let expected_case = ClassOrInterface {
							kind: ClassOrInterfaceType::Class("Case".into()),
						};

						assert_eq!(expected_case, coi);
					}
					_ => panic!("inner types did not parse correctly"),
				},
				_ => panic!("expected list, got {:?}", coll.kind),
			}
		} else {
			panic!("unexpected type kind found");
		}
	}
}