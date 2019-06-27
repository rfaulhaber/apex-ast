use super::identifier::Identifier;
use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

// "Ty" corresponds to the "basic_type" rule
#[derive(Debug, Clone)]
pub struct Ty {
	// does it contain array brackets?
	// sets, lists, and maps proper are always false
	pub array: bool,
	pub kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Collection(Collection),
	Primitive(Primitive),
	ClassOrInterface(ClassOrInterface),
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
			_ => unreachable!("expected basic_type, got {:?}", rule),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Collection {
	pub kind: CollectionType,
}

#[derive(Debug, Clone)]
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

// parsed when pair is Rule::colection_type
fn parse_collection_type(p: Pair<Rule>) -> Ty {
	let outer_rule = p.as_rule();
	let mut inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::set_type => parse_set_type(inner),
		Rule::list_type => parse_list_type(inner),
		Rule::map_type => parse_map_type(inner),
		_ => unreachable!("expected collection_type, got {:?}", outer_rule),
	}
}

fn parse_set_type(p: Pair<Rule>) -> Ty {
	let collection = Collection {
		kind: CollectionType::Set(Box::new(parse_typed_type(p.into_inner().next().unwrap()))),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

fn parse_list_type(p: Pair<Rule>) -> Ty {
	let collection = Collection {
		kind: CollectionType::List(Box::new(parse_typed_type(p.into_inner().next().unwrap()))),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

fn parse_map_type(p: Pair<Rule>) -> Ty {
	let mut inner = p.into_inner();

	let inner_types = (
		parse_typed_type(inner.next().unwrap()),
		parse_typed_type(inner.next().unwrap()),
	);
	let collection = Collection {
		kind: CollectionType::Map(Box::new(inner_types.0), Box::new(inner_types.1)),
	};

	Ty {
		array: false,
		kind: TyKind::Collection(collection),
	}
}

// typed_type is recursive
fn parse_typed_type(p: Pair<Rule>) -> Ty {
	Ty::from(p.into_inner().next().unwrap())
}

fn parse_primitive_type(pair: Pair<Rule>, is_array: bool) -> Ty {
	// NOTE this will have to be reworked for spans
	let kind_str = pair.into_inner().next().unwrap().as_str();

	let kind = PrimitiveType::from(kind_str);

	Ty {
		kind: TyKind::Primitive(kind.into()),
		array: is_array,
	}
}

fn parse_class_or_interface_type(pair: Pair<Rule>, is_array: bool) -> Ty {
	let mut inner = pair.into_inner();

	let class: Identifier = inner.next().unwrap().into();

	let subclass: Option<Identifier> = match inner.next() {
		Some(pair) => Some(pair.into()),
		None => None,
	};

	Ty {
		kind: TyKind::ClassOrInterface(ClassOrInterface { class, subclass }),
		array: is_array,
	}
}

// "primitive" in terms of Apex just means "built-in" since it technically does
// not have primitives

#[derive(Debug, Clone)]
pub struct Primitive {
	pub kind: PrimitiveType,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ClassOrInterface {
	pub class: Identifier,

	// if None, this would be like "String",
	// if Some, this would be like "Foo.Bar"
	pub subclass: Option<Identifier>,
}

#[cfg(test)]
mod expr_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;
}