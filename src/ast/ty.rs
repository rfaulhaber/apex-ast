use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

#[derive(Debug, Clone)]
pub struct Ty {
	pub name: String,
	pub kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Collection(Collection),
	Primitive(Primitive),
	ClassOrInterface(String),
}

impl<'a> From<Pair<'a, Rule>> for Ty {
	fn from(p: Pair<'a, Rule>) -> Ty {
		unimplemented!();
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