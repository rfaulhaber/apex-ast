use super::identifier::Identifier;
use crate::source::Span;
use serde::Serialize;

pub type TyRef = Box<Ty>;

macro_rules! type_args {
	($first:expr) => {
		Some((Box::new($first), None))
	};
	($first:expr, $second:expr) => {
		Some((Box::new($first), Some(Box::new($second))))
	};
}

/// An Apex type.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Ty {
	pub kind: TyKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TyKind {
	/// A user-defined type.
	ClassOrInterface(ClassOrInterface),
	/// An Apex "primitive".
	Primitive(Primitive),
	Void,
}

impl Ty {
	pub fn is_void(&self) -> bool {
		self.kind == TyKind::Void
	}
}

/// A class or interface.
///
/// The following fields, being options, should cover all forms.
///
/// In Apex, classes are in the following forms:
/// - `Foo` ("basic")
/// - `Foo.Bar` ("inner")
/// - `Foo<Bar>` ("generic" with one type argument)
/// - `Foo.Bar<Baz>` (generic inner with one type argument)
/// - `Foo[]` (syntactic sugar for List<Foo>)
/// - `Foo.Bar[]` (syntactic sugar for List<Foo.Bar>)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ClassOrInterface {
	pub name: Identifier,
	pub subclass: Option<Identifier>,
	pub type_arguments: Option<(TyRef, Option<TyRef>)>,
	pub is_array: bool,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Primitive {
	pub kind: PrimitiveKind,
	pub is_array: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PrimitiveKind {
	Blob,
	Boolean,
	Date,
	Datetime,
	Decimal,
	Double,
	ID,
	Integer,
	Long,
	Object,
	String,
	Time,
}
