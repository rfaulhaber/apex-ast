use super::identifier::Identifier;
use crate::source::Span;
use serde::Serialize;

pub type TyRef = Box<Ty>;

/// An Apex type.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Ty {
	pub kind: TyKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TyKind {
	/// A user-defined type.
	RefType(RefType),
	/// An Apex "primitive".
	Primitive(Primitive),
	Void,
}

impl Ty {
	pub fn is_void(&self) -> bool {
		self.kind == TyKind::Void
	}
}

/// A user-defined type.
///
/// The following fields, being options, should cover all forms.
///
/// In Apex, classes are in the following forms:
/// - `Foo` ("basic")
/// - `Foo.Bar` ("inner")
/// - `Foo<Bar>` ("generic" with one type argument)
/// - `Foo.Bar<Baz>` (generic inner with one type argument)
/// - `Foo<Bar>.Baz` (generic outer with inner class, may not exist in Apex)
/// - `Foo<Bar>.Baz<Quux>` (generic outer with generic inner, may not exist in Apex)
/// - `Foo[]` (syntactic sugar for List<Foo>)
/// - `Foo.Bar[]` (syntactic sugar for List<Foo.Bar>)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RefType {
	pub name: Identifier,
	pub inner: Option<InnerRefType>,
	pub type_arguments: Option<TypeArguments>,
	pub is_array: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InnerRefType {
	pub name: Identifier,
	pub type_arguments: Option<TypeArguments>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeArguments {
	Single(TyRef),
	Double(TyRef, TyRef),
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
