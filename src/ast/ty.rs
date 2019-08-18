use super::identifier::Identifier;

pub type TyRef = Box<Ty>;

/// An Apex type.
#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
	pub kind: TyKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
	/// A user-defined type.
	ClassOrInterface(ClassOrInterface),
	/// An Apex "primitive".
	Primitive(Primitive),
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
#[derive(Debug, PartialEq, Clone)]
pub struct ClassOrInterface {
	pub name: Identifier,
	pub subclass: Option<Identifier>,
	pub type_arguments: Option<(TyRef, Option<TyRef>)>,
	pub is_array: bool,
}

impl Into<Ty> for ClassOrInterface {
	fn into(self) -> Ty {
		Ty {
			kind: TyKind::ClassOrInterface(self),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Primitive {
	pub kind: PrimitiveKind,
	pub is_array: bool,
}

impl Into<Ty> for Primitive {
	fn into(self) -> Ty {
		Ty {
			kind: TyKind::Primitive(self)
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
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
