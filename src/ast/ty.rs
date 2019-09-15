use super::identifier::Identifier;

macro_rules! type_args {
	($first:expr) => {
		Some((Box::new($first), None))
	};
	($first:expr, $second:expr) => {
		Some((Box::new($first), Some(Box::new($second))))
	};
}

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
	Void,
}

impl Ty {
	pub fn void() -> Ty {
		Ty { kind: TyKind::Void }
	}

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
#[derive(Debug, PartialEq, Clone)]
pub struct ClassOrInterface {
	pub name: Identifier,
	pub subclass: Option<Identifier>,
	pub type_arguments: Option<(TyRef, Option<TyRef>)>,
	pub is_array: bool,
}

impl From<ClassOrInterface> for Ty {
	fn from(coi: ClassOrInterface) -> Ty {
		Ty {
			kind: TyKind::ClassOrInterface(coi),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Primitive {
	pub kind: PrimitiveKind,
	pub is_array: bool,
}

impl From<Primitive> for Ty {
	fn from(p: Primitive) -> Ty {
		Ty {
			kind: TyKind::Primitive(p),
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

impl From<PrimitiveKind> for Ty {
	fn from(pk: PrimitiveKind) -> Ty {
		Ty {
			kind: TyKind::Primitive(Primitive {
				kind: pk,
				is_array: false,
			}),
		}
	}
}

// mostly used for testing, shorthand way of definig easy classes on the fly
impl From<Identifier> for Ty {
	fn from(i: Identifier) -> Ty {
		Ty {
			kind: TyKind::ClassOrInterface(ClassOrInterface {
				name: i,
				subclass: None,
				type_arguments: None,
				is_array: false,
			}),
		}
	}
}
