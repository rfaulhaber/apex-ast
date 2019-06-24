#[derive(Debug, Clone)]
pub struct Ty {
	pub name: String,
	pub kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Collection,
	Primitive,
	ClassOrInterface,
}

#[derive(Debug, Clone)]
pub struct Collection {
	pub name: String,
	pub kind: CollectionType,
}

#[derive(Debug, Clone)]
pub enum CollectionType {
	Map(String, String),
	Set(String),
	List(String),
}