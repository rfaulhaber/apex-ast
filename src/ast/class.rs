use super::method::ClassMethod;
use super::modifier::{AccessModifier, SharingModifier};

pub struct Class {
	pub access_modifier: AccessModifier,
	pub sharing_modifier: SharingModifier,
	pub extensions: Vec<String>,
	pub implementations: Vec<String>,
	// pub properties: Vec<Property> ?
	pub methods: Vec<ClassMethod>,
	pub is_test: bool,
}

pub struct Property {
	pub access_mod: AccessModifier,
	pub static_mod: bool,  // "static" is rust keyword
	pub final_mod: bool,   // "final" is rust keyword
	pub prop_type: String, // "prop" is rust keyword
	pub name: String,
	pub getter: Option<Getter>,
	pub setter: Option<Setter>,
	// pub rhs: Option<Expr>,
}

pub struct Getter {
	pub access_mod: AccessModifier,
	// pub body: Option<CodeBlock>,
}

pub struct Setter {
	pub access_mod: AccessModifier,
	// pub body: Option<CodeBlock>,
}
