use super::expr::Expr;
use super::method::ClassMethod;
use super::modifier::{AccessModifier, SharingModifier};
use super::stmt::Block;

pub struct Class {
	pub access_modifier: AccessModifier,
	pub sharing_modifier: Option<SharingModifier>,
	pub impl_modifier: Option<ClassImplModifier>,
	pub extensions: Vec<String>,
	pub implementations: Vec<String>,
	pub variables: Vec<ClassVariable>,
	pub methods: Vec<ClassMethod>,
	pub is_test: bool,
	pub rest_resource: Option<RestResource>,
}

pub enum ClassImplModifier {
	Abstract,
	Virtual,
}

pub struct RestResource {
	url_mapping: String,
}

pub struct ClassVariable {
	pub access_mod: AccessModifier,
	pub instance_mod: ClassInstanceModifier,  // "static" is rust keyword
	pub final_mod: bool,   // "final" is rust keyword
	pub prop_type: String, // "prop" is rust keyword
	pub name: String,
	pub getter: Option<Property>,
	pub setter: Option<Property>,
	pub rhs: Option<Expr>,
}

pub enum ClassInstanceModifier {
	Static,
	Transient,
}

pub struct Property {
	pub access_mod: AccessModifier,
	pub body: Option<Block>,
}