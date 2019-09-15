use super::annotation::*;
use super::expr::Expr;
use super::identifier::*;
use super::method::*;
use super::modifier::*;
use super::stmt::Block;
use super::ty::*;

pub struct Class {
	pub annotation: Option<Annotation>,
	pub access_modifier: AccessModifier,
	pub sharing_modifier: Option<SharingModifier>,
	pub impl_modifier: Option<ClassImplModifier>,
	pub extensions: Vec<Ty>,
	pub implementations: Vec<Ty>,
	pub fields: Vec<ClassField>,
	pub methods: Vec<ClassMethod>,
}

pub enum ClassImplModifier {
	Abstract,
	Virtual,
}

pub struct ClassField {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub instance_mod: Option<ClassInstanceModifier>,
	pub final_mod: bool, // "final" is rust keyword
	pub ty: Ty,
	pub id: Identifier,
	pub getter: Option<Property>,
	pub setter: Option<Property>,
	pub rhs: Option<Expr>,
}

pub enum ClassInstanceModifier {
	Static,
	Transient,
}

pub struct Property {
	pub access_mod: Option<AccessModifier>,
	pub body: Option<Block>,
}
