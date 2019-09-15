use super::annotation::*;
use super::expr::Expr;
use super::identifier::*;
use super::method::*;
use super::modifier::*;
use super::stmt::Block;
use super::ty::*;

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum ClassImplModifier {
	Abstract,
	Virtual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub instance_mod: Option<ClassInstanceModifier>,
	pub is_final: bool, // "final" is rust keyword
	pub ty: Ty,
	pub id: Identifier,
	pub getter: Option<Property>,
	pub setter: Option<Property>,
	pub rhs: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassInstanceModifier {
	Static,
	Transient,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
	pub access_mod: Option<AccessModifier>,
	pub property_type: PropertyType,
	pub body: Option<Block>,
}

impl From<PropertyType> for Property {
	fn from(pt: PropertyType) -> Property {
		Property {
			access_mod: None,
			body: None,
			property_type: pt,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyType {
	Get,
	Set,
}
