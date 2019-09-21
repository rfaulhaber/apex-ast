use super::annotation::*;
use super::expr::Expr;
use super::identifier::*;
use super::interface::*;
use super::method::*;
use super::modifier::*;
use super::r#enum::*;
use super::stmt::Block;
use super::ty::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
	pub annotation: Option<Annotation>,
	pub access_modifier: AccessModifier,
	pub sharing_modifier: Option<SharingModifier>,
	pub impl_modifier: Option<ClassImplModifier>,
	pub name: Identifier,
	pub extensions: Vec<Ty>,
	pub implementations: Vec<Ty>,
	pub body: Vec<ClassBodyMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassBodyMember {
	InnerClass(Box<Class>),
	InnerInterface(Interface),
	Field(ClassField),
	Method(ClassMethod),
	Enum(Enum),
	StaticBlock(Block),
	InstanceBlock(Block),
	Constructor(ClassConstructor),
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

// constructors are basically methods without return types
#[derive(Debug, Clone, PartialEq)]
pub struct ClassConstructor {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub identifier: Identifier,
	pub params: Vec<(Ty, Identifier)>,
	pub block: Block,
}
