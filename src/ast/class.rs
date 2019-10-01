use super::annotation::*;
use super::expr::Expr;
use super::identifier::*;
use super::interface::*;
use super::method::*;
use super::modifier::*;
use super::r#enum::*;
use super::stmt::Block;
use super::ty::*;
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub sharing_or_impl_modifier: Option<ImplOrSharingMod>,
	pub name: Identifier,
	pub extension: Option<Ty>,
	pub implementations: Vec<Ty>,
	pub body: Vec<ClassBodyMember>,
	pub span: Span,
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
pub enum ImplOrSharingMod {
	Virtual,
	Abstract,
	With,
	Without,
	Inherited,
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
	pub span: Span,
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
	pub span: Span,
}

impl From<PropertyType> for Property {
	fn from(pt: PropertyType) -> Property {
		Property {
			access_mod: None,
			body: None,
			property_type: pt,
			span: Span::default(),
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
	pub span: Span,
}
