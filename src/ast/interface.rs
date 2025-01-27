use super::identifier::Identifier;
use super::method::ImplementableMethod;
use super::modifier::AccessModifier;
use super::ty::Ty;
use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Interface {
	pub access_mod: Option<AccessModifier>,
	// indicates presence of "virtual" keyword in inner interface
	pub is_virtual: bool,
	pub name: Identifier,
	pub extensions: Vec<Ty>,
	pub methods: Vec<ImplementableMethod>,
	pub span: Span,
}
