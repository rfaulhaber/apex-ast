use super::identifier::Identifier;
use super::method::ImplementableMethod;
use super::modifier::AccessModifier;
use super::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
	pub access_mod: Option<AccessModifier>,
	pub name: Identifier,
	pub extensions: Vec<Ty>,
	pub methods: Vec<ImplementableMethod>,
}
