use super::method::ImplementableMethod;
use super::identifier::Identifier;
use super::ty::Ty;
use super::modifier::AccessModifier;

pub struct Interface {
	pub access_mod: Option<AccessModifier>,
	pub name: Identifier,
	pub extensions: Vec<Ty>,
	pub methods: Vec<ImplementableMethod>,
}
