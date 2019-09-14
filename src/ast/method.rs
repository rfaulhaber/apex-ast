use super::annotation::Annotation;
use super::identifier::Identifier;
use super::modifier::{AccessModifier, ImplModifier};
use super::stmt::Block;
use super::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassMethod {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub impl_mod: Option<ImplModifier>,
	// indicates the presense of the deprecated "testMethod" keyword
	pub is_testmethod: bool,
	pub return_type: Ty,
	pub identifier: Identifier,
	pub params: Vec<(Ty, Identifier)>, // should this be an option?
	pub block: Block,
}

impl ClassMethod {
	pub fn is_static(&self) -> bool {
		if self.impl_mod.is_some() {
			let modifier = self.impl_mod.clone().unwrap();
			modifier == ImplModifier::Static
		} else {
			false
		}
	}
}

// either an interface method definition or an abstract method
#[derive(Debug, Clone, PartialEq)]
pub struct ImplementableMethod {
	pub ty: Ty,
	pub id: Identifier,
	pub params: Vec<(Ty, Identifier)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
	/// an ordinary class method, with a signature and body
	ClassMethod(ClassMethod),
	/// a method definition as defined by an abstract class or interface
	ImplementableMethod(ImplementableMethod),
}
