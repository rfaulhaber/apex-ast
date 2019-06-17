use super::modifier::{AccessModifier, ImplModifier};
use super::stmt::Block;

pub struct ClassMethod {
	pub annotation: String, // TODO should this be enum?
	pub access_mod: Option<AccessModifier>,
	pub impl_mod: Option<ImplModifier>,
	pub return_type: String,
	pub params: Vec<MethodParam>, // should this be an option?
	pub is_static: bool,

	// indicates presence of deprecated "testMethod" keyword
	pub is_test_method: bool,
	pub block: Block,
	pub identifier: String,
}

// either an interface method definition or an abstract method
pub struct ImplementableMethod {
	pub access_mod: Option<AccessModifier>,
	pub return_type: String,
	pub identifier: String,
	pub params: Vec<MethodParam>,
}

pub enum MethodKind {
	/// an ordinary class method, with a signature and body
	ClassMethod(ClassMethod),
	/// a method definition as defined by an abstract class or interface
	ImplementableMethod(ImplementableMethod),
}

pub struct MethodParam {
	pub return_type: String,
	pub identifier: String,
}