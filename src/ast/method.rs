pub struct Method {}


pub struct ClassMethod {}

pub struct ImplementableMethod {}


pub enum MethodKind {
	/// an ordinary class method, with a signature and body
	ClassMethod(ClassMethod),
	/// a method definition as defined by an abstract class or interface
	ImplementableMethod(ImplementableMethod),
}

pub struct MethodSignature {}

pub struct MethodParam {
	pub return_type: String,
	pub identifier: String,
}