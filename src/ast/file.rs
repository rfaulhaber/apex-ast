use super::class::Class;
use super::interface::Interface;
use super::trigger::Trigger;

// root of AST
#[derive(Debug, Clone, PartialEq)]
pub enum File {
	Class(Class),
	Interface(Interface),
	Trigger(Trigger),
}
