use super::class::Class;
use super::interface::Interface;
use super::trigger::Trigger;
use serde::Serialize;

// root of AST
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum File {
	Class(Class),
	Interface(Interface),
	Trigger(Trigger),
}
