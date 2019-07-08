use super::class::Class;
use super::interface::Interface;
use super::trigger::Trigger;

// root of AST
pub enum EntryKind {
	Class(Class),
	Interface(Interface),
	Trigger(Trigger),
}
