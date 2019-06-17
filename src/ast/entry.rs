use super::class::Class;
use super::interface::Interface;
use super::trigger::Trigger;

pub struct File {
	pub kind: FileKind,
	pub name: String,
}

pub enum FileKind {
	Class(Class),
	Interface(Interface),
	Trigger(Trigger),
}