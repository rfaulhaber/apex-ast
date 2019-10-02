use super::identifier::Identifier;
use super::stmt::{Block, DmlOp};
use super::ty::Ty;
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Trigger {
	pub name: Identifier,
	pub object: Ty,
	pub events: Vec<TriggerEvent>,
	pub body: Block,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TriggerEvent {
	pub when: TriggerWhen,
	pub op: DmlOp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TriggerWhen {
	Before,
	After,
}
