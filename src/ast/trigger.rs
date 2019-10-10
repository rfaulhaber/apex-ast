use super::identifier::Identifier;
use super::stmt::{Block, DmlOp};
use super::ty::Ty;
use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Trigger {
	pub name: Identifier,
	pub object: Ty,
	pub events: Vec<TriggerEvent>,
	pub body: Block,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TriggerEvent {
	pub when: TriggerWhen,
	pub op: DmlOp,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TriggerWhen {
	Before,
	After,
}
