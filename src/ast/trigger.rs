use super::identifier::Identifier;
use super::stmt::{Block, DmlOp};
use super::ty::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct Trigger {
	pub name: Identifier,
	pub object: Ty,
	pub events: Vec<TriggerEvent>,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TriggerEvent(pub TriggerWhen, pub DmlOp);

#[derive(Debug, Clone, PartialEq)]
pub enum TriggerWhen {
	Before,
	After,
}
