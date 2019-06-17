use super::stmt::Block;

pub struct Trigger {
	pub name: String,
	pub object: String,
	pub events: Vec<TriggerEvent>,
	pub body: Block,
}

pub enum TriggerEvent {
	BeforeInsert,
	BeforeUpdate,
	BeforeDelete,
	AfterInsert,
	AfterUpdate,
	AfterDelete,
	AfterUndelete,
}