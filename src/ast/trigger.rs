pub struct Trigger {
	pub name: String,
	pub object: String,
	pub events: Vec<TriggerEvent>,
	// pub body: Vec<Expr>,
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