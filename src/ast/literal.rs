#[derive(Debug, Clone)]
pub struct Literal {
	pub node: LiteralKind,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
	Float(f64),
	Long(i64),
	Integer(i64),
	String(String),
	Boolean(bool),
	Null,
}