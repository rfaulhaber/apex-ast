use crate::pos::Span;

#[derive(Debug, Clone)]
pub struct Literal {
	pub node: LiteralKind,
	pub span: Span,
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