use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
	pub kind: LiteralKind,
	// TODO is this needed?
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
	Float(f64),
	Long(i64),
	Integer(i64),
	// TODO change this to &'lit str to save memory
	String(String),
	Boolean(bool),
	Null,
}
