use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Literal {
	pub kind: LiteralKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum LiteralKind {
	Float(f64),
	Long(i64),
	Integer(i64),
	// TODO change this to &'lit str to save memory
	String(String),
	Boolean(bool),
	Null,
}
