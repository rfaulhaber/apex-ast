use super::identifier::Identifier;
use super::literal::Literal;
use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
	pub span: Span,
}
