use super::identifier::Identifier;
use super::literal::Literal;
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
	pub span: Span,
}
