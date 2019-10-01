use super::identifier::Identifier;
use super::literal::Literal;
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
	pub span: Span,
}

impl From<&str> for Annotation {
	fn from(s: &str) -> Annotation {
		Annotation {
			name: Identifier::from(s),
			keypairs: None,
			span: Span::default(),
		}
	}
}
