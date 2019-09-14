use super::identifier::Identifier;
use super::literal::Literal;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
}

impl From<&str> for Annotation {
	fn from(s: &str) -> Annotation {
		Annotation {
			name: Identifier::from(s),
			keypairs: None
		}
	}
}
