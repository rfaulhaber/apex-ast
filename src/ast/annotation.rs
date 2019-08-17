use super::identifier::Identifier;
use super::literal::Literal;

use crate::parser::Rule;
use pest::iterators::Pair;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
}