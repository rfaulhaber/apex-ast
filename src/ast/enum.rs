use super::annotation::Annotation;
use super::identifier::Identifier;
use super::modifier::AccessModifier;
use crate::source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub name: Identifier,
	pub ids: Vec<Identifier>,
	pub span: Span,
}
