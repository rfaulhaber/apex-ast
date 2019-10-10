use super::annotation::Annotation;
use super::identifier::Identifier;
use super::modifier::AccessModifier;
use crate::source::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Enum {
	pub annotation: Option<Annotation>,
	pub access_mod: Option<AccessModifier>,
	pub name: Identifier,
	pub ids: Vec<Identifier>,
	pub span: Span,
}
