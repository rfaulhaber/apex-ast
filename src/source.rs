use crate::pos::Span;

pub struct Spanned<T> {
	pub node: T,
	pub span: Span,
}