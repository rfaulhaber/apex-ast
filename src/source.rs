pub struct Span {
	pub start: usize,
	pub end: usize,
	pub start_pos: Position,
	pub end_pos: Position,
	pub input: String,
}

pub struct Position {
	pub pos: usize,
	pub line_col: (usize, usize),
}

pub struct SpanNode<T> {
	node: T,
	span: Span,
}

pub trait Spanned<T> {
	fn span(&self) -> SpanNode<T>;
}