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