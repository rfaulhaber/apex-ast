pub struct Span {
	start: usize,
	end: usize,
	start_pos: Position,
	end_pos: Position,
	input: String,
}

pub struct Position {
	pos: usize,
	line_col: (usize, usize),
}