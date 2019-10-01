use pest::Position as PestPosition;
use pest::Span as PestSpan;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub start_pos: Position,
	pub end_pos: Position,

	original_str: String,
}

impl<'s> From<PestSpan<'s>> for Span {
	fn from(ps: PestSpan) -> Span {
		Span {
			start: ps.start(),
			end: ps.end(),
			start_pos: Position::from(ps.start_pos()),
			end_pos: Position::from(ps.end_pos()),
			original_str: String::from(ps.as_str()),
		}
	}
}

impl Span {
	pub fn get_string(&self) -> String {
		self.original_str.to_owned()
	}

	pub fn dummy_span() -> Span {
		Span {
			start: 0,
			end: 0,
			start_pos: Position { line: 0, col: 0 },
			end_pos: Position { line: 0, col: 0 },
			original_str: String::new(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
	pub line: usize,
	pub col: usize,
}

impl<'s> From<PestPosition<'s>> for Position {
	fn from(pp: PestPosition) -> Position {
		let (line, col) = pp.line_col();
		Position { line, col }
	}
}
