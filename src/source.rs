use pest::Position as PestPosition;
use pest::Span as PestSpan;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Default, Serialize)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub start_pos: Position,
	pub end_pos: Position,
}

impl<'s> From<PestSpan<'s>> for Span {
	fn from(ps: PestSpan) -> Span {
		Span {
			start: ps.start(),
			end: ps.end(),
			start_pos: Position::from(ps.start_pos()),
			end_pos: Position::from(ps.end_pos()),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Position {
	pub line: usize,
	pub col: usize,
}

impl Default for Position {
	fn default() -> Position {
		Position { line: 1, col: 1 }
	}
}

impl<'s> From<PestPosition<'s>> for Position {
	fn from(pp: PestPosition) -> Position {
		let (line, col) = pp.line_col();
		Position { line, col }
	}
}
