use pest;

// NOTE: all nodes should implement a span derived from Pest!

#[derive(Debug, Clone)]
pub struct Span {
	pub start: usize,
	pub end: usize,
	pub start_pos: Position,
	pub end_pos: Position,
	pub input: String,
}

impl<'a> From<pest::Span<'a>> for Span {
	fn from(s: pest::Span) -> Span {
		Span {
			start: s.start(),
			end: s.end(),
			start_pos: Position::from(s.start_pos()),
			end_pos: Position::from(s.end_pos()),
			input: String::from(s.as_str()),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Position {
	pub pos: usize,
	pub line_col: (usize, usize),
}

impl<'a> From<pest::Position<'a>> for Position {
	fn from(p: pest::Position) -> Position {
		Position {
			pos: p.pos(),
			line_col: p.line_col(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Node<T> {
	pub node: T,
	pub span: Span,
}

pub type NodeId = usize;

// a memory arena mapping nodes to spans in their original source.
// each node type contains a NodeId, which is a reference to their source
// in the source "map"
pub struct SourceMap {
	spans: Vec<Span>,
}

impl SourceMap {
	// TODO should I return reference?
	pub fn get_span(&self, id: NodeId) -> Option<Span> {
		self.spans.get(id).cloned()
	}
}