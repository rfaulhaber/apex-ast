#[derive(Debug, Clone)]
pub struct Literal {
	pub node: LiteralKind,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
	Float(f64),
	Long(i64),
	Integer(i64),
	String(String),
	Boolean(bool),
	Null,
}

impl Into<Literal> for LiteralKind {
	fn into(self) -> Literal {
		Literal {
			node: self
		}
	}
}

impl From<String> for Literal {
	fn from(s: String) -> Literal {
		Literal {
			node: LiteralKind::String(s),
		}
	}
}

impl From<&str> for Literal {
	fn from(s: &str) -> Literal {
		Literal {
			node: LiteralKind::String(s.into()),
		}
	}
}

impl From<f64> for Literal {
	fn from(f: f64) -> Literal {
		Literal {
			node: LiteralKind::Float(f),
		}
	}
}

impl From<i64> for Literal {
	fn from(i: i64) -> Literal {
		Literal {
			node: LiteralKind::Integer(i),
		}
	}
}

impl From<bool> for Literal {
	fn from(b: bool) -> Literal {
		Literal {
			node: LiteralKind::Boolean(b),
		}
	}
}