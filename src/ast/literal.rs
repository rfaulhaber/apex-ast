#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
	pub kind: LiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
	Float(f64),
	Long(i64),
	Integer(i64),
	// TODO change this to &'lit str to save memory
	String(String),
	Boolean(bool),
	Null,
}

impl Into<Literal> for LiteralKind {
	fn into(self) -> Literal {
		Literal { kind: self }
	}
}

impl From<String> for Literal {
	fn from(s: String) -> Literal {
		Literal {
			kind: LiteralKind::String(s),
		}
	}
}

impl From<&str> for Literal {
	fn from(s: &str) -> Literal {
		Literal {
			kind: LiteralKind::String(s.into()),
		}
	}
}

impl From<f64> for Literal {
	fn from(f: f64) -> Literal {
		Literal {
			kind: LiteralKind::Float(f),
		}
	}
}

impl From<i64> for Literal {
	fn from(i: i64) -> Literal {
		Literal {
			kind: LiteralKind::Integer(i),
		}
	}
}

impl From<bool> for Literal {
	fn from(b: bool) -> Literal {
		Literal {
			kind: LiteralKind::Boolean(b),
		}
	}
}
