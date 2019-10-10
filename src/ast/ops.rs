use serde::Serialize;

/// binary operators
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Gt,
	Ge,
	Lt,
	Le,
	Eq,
	NotEq,
	And,
	Or,
	BitAnd,
	BitOr,
	BitXor,
	ShiftLeft,
	ShiftRight,
}

impl From<&str> for BinOp {
	fn from(s: &str) -> BinOp {
		match s {
			"+" => BinOp::Add,
			"-" => BinOp::Sub,
			"*" => BinOp::Mul,
			"/" => BinOp::Div,
			">" => BinOp::Gt,
			">=" => BinOp::Ge,
			"<" => BinOp::Lt,
			"<=" => BinOp::Le,
			"==" => BinOp::Eq,
			"!=" => BinOp::NotEq,
			"&&" => BinOp::And,
			"||" => BinOp::Or,
			"&" => BinOp::BitAnd,
			"|" => BinOp::BitOr,
			"^" => BinOp::BitXor,
			"<<" => BinOp::ShiftLeft,
			">>" => BinOp::ShiftRight,
			_ => panic!("invalid string found: {}", s),
		}
	}
}

impl BinOp {
	pub fn as_str(&self) -> &str {
		match self {
			BinOp::Add => "+",
			BinOp::Sub => "-",
			BinOp::Mul => "*",
			BinOp::Div => "/",
			BinOp::Gt => ">",
			BinOp::Ge => ">=",
			BinOp::Lt => "<",
			BinOp::Le => "<=",
			BinOp::Eq => "=",
			BinOp::NotEq => "!=",
			BinOp::And => "&&",
			BinOp::Or => "||",
			BinOp::BitAnd => "&",
			BinOp::BitOr => "|",
			BinOp::BitXor => "^",
			BinOp::ShiftLeft => "<<",
			BinOp::ShiftRight => ">>",
		}
	}
}

/// UnOp is prefix operators exclusively.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum UnOp {
	/// `!` operator
	Not,
	/// `~` operator
	BitNot,
	/// `++` operator
	Inc,
	/// `--` operator
	Dec,
}

impl From<&str> for UnOp {
	fn from(s: &str) -> UnOp {
		match s {
			"!" => UnOp::Not,
			"~" => UnOp::BitNot,
			"++" => UnOp::Inc,
			"--" => UnOp::Dec,
			_ => panic!("invalid string: {}", s),
		}
	}
}

impl UnOp {
	pub fn as_str(&self) -> &str {
		match self {
			UnOp::Not => "!",
			UnOp::BitNot => "~",
			UnOp::Inc => "++",
			UnOp::Dec => "--",
		}
	}
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum IncDecOp {
	Inc,
	Dec,
}

impl From<&str> for IncDecOp {
	fn from(s: &str) -> IncDecOp {
		match s {
			"++" => IncDecOp::Inc,
			"--" => IncDecOp::Dec,
			_ => panic!("invalid string: {}", s),
		}
	}
}

impl IncDecOp {
	pub fn as_str(&self) -> &str {
		match self {
			IncDecOp::Inc => "++",
			IncDecOp::Dec => "--",
		}
	}
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum AssignOp {
	Eq,
	Add,
	Sub,
	Mul,
	Div,
	And,
	Or,
	Xor,
	ShiftLeft,
	ShiftRight,
}

impl AssignOp {
	pub fn as_str(&self) -> &str {
		match self {
			AssignOp::Eq => "=",
			AssignOp::Add => "+=",
			AssignOp::Sub => "-=",
			AssignOp::Mul => "*=",
			AssignOp::Div => "/=",
			AssignOp::And => "&=",
			AssignOp::Or => "|=",
			AssignOp::Xor => "^=",
			AssignOp::ShiftLeft => "<<=",
			AssignOp::ShiftRight => ">>=",
		}
	}
}

impl From<&str> for AssignOp {
	fn from(s: &str) -> AssignOp {
		match s {
			"=" => AssignOp::Eq,
			"+=" => AssignOp::Add,
			"-=" => AssignOp::Sub,
			"*=" => AssignOp::Mul,
			"/=" => AssignOp::Div,
			"&=" => AssignOp::And,
			"|=" => AssignOp::Or,
			"^=" => AssignOp::Xor,
			"<<=" => AssignOp::ShiftLeft,
			">>=" => AssignOp::ShiftRight,
			_ => panic!("invalid string found: {}", s),
		}
	}
}
