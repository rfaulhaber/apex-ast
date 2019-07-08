/// binary operators
#[derive(Debug, Clone, PartialEq)]
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
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
	NotEq,
	And,
	Or,
	AndAssign,
	OrAssign,
	XorAssign,
	BitAnd,
	BitOr,
	BitXor,
	ShiftLeft,
	ShiftRight,
	ShiftLeftAssign,
	ShiftRightAssign,
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
			"=" => BinOp::Eq,
			"+=" => BinOp::AddAssign,
			"-=" => BinOp::SubAssign,
			"*=" => BinOp::MulAssign,
			"/=" => BinOp::DivAssign,
			"!=" => BinOp::NotEq,
			"&&" => BinOp::And,
			"||" => BinOp::Or,
			"&=" => BinOp::AndAssign,
			"|=" => BinOp::OrAssign,
			"^=" => BinOp::XorAssign,
			"&" => BinOp::BitAnd,
			"|" => BinOp::BitOr,
			"^" => BinOp::BitXor,
			"<<" => BinOp::ShiftLeft,
			">>" => BinOp::ShiftRight,
			"<<=" => BinOp::ShiftLeftAssign,
			">>=" => BinOp::ShiftRightAssign,
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
			BinOp::AddAssign => "+=",
			BinOp::SubAssign => "-=",
			BinOp::MulAssign => "*=",
			BinOp::DivAssign => "/=",
			BinOp::NotEq => "!=",
			BinOp::And => "&&",
			BinOp::Or => "||",
			BinOp::AndAssign => "&=",
			BinOp::OrAssign => "|=",
			BinOp::XorAssign => "^=",
			BinOp::BitAnd => "&",
			BinOp::BitOr => "|",
			BinOp::BitXor => "^",
			BinOp::ShiftLeft => "<<",
			BinOp::ShiftRight => ">>",
			BinOp::ShiftLeftAssign => "<<=",
			BinOp::ShiftRightAssign => ">>=",
		}
	}
}

/// UnOp is prefix operators exclusively.
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
	Inc,
	Dec,
}

impl From<&str> for PostfixOp {
	fn from(s: &str) -> PostfixOp {
		match s {
			"++" => PostfixOp::Inc,
			"--" => PostfixOp::Dec,
			_ => panic!("invalid string: {}", s),
		}
	}
}

impl PostfixOp {
	pub fn as_str(&self) -> &str {
		match self {
			PostfixOp::Inc => "++",
			PostfixOp::Dec => "--",
		}
	}
}
