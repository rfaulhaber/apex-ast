/// binary operators
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnOp {
	Not,
	BitNot,
	Inc,
	Dec,
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