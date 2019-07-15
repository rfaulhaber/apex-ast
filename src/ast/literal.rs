use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

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

impl<'a> From<Pair<'a, Rule>> for Literal {
	fn from(pair: Pair<Rule>) -> Literal {
		// thank you clippy
		let p = if pair.as_rule() == Rule::literal {
			pair.into_inner().next().unwrap()
		} else {
			pair
		};

		match p.as_rule() {
			Rule::string_literal => p.as_str().into(),
			Rule::float_literal => p.as_str().parse::<f64>().unwrap().into(),
			Rule::long_literal => Literal {
				kind: LiteralKind::Long(p.as_str().parse::<i64>().unwrap()),
			},
			Rule::integer_literal => {
				LiteralKind::Integer(p.as_str().parse::<i64>().unwrap()).into()
			}
			Rule::boolean_literal => {
				match p.into_inner().next().unwrap().as_rule() {
					Rule::TRUE => LiteralKind::Boolean(true).into(),
					Rule::FALSE => LiteralKind::Boolean(false).into(),
					_ => unreachable!(), // we hope!
				}
			}
			_ => unreachable!("got rule: {:?}", p.as_rule()),
		}
	}
}

#[cfg(test)]
mod literal_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

	macro_rules! literal_test {
		($name:ident, $test:literal, $expected:expr, $enum_type:ident) => {
			#[test]
			fn $name() {
				let parsed = GrammarParser::parse(Rule::literal, $test)
					.unwrap()
					.next()
					.unwrap();

				let expected: Literal = LiteralKind::$enum_type($expected).into();

				let result = Literal::from(parsed);

				assert_eq!(expected, result);
			}
		};
	}

	literal_test!(
		string_parses_correctly,
		"'hello'",
		String::from("'hello'"),
		String
	);

	literal_test!(integer_parses_correctly, "123", 123, Integer);
	literal_test!(boolean_parses_correctly, "true", true, Boolean);
}
