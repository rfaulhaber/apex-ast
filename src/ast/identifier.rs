use crate::parser::Rule;
use pest::iterators::Pair;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
	pub name: String,
}

impl<'a> From<Pair<'a, Rule>> for Identifier {
	fn from(p: Pair<'a, Rule>) -> Identifier {
		match p.as_rule() {
			Rule::identifier => Identifier {
				name: p.as_str().into(),
			},
			_ => panic!("cannot convert rule {:?} into identifier", p.as_rule()),
		}
	}
}

impl From<&str> for Identifier {
	fn from(s: &str) -> Identifier {
		Identifier {
			name: s.into(),
		}
	}
}