// use crate::ast::entry::*;
use crate::ast::annotation::Annotation;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use pest::iterators::{Pair, Pairs};
use pest::Parser;

#[derive(Parser)]
#[grammar = "./apex.pest"]
pub(crate) struct GrammarParser;

fn parse_annotation(p: Pair<Rule>) -> Annotation {
	let mut inner = p.into_inner();

	let name = parse_identifier(inner.next().unwrap());

	let next = inner.next();

	if next.is_none() {
		Annotation {
			name,
			keypairs: None,
		}
	} else {
		let mut first_pair = next.unwrap().into_inner();

		let mut keypairs: Vec<(Identifier, Literal)> = Vec::new();

		let first_id = parse_identifier(first_pair.next().unwrap());
		let first_lit = parse_literal(first_pair.next().unwrap());

		 keypairs.push((first_id, first_lit));

		for pairs in inner {
			let mut pair = pairs.into_inner();
			let id = parse_identifier(pair.next().unwrap());
			let lit = parse_literal(pair.next().unwrap());

			keypairs.push((id, lit));
		}

		Annotation {
			name,
			keypairs: Some(keypairs),
		}
	}
}

// when Rule::identifier is encountered
fn parse_identifier(p: Pair<Rule>) -> Identifier {
	if p.as_rule() == Rule::identifier {
		Identifier {
			name: String::from(p.as_str()),
		}
	} else {
		panic!("expected identifier, got {:?}", p.as_rule())
	}
}

// when Rule::literal is encountered
fn parse_literal(p: Pair<Rule>) -> Literal {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::float_literal => Literal {
			kind: LiteralKind::Float(inner.as_str().parse::<f64>().unwrap()),
		},
		Rule::long_literal => Literal {
			kind: LiteralKind::Long(inner.as_str().parse::<i64>().unwrap()),
		},
		Rule::integer_literal => Literal {
			kind: LiteralKind::Integer(inner.as_str().parse::<i64>().unwrap()),
		},
		Rule::string_literal => Literal {
			kind: LiteralKind::String(String::from(inner.as_str())),
		},
		Rule::bool_literal => {
			let literal_val = inner.into_inner().next().unwrap();

			match literal_val.as_rule() {
				Rule::TRUE => Literal {
					kind: LiteralKind::Boolean(true),
				},
				Rule::FALSE => Literal {
					kind: LiteralKind::Boolean(false),
				},
				_ => unreachable!("unexpected boolean value?"),
			}
		}
		Rule::null_literal => Literal {
			kind: LiteralKind::Null,
		},
		_ => unreachable!("unexpected literal rule found: {:?}", inner.as_rule()),
	}
}

#[cfg(test)]
mod test {
	use super::*;
	fn test_parse<F, E>(rule: Rule, input: &str, parse: F, expected: E)
	where
		F: FnOnce(Pair<Rule>) -> E,
		E: std::fmt::Debug + PartialEq,
	{
		let mut parsed = GrammarParser::parse(rule, input).unwrap();
		let result = parse(parsed.next().unwrap());

		assert_eq!(expected, result);
	}

	#[test]
	fn annotation_base_parses() {
		test_parse(
			Rule::annotation,
			"@AuraEnabled",
			parse_annotation,
			Annotation {
				name: Identifier {
					name: String::from("AuraEnabled"),
				},
				keypairs: None,
			},
		)
	}

	#[test]
	fn annotation_with_attributes_parses() {
		test_parse(
			Rule::annotation,
			"@AuraEnabled(continuation=true cacheable=true)",
			parse_annotation,
			Annotation {
				name: Identifier {
					name: String::from("AuraEnabled"),
				},
				keypairs: Some(vec![
					(Identifier::from("continuation"), Literal::from(true)),
					(Identifier::from("cacheable"), Literal::from(true)),
				]),
			},
		)
	}

	#[test]
	fn null_literal_parse_correctly() {
		test_parse(
			Rule::literal,
			"null",
			parse_literal,
			Literal {
				kind: LiteralKind::Null,
			},
		)
	}

	#[test]
	fn integer_literal_parse_correctly() {
		test_parse(
			Rule::literal,
			"2",
			parse_literal,
			Literal {
				kind: LiteralKind::Integer(2),
			},
		)
	}

	#[test]
	fn string_literal_parse_correctly() {
		let expected_str = "\'hello world\'";
		test_parse(
			Rule::literal,
			expected_str,
			parse_literal,
			Literal {
				kind: LiteralKind::String(String::from(expected_str)),
			},
		)
	}

	#[test]
	fn identifier_parses_correctly() {
		test_parse(
			Rule::identifier,
			"x",
			parse_identifier,
			Identifier {
				name: String::from("x"),
			},
		);
	}
}
