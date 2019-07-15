use super::identifier::Identifier;
use super::literal::Literal;

use crate::parser::Rule;
use pest::iterators::Pair;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
	pub name: Identifier,
	pub keypairs: Option<Vec<(Identifier, Literal)>>,
}

impl<'a> From<Pair<'a, Rule>> for Annotation {
	// must be called when the rule encountered is Rule::annotation
	fn from(pair: Pair<Rule>) -> Annotation {
		let mut inner = pair.into_inner();

		let name = Identifier::from(inner.next().unwrap());

		let keypairs_pairs: Vec<Pair<Rule>> = inner.collect();

		if keypairs_pairs.is_empty() {
			Annotation {
				name,
				keypairs: None,
			}
		} else {
			let keypairs: Vec<(Identifier, Literal)> = keypairs_pairs
				.chunks_exact(2)
				.map(|slice| {
					(
						Identifier::from(slice[0].clone()),
						Literal::from(slice[1].clone()),
					)
				})
				.collect();

			Annotation {
				name,
				keypairs: Some(keypairs),
			}
		}
	}
}

#[cfg(test)]
mod annotation_tests {
	use super::*;
	use crate::parser::GrammarParser;
	use pest::Parser;

	#[test]
	fn annotation_without_args_parses_correctly() {
		let mut parsed = GrammarParser::parse(Rule::annotation, "@AuraEnabled").unwrap();
		let item = parsed.next().unwrap();

		let result = Annotation::from(item);

		let expected = Annotation {
			name: Identifier::from("AuraEnabled"),
			keypairs: None,
		};

		assert_eq!(expected, result);
	}

	#[test]
	fn annotation_with_args_parses_correctly() {
		let mut parsed =
			GrammarParser::parse(Rule::annotation, "@AuraEnabled(cacheable = true)").unwrap();
		let item = parsed.next().unwrap();

		let result = Annotation::from(item);

		let expected = Annotation {
			name: Identifier::from("AuraEnabled"),
			keypairs: Some(vec![(Identifier::from("cacheable"), Literal::from(true))]),
		};

		assert_eq!(expected, result);
	}
}
