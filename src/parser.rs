
use crate::ast::entry::*;
use pest::Parser;
#[derive(Parser)]
#[grammar = "./apex.pest"]
struct GrammarParser;

// TODO possibly change to &str
pub fn parse_apex(contents: String) -> Result<EntryKind, String> {
	let pairs = match GrammarParser::parse(Rule::apex_file, contents.as_str()) {
		Ok(pairs) => pairs,
		Err(err)  => return Err(err.to_string()),
	};

	println!("paris: {:?}", pairs);

	unimplemented!();
}