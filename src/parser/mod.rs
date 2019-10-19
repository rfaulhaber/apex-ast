pub(crate) mod parse;
pub use parse::{parse_file, ParseError};
// lexically, this is dead code, but Pest needs this imported like so
#[allow(unused_imports)]
use pest::Parser;

/// Actual Pest parser to transform source code into `Pair<Rule>` objects.
#[derive(Parser)]
#[grammar = "./apex.pest"]
#[allow(dead_code)]
pub(crate) struct ApexParser;

#[cfg(test)]
mod tests;
