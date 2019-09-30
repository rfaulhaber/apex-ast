/// Parsing functions for the Apex AST.
/// Functions generally follow the pattern of `parse_*`, e.g. `parse_ty`.
pub mod parse;

#[cfg(test)]
mod tests;

// lexically, this is dead code, but Pest needs this imported like so
#[allow(unused_imports)]
use pest::Parser;

/// Actual Pest parser to transform source code into `Pair<Rule>` objects.
#[derive(Parser)]
#[grammar = "./apex.pest"]
#[allow(dead_code)]
pub(crate) struct GrammarParser;
