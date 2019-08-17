/// Parsing functions for the Apex AST.
/// Functions generally follow the pattern of `parse_*`, e.g. `parse_ty`.
pub mod fns;

#[cfg(test)]
mod tests;

use pest::iterators::Pair;
#[allow(unused_imports)]
use pest::Parser;

/// Actual Pest parser to transform source code into `Pair<Rule>` objects.
#[derive(Parser)]
#[grammar = "./apex.pest"]
#[allow(dead_code)]
pub(crate) struct GrammarParser;
