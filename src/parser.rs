use pest::Parser;

#[derive(Parser)]
#[grammar = "./apex.pest"]
pub struct ApexParser;