pub type CodeBlock = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct Statement {
	kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {}