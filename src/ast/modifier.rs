use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum AccessModifier {
	Global,
	Public,
	Protected,
	Private,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ImplModifier {
	Virtual,
	Override,
	Static,
	Abstract,
}
