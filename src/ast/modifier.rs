#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
	Global,
	Public,
	Protected,
	Private,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplModifier {
	Virtual,
	Override,
	Static,
	Abstract,
}
