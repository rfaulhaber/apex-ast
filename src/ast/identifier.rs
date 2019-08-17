#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
	// TODO change this to &'id str to save memory
	pub name: String,
}

impl From<&str> for Identifier {
	fn from(s: &str) -> Identifier {
		Identifier { name: s.into() }
	}
}

// Identifiers are basically just fancy wrappers for Strings, so we might as
// well implement PartialEq for string-like types

impl PartialEq<&str> for Identifier {
	fn eq(&self, other: &&str) -> bool {
		*self.name.as_str() == **other
	}
}

impl PartialEq<String> for Identifier {
	fn eq(&self, other: &String) -> bool {
		*self.name == **other
	}
}
