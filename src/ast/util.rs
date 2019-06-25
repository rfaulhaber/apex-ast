#[macro_use]
macro_rules! impl_from {
	($obj:ident, $arg:ident, $b:block) => {
		impl<'a> From<pest::iterators::Pair<'a, crate::parser::Rule>> for $obj {
			fn from($arg: pest::iterators::Pair<'a, crate::parser::Rule>) -> $obj $b
		}
	};
}