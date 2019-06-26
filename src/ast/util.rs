#[macro_export]
macro_rules! first_pair {
	($pair:expr) => {
		$pair.into_inner().next().unwrap()
	};
}