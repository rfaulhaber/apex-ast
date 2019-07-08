use super::method::ImplementableMethod;

pub struct Interface {
	pub name: String,
	pub extensions: Vec<String>,
	pub implementations: Vec<String>,
	pub methods: Vec<ImplementableMethod>,
}
