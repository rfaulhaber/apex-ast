pub struct Interface {
	pub name: String,
	pub extensions: Vec<String>,
	pub implementations: Vec<String>,
	// pub methods: Vec<InterfaceMethods>
}

pub struct InterfaceMethod {
	pub return_type: String,
	pub name: String,
	// pub parameters: Vec<MethodParam>,
}