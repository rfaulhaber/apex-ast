#[derive(Debug, Clone)]
pub struct SoqlQuery {
	pub select: Vec<String>,
	pub from: Vec<String>,
	pub where_clause: Option<Where>, // where is reserved by rust
	pub with: Option<With>,
	pub group_by: Option<GroupBy>,
	pub order_by: Option<OrderBy>,

	// trial and error testing determined the limit to a LIMIT clause is 
	// around 2100000000
	pub limit: Option<i32>,
	pub offset: Option<i32>, // see above
	pub for_clause: Option<For>,
}

#[derive(Debug, Clone)]
pub struct Where;

#[derive(Debug, Clone)]
pub struct With;

#[derive(Debug, Clone)]
pub struct GroupBy {
	pub having: Having,
}

#[derive(Debug, Clone)]
pub struct Having;

#[derive(Debug, Clone)]
pub struct OrderBy;

#[derive(Debug, Clone)]
pub struct For;
