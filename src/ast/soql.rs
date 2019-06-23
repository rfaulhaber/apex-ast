#[derive(Debug, Clone)]
pub struct SoqlQuery {
	pub select: Vec<String>,
	pub from: Vec<String>, // from clauses are always a list of fields
	pub where_clause: Option<Vec<Where>>, // where is reserved by rust
	pub with: Option<With>,
	pub group_by: Option<GroupBy>,
	pub order_by: Option<OrderBy>,

	// trial and error testing determined the limit to a LIMIT clause is
	// around 2100000000
	pub limit: Option<u32>,
	pub offset: Option<u32>, // see above
	pub for_clause: Option<For>,
}

#[derive(Debug, Clone)]
pub struct Where {}

pub struct QueryTerm {
	pub kind: QueryTermKind,
}

pub enum QueryTermKind {
	/// an aggregate function, such as COUNT(Id)
	Func(String, Vec<String>),

	/// An object field, like Id or Name
	Field(String),

	SubQuery(SoqlSubQuery),
}

pub struct SoqlSubQuery {
	pub select: Vec<QueryTerm>,
}

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
