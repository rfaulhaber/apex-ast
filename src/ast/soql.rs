pub struct SoqlQuery {
	pub select: Vec<String>,
	pub from: Vec<String>,
	pub where: WhereExpr,
}

pub struct WhereExpr;