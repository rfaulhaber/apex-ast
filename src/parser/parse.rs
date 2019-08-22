use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::expr::*;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use crate::ast::ops::*;
use crate::ast::ty::*;
use pest::iterators::Pair;

// NOTE: should this entire module be an object or generic function?

macro_rules! descend_pair {
	($pair:expr) => {
		$pair.into_inner().next().unwrap()
	};
}

pub fn parse_annotation(p: Pair<Rule>) -> Annotation {
	let mut inner = p.into_inner();

	let name = parse_identifier(inner.next().unwrap());

	let next = inner.next();

	if next.is_none() {
		Annotation {
			name,
			keypairs: None,
		}
	} else {
		let mut first_pair = next.unwrap().into_inner();

		let mut keypairs: Vec<(Identifier, Literal)> = Vec::new();

		let first_id = parse_identifier(first_pair.next().unwrap());
		let first_lit = parse_literal(first_pair.next().unwrap());

		keypairs.push((first_id, first_lit));

		for pairs in inner {
			let mut pair = pairs.into_inner();
			let id = parse_identifier(pair.next().unwrap());
			let lit = parse_literal(pair.next().unwrap());

			keypairs.push((id, lit));
		}

		Annotation {
			name,
			keypairs: Some(keypairs),
		}
	}
}

// Rule::expression or any sub-rule
pub fn parse_expr(p: Pair<Rule>) -> Expr {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::infix_expr => parse_infix_expr(inner),
		Rule::ternary_expr => parse_ternary_expr(inner),
		Rule::assignment_expr => parse_assignment_expr(inner),
		Rule::expr_inner => parse_expr_inner(inner),
		_ => unreachable!("unexpected rule: {:?}", inner.as_rule()),
	}
}

fn parse_infix_expr(p: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_ternary_expr(p: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_assignment_expr(p: Pair<Rule>) -> Expr {
	unimplemented!();
}

fn parse_expr_inner(p: Pair<Rule>) -> Expr {
	let inner = descend_pair!(p);

	match inner.as_rule() {
		Rule::braced_expr => Expr {
			kind: ExprKind::Braced(Box::new(parse_expr(descend_pair!(inner)))),
		},
		Rule::property_access => parse_property_access(inner),
		Rule::query_expression => unimplemented!(),
		Rule::list_access => parse_list_access(inner),
		Rule::new_instance_expr => parse_new_instance_expr(inner),
		Rule::method_call => parse_method_call(inner),
		Rule::unary_expr => {
			let mut unary_inner = inner.into_inner();

			let op = UnOp::from(unary_inner.next().unwrap().as_str());
			let expr = parse_expr(unary_inner.next().unwrap());

			ExprKind::Unary(op, Box::new(expr)).into()
		}
		Rule::prefix_expr => {
			let mut prefix_inner = inner.into_inner();

			let op = IncDecOp::from(prefix_inner.next().unwrap().as_str());
			let affixable = prefix_inner.next().unwrap().into_inner().next().unwrap();

			let affixable_expr = match affixable.as_rule() {
				Rule::property_access => parse_property_access(affixable),
				Rule::list_access => parse_list_access(affixable),
				Rule::identifier => ExprKind::Identifier(parse_identifier(affixable)).into(),
				_ => unreachable!("expected affixable subrule, got {:?}", affixable.as_rule()),
			};

			ExprKind::Prefix(op, Box::new(affixable_expr)).into()
		}
		Rule::postfix_expr => {
			let mut postfix_inner = inner.into_inner();

			let affixable = postfix_inner.next().unwrap().into_inner().next().unwrap();
			let op = IncDecOp::from(postfix_inner.next().unwrap().as_str());

			let affixable_expr = match affixable.as_rule() {
				Rule::property_access => parse_property_access(affixable),
				Rule::list_access => parse_list_access(affixable),
				Rule::identifier => ExprKind::Identifier(parse_identifier(affixable)).into(),
				_ => unreachable!("expected affixable subrule, got {:?}", affixable.as_rule()),
			};

			ExprKind::Postfix(Box::new(affixable_expr), op).into()
		}
		Rule::instanceof_expr => {
			let mut inst_pairs = inner.into_inner();

			let id = parse_identifier(inst_pairs.next().unwrap());
			inst_pairs.next(); // discard INSTANCEOF token
			let ty = parse_ty(inst_pairs.next().unwrap());

			ExprKind::Instanceof(id, ty).into()
		}
		Rule::cast_expression => parse_cast_expr(inner),
		Rule::primary => {
			let primary = inner.into_inner().next().unwrap();

			match primary.as_rule() {
				Rule::type_expr => ExprKind::Type(parse_ty(descend_pair!(primary))).into(),
				Rule::literal => ExprKind::Literal(parse_literal(primary)).into(),
				Rule::identifier => ExprKind::Identifier(parse_identifier(primary)).into(),
				_ => unimplemented!(
					"rule {:?} either unimplemented or unexpected",
					primary.as_rule()
				),
			}
		}
		_ => unreachable!(
			"unexpected expr_inner rule encountered: {:?}",
			inner.as_rule()
		),
	}
}

fn parse_property_access(p: Pair<Rule>) -> Expr {
	let mut prop_inner = p.into_inner();

	let first_pair = prop_inner.next().unwrap();

	let accessible = match first_pair.as_rule() {
		Rule::list_access => parse_list_access(first_pair),
		Rule::cast_expression => parse_cast_expr(first_pair),
		Rule::new_instance_expr => parse_new_instance_expr(first_pair),
		Rule::query_expression => parse_query_expression(first_pair),
		Rule::method_call => parse_method_call(first_pair),
		Rule::identifier => Expr::from(parse_identifier(first_pair)),
		_ => unreachable!("unexpected rule found: {:?}", first_pair.as_rule()),
	};

	let second_pair = prop_inner.next().unwrap();

	let selector = match second_pair.as_rule() {
		Rule::property_access => parse_property_access(second_pair),
		Rule::list_access => parse_list_access(second_pair),
		Rule::method_call => parse_method_call(second_pair),
		Rule::identifier => Expr::from(parse_identifier(second_pair)),
		_ => unreachable!("unexpected rule found: {:?}", second_pair.as_rule()),
	};

	Expr {
		kind: ExprKind::PropertyAccess(Box::new(accessible), Box::new(selector)),
	}
}

fn parse_query_expression(p: Pair<Rule>) -> Expr {
	unimplemented!("query expression parsing not implemented yet");
}

fn parse_list_access(p: Pair<Rule>) -> Expr {
	let mut list_inner = p.into_inner();

	let accessible_pair = list_inner.next().unwrap();

	let accessible = match accessible_pair.as_rule() {
		Rule::cast_expression => parse_cast_expr(accessible_pair),
		Rule::new_instance_expr => parse_new_instance_expr(accessible_pair),
		Rule::method_call => parse_method_call(accessible_pair),
		Rule::identifier => Expr::from(parse_identifier(accessible_pair)),
		_ => unreachable!(
			"unimplemented or unexpected rule: {:?}",
			accessible_pair.as_rule()
		),
	};

	let access_expr = parse_expr(list_inner.next().unwrap());

	Expr {
		kind: ExprKind::ListAccess(Box::new(accessible), Box::new(access_expr)),
	}
}

// for Rule::arguments
fn parse_arguments(p: Pair<Rule>) -> Option<Vec<Expr>> {
	let args_inner = p.into_inner();

	let pairs: Vec<Expr> = args_inner.map(parse_expr).collect();

	if pairs.is_empty() {
		None
	} else {
		Some(pairs)
	}
}

fn parse_cast_expr(p: Pair<Rule>) -> Expr {
	let mut cast_inner = p.into_inner();

	let ty = parse_ty(cast_inner.next().unwrap());
	let expr = parse_expr(cast_inner.next().unwrap());

	ExprKind::Cast(ty, Box::new(expr)).into()
}

fn parse_new_instance_expr(p: Pair<Rule>) -> Expr {
	let mut new_inst_inner = p.into_inner();

	new_inst_inner.next(); // discard "NEW"

	let subrule = new_inst_inner.next().unwrap();

	match subrule.as_rule() {
		Rule::map_literal_init => {
			let mut map_inner = subrule.into_inner();

			let map_name = Identifier::from(map_inner.next().unwrap().as_str());

			let mut two_types = map_inner.next().unwrap().into_inner();

			let first_type = parse_ty(two_types.next().unwrap());
			let second_type = parse_ty(two_types.next().unwrap());

			let ty = Ty {
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					name: map_name,
					subclass: None,
					type_arguments: type_args!(first_type, second_type),
					is_array: false,
				}),
			};

			let args = map_inner.next().unwrap();

			match args.as_rule() {
				Rule::map_literal_values => {
					let pairs: Vec<(Expr, Expr)> = args
						.into_inner()
						.map(|p| {
							let mut pair_inner = p.into_inner();

							let left = parse_expr(pair_inner.next().unwrap());
							let right = parse_expr(pair_inner.next().unwrap());

							(left, right)
						})
						.collect();

					Expr {
						kind: ExprKind::New(ty, Some(NewType::Map(pairs))),
					}
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(args)))).into()
				}
				_ => unreachable!("unexpected rule encountered: {:?}", args.as_rule()),
			}
		}
		Rule::collection_literal_init => {
			let mut lit_inner = subrule.into_inner();

			let collection_name = Identifier::from(lit_inner.next().unwrap().as_str());
			let gen_type = parse_ty(lit_inner.next().unwrap().into_inner().next().unwrap());

			let args = lit_inner.next().unwrap();

			let ty = Ty {
				kind: TyKind::ClassOrInterface(ClassOrInterface {
					name: collection_name,
					subclass: None,
					type_arguments: Some((Box::new(gen_type), None)),
					is_array: false,
				}),
			};

			match args.as_rule() {
				Rule::new_collection_literal => {
					let exprs: Vec<Expr> = args.into_inner().map(parse_expr).collect();

					ExprKind::New(ty, Some(NewType::Collection(exprs))).into()
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(args)))).into()
				}
				_ => unreachable!("encountered unexpected rule: {:?}", args.as_rule()),
			}
		}
		Rule::array_literal_init => {
			let mut lit_inner = subrule.into_inner();

			let ty = parse_ty(lit_inner.next().unwrap());

			let exprs: Vec<Expr> = lit_inner
				.next()
				.unwrap()
				.into_inner()
				.map(parse_expr)
				.collect();

			ExprKind::New(ty, Some(NewType::Array(exprs))).into()
		}
		Rule::new_class => {
			let mut class_inner = subrule.into_inner();

			let ty = parse_ty(class_inner.next().unwrap());

			let args = class_inner.next().unwrap();

			match args.as_rule() {
				Rule::sobject_arguments => {
					let args: Vec<(Identifier, Expr)> = args
						.into_inner()
						.map(|p| {
							let mut sobject_pair = p.into_inner();

							let id = parse_identifier(sobject_pair.next().unwrap());
							let expr = parse_expr(sobject_pair.next().unwrap());

							(id, expr)
						})
						.collect();

					ExprKind::New(ty, Some(NewType::Class(ClassArgs::SObject(args)))).into()
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, Some(NewType::Class(ClassArgs::Basic(args)))).into()
				}
				_ => unreachable!("unexpected rule found: {:?}", args.as_rule()),
			}
		}
		_ => unreachable!("expected new instance subrule, got {:?}", subrule.as_rule()),
	}
}

fn parse_method_call(p: Pair<Rule>) -> Expr {
	let mut method_inner = p.into_inner();

	let id = parse_identifier(method_inner.next().unwrap());
	let args = parse_arguments(method_inner.next().unwrap());

	ExprKind::Call(id, args).into()
}

// when Rule::identifier is encountered
pub fn parse_identifier(p: Pair<Rule>) -> Identifier {
	if p.as_rule() == Rule::identifier {
		Identifier {
			name: String::from(p.as_str()),
		}
	} else {
		panic!("expected identifier, got {:?}", p.as_rule())
	}
}

// when Rule::literal is encountered
pub fn parse_literal(p: Pair<Rule>) -> Literal {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::float_literal => Literal {
			kind: LiteralKind::Float(inner.as_str().parse::<f64>().unwrap()),
		},
		Rule::long_literal => Literal {
			kind: LiteralKind::Long(inner.as_str().parse::<i64>().unwrap()),
		},
		Rule::integer_literal => Literal {
			kind: LiteralKind::Integer(inner.as_str().parse::<i64>().unwrap()),
		},
		Rule::string_literal => Literal {
			kind: LiteralKind::String(String::from(inner.as_str())),
		},
		Rule::bool_literal => {
			let literal_val = inner.into_inner().next().unwrap();

			match literal_val.as_rule() {
				Rule::TRUE => Literal {
					kind: LiteralKind::Boolean(true),
				},
				Rule::FALSE => Literal {
					kind: LiteralKind::Boolean(false),
				},
				_ => unreachable!("unexpected boolean value?"),
			}
		}
		Rule::null_literal => Literal {
			kind: LiteralKind::Null,
		},
		_ => unreachable!("unexpected literal rule found: {:?}", inner.as_rule()),
	}
}

// Rule::basic_type
// TODO clean up
pub fn parse_ty(p: Pair<Rule>) -> Ty {
	let mut inner = p.into_inner();

	let t = inner.next().unwrap();

	match t.as_rule() {
		Rule::class_or_interface_type => {
			let mut coi_inner = t.into_inner();

			let name = parse_identifier(coi_inner.next().unwrap());

			match coi_inner.next() {
				Some(ref pair) if pair.as_rule() == Rule::identifier => {
					let subclass = parse_identifier(pair.clone());

					let next = coi_inner.next();

					if next.is_none() {
						Ty {
							kind: TyKind::ClassOrInterface(ClassOrInterface {
								name,
								subclass: Some(subclass),
								type_arguments: None,
								is_array: inner.next().is_some(),
							}),
						}
					} else {
						let ty_args = next.unwrap().into_inner().next().unwrap();

						match ty_args.as_rule() {
							Rule::two_type_arguments => {
								let mut type_inner = ty_args.into_inner();
								let first = parse_ty(type_inner.next().unwrap());
								let second = parse_ty(type_inner.next().unwrap());

								Ty {
									kind: TyKind::ClassOrInterface(ClassOrInterface {
										name,
										subclass: Some(subclass),
										type_arguments: Some((
											Box::new(first),
											Some(Box::new(second)),
										)),
										is_array: inner.next().is_some(),
									}),
								}
							}
							Rule::one_type_argument => {
								let mut type_inner = ty_args.into_inner();
								let first = parse_ty(type_inner.next().unwrap());
								Ty {
									kind: TyKind::ClassOrInterface(ClassOrInterface {
										name,
										subclass: Some(subclass),
										type_arguments: Some((Box::new(first), None)),
										is_array: inner.next().is_some(),
									}),
								}
							}
							_ => unreachable!("unexpected rule found: {:?}", ty_args.as_rule()),
						}
					}
				}
				Some(ref pair) if pair.as_rule() == Rule::type_arguments => {
					let ty_args = pair.clone().into_inner().next().unwrap();

					match ty_args.as_rule() {
						Rule::two_type_arguments => {
							let mut type_inner = ty_args.into_inner();
							let first = parse_ty(type_inner.next().unwrap());
							let second = parse_ty(type_inner.next().unwrap());

							Ty {
								kind: TyKind::ClassOrInterface(ClassOrInterface {
									name,
									subclass: None,
									type_arguments: Some((Box::new(first), Some(Box::new(second)))),
									is_array: inner.next().is_some(),
								}),
							}
						}
						Rule::one_type_argument => {
							let mut type_inner = ty_args.into_inner();
							let first = parse_ty(type_inner.next().unwrap());
							Ty {
								kind: TyKind::ClassOrInterface(ClassOrInterface {
									name,
									subclass: None,
									type_arguments: Some((Box::new(first), None)),
									is_array: inner.next().is_some(),
								}),
							}
						}
						_ => unreachable!("unexpected rule found: {:?}", ty_args.as_rule()),
					}
				}
				Some(_) => unreachable!("unexpected variant"),
				None => Ty {
					kind: TyKind::ClassOrInterface(ClassOrInterface {
						name,
						subclass: None,
						type_arguments: None,
						is_array: inner.next().is_some(),
					}),
				},
			}
		}
		Rule::primitive_type => {
			let prim_inner = t.into_inner().next().unwrap();

			let kind = match prim_inner.as_rule() {
				Rule::BLOB => PrimitiveKind::Blob,
				Rule::BOOLEAN => PrimitiveKind::Boolean,
				Rule::DATE => PrimitiveKind::Date,
				Rule::DATETIME => PrimitiveKind::Datetime,
				Rule::DECIMAL => PrimitiveKind::Decimal,
				Rule::DOUBLE => PrimitiveKind::Double,
				Rule::ID => PrimitiveKind::ID,
				Rule::INTEGER => PrimitiveKind::Integer,
				Rule::LONG => PrimitiveKind::Long,
				Rule::OBJECT => PrimitiveKind::Object,
				Rule::STRING => PrimitiveKind::String,
				Rule::TIME => PrimitiveKind::Time,
				_ => unreachable!("unexpected primitve kind: {:?}", prim_inner.as_rule()),
			};

			Ty {
				kind: TyKind::Primitive(Primitive {
					kind,
					is_array: inner.next().is_some(),
				}),
			}
		}
		_ => unreachable!("expected basic_type or sub-rule, found {:?}", t.as_rule()),
	}
}
