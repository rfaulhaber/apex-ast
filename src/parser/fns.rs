use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::identifier::Identifier;
use crate::ast::literal::*;
use crate::ast::ty::*;

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
