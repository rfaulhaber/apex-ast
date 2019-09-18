use super::*;
use crate::ast::annotation::Annotation;
use crate::ast::class::*;
use crate::ast::expr::*;
use crate::ast::identifier::Identifier;
use crate::ast::interface::*;
use crate::ast::literal::*;
use crate::ast::method::*;
use crate::ast::modifier::*;
use crate::ast::ops::*;
use crate::ast::r#enum::Enum;
use crate::ast::stmt::*;
use crate::ast::trigger::*;
use crate::ast::ty::*;
use pest::iterators::Pair;

// NOTE: should this entire module be an object or generic function?

macro_rules! parse_iter_if_rule {
	($iter:expr,$next:ident,$rule:expr,$parse:ident) => {
		if $next.as_rule() == $rule {
			let ret = $parse($next);
			$next = $iter.next().unwrap();
			Some(ret)
		} else {
			None
			};
	};
}

macro_rules! next_is_rule {
	($iter:expr, $next:expr, $rule:expr) => {
		if $next.as_rule() == $rule {
			$next = $iter.next().unwrap();
			true
		} else {
			false
			};
	};
}

macro_rules! match_as_rule {
	($pair:ident, $( $p:pat => $res:expr ),+ ) => {
		match $pair.as_rule() {
			$( $p => $res, )+
			_ => unreachable!("encountered unexpected rule: {:?}", $pair.as_rule()),
		}
	};
}

pub fn parse_interface(p: Pair<Rule>) -> Interface {
	let mut inner = p.into_inner();
	let mut next = inner.next().unwrap();

	let access_mod = parse_iter_if_rule!(inner, next, Rule::access_modifier, parse_access_modifier);

	inner.next(); // skip "interface"

	let name = parse_identifier(next);

	let following = inner.next().unwrap();

	let extensions: Vec<Ty> = if following.as_rule() == Rule::EXTENDS {
		inner.next().unwrap().into_inner().map(parse_ty).collect()
	} else {
		Vec::new()
	};

	let methods = inner
		.next()
		.unwrap()
		.into_inner()
		.map(parse_implementatble_method)
		.collect();

	Interface {
		access_mod,
		name,
		extensions,
		methods,
	}
}

pub fn parse_trigger(p: Pair<Rule>) -> Trigger {
	let mut inner = p.into_inner();

	inner.next(); // skip "trigger"

	let name = parse_identifier(inner.next().unwrap());

	inner.next(); // skip "on"

	let object = parse_ty(inner.next().unwrap());

	let events: Vec<TriggerEvent> = inner
		.next()
		.unwrap()
		.into_inner()
		.map(|event_pair| {
			let mut inner = event_pair.into_inner();

			let next = inner.next().unwrap();

			let when = match next.as_rule() {
				Rule::BEFORE => TriggerWhen::Before,
				Rule::AFTER => TriggerWhen::After,
				_ => unreachable!("unexpected trigger rule: {:?}", next.as_rule()),
			};

			let action_pair = inner.next().unwrap().into_inner().next().unwrap();

			let dml_action = match action_pair.as_rule() {
				Rule::INSERT => DmlOp::Insert,
				Rule::UPDATE => DmlOp::Update,
				Rule::UPSERT => DmlOp::Upsert,
				Rule::DELETE => DmlOp::Delete,
				Rule::UNDELETE => DmlOp::Undelete,
				Rule::MERGE => DmlOp::Merge,
				_ => unreachable!("unexpected dml action: {:?}", action_pair.as_rule()),
			};

			TriggerEvent(when, dml_action)
		})
		.collect();

	let body = parse_block(inner.next().unwrap());

	Trigger {
		name,
		object,
		events,
		body,
	}
}

pub fn parse_enum(p: Pair<Rule>) -> Enum {
	let mut inner = p.into_inner();
	let mut next = inner.next().unwrap();

	let annotation = parse_iter_if_rule!(inner, next, Rule::annotation, parse_annotation);
	let access_mod = parse_iter_if_rule!(inner, next, Rule::access_modifier, parse_access_modifier);

	let name = parse_identifier(inner.next().unwrap());
	let ids: Vec<Identifier> = inner
		.next()
		.unwrap()
		.into_inner()
		.map(parse_identifier)
		.collect();

	Enum {
		annotation,
		access_mod,
		name,
		ids,
	}
}

pub fn parse_class_field(p: Pair<Rule>) -> ClassField {
	let mut inner = p.into_inner();
	let mut next = inner.next().unwrap();

	let annotation = parse_iter_if_rule!(inner, next, Rule::annotation, parse_annotation);
	let access_mod = parse_iter_if_rule!(inner, next, Rule::access_modifier, parse_access_modifier);
	let instance_mod = parse_iter_if_rule!(
		inner,
		next,
		Rule::instance_modifier,
		parse_instance_modifier
	);

	let is_final = next_is_rule!(inner, next, Rule::FINAL);

	let ty = parse_ty(next);
	let id = parse_identifier(inner.next().unwrap());

	let after_id = inner.next();

	if after_id.is_some() {
		let after = after_id.unwrap();

		if after.as_rule() == Rule::class_field_accessors {
			let (first_prop, second_prop) = parse_field_accessors(after);

			let getter = match second_prop.clone() {
				Some(prop) => {
					if prop.property_type == PropertyType::Get {
						second_prop.clone()
					} else {
						Some(first_prop.clone())
					}
				}
				None => Some(first_prop.clone()),
			};

			let setter = if getter.is_some() && second_prop.is_some() {
				second_prop
			} else {
				Some(first_prop)
			};

			ClassField {
				annotation,
				access_mod,
				instance_mod,
				is_final,
				ty,
				id,
				getter,
				setter,
				rhs: None,
			}
		} else {
			let expr = Some(parse_expr(after));

			ClassField {
				annotation,
				access_mod,
				instance_mod,
				is_final,
				ty,
				id,
				getter: None,
				setter: None,
				rhs: expr,
			}
		}
	} else {
		ClassField {
			annotation,
			access_mod,
			instance_mod,
			is_final,
			ty,
			id,
			getter: None,
			setter: None,
			rhs: None,
		}
	}
}

// called upon Rule::class_field_accessors
fn parse_field_accessors(p: Pair<Rule>) -> (Property, Option<Property>) {
	let mut inner = p.into_inner();

	let first = parse_class_field_accessor(inner.next().unwrap());

	let next = inner.next();

	let second = if next.is_some() {
		Some(parse_class_field_accessor(next.unwrap()))
	} else {
		None
	};

	(first, second)
}

fn parse_class_field_accessor(p: Pair<Rule>) -> Property {
	let mut inner = p.into_inner();
	let mut next = inner.next().unwrap();

	let access_mod = parse_iter_if_rule!(inner, next, Rule::access_modifier, parse_access_modifier);
	let property_type = parse_property_type(next);

	let after = inner.next();

	let body = if after.is_some() {
		Some(parse_block(after.unwrap()))
	} else {
		None
	};

	Property {
		access_mod,
		property_type,
		body,
	}
}

fn parse_instance_modifier(p: Pair<Rule>) -> ClassInstanceModifier {
	let inner = p.into_inner().next().unwrap();

	match_as_rule!(inner,
		Rule::STATIC => ClassInstanceModifier::Static,
		Rule::TRANSIENT => ClassInstanceModifier::Transient
	)
}

fn parse_property_type(p: Pair<Rule>) -> PropertyType {
	match_as_rule!(p,
		Rule::GET => PropertyType::Get,
		Rule::SET => PropertyType::Set
	)
}

pub fn parse_implementatble_method(p: Pair<Rule>) -> ImplementableMethod {
	let mut inner = p.into_inner();

	let ty = parse_ty(inner.next().unwrap());
	let id = parse_identifier(inner.next().unwrap());
	let params = parse_parameter_list(inner.next().unwrap());

	ImplementableMethod { ty, id, params }
}

pub fn parse_class_method(p: Pair<Rule>) -> ClassMethod {
	let mut inner = p.into_inner();

	let mut next = inner.next().unwrap();

	let annotation = parse_iter_if_rule!(inner, next, Rule::annotation, parse_annotation);
	let access_mod = parse_iter_if_rule!(inner, next, Rule::access_modifier, parse_access_modifier);
	let impl_mod = parse_iter_if_rule!(inner, next, Rule::impl_modifier, parse_impl_modifier);

	let is_testmethod = if next.as_rule() == Rule::TESTMETHOD {
		next = inner.next().unwrap();
		true
	} else {
		false
	};

	let return_type = parse_ty(next);
	let identifier = parse_identifier(inner.next().unwrap());
	let params = parse_parameter_list(inner.next().unwrap());
	let block = parse_block(inner.next().unwrap());

	ClassMethod {
		annotation,
		access_mod,
		impl_mod,
		is_testmethod,
		return_type,
		identifier,
		params,
		block,
	}
}

fn parse_access_modifier(p: Pair<Rule>) -> AccessModifier {
	let inner = p.into_inner().next().unwrap();

	match_as_rule!(inner,
		Rule::GLOBAL => AccessModifier::Global,
		Rule::PUBLIC => AccessModifier::Public,
		Rule::PROTECTED => AccessModifier::Protected,
		Rule::PRIVATE => AccessModifier::Private
	)
}

fn parse_impl_modifier(p: Pair<Rule>) -> ImplModifier {
	let inner = p.into_inner().next().unwrap();

	match_as_rule!(inner,
		Rule::OVERRIDE => ImplModifier::Override,
		Rule::STATIC => ImplModifier::Static,
		Rule::VIRTUAL => ImplModifier::Virtual,
		Rule::ABSTRACT => ImplModifier::Abstract
	)
}

fn parse_parameter_list(p: Pair<Rule>) -> Vec<(Ty, Identifier)> {
	let inner = p.into_inner();

	inner
		.map(|pair| {
			let mut iter_inner = pair.into_inner();
			let ty = parse_ty(iter_inner.next().unwrap());
			let id = parse_identifier(iter_inner.next().unwrap());

			(ty, id)
		})
		.collect()
}

pub fn parse_stmt(p: Pair<Rule>) -> Stmt {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::for_stmt => parse_for_stmt(inner),
		Rule::do_while_stmt => parse_do_while_stmt(inner),
		Rule::while_stmt => parse_while_stmt(inner),
		Rule::if_stmt => parse_if_stmt(inner),
		Rule::switch_stmt => parse_switch_stmt(inner),
		Rule::try_catch_stmt => parse_try_catch_stmt(inner),
		Rule::block => Stmt {
			kind: StmtKind::Block(parse_block(inner)),
		},
		Rule::return_stmt => parse_return_stmt(inner),
		Rule::dml_stmt => parse_dml_stmt(inner),
		Rule::throw_stmt => parse_throw_stmt(inner),
		Rule::break_stmt => parse_break_stmt(inner),
		Rule::continue_stmt => parse_continue_stmt(inner),
		Rule::stmt_expr => parse_stmt_expr(inner),
		_ => unreachable!("unexpected rule, {:?}", inner.as_rule()),
	}
}

fn parse_for_stmt(p: Pair<Rule>) -> Stmt {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::for_basic => parse_for_basic(inner),
		Rule::for_enhanced => parse_for_enhanced(inner),
		_ => unreachable!("unexpected rule: {:?}", inner.as_rule()),
	}
}

fn parse_for_basic(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // skip "for" token

	let mut next = inner.next().unwrap();

	let init = parse_iter_if_rule!(inner, next, Rule::for_init, parse_for_init);
	let expr = parse_iter_if_rule!(inner, next, Rule::expression, parse_expr);
	let update = parse_iter_if_rule!(inner, next, Rule::stmt_expr, parse_stmt_expr_literal);

	let block = parse_any_block(next);

	Stmt {
		kind: StmtKind::For(ForStmt::Basic(init, expr, update, Box::new(block))),
	}
}

fn parse_for_init(p: Pair<Rule>) -> Vec<StmtExpr> {
	p.into_inner().map(parse_stmt_expr_literal).collect()
}

fn parse_for_enhanced(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "for"

	let ty = parse_ty(inner.next().unwrap());
	let id = parse_identifier(inner.next().unwrap());
	let expr = parse_expr(inner.next().unwrap());
	let block = parse_any_block(inner.next().unwrap());

	Stmt {
		kind: StmtKind::For(ForStmt::Enhanced(ty, id, expr, Box::new(block))),
	}
}

fn parse_do_while_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "do"

	let block = parse_any_block(inner.next().unwrap());

	inner.next(); // discard "while"

	let expr = parse_expr(inner.next().unwrap());

	Stmt {
		kind: StmtKind::DoWhile(Box::new(block), expr),
	}
}

fn parse_while_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "while"

	let expr = parse_expr(inner.next().unwrap());
	let block = parse_any_block(inner.next().unwrap());

	Stmt {
		kind: StmtKind::While(expr, Box::new(block)),
	}
}

fn parse_if_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "if"

	let test_expr = parse_expr(inner.next().unwrap().into_inner().next().unwrap());
	let block = Box::new(parse_any_block(inner.next().unwrap()));

	let mut else_ifs = Vec::new();
	let mut else_block = None;

	for pair in inner {
		match pair.as_rule() {
			Rule::else_if_block => else_ifs.push(parse_else_if_stmt(pair)),
			Rule::else_block => {
				let mut else_inner = pair.into_inner();
				else_inner.next(); // discard "else"
				else_block = Some(Box::new(parse_any_block(else_inner.next().unwrap())));
			}
			_ => unreachable!("parsing if got this rule: {:?}", pair.as_rule()),
		}
	}

	let kind = StmtKind::If(
		test_expr,
		block,
		if else_ifs.is_empty() {
			None
		} else {
			Some(else_ifs)
		},
		else_block,
	);

	Stmt { kind }
}

fn parse_else_if_stmt(p: Pair<Rule>) -> (Expr, Box<Block>) {
	let mut inner = p.into_inner();

	// discard "else" and "if"
	inner.next();
	inner.next();

	let expr = parse_expr(inner.next().unwrap().into_inner().next().unwrap());
	let block = parse_any_block(inner.next().unwrap());

	(expr, Box::new(block))
}

fn parse_switch_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();
	inner.next(); // discared "switch"
	inner.next(); // discard "on"

	let test_expr = parse_expr(inner.next().unwrap());

	let inner_clone = inner.clone();

	let next = inner.next();

	if next.is_some() {
		let when_case_pairs: Vec<Pair<Rule>> = inner_clone
			.clone()
			.filter(|p| p.as_rule() == Rule::when_case)
			.collect();

		let when_else_pairs: Vec<Pair<Rule>> = inner
			.clone()
			.filter(|p| p.as_rule() == Rule::when_else)
			.collect();

		let when_case: Option<Vec<WhenCase>> = if when_case_pairs.is_empty() {
			None
		} else {
			Some(
				when_case_pairs
					.iter()
					.map(|p| parse_when_case(p.clone()))
					.collect(),
			)
		};

		let when_else: Option<Block> = if when_else_pairs.is_empty() {
			None
		} else {
			// there will only ever be one!
			let mut else_inner = when_else_pairs.first().unwrap().clone().into_inner();
			else_inner.next(); // discard "when"
			else_inner.next(); // discard "else"
			Some(parse_block(else_inner.next().unwrap()))
		};

		Stmt {
			kind: StmtKind::Switch(test_expr, when_case, when_else),
		}
	} else {
		Stmt {
			kind: StmtKind::Switch(test_expr, None, None),
		}
	}
}

fn parse_when_case(p: Pair<Rule>) -> WhenCase {
	let mut inner = p.into_inner();

	inner.next(); // discard "when"

	let next = inner.next().unwrap().into_inner().next().unwrap();

	let when_conds = match next.as_rule() {
		Rule::when_type => {
			let mut next_inner = next.into_inner();

			let ty = parse_ty(next_inner.next().unwrap());
			let id = parse_identifier(next_inner.next().unwrap());
			WhenCondition::Type(ty, id)
		}
		Rule::when_value_list => {
			let vals_inner = next.into_inner();

			let vals = vals_inner
				.map(|p| match p.as_rule() {
					Rule::identifier => WhenValue::from(parse_identifier(p)),
					Rule::literal => WhenValue::from(parse_literal(p)),
					_ => unreachable!("encountered unexpected rule: {:?}", p.as_rule()),
				})
				.collect();

			WhenCondition::Value(vals)
		}
		_ => unreachable!("encountered unexpected rule: {:?}", next.as_rule()),
	};

	(when_conds, parse_block(inner.next().unwrap()))
}

fn parse_try_catch_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "try"

	let try_block = parse_block(inner.next().unwrap());
	let catch_block = parse_catch_clause(inner.next().unwrap());

	let mut catches = Vec::new();
	let mut finally: Option<Block> = None;

	for pair in inner {
		match pair.as_rule() {
			Rule::catch_clause => catches.push(parse_catch_clause(pair)),
			// this is the finally block we've been looking for
			Rule::block => finally = Some(parse_block(pair)),
			Rule::FINALLY => continue, // just discard the token!
			_ => unreachable!("unexected rule: {:?}", pair.as_rule()),
		}
	}

	Stmt {
		kind: StmtKind::TryCatch(
			Box::new(try_block),
			catch_block,
			if catches.is_empty() {
				None
			} else {
				Some(catches)
			},
			finally,
		),
	}
}

fn parse_catch_clause(p: Pair<Rule>) -> (Ty, Identifier, Block) {
	let mut inner = p.into_inner();

	inner.next(); // discard "catch"

	let ty = parse_ty(inner.next().unwrap());
	let id = parse_identifier(inner.next().unwrap());
	let block = parse_block(inner.next().unwrap());

	(ty, id, block)
}

fn parse_any_block(p: Pair<Rule>) -> Block {
	match p.as_rule() {
		Rule::block => parse_block(p),
		Rule::inline_block => parse_inline_block(p),
		_ => unreachable!(
			"expected either block or inline block, got: {:?}",
			p.as_rule()
		),
	}
}

fn parse_block(p: Pair<Rule>) -> Block {
	let inner = p.into_inner();

	let stmts = inner.map(parse_stmt).collect();

	Block::Body(stmts)
}

fn parse_inline_block(p: Pair<Rule>) -> Block {
	let mut inner = p.into_inner();

	let stmt = parse_stmt(inner.next().unwrap());

	Block::Inline(stmt.to_boxed())
}

fn parse_return_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard token

	let expr = match inner.next() {
		Some(pair) => Some(parse_expr(pair)),
		None => None,
	};

	Stmt {
		kind: StmtKind::Return(expr),
	}
}

fn parse_dml_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	let dml_action = match inner.next().unwrap().into_inner().next().unwrap().as_rule() {
		Rule::INSERT => DmlOp::Insert,
		Rule::UPDATE => DmlOp::Update,
		Rule::UPSERT => DmlOp::Upsert,
		Rule::DELETE => DmlOp::Delete,
		Rule::UNDELETE => DmlOp::Undelete,
		Rule::MERGE => DmlOp::Merge,
		_ => unreachable!("unexpected rule found"),
	};

	let next_pair = inner.next().unwrap();

	let expr = match next_pair.as_rule() {
		Rule::identifier => Expr::from(parse_identifier(next_pair)),
		Rule::new_instance_expr => parse_new_instance_expr(next_pair),
		_ => unreachable!("unexpected rule: {:?}", next_pair.as_rule()),
	};

	Stmt {
		kind: StmtKind::Dml(dml_action, expr),
	}
}

fn parse_throw_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();

	inner.next(); // discard "try" for now

	let expr = parse_expr(inner.next().unwrap());

	Stmt {
		kind: StmtKind::Throw(expr),
	}
}

fn parse_break_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();
	// TODO store span, token
	inner.next();

	Stmt {
		kind: StmtKind::Break,
	}
}

fn parse_continue_stmt(p: Pair<Rule>) -> Stmt {
	let mut inner = p.into_inner();
	// TODO store span, token
	inner.next();

	Stmt {
		kind: StmtKind::Continue,
	}
}

fn parse_stmt_expr(p: Pair<Rule>) -> Stmt {
	Stmt::from(parse_stmt_expr_literal(p))
}

fn parse_stmt_expr_literal(p: Pair<Rule>) -> StmtExpr {
	let stmt_expr_pair = p.into_inner().next().unwrap();

	match stmt_expr_pair.as_rule() {
		Rule::local_variable_declaration => parse_local_variable_declaration(stmt_expr_pair),
		Rule::assignment_expr => StmtExpr::from(parse_assignment_expr(stmt_expr_pair)),
		Rule::property_access => StmtExpr::from(parse_property_access(stmt_expr_pair)),
		Rule::prefix_expr => StmtExpr::from(parse_prefix_expr(stmt_expr_pair)),
		Rule::postfix_expr => StmtExpr::from(parse_postfix_expr(stmt_expr_pair)),
		Rule::method_call => StmtExpr::from(parse_expr_inner(stmt_expr_pair)),
		Rule::new_instance_expr => (StmtExpr::from(parse_expr(stmt_expr_pair))),
		_ => unreachable!("unexpected stmt expr rule: {:?}", stmt_expr_pair.as_rule()),
	}
}

fn parse_local_variable_declaration(p: Pair<Rule>) -> StmtExpr {
	let mut inner = p.into_inner();
	let mut next = inner.next().unwrap();

	let annotation = parse_iter_if_rule!(inner, next, Rule::annotation, parse_annotation);

	let is_final = if next.as_rule() == Rule::FINAL {
		next = inner.next().unwrap();
		true
	} else {
		false
	};

	let ty = parse_ty(next);
	let id = parse_identifier(inner.next().unwrap());

	let rhs_pair = inner.next();

	let rhs = if rhs_pair.is_some() {
		Some(parse_expr(rhs_pair.unwrap()))
	} else {
		None
	};

	StmtExpr::Local(Local {
		annotation,
		is_final,
		ty,
		id,
		rhs,
	})
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
	let mut inner = p.into_inner();

	let lhs = parse_expr_inner(inner.next().unwrap());
	let op = BinOp::from(inner.next().unwrap().as_str());
	let rhs = parse_expr(inner.next().unwrap());

	Expr {
		kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
	}
}

fn parse_ternary_expr(p: Pair<Rule>) -> Expr {
	let mut inner = p.into_inner();

	let test_pair = inner.next().unwrap();

	let test = match test_pair.as_rule() {
		Rule::infix_expr => parse_infix_expr(test_pair),
		Rule::expr_inner => parse_expr_inner(test_pair),
		_ => unreachable!("unexpected rule encountered: {:?}", test_pair.as_rule()),
	};

	let pos = parse_expr(inner.next().unwrap());
	let neg = parse_expr(inner.next().unwrap());

	Expr {
		kind: ExprKind::Ternary(Box::new(test), Box::new(pos), Box::new(neg)),
	}
}

fn parse_assignment_expr(p: Pair<Rule>) -> Expr {
	let mut inner = p.into_inner();

	let lhs = parse_expr_inner(inner.next().unwrap());
	let assign_op = AssignOp::from(inner.next().unwrap().as_str());
	let rhs = parse_expr(inner.next().unwrap());

	Expr {
		kind: ExprKind::Assignment(Box::new(lhs), assign_op, Box::new(rhs)),
	}
}

fn parse_expr_inner(p: Pair<Rule>) -> Expr {
	let inner = p.into_inner().next().unwrap();

	match inner.as_rule() {
		Rule::braced_expr => Expr {
			kind: ExprKind::Braced(Box::new(parse_expr(inner.into_inner().next().unwrap()))),
		},
		Rule::property_access => parse_property_access(inner),
		Rule::query_expression => parse_query_expression(inner),
		Rule::list_access => parse_list_access(inner),
		Rule::new_instance_expr => parse_new_instance_expr(inner),
		Rule::method_call => parse_method_call(inner),
		Rule::unary_expr => {
			let mut unary_inner = inner.into_inner();

			let op = UnOp::from(unary_inner.next().unwrap().as_str());
			let expr = parse_expr(unary_inner.next().unwrap());

			ExprKind::Unary(op, Box::new(expr)).into()
		}
		Rule::prefix_expr => parse_prefix_expr(inner),
		Rule::postfix_expr => parse_postfix_expr(inner),
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
				Rule::type_expr => {
					ExprKind::Type(parse_ty(primary.into_inner().next().unwrap())).into()
				}
				Rule::literal => ExprKind::Literal(parse_literal(primary)).into(),
				Rule::identifier => ExprKind::Identifier(parse_identifier(primary)).into(),
				_ => unimplemented!(
					"rule {:?} either unimplemented or unexpected",
					primary.as_rule()
				),
			}
		}
		_ => unreachable!(
			"unexpected expr_inner rule encountered: {:?}, inner: {:?}",
			inner.as_rule(),
			inner
		),
	}
}

fn parse_prefix_expr(p: Pair<Rule>) -> Expr {
	let mut prefix_inner = p.into_inner();

	let op = IncDecOp::from(prefix_inner.next().unwrap().as_str());
	let affixable = prefix_inner.next().unwrap().into_inner().next().unwrap();

	let affixable_expr = match affixable.as_rule() {
		Rule::property_access => parse_property_access(affixable),
		Rule::list_access => parse_list_access(affixable),
		Rule::identifier => ExprKind::Identifier(parse_identifier(affixable)).into(),
		_ => unreachable!("expected affixable subrule, got {:?}", affixable.as_rule()),
	};

	Expr::from(ExprKind::Prefix(op, Box::new(affixable_expr)))
}

fn parse_postfix_expr(p: Pair<Rule>) -> Expr {
	let mut postfix_inner = p.into_inner();

	let affixable = postfix_inner.next().unwrap().into_inner().next().unwrap();
	let op = IncDecOp::from(postfix_inner.next().unwrap().as_str());

	let affixable_expr = match affixable.as_rule() {
		Rule::property_access => parse_property_access(affixable),
		Rule::list_access => parse_list_access(affixable),
		Rule::identifier => ExprKind::Identifier(parse_identifier(affixable)).into(),
		_ => unreachable!("expected affixable subrule, got {:?}", affixable.as_rule()),
	};

	Expr::from(ExprKind::Postfix(Box::new(affixable_expr), op))
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
	let mut query_inner = p.into_inner();

	let query_body = String::from(query_inner.as_str().trim());

	let query_type = match query_inner.next().unwrap().as_rule() {
		Rule::SELECT => QueryKind::Soql,
		Rule::FIND => QueryKind::Sosl,
		_ => unreachable!("unexpected query variant"),
	};

	Expr {
		kind: ExprKind::Query(query_type, query_body),
	}
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
						kind: ExprKind::New(ty, NewType::Map(pairs)),
					}
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, NewType::Class(ClassArgs::Basic(args))).into()
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

					ExprKind::New(ty, NewType::Collection(exprs)).into()
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, NewType::Class(ClassArgs::Basic(args))).into()
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

			ExprKind::New(ty, NewType::Array(exprs)).into()
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

					ExprKind::New(ty, NewType::Class(ClassArgs::SObject(args))).into()
				}
				Rule::arguments => {
					let args = parse_arguments(args);
					ExprKind::New(ty, NewType::Class(ClassArgs::Basic(args))).into()
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

			if name.eq_case_insensitive("void") {
				return Ty::void();
			}

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
