use super::annotation::*;
use super::class::*;
use super::expr::*;
use super::file::File;
use super::identifier::*;
use super::interface::*;
use super::literal::*;
use super::method::*;
use super::r#enum::*;
use super::stmt::*;
use super::trigger::*;
use super::ty::*;

// see https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md

pub trait Visitor: Sized {
	fn visit_file(&mut self, file: File) {
		walk_file(self, file);
	}

	fn visit_class(&mut self, class: Class) {
		walk_class(self, class);
	}

	fn visit_class_body_member(&mut self, cbm: ClassBodyMember) {
		walk_class_body_member(self, cbm);
	}

	fn visit_class_field(&mut self, field: ClassField) {
		walk_class_field(self, field);
	}

	fn visit_class_method(&mut self, method: ClassMethod) {
		walk_class_method(self, method);
	}

	fn visit_enum(&mut self, enum_def: Enum) {
		walk_enum(self, enum_def);
	}

	fn visit_constructor(&mut self, constructor: ClassConstructor) {
		walk_class_constructor(self, constructor);
	}

	fn visit_interface(&mut self, interface: Interface) {
		walk_interface(self, interface);
	}

	fn visit_trigger(&mut self, trigger: Trigger) {
		walk_trigger(self, trigger);
	}

	fn visit_interface_method(&mut self, method: ImplementableMethod) {
		walk_interface_method(self, method);
	}

	fn visit_identifier(&mut self, identifier: Identifier) {
		// nothing to do - leaf of tree
	}

	fn visit_param(&mut self, param: (Ty, Identifier)) {
		walk_param(self, param);
	}

	fn visit_ty(&mut self, ty: Ty) {
		walk_ty(self, ty);
	}

	fn visit_coi_ty(&mut self, coi: ClassOrInterface) {
		walk_coi_ty(self, coi);
	}

	fn visit_primitive(&mut self, primitive: Primitive) {
		walk_primitive(self, primitive);
	}

	fn visit_primitive_kind(&mut self, pk: PrimitiveKind) {}

	fn visit_block(&mut self, block: Block) {
		walk_block(self, block);
	}

	fn visit_stmt(&mut self, stmt: Stmt) {
		walk_stmt(self, stmt);
	}

	fn visit_stmt_expr(&mut self, stmt_expr: StmtExpr) {
		walk_stmt_expr(self, stmt_expr);
	}

	fn visit_annotation(&mut self, annotation: Annotation) {
		walk_annotation(self, annotation);
	}

	fn visit_expr(&mut self, expr: Expr) {
		walk_expr(self, expr);
	}

	fn visit_local(&mut self, local: Local) {
		walk_local(self, local);
	}

	fn visit_literal(&mut self, literal: Literal) {
		// nothing to do
	}

	fn visit_soql(&mut self, query_str: String) {}

	fn visit_sosl(&mut self, query_str: String) {}
}

// thank you rust libsyntax
macro_rules! walk_list {
	($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
    ($visitor: expr, $method: ident, $list: expr, $($extra_args: expr),*) => {
        for elem in $list {
            $visitor.$method(elem, $($extra_args,)*)
        }
    }
}

pub fn walk_file<V: Visitor>(visitor: &mut V, f: File) {
	match f {
		File::Class(c) => visitor.visit_class(c),
		File::Interface(i) => visitor.visit_interface(i),
		File::Trigger(t) => visitor.visit_trigger(t),
	}
}

pub fn walk_class<V: Visitor>(visitor: &mut V, class: Class) {
	if let Some(annotation) = class.annotation {
		visitor.visit_annotation(annotation);
	}

	visitor.visit_identifier(class.name);

	if let Some(extension) = class.extension {
		visitor.visit_ty(extension);
	}

	walk_list!(visitor, visit_ty, class.implementations);
	walk_list!(visitor, visit_class_body_member, class.body);
}

pub fn walk_interface<V: Visitor>(visitor: &mut V, interface: Interface) {
	visitor.visit_identifier(interface.name);
	walk_list!(visitor, visit_ty, interface.extensions);
	walk_list!(visitor, visit_interface_method, interface.methods);
}

pub fn walk_interface_method<V: Visitor>(visitor: &mut V, method: ImplementableMethod) {
	visitor.visit_ty(method.ty);
	visitor.visit_identifier(method.id);
	walk_list!(visitor, visit_param, method.params);
}

pub fn walk_param<V: Visitor>(visitor: &mut V, param: (Ty, Identifier)) {
	visitor.visit_ty(param.0);
	visitor.visit_identifier(param.1);
}

pub fn walk_trigger<V: Visitor>(visitor: &mut V, trigger: Trigger) {
	visitor.visit_identifier(trigger.name);
	visitor.visit_ty(trigger.object);
	// TODO should I visit trigger events?
	visitor.visit_block(trigger.body);
}

pub fn walk_ty<V: Visitor>(visitor: &mut V, ty: Ty) {
	match ty.kind {
		TyKind::ClassOrInterface(coi) => visitor.visit_coi_ty(coi),
		TyKind::Primitive(prim) => visitor.visit_primitive(prim),
		TyKind::Void => (), // nothing to do
	}
}

pub fn walk_coi_ty<V: Visitor>(visitor: &mut V, coi: ClassOrInterface) {
	visitor.visit_identifier(coi.name);

	if let Some(subclass) = coi.subclass {
		visitor.visit_identifier(subclass);
	}

	if let Some(type_args) = coi.type_arguments {
		visitor.visit_ty(*type_args.0);

		if let Some(second_arg) = type_args.1 {
			visitor.visit_ty(*second_arg);
		}
	}
}

pub fn walk_primitive<V: Visitor>(visitor: &mut V, prim: Primitive) {
	visitor.visit_primitive_kind(prim.kind);
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block: Block) {
	match block {
		Block::Body(stmts) => walk_list!(visitor, visit_stmt, stmts),
		Block::Inline(stmt) => visitor.visit_stmt(*stmt),
	}
}

pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: Stmt) {
	match stmt.kind {
		StmtKind::For(for_stmt) => match for_stmt {
			ForStmt::Basic(decl, cond, update, block) => {
				if let Some(stmt_exprs) = decl {
					walk_list!(visitor, visit_stmt_expr, stmt_exprs);
				}

				if let Some(cond_expr) = cond {
					visitor.visit_expr(cond_expr);
				}

				if let Some(update_stmt_expr) = update {
					visitor.visit_stmt_expr(update_stmt_expr);
				}

				visitor.visit_block(*block);
			}
			ForStmt::Enhanced(ty, id, expr, block) => {
				visitor.visit_ty(ty);
				visitor.visit_identifier(id);
				visitor.visit_expr(expr);
				visitor.visit_block(*block);
			}
		},
		StmtKind::DoWhile(block, expr) => {
			visitor.visit_block(*block);
			visitor.visit_expr(expr);
		}
		StmtKind::While(expr, block) => {
			visitor.visit_expr(expr);
			visitor.visit_block(*block);
		}
		StmtKind::If(expr, head_block, else_ifs, else_block) => {
			visitor.visit_expr(expr);
			visitor.visit_block(*head_block);

			if let Some(else_if_pairs) = else_ifs {
				for (expr, block_ref) in else_if_pairs {
					visitor.visit_expr(expr);
					visitor.visit_block(*block_ref);
				}
			}

			if let Some(else_block_ref) = else_block {
				visitor.visit_block(*else_block_ref);
			}
		}
		StmtKind::Switch(head_expr, optional_when_cases, optional_default_case) => {
			visitor.visit_expr(head_expr);

			if let Some(when_cases) = optional_when_cases {
				for (when_cond, block) in when_cases {
					match when_cond {
						WhenCondition::Type(ty, id) => {
							visitor.visit_ty(ty);
							visitor.visit_identifier(id);
						}
						WhenCondition::Value(when_values) => {
							for when_value in when_values {
								match when_value {
									WhenValue::Literal(literal) => visitor.visit_literal(literal),
									WhenValue::Identifier(id) => visitor.visit_identifier(id),
								}
							}
						}
					};
				}
			}

			if let Some(block) = optional_default_case {
				visitor.visit_block(block);
			}
		}
		StmtKind::TryCatch(try_block, default_catch, optional_catches, finally_block) => {
			visitor.visit_block(*try_block);

			let (default_ty, default_id, default_block) = default_catch;

			visitor.visit_ty(default_ty);
			visitor.visit_identifier(default_id);
			visitor.visit_block(default_block);

			if let Some(catches) = optional_catches {
				for (ty, id, block) in catches {
					visitor.visit_ty(ty);
					visitor.visit_identifier(id);
					visitor.visit_block(block);
				}
			}

			if let Some(block) = finally_block {
				visitor.visit_block(block);
			}
		}
		StmtKind::Block(block) => {
			visitor.visit_block(block);
		}
		StmtKind::Return(optional_expr) => {
			if let Some(expr) = optional_expr {
				visitor.visit_expr(expr);
			}
		}
		StmtKind::Dml(_op, expr) => {
			visitor.visit_expr(expr);
		}
		StmtKind::Throw(throw_expr) => {
			visitor.visit_expr(throw_expr);
		}
		StmtKind::Break | StmtKind::Continue => (),
		StmtKind::StmtExpr(stmt_expr) => {
			visitor.visit_stmt_expr(stmt_expr);
		}
		StmtKind::Local(local) => {
			visitor.visit_local(local);
		}
	}
}

pub fn walk_stmt_expr<V: Visitor>(visitor: &mut V, stmt_expr: StmtExpr) {
	match stmt_expr {
		StmtExpr::Expr(expr) => visitor.visit_expr(expr),
		StmtExpr::Local(local) => visitor.visit_local(local),
	}
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: Expr) {
	match expr.kind {
		ExprKind::Infix(expr_ref, op, expr_rhs_ref) => {
			visitor.visit_expr(*expr_ref);
			visitor.visit_expr(*expr_rhs_ref);
		}
		ExprKind::Ternary(cond_expr, true_expr, false_expr) => {
			visitor.visit_expr(*cond_expr);
			visitor.visit_expr(*true_expr);
			visitor.visit_expr(*false_expr);
		}
		ExprKind::Assignment(lhs_expr, op, rhs_expr) => {
			visitor.visit_expr(*lhs_expr);
			visitor.visit_expr(*rhs_expr);
		}
		ExprKind::Braced(expr) => visitor.visit_expr(*expr),
		ExprKind::PropertyAccess(lhs_expr, rhs_expr) => {
			visitor.visit_expr(*lhs_expr);
			visitor.visit_expr(*rhs_expr);
		}
		ExprKind::ListAccess(lhs_expr, rhs_expr) => {
			visitor.visit_expr(*lhs_expr);
			visitor.visit_expr(*rhs_expr);
		}
		ExprKind::Query(query) => match query {
			Query::Soql(query_str) => visitor.visit_soql(query_str),
			Query::Sosl(query_str) => visitor.visit_sosl(query_str),
		},
		ExprKind::New(ty, new_type) => {
			visitor.visit_ty(ty);

			match new_type {
				NewType::Map(mapping) => {
					for (l, r) in mapping {
						visitor.visit_expr(l);
						visitor.visit_expr(r);
					}
				}
				NewType::Collection(col) | NewType::Array(col) => {
					walk_list!(visitor, visit_expr, col);
				}
				NewType::Class(args) => match args {
					ClassArgs::Basic(optional_args) => {
						if let Some(basic_args) = optional_args {
							walk_list!(visitor, visit_expr, basic_args);
						}
					}
					ClassArgs::SObject(pairs) => {
						for (id, expr) in pairs {
							visitor.visit_identifier(id);
							visitor.visit_expr(expr);
						}
					}
				},
			}
		}
		ExprKind::Call(id, optional_args) => {
			visitor.visit_identifier(id);

			if let Some(exprs) = optional_args {
				walk_list!(visitor, visit_expr, exprs);
			}
		}
		ExprKind::Unary(_op, expr) => {
			visitor.visit_expr(*expr);
		}
		ExprKind::Prefix(_op, expr) => {
			visitor.visit_expr(*expr);
		}
		ExprKind::Postfix(expr, _op) => {
			visitor.visit_expr(*expr);
		}
		ExprKind::Instanceof(id, ty) => {
			visitor.visit_identifier(id);
			visitor.visit_ty(ty);
		}
		ExprKind::Cast(ty, expr) => {
			visitor.visit_ty(ty);
			visitor.visit_expr(*expr);
		}
		ExprKind::Type(ty) => {
			visitor.visit_ty(ty);
		}
		ExprKind::Literal(lit) => {
			visitor.visit_literal(lit);
		}
		ExprKind::Identifier(id) => {
			visitor.visit_identifier(id);
		}
	};
}

pub fn walk_local<V: Visitor>(visitor: &mut V, local: Local) {
	if let Some(annotation) = local.annotation {
		visitor.visit_annotation(annotation);
	}

	visitor.visit_ty(local.ty);
	visitor.visit_identifier(local.id);

	if let Some(rhs) = local.rhs {
		visitor.visit_expr(rhs);
	}
}

pub fn walk_annotation<V: Visitor>(visitor: &mut V, annotation: Annotation) {
	visitor.visit_identifier(annotation.name);

	if let Some(keypairs) = annotation.keypairs {
		for (id, lit) in keypairs {
			visitor.visit_identifier(id);
			visitor.visit_literal(lit);
		}
	}
}

pub fn walk_class_body_member<V: Visitor>(visitor: &mut V, cbm: ClassBodyMember) {
	match cbm {
		ClassBodyMember::InnerClass(class) => visitor.visit_class(*class),
		ClassBodyMember::InnerInterface(interface) => visitor.visit_interface(interface),
		ClassBodyMember::Field(field) => visitor.visit_class_field(field),
		ClassBodyMember::Method(method) => visitor.visit_class_method(method),
		ClassBodyMember::Enum(enum_def) => visitor.visit_enum(enum_def),
		ClassBodyMember::StaticBlock(block) | ClassBodyMember::InstanceBlock(block) => {
			visitor.visit_block(block)
		}
		ClassBodyMember::Constructor(constructor) => visitor.visit_constructor(constructor),
	}
}

pub fn walk_class_field<V: Visitor>(visitor: &mut V, field: ClassField) {
	unimplemented!();
}

pub fn walk_class_method<V: Visitor>(visitor: &mut V, method: ClassMethod) {
	unimplemented!();
}

pub fn walk_enum<V: Visitor>(visitor: &mut V, enum_def: Enum) {
	unimplemented!();
}

pub fn walk_class_constructor<V: Visitor>(visitor: &mut V, constructor: ClassConstructor) {
	unimplemented!();
}
