use super::class::*;
use super::file::File;
use super::identifier::*;
use super::interface::*;
use super::method::*;
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
	// TODO implement
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
	// match stmt.kind {}
	unimplemented!();
}
