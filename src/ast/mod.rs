pub mod annotation;
pub mod class;
pub mod r#enum;
pub mod expr;
pub mod file;
pub mod identifier;
pub mod interface;
pub mod literal;
pub mod method;
pub mod modifier;
pub mod ops;
pub mod stmt;
pub mod trigger;
#[macro_use]
pub mod ty; // "type" is reserved

// TODO implement some kind of visitor pattern
// https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md
