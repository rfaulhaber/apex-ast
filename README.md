# apex-ast

`apex-ast` is a Rust library for parsing Apex files and generating ASTs.

# Getting Started

At the moment this crate is not on crates.io, so you must add the following to your `Cargo.toml` file:

```toml
[dependencies]
apex-ast = { git = "https://github.com/rfaulhaber/apex-ast", tag = "0.1.0" }
```

# Usage

The `parser` module contains a function called `parse_file`, which returns a `File` AST node.
From there you can continue traversing the AST.

Also worth looking at is the `Visitor` trait in the `ast` module that allows you to traverse the AST by node.