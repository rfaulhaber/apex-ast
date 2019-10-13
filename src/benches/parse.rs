use apex_ast::parser::parse_file;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs;
use std::io::prelude::*;

fn parse_benchmark(c: &mut Criterion) {
	let mut file = fs::File::open("src/benches/testdata/fflib_Application.cls").unwrap();

	let mut input_buf = String::new();
	file.read_to_string(&mut input_buf).unwrap();

	c.bench_function("parse class", |b| {
		b.iter(|| parse_file(black_box(input_buf.as_str())))
	});
}

criterion_group!(parse, parse_benchmark);
criterion_main!(parse);
