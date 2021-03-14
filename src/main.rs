use std::fs::File;
use std::io::Write;
use std::io::prelude::*;
use std::env;
use std::path::Path;
use std::process::Command;

mod span;
mod syntax;
mod c_codegen;

use syntax::{ast, lexer, parser};

fn parse_source_file<P: AsRef<Path>>(path: P) -> Vec<ast::Item> {
    let mut file = File::open(path).unwrap();
    let mut source = String::new();
    let _ = file.read_to_string(&mut source);

    let tokens = lexer::lex(source);
    parser::parse(tokens)
}

fn main() {
    if env::args().len() != 2 {
        panic!("Arguments must be exactly one filepath!");
    }
    let path_str = &env::args().last().unwrap();
    let path = Path::new(path_str);
    let items = parse_source_file(&path);

    let file_stem = path.file_stem().unwrap().to_str().unwrap();

    //let typed_items = type_checking::check(items);

    let output = c_codegen::generate(items);

    let c_file_path = format!("build/{}.c", file_stem);
    let obj_file_path = format!("build/{}.o", file_stem);
    let exe_file_path = format!("{}", file_stem);

    let mut output_file = File::create(&c_file_path).unwrap();
    let _ = output_file.write(output.as_bytes());

    Command::new("cc")
        .arg("-c")
        .arg(&c_file_path)
        .arg("-o")
        .arg(&obj_file_path)
        .output()
        .expect("GCC failed to compile");

    Command::new("cc")
        .arg(&obj_file_path)
        .arg("-o")
        .arg(&exe_file_path)
        .output()
        .expect("GCC failed to link");
}
