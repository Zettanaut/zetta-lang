use std::fs::File;
use std::io::Write;
use std::io::prelude::*;
use std::env;
use std::path::Path;

mod lexer;
mod tokens;
mod ast;
mod parser;
mod c_codegen;

fn parse_source_file(filename: &str) -> Vec<ast::Item> {
    let mut file = File::open(filename).unwrap();
    let mut source = String::new();
    let _ = file.read_to_string(&mut source);

    let tokens = lexer::lex(source);
    parser::parse(tokens)
}

fn execute_include_directives(items: Vec<ast::Item>) -> Vec<ast::Item> {

    let mut resulting_items = Vec::new();
    resulting_items.reserve(items.len());

    for item in items {

        if let ast::ItemKind::Directive(ast::DirectiveKind::Include(ref filename)) = item.node {
            let mut additional_nodes = parse_source_file(filename.as_ref());
            resulting_items.append(&mut additional_nodes);
        } else {
            resulting_items.push(item);
        }
    }

    resulting_items
}

fn main() {

    let _ = env::set_current_dir(&Path::new(".."));

    let mut items = parse_source_file("src/compiler.par");

    items = execute_include_directives(items);

    //let typed_items = type_checking::check(items);

    let output = c_codegen::generate(items);

    let mut output_file = File::create("main.c").unwrap();
    let _ = output_file.write(output.as_bytes());
}
