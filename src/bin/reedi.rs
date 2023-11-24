use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "reedi")]
#[command(author = "Ian Shehadeh <ian@shehadeh.net>")]
#[command(version = "0.0")]
#[command(about = "reed programming language interpreter")]
struct Args {
    program: String,
}

use reed::{
    ir::{compiler, vm::Vm},
    parser::{ast, cst},
};

fn main() {
    let args = Args::parse();

    let cst_root = cst::cst_parse(&args.program)
        .expect("failed to parse into cst")
        .next()
        .unwrap();
    let ast_root = ast::pair_to_ast(cst_root);
    let mut compiler = compiler::Compiler::new();
    compiler.add_node(&ast_root).expect("failed to compile");

    let mut vm = Vm::new(compiler.into_program());
    while vm.step() {}
    println!("result: {}", vm.pop());
}
