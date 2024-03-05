use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "hlti")]
#[command(author = "Ian Shehadeh <ian@shehadeh.net>")]
#[command(version = "0.0")]
#[command(about = "Howlite programming language interpreter")]
struct Args {
    program: String,
}

use howlite::{
    ir::{compiler, vm::Vm},
    parser::parse,
};

fn main() {
    let args = Args::parse();

    let ast_root = parse(&args.program)
        .into_result()
        .expect("failed to compile");

    let mut compiler = compiler::Compiler::new();
    compiler.add_node(&ast_root).expect("failed to compile");

    let mut vm = Vm::new(compiler.into_program());
    while vm.step() {}
    println!("result: {}", vm.pop());
}
