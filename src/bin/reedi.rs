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
    parser::ast::parse,
};

fn main() {
    let args = Args::parse();

    let mut recovered_errors = Vec::new();
    let ast_root = parse(&args.program, &mut recovered_errors).expect("failed to compile");
    if recovered_errors.len() > 0 {
        for err in recovered_errors {
            println!("{:#?}", err);
        }
        panic!("errors occured during compile");
    }

    let mut compiler = compiler::Compiler::new();
    compiler.add_node(&ast_root).expect("failed to compile");

    let mut vm = Vm::new(compiler.into_program());
    while vm.step() {}
    println!("result: {}", vm.pop());
}
