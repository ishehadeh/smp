use std::env;
use std::io;
use std::io::Read;

use reed::ir::compiler::Compiler;
use reed::ir::vm::Vm;
use reed::parser::Ast;

pub fn main() {
    let mut program = String::new();
    if let Some(arg1) = env::args().nth(1) {
        program.push_str(&arg1);
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let result = reed::parser::parse(&program);
    dbg!(result.errors);
    let mut compiler = Compiler::new();
    match result.ast {
        Ast::Program { definitions } => match definitions.get(0).expect("expected function def") {
            Ast::DefFunction {
                name: _,
                params: _,
                return_type: _,
                body,
            } => {
                compiler.push_frame();
                compiler.add_node(body.as_ref()).unwrap();
                dbg!(&compiler);
            }
            _ => panic!("expected top-level main function"),
        },
        _ => panic!("expected top-level program"),
    }

    let program = compiler.frame().compile();
    dbg!(&program);
    let mut vm = Vm::new(program);
    while vm.step() {
        vm.dbg_state();
    }
    dbg!(vm.pop());
}
