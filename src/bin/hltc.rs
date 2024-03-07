use std::{
    env,
    io::{self, Read},
};

use howlite::{
    ir::{asmcompiler::RiscVCompiler, ircompiler::IrCompiler},
    parser::Ast,
};

const DEBUG_PRELUDE: &'static str = r"
.globl __hw_breakpoint
__hw_breakpoint:
    ebreak
    jr ra
";

fn main() {
    let mut program = String::new();
    if let Some(arg1) = env::args().nth(1) {
        program.push_str(&arg1);
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let result: howlite::parser::ParseResult = howlite::parser::parse(&program);
    dbg!(result.errors);
    dbg!(program);

    let mut ircompiler = IrCompiler::new();

    match result.ast {
        Ast::Program { definitions } => {
            ircompiler
                .scan_declarations(definitions.iter())
                .expect("failed to compile from ast to ir");
            ircompiler
                .compile_functions(definitions.iter())
                .expect("failde to compile functions")
        }
        _ => panic!("expected top-level program"),
    };

    let IrCompiler { functions, .. } = ircompiler;
    dbg!(&functions);
    let mut compiler = RiscVCompiler::new();
    for (func_name, func_ir) in functions.iter() {
        compiler.compile_frame(func_name, func_ir)
    }

    println!(".text\n{}\n{}", compiler.text(), DEBUG_PRELUDE)
}
