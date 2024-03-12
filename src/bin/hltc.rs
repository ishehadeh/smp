use clap::Parser;
use std::{
    env, fs,
    io::{self, Read},
    path::PathBuf,
};

use howlite::{ir::compiler::IrCompiler, parser::Ast, riscv::compiler::RiscVCompiler};

const DEBUG_PRELUDE: &str = r"
.globl __hw_breakpoint
__hw_breakpoint:
    ebreak
    jr ra
";

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// add debug prelude
    #[arg(short, long, default_value_t = false)]
    debug: bool,

    /// file to compile, defaults to stdin
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mut program = String::new();
    if let Some(file) = args.file {
        program = fs::read_to_string(file).unwrap();
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let result: howlite::parser::ParseResult = howlite::parser::parse(&program);

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

    let mut compiler = RiscVCompiler::new();
    for (func_name, func_ir) in functions.iter() {
        compiler.compile_frame(func_name, func_ir)
    }

    if args.debug {
        println!(".text\n{}\n{}", compiler.text(), DEBUG_PRELUDE)
    }
}
