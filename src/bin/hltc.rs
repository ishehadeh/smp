use clap::Parser;
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

use howlite::{
    riscv::ttcompiler::Compiler, typecheck::typetree::TypeInterpreter, util::ast::scan_declarations,
};

const DEBUG_PRELUDE: &str = r"
.globl __hw_breakpoint
__hw_breakpoint:
    ebreak
    ret

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
    let parse_result: howlite::parser::ParseResult = howlite::parser::parse(&program);

    let decl = scan_declarations(std::iter::once(&parse_result.ast));
    let mut type_interp = TypeInterpreter::new(decl);
    let type_tree = type_interp.eval_ast(parse_result.ast);

    let mut compiler = Compiler::new();
    let result = compiler.eval_ast(&type_tree);
    println!(".text\n{}", result.buffer.get_text());

    if args.debug {
        println!("\n{}", DEBUG_PRELUDE)
    }
}
