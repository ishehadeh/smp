use clap::{Parser, ValueEnum};
use core::fmt;
use howlite::parser::Ast;
use howlite::typecheck::typetree::TypeInterpreter;
use howlite::util::ast::scan_declarations;
use std::fs;
use std::io;
use std::io::stdout;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Format {
    Json,
    Text,
}

impl fmt::Display for Format {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json => f.write_str("json"),
            Self::Text => f.write_str("text"),
        }
    }
}

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// output format
    #[arg(short, long, default_value_t = Format::Text )]
    format: Format,

    /// file to parse, defaults to stdin
    file: Option<PathBuf>,

    #[arg(short, long, default_value_t = false)]
    typed: bool,
}

fn write_ast<X>(args: &Args, ast: Ast<X>)
where
    X: serde::Serialize + std::fmt::Debug + Clone,
{
    match args.format {
        Format::Text => {
            println!("{:#?}", ast);
        }
        Format::Json => {
            serde_json::to_writer_pretty(stdout(), &ast).unwrap();
            stdout().flush().unwrap();
        }
    }
}

pub fn main() {
    let args = Args::parse();
    let mut program = String::new();
    if let Some(ref file) = args.file {
        program = fs::read_to_string(file).unwrap();
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let parse_result = howlite::parser::parse(&program);
    if args.typed {
        let decl = scan_declarations(std::iter::once(&parse_result.ast));
        let mut type_interp = TypeInterpreter::new(decl);
        write_ast(&args, type_interp.eval_ast(parse_result.ast))
    } else {
        write_ast(&args, parse_result.ast)
    };
}
