use clap::{Parser, ValueEnum};
use core::fmt;
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
}

pub fn main() {
    let args = Args::parse();
    let mut program = String::new();
    if let Some(file) = args.file {
        program = fs::read_to_string(file).unwrap();
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let parse_result = howlite::parser::parse(&program);

    match args.format {
        Format::Text => {
            println!("ast = {:#?}", parse_result.ast);
            println!("errors = {:#?}", parse_result.errors);
        }
        Format::Json => {
            serde_json::to_writer(stdout(), &parse_result).unwrap();
            stdout().flush().unwrap();
        }
    }
}
