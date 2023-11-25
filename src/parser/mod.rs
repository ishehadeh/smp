pub mod ast;
mod errors;

lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub use errors::*;
use lalrpop_util::lalrpop_mod;
