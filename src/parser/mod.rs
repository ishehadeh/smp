mod ast;
mod errors;

lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub use ast::*;
pub use errors::*;

use lalrpop_util::lalrpop_mod;

#[cfg(test)]
mod test;

pub struct ParseResult {
    pub ast: Ast,
    pub errors: Vec<ParseError>,
}

impl ParseResult {
    pub fn new<T: Into<Vec<ParseError>>>(ast: Ast, errors: T) -> ParseResult {
        ParseResult {
            ast,
            errors: errors.into(),
        }
    }

    pub fn is_ok(&self) -> bool {
        return self.errors.len() == 0;
    }
    pub fn is_err(&self) -> bool {
        return !self.is_ok();
    }

    pub fn into_result(self) -> Result<Ast, Vec<ParseError>> {
        if self.is_err() {
            Err(self.errors)
        } else {
            Ok(self.ast)
        }
    }

    pub fn into_option(self) -> Option<Ast> {
        self.into_result().ok()
    }
}

pub fn parse(s: &str) -> ParseResult {
    let mut recovered_errors = Vec::new();
    let result = grammar::ProgramParser::new().parse(&mut recovered_errors, s);

    let mut errors: Vec<_> = recovered_errors
        .into_iter()
        .map(|e| ParseError::from(e.error))
        .collect();
    let ast = match result {
        Ok(v) => v,
        Err(e) => {
            errors.push(ParseError::from(e));
            Ast::Error
        }
    };

    ParseResult::new(ast, errors)
}
