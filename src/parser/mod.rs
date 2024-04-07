pub mod ast;
mod errors;
pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused)]
    pub grammar, "/parser/grammar.rs");

pub use ast::Ast;
pub use errors::*;

use lalrpop_util::lalrpop_mod;

use self::lexer::Lexer;

#[cfg(test)]
mod test;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
        self.errors.is_empty()
    }
    pub fn is_err(&self) -> bool {
        !self.is_ok()
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
    let lexer = Lexer::new(s);
    let mut recovered_errors = Vec::new();
    let result = grammar::ProgramParser::new().parse(&mut recovered_errors, lexer);

    let mut errors: Vec<_> = recovered_errors
        .into_iter()
        .map(|e| ParseError::from(e.error))
        .collect();
    let ast = match result {
        Ok(v) => v,
        Err(e) => {
            errors.push(ParseError::from(e));
            Ast::Repaired(ast::Repaired {
                tree: None,
                xdata: (),
                span: (0, 0).into(),
            })
        }
    };

    ParseResult::new(ast, errors)
}

pub fn parse_expr(s: &str) -> ParseResult {
    let lexer = Lexer::new(s);
    let mut recovered_errors = Vec::new();
    let result = grammar::ExprParser::new().parse(&mut recovered_errors, lexer);

    let mut errors: Vec<_> = recovered_errors
        .into_iter()
        .map(|e| ParseError::from(e.error))
        .collect();
    let ast = match result {
        Ok(v) => v,
        Err(e) => {
            errors.push(ParseError::from(e));
            Ast::Repaired(ast::Repaired {
                tree: None,
                xdata: (),
                span: (0, 0).into(),
            })
        }
    };

    ParseResult::new(ast, errors)
}
