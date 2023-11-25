use super::{grammar::ExprParser, ParseError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Number(i32),
    Ident(String),
    Error,

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Box<Node>),

    Expr {
        lhs: Box<Node>,
        op: InfixOp,
        rhs: Box<Node>,
    },
}

pub struct ParseResult {
    pub ast: Node,
    pub errors: Vec<ParseError>,
}

impl ParseResult {
    pub fn new<T: Into<Vec<ParseError>>>(ast: Node, errors: T) -> ParseResult {
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

    pub fn into_result(self) -> Result<Node, Vec<ParseError>> {
        if self.is_err() {
            Err(self.errors)
        } else {
            Ok(self.ast)
        }
    }

    pub fn into_option(self) -> Option<Node> {
        self.into_result().ok()
    }
}

pub fn parse(s: &str) -> ParseResult {
    let mut recovered_errors = Vec::new();
    let result = ExprParser::new().parse(&mut recovered_errors, s);

    let mut errors: Vec<_> = recovered_errors
        .into_iter()
        .map(|e| ParseError::from(e.error))
        .collect();
    let ast = match result {
        Ok(v) => v,
        Err(e) => {
            errors.push(ParseError::from(e));
            Node::Error
        }
    };

    ParseResult::new(ast, errors)
}
