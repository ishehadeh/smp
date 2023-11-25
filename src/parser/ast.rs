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

#[cfg(test)]
mod test {
    use crate::parser::ast::{parse, InfixOp, Node};

    fn must_parse(s: &str) -> Node {
        parse(s).into_result().expect("failed to parse")
    }

    #[test]
    fn simple_expr() {
        assert_eq!(
            must_parse("1 + 1"),
            Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Number(1))
            }
        );
    }

    #[test]
    fn simple_expr_ident() {
        assert_eq!(
            must_parse("1 + abc"),
            Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Ident("abc".to_owned()))
            }
        );
    }

    #[test]
    fn parens_expr() {
        assert_eq!(
            must_parse("(1 + abc) * (10 / (5 * _1))"),
            Node::Expr {
                lhs: Box::new(Node::Expr {
                    lhs: Box::new(Node::Number(1)),
                    op: InfixOp::Add,
                    rhs: Box::new(Node::Ident("abc".to_owned()))
                }),
                op: InfixOp::Mul,
                rhs: Box::new(Node::Expr {
                    lhs: Box::new(Node::Number(10)),
                    op: InfixOp::Div,
                    rhs: Box::new(Node::Expr {
                        lhs: Box::new(Node::Number(5)),
                        op: InfixOp::Mul,
                        rhs: Box::new(Node::Ident("_1".to_owned()))
                    })
                })
            }
        );
    }

    #[test]
    fn lalrpop_simple_error() {
        let res = parse("1 + 1 + 1");
        assert!(matches!(res.errors[..], []), "errors = {:#?}", res.errors);
        assert_eq!(
            res.ast,
            Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Repaired(Box::new(Node::Expr {
                    lhs: Box::new(Node::Number(1)),
                    op: InfixOp::Add,
                    rhs: Box::new(Node::Number(1))
                })))
            }
        );
    }
}
