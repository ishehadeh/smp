use super::grammar::ExprParser;
use lalrpop_util::{lexer::Token, ErrorRecovery, ParseError};
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

pub type LalrpopError<'input> = ErrorRecovery<usize, Token<'input>, &'static str>;

pub fn parse<'i>(
    s: &'i str,
    recovered_errors: &mut Vec<LalrpopError<'i>>,
) -> Result<Node, ParseError<usize, Token<'i>, &'static str>> {
    ExprParser::new().parse(recovered_errors, s)
}

#[cfg(test)]
mod test {
    use crate::parser::ast::{parse, InfixOp, Node};

    use super::LalrpopError;

    fn str_to_ast_and_errors(s: &str) -> (Node, Vec<LalrpopError<'_>>) {
        let mut recovered = vec![];
        let ast = parse(s, &mut recovered).expect("failed to parse");
        (ast, recovered)
    }

    fn str_to_ast(s: &str) -> Node {
        let (ast, _) = str_to_ast_and_errors(s);
        ast
    }

    #[test]
    fn simple_expr() {
        assert_eq!(
            str_to_ast("1 + 1"),
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
            str_to_ast("1 + abc"),
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
            str_to_ast("(1 + abc) * (10 / (5 * _1))"),
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
        let (ast, errors) = str_to_ast_and_errors("1 + 1 + 1");
        assert!(matches!(errors[..], []), "errors = {:#?}", errors);
        assert_eq!(
            ast,
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
