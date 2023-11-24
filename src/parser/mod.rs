use lalrpop_util::{lexer::Token, ErrorRecovery};

pub mod ast;
pub mod cst;

pub mod lalrpop_grammar;

pub type LalrpopError<'input> = ErrorRecovery<usize, Token<'input>, &'static str>;

#[cfg(test)]
mod test {
    use crate::parser::ast::InfixOp;

    use super::ast::Node;
    use super::{lalrpop_grammar, LalrpopError};
    fn str_to_ast<'i>(s: &'i str) -> (Vec<LalrpopError<'i>>, Node) {
        let mut errors: Vec<LalrpopError<'i>> = Vec::new();
        let ast = lalrpop_grammar::ExprParser::new()
            .parse(&mut errors, s)
            .unwrap();
        (errors, ast)
    }

    #[test]
    fn lalrpop_simple() {
        let (_, ast) = str_to_ast("1 + 1");
        assert_eq!(
            ast,
            Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Number(1))
            }
        );
    }

    #[test]
    fn lalrpop_simple_error() {
        let (errors, ast) = str_to_ast("1 + 1 + 1");
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
