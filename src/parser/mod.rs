pub mod ast;
pub mod cst;

pub mod lalrpop_grammar;

#[cfg(test)]
mod test {
    use crate::parser::ast::InfixOp;

    use super::ast::Node;
    use super::lalrpop_grammar;
    fn str_to_ast(s: &str) -> Node {
        lalrpop_grammar::ExprParser::new().parse(s).unwrap()
    }

    #[test]
    fn test_lalrpop_simple() {
        assert_eq!(
            str_to_ast("1 + 1"),
            Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Number(1))
            }
        );
    }
}
