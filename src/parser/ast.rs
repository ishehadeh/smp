use super::cst::Rule;
use pest::iterators::Pair;

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
    Expr {
        lhs: Box<Node>,
        op: InfixOp,
        rhs: Box<Node>,
    },
}

pub fn pair_to_op(pair: Pair<'_, Rule>) -> InfixOp {
    match pair.as_str() {
        "+" => InfixOp::Add,
        "-" => InfixOp::Sub,
        "*" => InfixOp::Mul,
        "/" => InfixOp::Div,
        _ => panic!("Unexpected operaterator {:?}", pair.as_str()),
    }
}

pub fn pair_to_ast(pair: Pair<'_, Rule>) -> Node {
    match pair.as_rule() {
        Rule::expr => {
            let mut children = pair.into_inner();
            let lhs = Box::new(pair_to_ast(children.next().unwrap()));
            let op = pair_to_op(children.next().unwrap());
            let rhs = Box::new(pair_to_ast(children.next().unwrap()));
            Node::Expr { lhs, op, rhs }
        }
        Rule::number => Node::Number(pair.as_str().parse::<i32>().unwrap()),
        Rule::ident => Node::Ident(pair.as_str().to_owned()),
        Rule::program | Rule::atomic_expr => pair_to_ast(pair.into_inner().next().unwrap()),

        Rule::err_no_explicit_grouping => Node::Error,
        Rule::infix_op | Rule::EOI => unreachable!(),
        Rule::WHITESPACE => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use crate::parser::ast::{pair_to_ast, InfixOp, Node};
    use crate::parser::cst::cst_parse;

    fn str_to_ast(s: &str) -> Node {
        cst_parse(s)
            .expect("failed to parse")
            .next()
            .map(|x| pair_to_ast(x))
            .unwrap()
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
}
