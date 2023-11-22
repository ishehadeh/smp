use pest::iterators::Pair;

#[derive(pest_derive::Parser)]
#[grammar = "src/parser/grammar.pest"]
pub struct ReedParser;

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
    Expr {
        lhs: Box<Node>,
        op: InfixOp,
        rhs: Box<Node>,
    },
}

pub fn pair_to_op<'i>(pair: Pair<'i, Rule>) -> InfixOp {
    match pair.as_str() {
        "+" => InfixOp::Add,
        "-" => InfixOp::Sub,
        "*" => InfixOp::Mul,
        "/" => InfixOp::Div,
        _ => panic!("Unexpected operaterator {:?}", pair.as_str()),
    }
}

pub fn pair_to_ast<'i>(pair: Pair<'i, Rule>) -> Node {
    match pair.as_rule() {
        Rule::expr => {
            let mut children = pair.into_inner();
            let lhs = Box::new(pair_to_ast(children.next().unwrap()));
            let op = pair_to_op(children.next().unwrap());
            let rhs = Box::new(pair_to_ast(children.next().unwrap()));
            Node::Expr { lhs, op, rhs }
        }
        Rule::number => Node::Number(i32::from_str_radix(pair.as_str(), 10).unwrap()),
        Rule::program | Rule::atomic_expr => pair_to_ast(pair.into_inner().next().unwrap()),
        Rule::infix_op | Rule::EOI => unreachable!(),
        Rule::WHITESPACE => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{pair_to_ast, InfixOp, Node};

    use super::{ReedParser, Rule};
    use pest::Parser;

    #[test]
    fn test() {
        let nodes: Vec<_> = ReedParser::parse(Rule::program, "1 + 1")
            .expect("failed to parse")
            .take_while(|x| x.as_rule() != Rule::EOI)
            .map(|x| pair_to_ast(x))
            .collect();
        assert_eq!(
            nodes,
            vec![Node::Expr {
                lhs: Box::new(Node::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Node::Number(1))
            }]
        );
    }
}
