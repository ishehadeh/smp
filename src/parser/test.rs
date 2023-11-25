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
