use crate::parser::{parse, Ast, InfixOp};

fn must_parse(s: &str) -> Ast {
    parse(s).into_result().expect("failed to parse")
}

#[test]
fn simple_expr() {
    assert_eq!(
        must_parse("1 + 1"),
        Ast::Expr {
            lhs: Box::new(Ast::Number(1)),
            op: InfixOp::Add,
            rhs: Box::new(Ast::Number(1))
        }
    );
}

#[test]
fn simple_expr_ident() {
    assert_eq!(
        must_parse("1 + abc"),
        Ast::Expr {
            lhs: Box::new(Ast::Number(1)),
            op: InfixOp::Add,
            rhs: Box::new(Ast::Ident("abc".to_owned()))
        }
    );
}

#[test]
fn parens_expr() {
    assert_eq!(
        must_parse("(1 + abc) * (10 / (5 * _1))"),
        Ast::Expr {
            lhs: Box::new(Ast::Expr {
                lhs: Box::new(Ast::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Ast::Ident("abc".to_owned()))
            }),
            op: InfixOp::Mul,
            rhs: Box::new(Ast::Expr {
                lhs: Box::new(Ast::Number(10)),
                op: InfixOp::Div,
                rhs: Box::new(Ast::Expr {
                    lhs: Box::new(Ast::Number(5)),
                    op: InfixOp::Mul,
                    rhs: Box::new(Ast::Ident("_1".to_owned()))
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
        Ast::Expr {
            lhs: Box::new(Ast::Number(1)),
            op: InfixOp::Add,
            rhs: Box::new(Ast::Repaired(Box::new(Ast::Expr {
                lhs: Box::new(Ast::Number(1)),
                op: InfixOp::Add,
                rhs: Box::new(Ast::Number(1))
            })))
        }
    );
}
