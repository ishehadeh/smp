use crate::parser::{
    ast::{Expr, FieldAccess, Ident, InfixOp},
    parse, Ast,
};

use super::parse_expr;

fn must_parse(s: &str) -> Ast {
    parse(s).into_result().expect("failed to parse")
}

fn must_parse_expr(s: &str) -> Ast {
    parse_expr(s).into_result().expect("failed to parse")
}

#[test]
fn simple_access() {
    assert_eq!(
        must_parse_expr("a.b"),
        Ast::FieldAccess(FieldAccess {
            span: (0, 3).into(),
            xdata: (),
            object: Box::new(Ast::Ident(Ident {
                symbol: String::from("a"),
                xdata: (),
                span: (0, 1).into()
            })),
            field: Ident {
                symbol: String::from("b"),
                xdata: (),
                span: (2, 3).into()
            }
        })
    );
}

#[test]
fn access_and_add() {
    assert_eq!(
        must_parse_expr("a.b + c"),
        Ast::Expr(Expr {
            span: (0, 7).into(),
            xdata: (),
            lhs: Box::new(Ast::FieldAccess(FieldAccess {
                span: (0, 3).into(),
                xdata: (),
                object: Box::new(Ast::Ident(Ident {
                    symbol: String::from("a"),
                    xdata: (),
                    span: (0, 1).into()
                })),
                field: Ident {
                    symbol: String::from("b"),
                    xdata: (),
                    span: (2, 3).into()
                }
            })),
            op: InfixOp::Add,
            rhs: Box::new(Ast::Ident(Ident {
                symbol: String::from("c"),
                xdata: (),
                span: (6, 7).into()
            }))
        })
    );
}
