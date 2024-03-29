use std::vec;

use crate::parser::{
    ast::{self, Expr, FieldAccess, Ident, InfixOp, StructLiteral},
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

#[test]
fn struct_literal_1() {
    let expected = Ast::StructLiteral(StructLiteral {
        span: (0, 15).into(),
        xdata: (),
        members: vec![ast::StructLiteralMember {
            span: (8, 14).into(),
            xdata: (),
            field: ast::Ident {
                span: (8, 9).into(),
                xdata: (),
                symbol: "c".to_string(),
            },
            value: Box::new(Ast::LiteralInteger(ast::LiteralInteger {
                span: (12, 13).into(),
                xdata: (),
                value: 1,
            })),
        }],
    });
    assert_eq!(must_parse_expr("struct{ c = 1 }"), expected);
}

#[test]
fn struct_literal_2() {
    let expected = Ast::StructLiteral(StructLiteral {
        span: (0, 15).into(),
        xdata: (),
        members: vec![
            ast::StructLiteralMember {
                span: (8, 14).into(),
                xdata: (),
                field: ast::Ident {
                    span: (8, 9).into(),
                    xdata: (),
                    symbol: "a".to_string(),
                },
                value: Box::new(Ast::LiteralInteger(ast::LiteralInteger {
                    span: (12, 13).into(),
                    xdata: (),
                    value: 1,
                })),
            },
            ast::StructLiteralMember {
                span: (8, 14).into(),
                xdata: (),
                field: ast::Ident {
                    span: (8, 9).into(),
                    xdata: (),
                    symbol: "b".to_string(),
                },
                value: Box::new(Ast::LiteralInteger(ast::LiteralInteger {
                    span: (12, 13).into(),
                    xdata: (),
                    value: 2,
                })),
            },
        ],
    });
    assert_eq!(must_parse_expr("struct { a = 1, b = 2 }"), expected);
}
