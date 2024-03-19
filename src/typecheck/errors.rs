use crate::{parser::ast::InfixOp, typecheck::TypeInfo};

#[derive(Clone, Debug)]
pub enum TypeError {
    BadExpression {
        lhs: TypeInfo,
        rhs: TypeInfo,
        op: InfixOp,
    },
}
