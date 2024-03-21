use crate::{parser::ast::InfixOp, typecheck::TypeInfo};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub enum TypeError {
    BadExpression {
        lhs: TypeInfo,
        rhs: TypeInfo,
        op: InfixOp,
    },
    UnknownVariable {
        name: String,
    },
    BadFunctionReturnType {
        expected: TypeInfo,
        returned: TypeInfo,
    },
}
