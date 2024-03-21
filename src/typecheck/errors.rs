use crate::{parser::ast::InfixOp, typecheck::TypeInfo};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub enum TypeError {
    BadExpression {
        lhs: TypeInfo,
        rhs: TypeInfo,
        op: InfixOp,
    },
    BadAssignment {
        binding_type: TypeInfo,
        value_type: TypeInfo,
        binding_name: String,
    },
    ExpectedCondition {
        recieved: TypeInfo,
    },
    UnknownVariable {
        name: String,
    },
    UnknownFunction {
        name: String,
    },
    BadFunctionReturnType {
        expected: TypeInfo,
        returned: TypeInfo,
    },
}
