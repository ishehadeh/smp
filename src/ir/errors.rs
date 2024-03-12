use crate::typecheck::TypeInfo;

#[derive(Clone, Debug)]
pub enum CompileError {
    TypeError { left: TypeInfo, right: TypeInfo },
    ArithmeticIncompatible { left: TypeInfo, right: TypeInfo },
}
