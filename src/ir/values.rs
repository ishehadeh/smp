use crate::{typecheck::TypeInfo, util::idvec::Id};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Integer(i32),
}

#[derive(Debug, Clone)]
pub struct ValueCell {
    pub typ: NamedType,
}

pub type VReg = Id<ValueCell>;

/// Type name or literal type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NamedType {
    Named(String),
    Value(TypeInfo),
}

impl<T: Into<String>> From<T> for NamedType {
    fn from(value: T) -> Self {
        Self::Named(value.into())
    }
}

impl From<TypeInfo> for NamedType {
    fn from(value: TypeInfo) -> Self {
        Self::Value(value)
    }
}
