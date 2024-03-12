use crate::{typecheck::TypeInfo, util::idvec::Id};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Unit,
    Integer(i32),
}

#[derive(Debug, Clone)]
pub struct ValueCell {
    pub typ: TypeInfo,
}

pub type VReg = Id<ValueCell>;
