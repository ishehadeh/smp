use crate::typecheck::TypeInfo;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayType {
    pub length: u32,
    pub element_ty: Box<TypeInfo>,
}
