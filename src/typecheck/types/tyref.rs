use crate::typecheck::TypeInfo;

/// Not a seperate type itsef, but a reference by name to declared type
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyRef {
    pub parameters: Vec<TypeInfo>,
    pub name: String,
}
