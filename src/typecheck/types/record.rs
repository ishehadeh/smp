use std::collections::BTreeSet;

use crate::typecheck::TypeInfo;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RecordType {
    pub fields: BTreeSet<RecordCell>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RecordCell {
    pub name: String,
    pub offset: usize,
    pub length: usize,
    pub type_info: TypeInfo,
}

impl RecordCell {
    pub fn new(
        name: impl Into<String>,
        offset: usize,
        length: usize,
        type_info: TypeInfo,
    ) -> RecordCell {
        RecordCell {
            name: name.into(),
            offset,
            length,
            type_info,
        }
    }
}
