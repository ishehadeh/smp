use std::collections::BTreeSet;

use crate::parser::AnonType;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerType {
    // TODO: allow for integers larger than a single pointer
    // FIXME: sync these field names with the AST integer range names
    pub lo: usize,
    pub hi: usize,
}

impl IntegerType {
    pub fn new(lo: usize, hi: usize) -> IntegerType {
        IntegerType { lo, hi }
    }

    pub fn intersect(&self, other: &IntegerType) -> Option<IntegerType> {
        let lo_max = self.lo.max(other.lo);
        let hi_min = self.hi.min(other.hi);
        if lo_max <= hi_min {
            Some(IntegerType::new(lo_max, hi_min))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScalarType {
    Float64,
    Float32,
    Integer(IntegerType),
    Character,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionType {
    pub types: BTreeSet<TypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntersectionType {
    pub types: BTreeSet<TypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RecordType {
    pub fields: BTreeSet<RecordCell>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeInfo {
    Unit,
    // TODO tuples?
    /// A type with its only value being '()'
    Scalar(ScalarType),
    Union(UnionType),
    Record(RecordType),
}

impl TypeInfo {
    pub fn from_ast(ast_type: &AnonType) -> TypeInfo {
        match ast_type {
            AnonType::IntegerRange {
                inclusive_low,
                inclusive_high,
            } => TypeInfo::integer(
                inclusive_low.parse().unwrap(),
                inclusive_high.parse().unwrap(),
            ),
            _ => todo!(),
        }
    }
    pub fn integer(lo: usize, hi: usize) -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Integer(IntegerType::new(lo, hi)))
    }
    pub fn union(types: impl Into<BTreeSet<TypeInfo>>) -> TypeInfo {
        TypeInfo::Union(UnionType {
            types: types.into(),
        })
    }
    pub fn record(fields: impl Into<BTreeSet<RecordCell>>) -> TypeInfo {
        TypeInfo::Record(RecordType {
            fields: fields.into(),
        })
    }
    pub fn get_size(&self) -> usize {
        match self {
            TypeInfo::Scalar(ScalarType::Integer(x)) => 4,
            _ => todo!(),
        }
    }

    pub fn intersect(&self, other: &TypeInfo) -> TypeInfo {
        match (self, other) {
            // trivial case
            (a, b) if a == b => a.clone(),

            // () intersect a = ()
            (TypeInfo::Unit, _) | (_, TypeInfo::Unit) => TypeInfo::Unit,

            // integer range intersect
            (
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => a
                .intersect(b)
                .map(|i| TypeInfo::Scalar(ScalarType::Integer(i)))
                .unwrap_or(TypeInfo::Unit),

            // except for integers scalars have no non-trivial intersect
            (TypeInfo::Scalar(_), TypeInfo::Scalar(_)) => TypeInfo::Unit,

            (TypeInfo::Union(union), other) | (other, TypeInfo::Union(union)) => {
                TypeInfo::Union(UnionType {
                    types: union.types.iter().map(|t| other.intersect(t)).collect(),
                })
            }
            (TypeInfo::Record(a), TypeInfo::Record(b)) => {
                let fields: BTreeSet<RecordCell> =
                    a.fields.intersection(&b.fields).cloned().collect();
                if fields.len() > 0 {
                    TypeInfo::Record(RecordType { fields })
                } else {
                    TypeInfo::Unit
                }
            }

            (TypeInfo::Record(_), TypeInfo::Scalar(_))
            | (TypeInfo::Scalar(_), TypeInfo::Record(_)) => TypeInfo::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RecordCell {
    name: String,
    offset: usize,
    length: usize,
    type_info: TypeInfo,
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