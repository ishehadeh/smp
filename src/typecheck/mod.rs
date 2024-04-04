use std::collections::BTreeSet;

use crate::parser::ast::AnonType;
pub mod typetree;

mod errors;
pub mod types;
pub use errors::*;

use types::{IntegerType, RecordCell, RecordType};

pub use self::typetree::TypeTree;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScalarType {
    Float64,
    Float32,
    Integer(IntegerType),
    Character,

    /// booleans may be used as a true/false unit type
    Boolean(Option<bool>),
}

impl ScalarType {
    /// apply an operation to both sides of an integer range type, return none if self or rhs is not an integer type.
    fn int_op(&self, map: fn(l: i32, r: i32) -> i32, rhs: &ScalarType) -> Option<ScalarType> {
        match (self, rhs) {
            (ScalarType::Integer(a), ScalarType::Integer(b)) => {
                Some(ScalarType::Integer(IntegerType {
                    lo: map(a.lo, b.lo),
                    hi: map(a.hi, b.hi),
                }))
            }
            (a, b) if a == b => Some(a.clone()),
            _ => None,
        }
    }

    pub fn add(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a + b, rhs)
    }
    pub fn sub(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a - b, rhs)
    }
    pub fn mul(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a * b, rhs)
    }
    pub fn div(&self, rhs: &ScalarType) -> Option<ScalarType> {
        self.int_op(|a, b| a / b, rhs)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionType {
    pub types: BTreeSet<TypeInfo>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntersectionType {
    pub types: BTreeSet<TypeInfo>,
}

impl UnionType {
    pub fn intersect(&self) -> TypeInfo {
        self.types
            .iter()
            .cloned()
            .reduce(|intersect, next| intersect.intersect(&next))
            .unwrap_or(TypeInfo::Unit)
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeInfo {
    /// A type with its only value being 'unit'
    Unit,
    // TODO tuples?
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
            // TODO: unit keyword type
            AnonType::TypeReference {
                name,
                parameters: _,
            } if name == "unit" => TypeInfo::Unit,
            AnonType::Bool => TypeInfo::Scalar(ScalarType::Boolean(None)),
            a => panic!("TODO: from_ast for {:?}", a),
        }
    }
    pub fn integer(lo: i32, hi: i32) -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Integer(IntegerType::new(lo, hi)))
    }

    pub fn bool() -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Boolean(None))
    }

    pub fn bool_valued(value: bool) -> TypeInfo {
        TypeInfo::Scalar(ScalarType::Boolean(Some(value)))
    }

    pub fn union(types: impl Into<BTreeSet<TypeInfo>>) -> TypeInfo {
        TypeInfo::Union(UnionType {
            types: types.into(),
        })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, TypeInfo::Scalar(ScalarType::Integer(_)))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, TypeInfo::Scalar(ScalarType::Boolean(_)))
    }

    pub fn is_record(&self) -> bool {
        matches!(self, TypeInfo::Record(_))
    }

    pub fn record(fields: impl Into<BTreeSet<RecordCell>>) -> TypeInfo {
        TypeInfo::Record(RecordType {
            fields: fields.into(),
        })
    }

    pub fn get_size(&self) -> usize {
        match self {
            TypeInfo::Scalar(ScalarType::Integer(_)) => 4,
            TypeInfo::Scalar(ScalarType::Boolean(_)) => 1,
            TypeInfo::Unit => 0,
            TypeInfo::Union(u) => u.types.iter().map(|x| x.get_size()).max().unwrap_or(0),
            TypeInfo::Record(r) => r.fields.iter().map(|x| x.length).sum::<usize>(),
            a => panic!("unimplemented: TypeInfo::get_size() => {:?}", a),
        }
    }

    pub fn access<S: AsRef<str>>(&self, symbol: S) -> Result<TypeInfo, TypeError> {
        match self {
            TypeInfo::Scalar(_) | TypeInfo::Unit => Err(TypeError::InvalidFieldAccess {
                symbol: symbol.as_ref().to_string(),
                object: self.clone(),
            }),
            TypeInfo::Union(u) => {
                let union_intersect = u.intersect();
                union_intersect.access(symbol)
            }
            TypeInfo::Record(r) => {
                let s_ref = symbol.as_ref();
                r.fields
                    .iter()
                    .find(|f| f.name == s_ref)
                    .map(|f| f.type_info.clone())
                    .ok_or(TypeError::InvalidFieldAccess {
                        symbol: s_ref.to_string(),
                        object: self.clone(),
                    })
            }
        }
    }

    pub fn is_subset(&self, other: &TypeInfo) -> bool {
        match (self, other) {
            // trivial case
            (a, b) if a == b => true,

            // unit only matches unit
            (TypeInfo::Unit, TypeInfo::Unit) => true,

            // integer range intersect
            (
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => a.is_subset(b),

            (TypeInfo::Scalar(ScalarType::Float64), TypeInfo::Scalar(ScalarType::Float32)) => true,

            (TypeInfo::Union(union), other) => {
                for t in &union.types {
                    if !t.is_subset(other) {
                        return false;
                    }
                }
                true
            }
            (TypeInfo::Record(a), TypeInfo::Record(b)) => {
                for field in &a.fields {
                    if !b.fields.contains(field) {
                        return false;
                    }
                }
                true
            }

            (me, TypeInfo::Union(union)) => {
                for t in &union.types {
                    if me.is_subset(t) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub fn intersect(&self, other: &TypeInfo) -> TypeInfo {
        // TODO no intersect and unit are different things, intersect should return some kind of None or 0 type.

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
                if !fields.is_empty() {
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

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub paramaters: Vec<(String, TypeInfo)>,
    pub returns: TypeInfo,
}

impl Default for FunctionDeclaration {
    fn default() -> Self {
        FunctionDeclaration {
            paramaters: Vec::new(),
            returns: TypeInfo::Unit,
        }
    }
}
