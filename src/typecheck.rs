use std::collections::{BTreeSet, HashMap};

use crate::{
    parser::{Ast, AstRef},
    util::idvec::IdVec,
};

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
        todo!()
        // match self {
        //     &TypeInfo::ScalarType(_) => 4,
        // }
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

pub struct AstType {
    dependecies: Vec<AstRef>,
    type_info: Option<TypeInfo>,
}

impl AstType {
    pub fn new(t: impl Into<Option<TypeInfo>>, deps: impl Into<Vec<AstRef>>) -> AstType {
        AstType {
            dependecies: deps.into(),
            type_info: t.into(),
        }
    }
}

impl From<TypeInfo> for AstType {
    fn from(type_info: TypeInfo) -> Self {
        AstType {
            type_info: Some(type_info),
            dependecies: Vec::new(),
        }
    }
}

pub enum TypeConstraint {
    Equivalent(),
}

#[derive(Default)]
pub struct TypeEnvironment {
    type_holes: IdVec<TypeConstraint>,
    ast_types: HashMap<AstRef, AstType>,
}

impl TypeEnvironment {
    pub fn new() -> TypeEnvironment {
        TypeEnvironment::default()
    }

    pub fn get_expr_type(&self, expr: &Ast) -> AstType {
        match expr {
            Ast::Number(n) => TypeInfo::integer(*n as usize, *n as usize).into(),
            Ast::Ident(_) => todo!(),
            Ast::Error => todo!(),
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction {
                name,
                params,
                return_type,
                body,
            } => todo!(),
            Ast::Block { returns: false, .. } => TypeInfo::Unit.into(),
            Ast::Block {
                returns: true,
                statements,
            } => statements
                .last()
                .map(|s: &Ast| self.get_expr_type(s))
                .unwrap_or(TypeInfo::Unit.into()),
            Ast::StmtIf {
                condition,
                body,
                else_,
            } => todo!(),
            Ast::ExprCall {
                function_name,
                paramaters,
            } => todo!(),
            Ast::StmtLet {
                name,
                value_type,
                value,
            } => todo!(),
            Ast::DefType { name, typ } => todo!(),
            Ast::Expr { lhs, op, rhs } => todo!(),
            Ast::Program { definitions } => todo!(),
        }
    }
}

#[cfg(test)]

mod test {
    use crate::typecheck::{RecordCell, ScalarType, TypeInfo};

    #[test]
    pub fn int_intersect() {
        assert_eq!(
            TypeInfo::integer(0, 15).intersect(&TypeInfo::integer(5, 20)),
            TypeInfo::integer(5, 15)
        );

        assert_eq!(
            TypeInfo::integer(0, 3).intersect(&TypeInfo::integer(5, 20)),
            TypeInfo::Unit
        )
    }

    #[test]
    pub fn record_intersect() {
        assert_eq!(
            TypeInfo::record([
                RecordCell::new("foo", 0, 4, TypeInfo::Unit),
                RecordCell::new("bar", 4, 4, TypeInfo::Scalar(ScalarType::Boolean))
            ])
            .intersect(&TypeInfo::record([
                RecordCell::new("foo", 0, 4, TypeInfo::Unit),
                RecordCell::new("bar", 4, 4, TypeInfo::Scalar(ScalarType::Float64))
            ])),
            TypeInfo::record([RecordCell::new("foo", 0, 4, TypeInfo::Unit),])
        );

        assert_eq!(
            TypeInfo::integer(0, 3).intersect(&TypeInfo::integer(5, 20)),
            TypeInfo::Unit
        )
    }
}
