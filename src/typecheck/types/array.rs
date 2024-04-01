use crate::typecheck::TypeInfo;

pub struct ArrayType {
    pub capacity_ty: TypeInfo,
    pub element_ty: TypeInfo,
}
