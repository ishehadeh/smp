use std::collections::BTreeMap;

use crate::{
    typecheck::TypeInfo,
    util::idvec::{Id, IdVec},
};

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

#[derive(Debug, Clone)]
pub struct ValueCell {
    pub typ: NamedType,
}

pub type VReg = Id<ValueCell>;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub scopes: Vec<Scope>,
    // figure out how these can be freed
    pub virtual_registers: IdVec<ValueCell>,
    pub types: BTreeMap<String, TypeInfo>,
    pub functions: BTreeMap<String, Function>,
    unit_reg: Option<VReg>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<NamedType>,
    pub return_type: NamedType,
}

#[derive(Default, Clone, Debug)]
pub struct Scope {
    pub variables: BTreeMap<String, VReg>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![Scope::default()],
            virtual_registers: IdVec::default(),
            types: BTreeMap::default(),
            functions: BTreeMap::default(),
            unit_reg: None,
        }
    }

    pub fn unit_reg(&mut self) -> VReg {
        if let Some(r) = self.unit_reg {
            return r;
        }

        let reg = self.alloc_reg(TypeInfo::Unit.into());
        self.unit_reg = Some(reg);
        reg
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes
            .last()
            .expect("There must be at least one scope, this should be unreachable")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("There must be at least one scope, this should be unreachable")
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default())
    }

    pub fn pop_scope(&mut self) -> bool {
        if self.scopes.len() <= 1 {
            false
        } else {
            self.scopes.pop().unwrap();
            true
        }
    }

    pub fn alloc_reg(&mut self, typ: NamedType) -> VReg {
        self.virtual_registers.push(ValueCell { typ })
    }

    pub fn resolve_type<'a>(&'a self, typename: &'a NamedType) -> &'a TypeInfo {
        match typename {
            NamedType::Value(t) => t,
            NamedType::Named(n) => self.types.get(n).unwrap(), // TODO error dont unwrap
        }
    }

    pub fn get_type(&self, vreg: VReg) -> &TypeInfo {
        self.resolve_type(&self.virtual_registers.get(vreg).typ)
    }

    /// Add a variable to the current scope
    pub fn add_variable(&mut self, name: impl Into<String>, typ: impl Into<NamedType>) -> VReg {
        let reg = self.alloc_reg(typ.into());
        self.current_scope_mut().variables.insert(name.into(), reg);
        reg
    }
}
