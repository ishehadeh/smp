use std::{collections::BTreeMap, rc::Rc};

use crate::typecheck::TypeInfo;

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

#[derive(Debug, Clone)]
pub struct Environment {
    pub scopes: Vec<Scope>,
    pub types: BTreeMap<String, TypeInfo>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: NamedType,
}

#[derive(Default, Clone, Debug)]
pub struct Scope {
    pub variables: BTreeMap<String, Rc<ValueCell>>,
}

impl Environment {
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

    /// Add a variable to the current scope
    pub fn add_variable(
        &mut self,
        name: impl Into<String>,
        typ: impl Into<NamedType>,
    ) -> Rc<ValueCell> {
        let val = Rc::new(ValueCell { typ: typ.into() });
        self.current_scope_mut()
            .variables
            .insert(name.into(), val.clone());
        val
    }
}
