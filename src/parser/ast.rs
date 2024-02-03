#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInstance {
    pub name: String,
    pub parameters: () // TODO        
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub typ: TypeInstance
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructMember {
    pub mutable: bool,
    pub name: String,
    pub typ: TypeInstance,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnonType {
    StructBody {
        members: Vec<StructMember>,
    },
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Number(i32),
    Ident(String),
    Error,

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Box<Ast>),

    DefFunction { 
        name: String,
        params: Vec<Param>,
        return_type: TypeInstance,
        body: Vec<Ast>,
    },

    DefType {
        name: String,
        typ: AnonType,
    },

    Expr {
        lhs: Box<Ast>,
        op: InfixOp,
        rhs: Box<Ast>,
    },
}
