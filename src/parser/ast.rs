#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub typ: AnonType
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructMember {
    pub mutable: bool,
    pub name: String,
    pub typ: AnonType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnonType {
    TypeReference {
        name: String,
        parameters: (),
    },
    StructBody {
        members: Vec<StructMember>,
    },
    IntegerRange {
        /// TODO: figure out how to represent these rust
        inclusive_low: String,
        inclusive_high: String,
    }
}


// TODO split this into several enum types "ValueNode", "DefinitionNode", "Statement"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Number(i32),
    Ident(String),
    // TODO: remove this
    Error,

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Box<Ast>),

    DefFunction { 
        name: String,
        params: Vec<Param>,
        return_type: AnonType,
        body: Box<Ast>,
    },

    Block {
        /// Indicates that the value of the final value in `statements` should be the value of this block.
        /// For example:
        /// ```txt
        /// {
        ///     1 + 1
        /// }
        /// // Evaluates to `2`, `returns = true`
        /// ```
        /// 
        /// ```txt
        /// {
        ///     1 + 1;
        /// }
        /// // evaluates to `unit`, `returns = false`
        /// ```
        returns: bool,
        statements: Vec<Ast>,
    },

    StmtLet {
        name: String,
        return_type: AnonType,
        value: Box<Ast>,
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
