use std::{
    collections::BTreeMap,
    sync::atomic::{self, AtomicUsize},
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,
    CmpNotEqual,
    CmpEqual,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructMember {
    pub mutable: bool,
    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
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
    },
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
// TODO split this into several enum types "ValueNode", "DefinitionNode", "Statement"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    // TODO convert this to usize
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

    StmtIf {
        condition: Box<Ast>,
        body: Box<Ast>,
        else_: Option<Box<Ast>>,
    },

    ExprCall {
        function_name: String,
        paramaters: Vec<Ast>,
    },

    StmtLet {
        name: String,
        value_type: AnonType,
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

    Program {
        definitions: Vec<Ast>,
    },
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct AstRef {
    id: usize,
    syntax_tree_id: usize,
}

pub struct SyntaxTree {
    nodes: BTreeMap<AstRef, Ast>,
    next_ref: usize,
    syntax_tree_id: usize,
}

impl SyntaxTree {
    pub fn new() -> SyntaxTree {
        static SYNTAX_TREE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
        let syntax_tree_id = SYNTAX_TREE_ID_COUNTER.fetch_add(1, atomic::Ordering::SeqCst);

        SyntaxTree {
            nodes: BTreeMap::new(),
            next_ref: 0,
            syntax_tree_id,
        }
    }

    fn new_ref(&mut self) -> AstRef {
        let r = AstRef {
            id: self.next_ref,
            syntax_tree_id: self.syntax_tree_id,
        };
        self.next_ref += 1;
        r
    }

    pub fn add(&mut self, node: Ast) -> AstRef {
        let r = self.new_ref();
        self.nodes.insert(r, node);
        r
    }

    pub fn is_my_ref(&self, r: AstRef) -> bool {
        r.syntax_tree_id == self.syntax_tree_id
    }

    pub fn get(&mut self, r: AstRef) -> &Ast {
        if !self.is_my_ref(r) {
            // this probably shouldn't panic?
            panic!("this reference is from a different syntax tree!")
        }

        self.nodes.get(&r).unwrap()
    }
}

impl Default for SyntaxTree {
    fn default() -> Self {
        Self::new()
    }
}
