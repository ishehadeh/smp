use crate::span::{SourceSpan, Spanned};

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
    LiteralInteger(LiteralInteger),
    Ident(Ident),

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Option<Box<Ast>>),

    DefFunction(DefFunction),
    Block(Block),
    StmtIf(StmtIf),
    ExprCall(ExprCall),
    Expr(Expr),

    StmtLet(StmtLet),

    DefType(DefType),

    Program(Program),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub span: SourceSpan,
    pub lhs: Box<Ast>,
    pub op: InfixOp,
    pub rhs: Box<Ast>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprCall {
    pub span: SourceSpan,
    pub function_name: String,
    pub paramaters: Vec<Ast>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StmtLet {
    pub span: SourceSpan,

    pub name: String,
    pub value_type: AnonType,
    pub value: Box<Ast>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub span: SourceSpan,
    pub definitions: Vec<Ast>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefType {
    pub span: SourceSpan,
    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StmtIf {
    pub span: SourceSpan,
    pub condition: Box<Ast>,
    pub body: Box<Ast>,
    pub else_: Option<Box<Ast>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub span: SourceSpan,

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
    pub returns: bool,
    pub statements: Vec<Ast>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LiteralInteger {
    pub span: SourceSpan,
    pub value: i32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub span: SourceSpan,
    pub symbol: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefFunction {
    pub span: SourceSpan,

    pub name: String,
    pub params: Vec<Param>,
    pub return_type: AnonType,
    pub body: Box<Ast>,
}

/// Implement Spanned for a struct, with the given member of type SourceSpan
macro_rules! impl_spanned {
    ($t:ty, $member:ident) => {
        impl Spanned for $t {
            fn span(&self) -> &SourceSpan {
                &self.$member
            }
        }
    };

    ($t:ty) => {
        impl_spanned!($t, span);
    };
}

impl_spanned!(Program);
impl_spanned!(Expr);
impl_spanned!(DefType);
impl_spanned!(ExprCall);
impl_spanned!(StmtIf);
impl_spanned!(Block);
impl_spanned!(DefFunction);
impl_spanned!(Ident);
impl_spanned!(LiteralInteger);
