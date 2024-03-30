use crate::span::{SourceSpan, Spanned};
use std::fmt::Debug;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOp {
    Add,
    Sub,
    Div,
    Mul,
    CmpNotEqual,
    CmpEqual,
    CmpLess,
    Assign,
}

// TODO make Param, StructMember and AnonType a proper part of the AST

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Param<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    pub mutable: bool,
    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
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
    Bool,
}

/// Trait to access extension data on an ast node
pub trait XData<X> {
    fn xdata(&self) -> &X;
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "data"))]
// TODO split this into several enum types "ValueNode", "DefinitionNode", "Statement"
#[derive(Debug, Clone, PartialEq)]
pub enum Ast<X: Debug + Clone = ()> {
    // TODO convert this to usize
    LiteralInteger(LiteralInteger<X>),
    LiteralBool(LiteralBool<X>),
    Ident(Ident<X>),
    FieldAccess(FieldAccess<X>),

    /// A repaired node is one where an error occured but parsing was still able to be completed
    /// This is typically used for non-critical errors like 1 + 1 + 1 instead of 1 + (1 + 1)
    Repaired(Repaired<X>),

    DefFunction(DefFunction<X>),
    Block(Block<X>),
    StmtIf(StmtIf<X>),
    ExprCall(ExprCall<X>),
    Expr(Expr<X>),

    StructLiteral(StructLiteral<X>),
    StmtLet(StmtLet<X>),

    DefType(DefType<X>),

    Program(Program<X>),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Repaired<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub tree: Option<Box<Ast<X>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub object: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub lhs: Box<Ast<X>>,
    pub op: InfixOp,
    pub rhs: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub function_name: String,
    pub paramaters: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtLet<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub value_type: AnonType,
    pub value: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Program<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub definitions: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefType<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub typ: AnonType,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub condition: Box<Ast<X>>,
    pub body: Box<Ast<X>>,
    pub else_: Option<Box<Ast<X>>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Block<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

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
    pub statements: Vec<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralInteger<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub value: i32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralBool<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub value: bool,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Ident<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub symbol: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct DefFunction<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub name: String,
    pub params: Vec<Param<X>>,
    pub return_type: AnonType,
    pub body: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralMember<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub field: Ident,
    pub value: Box<Ast<X>>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral<X: Debug + Clone = ()> {
    pub span: SourceSpan,
    pub xdata: X,

    pub members: Vec<StructLiteralMember<X>>,
}

/// Implement Spanned for a struct, with the given member of type SourceSpan
macro_rules! impl_ast_node {
    ($t:ident) => {
        impl<X: Debug + Clone> Spanned for $t<X> {
            fn span(&self) -> &SourceSpan {
                &self.span
            }
        }

        impl<X: Debug + Clone> XData<X> for $t<X> {
            fn xdata(&self) -> &X {
                &self.xdata
            }
        }
    };
}

impl_ast_node! { StructLiteralMember }
impl_ast_node! { Param }

macro_rules! impl_ast_traits {
    ($ast_ty:ident : $($member:ident),*) => {
        $(impl_ast_node!{ $member })*

        impl<X: Debug + Clone> Spanned for $ast_ty<X> {
            fn span(&self) -> &SourceSpan {
                match self {
                    $(Ast::$member(a) => a.span()),*
                }
            }
        }

        impl<X: Debug + Clone> XData<X> for $ast_ty<X> {
            fn xdata(&self) -> &X {
                match self {
                    $(Ast::$member(a) => a.xdata()),*
                }
            }
        }
    };
}

impl_ast_traits!(Ast :
    Program,
    Expr,
    DefType,
    ExprCall,
    StmtIf,
    StmtLet,
    Block,
    DefFunction,
    Ident,
    LiteralInteger,
    LiteralBool,
    Repaired,
    FieldAccess,
    StructLiteral
);
