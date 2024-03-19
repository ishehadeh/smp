use crate::{
    ir::compiler::Declarations,
    parser::{
        ast::{self, InfixOp, XData},
        Ast,
    },
};

use super::{ScalarType, TypeError, TypeInfo};

#[derive(Clone, Debug)]
pub struct TypeTreeXData {
    pub declared_type: TypeInfo,
    pub value_type: Option<TypeInfo>,
    pub error: Option<TypeError>,
}

pub type TypeTree = Ast<TypeTreeXData>;

#[derive(Default, Clone, Debug)]
pub struct TypeInterpreter {
    declarations: Declarations,
}

impl TypeInterpreter {
    pub fn new() -> TypeInterpreter {
        TypeInterpreter::default()
    }

    pub fn eval_ast(&mut self, ast: Ast) -> TypeTree {
        match ast {
            Ast::LiteralInteger(i) => Ast::LiteralInteger(TypeInterpreter::eval_literal_integer(i)),
            Ast::LiteralBool(b) => Ast::LiteralBool(TypeInterpreter::eval_literal_bool(b)),
            Ast::Ident(_) => todo!(),
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction(_) => todo!(),
            Ast::Block(_) => todo!(),
            Ast::StmtIf(_) => todo!(),
            Ast::ExprCall(_) => todo!(),
            Ast::Expr(_) => todo!(),
            Ast::StmtLet(_) => todo!(),
            Ast::DefType(_) => todo!(),
            Ast::Program(_) => todo!(),
        }
    }

    pub fn eval_literal_integer(i: ast::LiteralInteger) -> ast::LiteralInteger<TypeTreeXData> {
        let ty = TypeInfo::integer(i.value, i.value);

        ast::LiteralInteger {
            span: i.span,
            xdata: TypeTreeXData {
                declared_type: ty.clone(),
                value_type: Some(ty),
                error: None,
            },
            value: i.value,
        }
    }

    pub fn eval_literal_bool(i: ast::LiteralBool) -> ast::LiteralBool<TypeTreeXData> {
        let ty = TypeInfo::bool_valued(i.value);
        ast::LiteralBool {
            span: i.span,
            xdata: TypeTreeXData {
                declared_type: ty.clone(),
                value_type: Some(ty),
                error: None,
            },
            value: i.value,
        }
    }

    pub fn apply_infix_op_on_type(
        op: InfixOp,
        lhs: &TypeInfo,
        rhs: &TypeInfo,
    ) -> Result<TypeInfo, TypeError> {
        match (lhs, rhs) {
            (TypeInfo::Scalar(_), TypeInfo::Scalar(_))
                if matches!(op, InfixOp::CmpEqual | InfixOp::CmpNotEqual) =>
            {
                Ok(TypeInfo::bool())
            }

            (
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => {
                Ok(match op {
                    InfixOp::Add => TypeInfo::integer(a.lo + b.lo, a.hi + b.hi),
                    InfixOp::Sub => TypeInfo::integer(a.lo - b.hi, a.hi - b.lo),
                    InfixOp::Div => TypeInfo::integer(a.lo / b.hi, a.hi / b.lo),
                    InfixOp::Mul => TypeInfo::integer(a.lo * b.lo, a.hi * b.hi),

                    // these cases SHOULD have been covered above
                    InfixOp::CmpNotEqual => unreachable!(),
                    InfixOp::CmpEqual => unreachable!(),
                })
            }

            _ => Err(TypeError::BadExpression {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                op,
            }),
        }
    }

    pub fn eval_expr(&mut self, expr: ast::Expr) -> ast::Expr<TypeTreeXData> {
        let lhs = self.eval_ast(*expr.lhs);
        let rhs = self.eval_ast(*expr.rhs);
        let lhs_ty = lhs.xdata();
        let rhs_ty = lhs.xdata();

        let val_ty_tuple = match (&lhs_ty.value_type, &rhs_ty.value_type) {
            (Some(a), Some(b)) => Some((a, b)),
            _ => None,
        };
        let decl =
            Self::apply_infix_op_on_type(expr.op, &lhs_ty.declared_type, &rhs_ty.declared_type);
        let value_type = val_ty_tuple
            .map(|(lhs, rhs)| Self::apply_infix_op_on_type(expr.op, &lhs, &rhs).ok())
            .flatten();

        ast::Expr {
            span: expr.span,
            xdata: TypeTreeXData {
                error: decl.clone().err(),
                declared_type: decl.unwrap_or(TypeInfo::Unit),
                value_type,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: expr.op,
        }
    }
}
