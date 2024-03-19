use std::collections::HashMap;

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

impl Default for TypeTreeXData {
    fn default() -> TypeTreeXData {
        TypeTreeXData {
            declared_type: TypeInfo::Unit,
            value_type: None,
            error: None,
        }
    }
}

pub type TypeTree = Ast<TypeTreeXData>;

#[derive(Default, Clone, Debug)]
pub struct TypeInterpreter {
    declarations: Declarations,
    scopes: Vec<TypeScope>,
}

#[derive(Default, Clone, Debug)]

pub struct TypeScope {
    pub variables: HashMap<String, TypeTreeXData>,
}

impl TypeInterpreter {
    pub fn new() -> TypeInterpreter {
        TypeInterpreter::default()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(TypeScope::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("no scope to pop");
    }

    // retrieve a copy of a variable's type data, or return an error and with declt type Unit if no such variable exists
    pub fn get_var(&self, name: &str) -> TypeTreeXData {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.variables.get(name) {
                return v.clone();
            }
        }

        TypeTreeXData {
            declared_type: TypeInfo::Unit,
            value_type: None,
            error: Some(TypeError::UnknownVariable {
                name: name.to_string(),
            }),
        }
    }

    pub fn eval_ast(&mut self, ast: Ast) -> TypeTree {
        match ast {
            Ast::LiteralInteger(i) => Ast::LiteralInteger(TypeInterpreter::eval_literal_integer(i)),
            Ast::LiteralBool(b) => Ast::LiteralBool(TypeInterpreter::eval_literal_bool(b)),
            Ast::Ident(i) => Ast::Ident(ast::Ident {
                span: i.span,
                xdata: self.get_var(&i.symbol),
                symbol: i.symbol,
            }),
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction(f) => Ast::DefFunction(self.eval_def_function(f)),
            Ast::Block(b) => Ast::Block(self.eval_block(b)),
            Ast::StmtIf(_) => todo!(),
            Ast::ExprCall(_) => todo!(),
            Ast::Expr(e) => Ast::Expr(self.eval_expr(e)),
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

    pub fn eval_def_function(&mut self, f: ast::DefFunction) -> ast::DefFunction<TypeTreeXData> {
        let body_type_tree = self.eval_ast(*f.body);
        let return_ty = TypeInfo::from_ast(&f.return_type);
        let error = if !body_type_tree.xdata().declared_type.is_subset(&return_ty) {
            Some(TypeError::BadFunctionReturnType {
                returned: body_type_tree.xdata().declared_type.clone(),
                expected: return_ty.clone(),
            })
        } else {
            None
        };
        ast::DefFunction {
            span: f.span,
            xdata: TypeTreeXData {
                declared_type: return_ty,
                error,
                value_type: None,
            },
            name: f.name,
            params: f.params,
            return_type: f.return_type,
            body: Box::new(body_type_tree),
        }
    }

    pub fn eval_block(&mut self, b: ast::Block) -> ast::Block<TypeTreeXData> {
        let statements: Vec<_> = b.statements.into_iter().map(|s| self.eval_ast(s)).collect();

        let xdata = if b.returns {
            statements
                .last()
                .map(|s| s.xdata().clone())
                .unwrap_or(Default::default())
        } else {
            Default::default()
        };

        ast::Block {
            span: b.span,
            xdata,
            returns: b.returns,
            statements,
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
            .and_then(|(lhs, rhs)| Self::apply_infix_op_on_type(expr.op, &lhs, &rhs).ok());

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
