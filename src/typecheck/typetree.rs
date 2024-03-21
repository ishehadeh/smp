use std::{collections::HashMap, default};

use crate::{
    ir::compiler::Declarations,
    parser::{
        ast::{self, InfixOp, XData},
        Ast,
    },
};

use super::{ScalarType, TypeError, TypeInfo};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub struct TypeTreeXData {
    pub declared_type: TypeInfo,
    pub value_type: Option<TypeInfo>,
    pub error: Option<TypeError>,
    pub cond_true: HashMap<String, TypeInfo>,
    pub cond_false: HashMap<String, TypeInfo>,
}

impl TypeTreeXData {
    pub fn new(declared_type: TypeInfo) -> TypeTreeXData {
        TypeTreeXData {
            declared_type,
            value_type: None,
            error: None,
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }

    // get the current value type or declared type if there is no value type
    pub fn current_type(&self) -> &TypeInfo {
        match &self.value_type {
            Some(a) => a,
            None => &self.declared_type,
        }
    }

    pub fn new_value(declared_type: TypeInfo, value: TypeInfo) -> TypeTreeXData {
        TypeTreeXData {
            declared_type,
            value_type: Some(value),
            error: None,
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }

    pub fn new_error(declared_type: TypeInfo, error: TypeError) -> TypeTreeXData {
        TypeTreeXData {
            declared_type,
            value_type: None,
            error: Some(error),
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }
}

impl Default for TypeTreeXData {
    fn default() -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::Unit)
    }
}

pub type TypeTree = Ast<TypeTreeXData>;

#[derive(Default, Clone, Debug)]
pub struct TypeInterpreter {
    _declarations: Declarations,
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

        TypeTreeXData::new_error(
            TypeInfo::Unit,
            TypeError::UnknownVariable {
                name: name.to_string(),
            },
        )
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
            Ast::Program(p) => Ast::Program(ast::Program {
                span: p.span,
                xdata: Default::default(),
                definitions: p
                    .definitions
                    .into_iter()
                    .map(|a| self.eval_ast(a))
                    .collect(),
            }),
        }
    }

    pub fn eval_literal_integer(i: ast::LiteralInteger) -> ast::LiteralInteger<TypeTreeXData> {
        let ty = TypeInfo::integer(i.value, i.value);

        ast::LiteralInteger {
            span: i.span,
            xdata: TypeTreeXData::new_value(ty.clone(), ty),
            value: i.value,
        }
    }

    pub fn eval_literal_bool(i: ast::LiteralBool) -> ast::LiteralBool<TypeTreeXData> {
        let ty = TypeInfo::bool_valued(i.value);
        ast::LiteralBool {
            span: i.span,
            xdata: TypeTreeXData::new_value(ty.clone(), ty),
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
                cond_false: Default::default(),
                cond_true: Default::default(),
            },
            name: f.name,
            params: f.params,
            return_type: f.return_type,
            body: Box::new(body_type_tree),
        }
    }

    pub fn eval_block(&mut self, b: ast::Block) -> ast::Block<TypeTreeXData> {
        self.push_scope();

        let statements: Vec<_> = b.statements.into_iter().map(|s| self.eval_ast(s)).collect();

        let xdata = if b.returns {
            statements
                .last()
                .map(|s| s.xdata().clone())
                .unwrap_or(Default::default())
        } else {
            Default::default()
        };

        self.pop_scope();
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
                if matches!(
                    op,
                    InfixOp::CmpEqual | InfixOp::CmpNotEqual | InfixOp::CmpLess
                ) =>
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
                    InfixOp::CmpLess => todo!(),
                })
            }

            _ => Err(TypeError::BadExpression {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                op,
            }),
        }
    }

    pub fn type_from_conditional(
        &mut self,
        op: InfixOp,
        lhs: &ast::Ast<TypeTreeXData>,
        rhs: &ast::Ast<TypeTreeXData>,
    ) -> (HashMap<String, TypeInfo>, HashMap<String, TypeInfo>) {
        let (ident, expr, is_expr_rhs) = match (lhs, rhs) {
            (lhs, TypeTree::Ident(ident)) => (ident, lhs, false),
            (TypeTree::Ident(ident), rhs) => (ident, rhs, true),
            _ => return Default::default(),
        };

        let expr_ty = expr.xdata().current_type();
        let ident_ty = ident.xdata().current_type();
        let (cond_true, cond_false, should_flip) = match op {
            InfixOp::Mul | InfixOp::Div | InfixOp::Add | InfixOp::Sub => return Default::default(),
            InfixOp::CmpEqual => (expr_ty.clone(), ident_ty.clone(), false),
            InfixOp::CmpNotEqual => (ident_ty.clone(), expr_ty.clone(), false),
            InfixOp::CmpLess => {
                if let TypeInfo::Scalar(ScalarType::Integer(x)) = expr_ty {
                    let less_ty = TypeInfo::integer(i32::MIN, x.hi - 1);
                    let ge_ty = TypeInfo::integer(x.lo, i32::MAX);
                    (
                        ident_ty.intersect(&less_ty),
                        ident_ty.intersect(&ge_ty),
                        true,
                    )
                } else {
                    return Default::default();
                }
            }
        };

        // output of the above match assumes the variable is on the left and expr is on the right
        // if that isn't the case (is_expr_rhs == false) and the result is inverted if the operatorands are switched
        // (should_flip) then flip invert the conditions
        let (cond_true, cond_false) = if should_flip && !is_expr_rhs {
            (cond_true, cond_false)
        } else {
            (cond_false, cond_true)
        };
        (
            HashMap::from([(ident.symbol.clone(), cond_true)]),
            HashMap::from([(ident.symbol.clone(), cond_false)]),
        )
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
            .and_then(|(lhs, rhs)| Self::apply_infix_op_on_type(expr.op, lhs, rhs).ok());

        let (cond_true, cond_false) = self.type_from_conditional(expr.op, &lhs, &rhs);

        ast::Expr {
            span: expr.span,
            xdata: TypeTreeXData {
                error: decl.clone().err(),
                declared_type: decl.unwrap_or(TypeInfo::Unit),
                value_type,
                cond_false,
                cond_true,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: expr.op,
        }
    }
}
