use std::collections::HashMap;

use crate::{
    parser::{ast::InfixOp, Ast},
    typecheck::{FunctionDeclaration, ScalarType, TypeInfo},
    util::idvec::IdVec,
};

use super::{
    values::{VReg, Value, ValueCell},
    CompileError, IrOp,
};

/// Intermediate representation of a stack frame
#[derive(Debug, Clone)]
pub struct FrameData {
    pub registers: IdVec<ValueCell>,

    /// Registers with their values known at compile time
    pub constants: HashMap<VReg, Value>,
    pub inputs: Vec<VReg>,
    pub output: VReg,
    pub operations: Vec<IrOp>,
}

#[derive(Default, Clone, Debug)]
pub struct Scope {
    pub variables: HashMap<String, VReg>,
}

impl FrameData {
    /// Get the value cell associated with a given register
    pub fn cell(&self, vreg: VReg) -> &ValueCell {
        self.registers.get(vreg)
    }
}

#[derive(Debug, Clone)]
pub struct FrameCompiler {
    frame: FrameData,
    scopes: Vec<Scope>,
    unit_register: VReg,
}

impl FrameCompiler {
    pub fn from_function_declaration(decl: &FunctionDeclaration) -> FrameCompiler {
        let mut registers = IdVec::default();
        let unit_register = registers.push(ValueCell {
            typ: TypeInfo::Unit,
        });
        let output = if decl.returns == TypeInfo::Unit {
            unit_register
        } else {
            registers.push(ValueCell {
                typ: decl.returns.clone(),
            })
        };

        let mut frame = FrameData {
            registers,
            output,
            constants: HashMap::new(),
            inputs: Vec::with_capacity(decl.paramaters.len()),
            operations: Vec::new(),
        };

        let mut top_scope = Scope::default();
        for (name, typ) in &decl.paramaters {
            let reg = frame.registers.push(ValueCell { typ: typ.clone() });
            top_scope.variables.insert(name.clone(), reg);
            frame.inputs.push(reg);
        }

        FrameCompiler {
            scopes: vec![top_scope],
            frame,
            unit_register,
        }
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("frame compiler has no scopes")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("frame compiler has no scopes")
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() != 1);

        self.scopes.pop();
    }

    pub fn unit(&self) -> VReg {
        self.unit_register
    }

    pub fn get_frame(&self) -> &FrameData {
        &self.frame
    }

    pub fn into_frame(self) -> FrameData {
        self.frame
    }

    pub fn compile_expr(&mut self, expr: &Ast) -> Result<VReg, CompileError> {
        // TODO maybe pass in the result reg, so we're not constantly allocating registers?
        match expr {
            Ast::LiteralInteger(x) => Ok(self.add_store_integer_imm(x.value)),
            Ast::LiteralBool(x) => Ok(self.add_store_bool_imm(x.value)),
            Ast::Ident(ident) => Ok(*self
                .current_scope()
                .variables
                .get(&ident.symbol)
                .expect("no such variable")), // TODO error checking
            Ast::Expr(expr) => {
                let lhs_vreg = self.compile_expr(&expr.lhs)?;
                let rhs_vreg = self.compile_expr(&expr.rhs)?;
                self.add_op(expr.op, lhs_vreg, rhs_vreg)
            }
            Ast::Repaired(r) => panic!("{:?}", r),
            Ast::DefFunction(_) => todo!(),
            Ast::Block(block) => {
                let mut last_result = None;
                for stmt in &block.statements {
                    last_result = Some(self.compile_expr(stmt)?);
                }
                if let Some(last_result) = last_result {
                    if block.returns {
                        return Ok(last_result);
                    }
                }
                Ok(self.unit())
            }
            Ast::StmtIf { .. } => todo!(),
            Ast::ExprCall(expr_call) => {
                // TODO create a real return value reg
                let r = self.unit();
                let mut param_vregs: Vec<VReg> = vec![];
                for param in &expr_call.paramaters {
                    let result = self.compile_expr(param)?;
                    param_vregs.push(result);
                }

                self.frame.operations.push(IrOp::Call(
                    r,
                    expr_call.function_name.clone(),
                    param_vregs,
                ));
                Ok(r)
            }
            Ast::StmtLet(stmt_let) => {
                let var_typ = TypeInfo::from_ast(&stmt_let.value_type);
                let value_reg = self.compile_expr(&stmt_let.value)?;
                let value_typ = &self.get_frame().cell(value_reg).typ;

                if !value_typ.is_subset(&var_typ) {
                    return Err(CompileError::TypeError {
                        left: var_typ,
                        right: value_typ.clone(),
                    });
                }
                self.current_scope_mut()
                    .variables
                    .insert(stmt_let.name.clone(), value_reg);
                Ok(value_reg)
            }
            Ast::DefType { .. } => todo!(),
            Ast::Program { .. } => todo!(),
        }
    }

    pub fn compile(&mut self, expr: &Ast) -> Result<(), CompileError> {
        let result = self.compile_expr(expr)?;

        // unify body result and return type.
        if !self
            .get_frame()
            .cell(result)
            .typ
            .is_subset(&self.get_frame().cell(self.frame.output).typ)
        {
            return Err(CompileError::TypeError {
                left: self.get_frame().cell(result).typ.clone(),
                right: self.get_frame().cell(self.frame.output).typ.clone(),
            });
        }

        // map frame result to output vreg.
        self.frame
            .operations
            .push(IrOp::Eq(self.frame.output, result));

        Ok(())
    }

    pub fn allocate_register(&mut self, typ: TypeInfo) -> VReg {
        self.frame.registers.push(ValueCell { typ })
    }

    pub fn add_store_integer_imm(&mut self, value: i32) -> VReg {
        let r = self.allocate_register(TypeInfo::integer(value, value));
        self.frame.operations.push(IrOp::IStoreImm(r, value));
        r
    }

    pub fn add_store_bool_imm(&mut self, value: bool) -> VReg {
        let r = self.allocate_register(TypeInfo::Scalar(ScalarType::Boolean));
        self.frame
            .operations
            .push(IrOp::IStoreImm(r, if value { 1 } else { 0 }));
        r
    }

    pub fn add_op(&mut self, op: InfixOp, lhs: VReg, rhs: VReg) -> Result<VReg, CompileError> {
        let ltyp = &self.frame.cell(lhs).typ;
        let rtyp = &self.frame.cell(rhs).typ;
        let result_type = match (ltyp, rtyp) {
            (TypeInfo::Scalar(lhs), TypeInfo::Scalar(rhs)) => match op {
                InfixOp::Add => lhs.add(rhs),
                InfixOp::Sub => lhs.sub(rhs),
                InfixOp::Mul => lhs.mul(rhs),
                InfixOp::Div => lhs.div(rhs),
                _ => todo!(),
            },
            _ => None,
        }
        .ok_or(CompileError::TypeError {
            left: ltyp.clone(),
            right: rtyp.clone(),
        })?;

        let result = self.allocate_register(TypeInfo::Scalar(result_type));

        match op {
            InfixOp::Add => self.frame.operations.push(IrOp::IAdd(result, lhs, rhs)),
            InfixOp::Sub => self.frame.operations.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::Div => self.frame.operations.push(IrOp::IDiv(result, rhs, lhs)),
            InfixOp::Mul => self.frame.operations.push(IrOp::IMul(result, rhs, lhs)),
            InfixOp::CmpNotEqual => self.frame.operations.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::CmpEqual => todo!(),
        }

        Ok(result)
    }
}
