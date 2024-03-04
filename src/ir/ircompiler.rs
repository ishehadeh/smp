use std::collections::BTreeSet;

use crate::{
    parser::{Ast, InfixOp},
    typecheck::{ScalarType, TypeInfo},
};

pub use super::environment::VReg;
use super::{
    compiler::CompileError,
    environment::{Environment, NamedType},
};

#[derive(Clone, Debug)]
pub enum IrOp {
    /// IAdd.0 = IAdd.1 + IAdd.2
    IAdd(VReg, VReg, VReg),

    /// ISub.0 = ISub.1 - ISub.2
    ISub(VReg, VReg, VReg),

    /// IDiv.0 = IDiv.1 / IDiv.2
    IDiv(VReg, VReg, VReg),

    /// IMul.0 = IMul.1 * IMul.2
    IMul(VReg, VReg, VReg),

    /// store an immediate integer in a virtual register
    IStoreImm(VReg, i32),

    /// Using params from Call.2 jump to the absolute offset in Call.1
    /// storing the return value in  Call.0
    Call(VReg, VReg, Vec<VReg>),
}

impl IrOp {
    pub fn registers(&self) -> BTreeSet<VReg> {
        match self {
            IrOp::IAdd(r, a, b)
            | IrOp::ISub(r, a, b)
            | IrOp::IDiv(r, a, b)
            | IrOp::IMul(r, a, b) => BTreeSet::from([*r, *a, *b]),
            IrOp::IStoreImm(r, _) => BTreeSet::from([*r]),
            IrOp::Call(r, f, params) => {
                BTreeSet::from_iter([r, f].into_iter().chain(params.iter()).cloned())
            }
        }
    }
}

#[derive(Debug)]
pub struct IrCompiler<'a> {
    pub environ: &'a mut Environment,
    pub ops: Vec<IrOp>,
}

impl<'a> IrCompiler<'a> {
    pub fn new(environ: &'a mut Environment) -> IrCompiler {
        IrCompiler {
            environ,
            ops: Vec::new(),
        }
    }

    pub fn compile_expr(&mut self, expr: &Ast) -> Result<VReg, CompileError> {
        match expr {
            Ast::Number(x) => Ok(self.add_store_integer_imm(*x)),
            Ast::Ident(varname) => Ok(self
                .environ
                .current_scope()
                .variables
                .get(varname)
                .expect("no such variable")
                .clone()), // TODO error checking
            Ast::Expr { lhs, op, rhs } => {
                let lhs_vreg = self.compile_expr(lhs)?;
                let rhs_vreg = self.compile_expr(rhs)?;
                self.add_op(*op, lhs_vreg, rhs_vreg)
            }

            Ast::Error => todo!(),
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction { .. } => todo!(),
            Ast::Block { .. } => todo!(),
            Ast::StmtIf { .. } => todo!(),
            Ast::ExprCall { .. } => todo!(),
            Ast::StmtLet { .. } => todo!(),
            Ast::DefType { .. } => todo!(),
            Ast::Program { .. } => todo!(),
        }
    }

    pub fn add_store_integer_imm(&mut self, value: i32) -> VReg {
        let r = self
            .environ
            .alloc_reg(TypeInfo::integer(value, value).into());
        self.ops.push(IrOp::IStoreImm(r, value));
        r
    }

    pub fn add_op(&mut self, op: InfixOp, lhs: VReg, rhs: VReg) -> Result<VReg, CompileError> {
        let ltyp = self.environ.get_type(lhs);
        let rtyp = self.environ.get_type(rhs);
        if !ltyp.is_subset(rtyp) || !rtyp.is_subset(ltyp) {
            return Err(CompileError::TypeError {
                left: ltyp.clone(),
                right: rtyp.clone(),
            });
        }

        // we know the left and right types are equivalent now
        let typ = ltyp;

        let result = if matches!(op, InfixOp::CmpEqual | InfixOp::CmpNotEqual) {
            self.environ
                .alloc_reg(NamedType::Value(TypeInfo::Scalar(ScalarType::Boolean)))
        } else {
            self.environ.alloc_reg(NamedType::Value(typ.clone()))
        };

        match op {
            InfixOp::Add => self.ops.push(IrOp::IAdd(result, lhs, rhs)),
            InfixOp::Sub => self.ops.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::Div => self.ops.push(IrOp::IDiv(result, rhs, lhs)),
            InfixOp::Mul => self.ops.push(IrOp::IMul(result, rhs, lhs)),
            InfixOp::CmpNotEqual => self.ops.push(IrOp::ISub(result, rhs, lhs)),
            InfixOp::CmpEqual => todo!(),
        }

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::IrCompiler;
    use crate::ir::ircompiler::IrOp;
    use crate::parser::lexer::Lexer;
    use crate::parser::InfixOp;
    use crate::parser::{grammar, Ast, ParseError};
    use crate::{ir::environment::Environment, typecheck::TypeInfo};

    #[cfg(test)]
    pub fn must_parse_expr(s: &str) -> Ast {
        let lexer = Lexer::new(s);
        let mut recovered_errors = Vec::new();
        let result = grammar::ExprParser::new().parse(&mut recovered_errors, lexer);

        let mut errors: Vec<_> = recovered_errors
            .into_iter()
            .map(|e| ParseError::from(e.error))
            .collect();
        match result {
            Ok(v) => return v,
            Err(e) => {
                errors.push(ParseError::from(e));
            }
        };

        panic!("parser encountered errors: {:?}", errors);
    }

    #[test]
    fn add_arith_op() {
        let mut environ = Environment::default();
        let l = environ.alloc_reg(TypeInfo::integer(0, 100).into());
        let r = environ.alloc_reg(TypeInfo::integer(0, 100).into());

        let mut irc = IrCompiler::new(&mut environ);

        let result = irc.add_op(InfixOp::Add, l, r).unwrap();
        let t = environ.get_type(result);
        assert_eq!(t.clone(), TypeInfo::integer(0, 100));
    }

    #[test]
    fn compile_simple_expr() {
        let mut environ = Environment::default();
        let mut irc = IrCompiler::new(&mut environ);

        irc.compile_expr(&must_parse_expr("1 + 2"))
            .expect("compile expr failed");
        assert!(matches!(
            &irc.ops[..],
            &[
                IrOp::IStoreImm(a, 1),
                IrOp::IStoreImm(b, 2),
                IrOp::IAdd(_, a_, b_)
            ] if a_ == a && b_ == b
        ));
    }
}
