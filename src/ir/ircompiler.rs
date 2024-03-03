use crate::{
    parser::InfixOp,
    typecheck::{ScalarType, TypeInfo},
};

pub use super::environment::VReg;
use super::{
    compiler::CompileError,
    environment::{Environment, NamedType},
};
pub enum IrOp {
    IAdd(VReg, VReg, VReg),
    ISub(VReg, VReg, VReg),
    IDiv(VReg, VReg, VReg),
    IMul(VReg, VReg, VReg),
}
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
    use crate::{ir::environment::Environment, typecheck::TypeInfo};

    use super::IrCompiler;
    use crate::parser::InfixOp;

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
}
