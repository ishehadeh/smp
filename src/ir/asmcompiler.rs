use std::collections::BTreeSet;

use super::{
    environment::Environment,
    ircompiler::{IrOp, VReg},
};

#[derive(Default)]
pub struct RiscVCompiler {
    /// text section of the output asm
    text: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackAllocation {
    offset: usize,
    vreg: VReg,
}

#[derive(Clone, Debug, Default)]
pub struct FrameAllocations {
    /// Allocations of the x18-x27 registers (ABI: s2-s11), followed by the 6 temporary registers
    saved_registers: [Option<VReg>; 14],

    stack: Vec<VReg>,
}

impl RiscVCompiler {
    pub fn new() -> RiscVCompiler {
        RiscVCompiler::default()
    }

    pub fn alloc_vregs(&self, environ: &Environment, ops: &[IrOp]) -> FrameAllocations {
        // FIXME: this needs to be optimized *Really* badly
        let all_vregs = ops
            .iter()
            .map(|o| o.registers())
            .fold(BTreeSet::new(), |r, x| r.union(&x).cloned().collect());
        let mut allocs: FrameAllocations = FrameAllocations::default();
        let mut i = 0;
        for &vreg in all_vregs.iter() {
            // if possible put the value in the next available register
            if i < 14 && environ.get_type(vreg).get_size() <= 4 {
                allocs.saved_registers[i] = Some(vreg);
                i += 1;
            } else {
                // otherwise add it to the list of stack allocations
                allocs.stack.push(vreg);
            }
        }

        allocs
    }

    pub fn compile_frame(name: impl Into<String>, environ: &Environment, ops: &[IrOp]) {}
}

#[cfg(test)]
mod test {
    use crate::{
        ir::{
            environment::{Environment, NamedType},
            ircompiler::IrOp,
        },
        typecheck::TypeInfo,
    };

    use super::RiscVCompiler;

    #[test]
    fn alloc_vregs_simple() {
        let mut environ = Environment::new();
        let compiler = RiscVCompiler::new();
        let t: NamedType = TypeInfo::integer(0, 100).into();
        let r = environ.alloc_reg(t.clone());
        let a = environ.alloc_reg(t.clone());
        let b = environ.alloc_reg(t.clone());
        let c = environ.alloc_reg(t.clone());

        // r = a + b + c;
        let ops = [IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)];
        let allocs = compiler.alloc_vregs(&environ, &ops);
        assert_eq!(allocs.saved_registers[0], Some(r));
        assert_eq!(allocs.saved_registers[1], Some(c));
        assert_eq!(allocs.saved_registers[2], Some(b));
        assert_eq!(allocs.saved_registers[3], Some(a));
    }
}
