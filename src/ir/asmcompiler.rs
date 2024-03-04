use std::collections::BTreeSet;
use std::fmt::Write;

use super::{
    environment::Environment,
    ircompiler::{IrOp, VReg},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// source https://www.allaboutcircuits.com/technical-articles/introductions-to-risc-v-instruction-set-understanding-this-open-instruction-set-architecture/
pub enum Register {
    Zero = 0,

    /// Return address
    Ra,

    /// stack pointer
    Sp,

    /// Global Pointer
    Gp,

    /// Thread pointer
    Tp,

    // Temporaries
    /// Temporary/ alternate link register
    T0,
    T1,
    T2,

    /// Frame pointer
    Fp,

    /// Saved register
    S1,

    /// Return Value or Argument 1
    A0,

    /// Return or Argument 2
    A1,

    // args
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,

    ///Saved register
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,

    // Temporaries
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    pub fn to_abi_name(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::Ra => "ra",
            Register::Sp => "sp",
            Register::Gp => "gp",
            Register::Tp => "tp",
            Register::T0 => "t0",
            Register::T1 => "t1",
            Register::T2 => "t2",
            Register::Fp => "fp",
            Register::S1 => "s1",
            Register::A0 => "a0",
            Register::A1 => "a1",
            Register::A2 => "a2",
            Register::A3 => "a3",
            Register::A4 => "a4",
            Register::A5 => "a5",
            Register::A6 => "a6",
            Register::A7 => "a7",
            Register::S2 => "s2",
            Register::S3 => "s3",
            Register::S4 => "s4",
            Register::S5 => "s5",
            Register::S6 => "s6",
            Register::S7 => "s7",
            Register::S8 => "s8",
            Register::S9 => "s9",
            Register::S10 => "s10",
            Register::S11 => "s11",
            Register::T3 => "t3",
            Register::T4 => "t4",
            Register::T5 => "t5",
            Register::T6 => "t6",
        }
    }
}

#[derive(Debug)]
pub struct RiscVCompiler<'env> {
    /// text section of the output asm
    text: String,
    environ: &'env Environment,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackAllocation {
    offset: usize,
    vreg: VReg,
}

#[derive(Clone, Debug)]
pub struct FrameAllocations<'env> {
    /// Allocations of the x18-x27 registers (ABI: s1-s11), followed by the 6 temporary registers
    saved_registers: [Option<VReg>; 16],

    stack: Vec<VReg>,

    environ: &'env Environment,
}

impl<'env> FrameAllocations<'env> {
    pub fn new(environ: &'env Environment) -> FrameAllocations<'env> {
        FrameAllocations {
            saved_registers: [None; 16],
            stack: Vec::default(),
            environ,
        }
    }

    pub fn iter_saved_registers(&'_ self) -> impl Iterator<Item = (Register, VReg)> + '_ {
        static SAVED_REGISTER_MAP: [Register; 16] = [
            // S0 is frame pointer
            Register::S1,
            Register::S2,
            Register::S3,
            Register::S4,
            Register::S5,
            Register::S6,
            Register::S7,
            Register::S8,
            Register::S9,
            Register::S10,
            Register::S11,
            Register::T0,
            Register::T1,
            Register::T2,
            Register::T3,
            Register::T4,
        ];
        self.saved_registers
            .iter()
            .copied()
            .enumerate()
            .filter(|(_, vreg)| vreg.is_some())
            .map(|(i, vreg)| (SAVED_REGISTER_MAP[i], vreg.unwrap()))
    }

    /// Required space to save registers which will be used in this frame
    pub fn saved_register_count(&self) -> usize {
        // add one because the frame pointer will always need to be saved
        self.saved_registers.iter().filter(|r| r.is_some()).count() + 1
    }

    /// Required space for variables stored exclusively on the stack
    pub fn stack_allocation_size(&self) -> usize {
        self.stack
            .iter()
            .map(|a| self.environ.get_type(*a).get_size())
            .sum()
    }

    /// Total space needed to hold the frame info
    pub fn required_stack_space(&self) -> usize {
        self.saved_register_count() * 4 + self.stack_allocation_size()
    }
}

impl<'env> RiscVCompiler<'env> {
    pub fn new(environ: &'env Environment) -> RiscVCompiler {
        RiscVCompiler {
            environ,
            text: String::new(),
        }
    }

    pub fn alloc_vregs(&self, ops: &[IrOp]) -> FrameAllocations<'env> {
        // FIXME: this needs to be optimized *Really* badly
        let all_vregs = ops
            .iter()
            .map(|o| o.registers())
            .fold(BTreeSet::new(), |r, x| r.union(&x).cloned().collect());
        let mut allocs = FrameAllocations::new(self.environ);
        let mut i = 0;
        for &vreg in all_vregs.iter() {
            // if possible put the value in the next available register
            if i < 14 && self.environ.get_type(vreg).get_size() <= 4 {
                allocs.saved_registers[i] = Some(vreg);
                i += 1;
            } else {
                // otherwise add it to the list of stack allocations
                allocs.stack.push(vreg);
            }
        }

        allocs
    }

    fn emit_stack_shift(&mut self, offset: i16) {
        assert!((-2048_i16..2048_i16).contains(&offset));

        // TODO: should I truly handle this error? I don't *think* it can really fail
        writeln!(self.text, "addi sp, sp, {}", offset)
            .expect("failed to emit instruction, format error")
    }

    fn emit_store_register(&mut self, reg: Register, base: Register, offset: i16) {
        assert!((-2048_i16..2048_i16).contains(&offset));

        writeln!(
            self.text,
            "sw {}, {}({})",
            reg.to_abi_name(),
            offset,
            base.to_abi_name()
        )
        .expect("failed to emit instruction, format error")
    }

    fn emit_frame_setup(&mut self, allocs: &FrameAllocations<'env>) {
        let frame_header_size = allocs.required_stack_space();
        assert!(frame_header_size <= 2048); // TODO allow bigger frames, or fail with a hard error, since no function should use 2kb of stack space lmao.

        let frame_header_size = frame_header_size as i16;
        self.emit_stack_shift(-frame_header_size);

        let mut store_offset = frame_header_size - 4;
        self.emit_store_register(Register::Fp, Register::Sp, store_offset);
        store_offset -= 4;

        for (reg, _) in allocs.iter_saved_registers() {
            self.emit_store_register(reg, Register::Sp, store_offset);
            store_offset -= 4;
        }
    }

    pub fn compile_frame(&mut self, name: &str, ops: &[IrOp]) {
        self.text.push_str(name);
        self.text.push_str(":\n");
        let allocs = self.alloc_vregs(ops);
        self.emit_frame_setup(&allocs)
    }

    pub fn compile_op(&mut self, op: IrOp) {
        match op {
            IrOp::IAdd(r, a, b) => todo!(),
            IrOp::ISub(_, _, _) => todo!(),
            IrOp::IDiv(_, _, _) => todo!(),
            IrOp::IMul(_, _, _) => todo!(),
            IrOp::Call(_, _, _) => todo!(),
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }
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
        let t: NamedType = TypeInfo::integer(0, 100).into();
        let r = environ.alloc_reg(t.clone());
        let a = environ.alloc_reg(t.clone());
        let b = environ.alloc_reg(t.clone());
        let c = environ.alloc_reg(t.clone());

        let compiler = RiscVCompiler::new(&environ);

        // r = a + b + c;
        let ops = [IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)];

        let allocs = compiler.alloc_vregs(&ops);
        assert_eq!(allocs.saved_registers[0], Some(r));
        assert_eq!(allocs.saved_registers[1], Some(c));
        assert_eq!(allocs.saved_registers[2], Some(b));
        assert_eq!(allocs.saved_registers[3], Some(a));
    }

    #[test]
    fn emit_frame_setup_simple() {
        let mut environ = Environment::new();
        let t: NamedType = TypeInfo::integer(0, 100).into();
        let r = environ.alloc_reg(t.clone());
        let a = environ.alloc_reg(t.clone());
        let b = environ.alloc_reg(t.clone());
        let c = environ.alloc_reg(t.clone());

        let mut compiler = RiscVCompiler::new(&environ);

        // r = a + b + c;
        let ops = [IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)];

        compiler.compile_frame("test", &ops);
        assert_eq!(
            compiler.text(),
            r"test:
addi sp, sp, -20
sw fp, 16(sp)
sw s1, 12(sp)
sw s2, 8(sp)
sw s3, 4(sp)
sw s4, 0(sp)
"
        )
    }
}
