use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::hash::Hash;

use crate::ir::frame::FrameData;
use crate::ir::values::VReg;
use crate::ir::IrOp;
use crate::typecheck::TypeInfo;
use crate::util::bidihashmap::BidiHashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn is_callee_saved(&self) -> bool {
        // TODO there are more callee registers I THINK
        matches!(
            self,
            Register::S1
                | Register::S2
                | Register::S3
                | Register::S4
                | Register::S5
                | Register::S6
                | Register::S7
        )
    }
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackAllocation {
    offset: usize,
    size: usize,
    vreg: VReg,
}

#[derive(Clone, Debug)]
pub struct FrameAllocations {
    pub register_allocations: BidiHashMap<VReg, Register>,

    pub stack: Vec<StackAllocation>,
}

impl FrameAllocations {
    pub fn new() -> FrameAllocations {
        FrameAllocations {
            stack: Vec::default(),
            register_allocations: BidiHashMap::new(),
        }
    }

    /// Required space to save registers which will be used in this frame
    pub fn saved_register_count(&self) -> usize {
        // add one because the frame pointer will always need to be saved
        self.register_allocations.forward.len()
    }

    /// Required space for variables stored exclusively on the stack
    pub fn stack_allocation_size(&self) -> usize {
        self.stack.iter().map(|a| a.size).sum()
    }

    pub fn stack_offset_of(&self, vreg: VReg) -> Option<usize> {
        self.stack
            .iter()
            .find(|alloc| alloc.vreg == vreg)
            .map(|alloc| alloc.offset)
    }
}

impl Default for FrameAllocations {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct RiscVCompiler {
    /// text section of the output asm
    text: String,

    temporaries: HashMap<VReg, Register>,

    aliases: HashMap<VReg, VReg>,
}

impl RiscVCompiler {
    pub fn new() -> RiscVCompiler {
        RiscVCompiler {
            text: String::new(),
            temporaries: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn allocate_registers(&self, frame: &FrameData) -> FrameAllocations {
        const PARAM_REGISTERS: [Register; 8] = [
            Register::A0,
            Register::A1,
            Register::A2,
            Register::A3,
            Register::A4,
            Register::A5,
            Register::A6,
            Register::A7,
        ];

        let mut alloc_queue: Vec<Register> = PARAM_REGISTERS.iter().rev().copied().collect();

        let mut allocs = FrameAllocations::new();
        for &vreg in frame.inputs.iter() {
            if frame.cell(vreg).typ.get_size() <= 4 {
                let reg = alloc_queue.pop().unwrap(); // TODO break instead of unwrapping
                allocs.register_allocations.insert(vreg, reg);
            } else {
                todo!("stack allocate, or double-up")
            }
        }
        for (vreg, _) in frame.registers.iter() {
            if allocs.register_allocations.forward().get(&vreg).is_some() {
                continue;
            }

            if frame.cell(vreg).typ.get_size() <= 4 {
                let reg = alloc_queue.pop().unwrap(); // TODO break instead of unwrapping
                allocs.register_allocations.insert(vreg, reg);
            } else {
                todo!("stack allocate, or double-up")
            }
        }

        // TODO stack allocations

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

    fn emit_load_register(&mut self, target: Register, src: Register, offset: i16) {
        assert!((-2048_i16..2048_i16).contains(&offset));

        writeln!(
            self.text,
            "lw {}, {}({})",
            target.to_abi_name(),
            offset,
            src.to_abi_name()
        )
        .expect("failed to emit instruction, format error")
    }

    fn emit_func_prelude(&mut self, allocs: &FrameAllocations) {
        let stack_alloc_size = allocs.stack_allocation_size();
        assert!(stack_alloc_size <= 2048); // TODO allow bigger frames, or fail with a hard error, since no function should use 2kb of stack space lmao.

        let frame_header_size = stack_alloc_size as i16
            + ((allocs
                .register_allocations
                .forward()
                .iter()
                .filter(|(_, reg)| reg.is_callee_saved())
                .count()
                + 1)
                * 4) as i16;

        self.emit_stack_shift(-frame_header_size);

        let mut store_offset = frame_header_size - 4;
        self.emit_store_register(Register::Fp, Register::Sp, store_offset);
        store_offset -= 4;

        for (_, &reg) in allocs
            .register_allocations
            .forward()
            .iter()
            .filter(|(_, reg)| reg.is_callee_saved())
        {
            self.emit_store_register(reg, Register::Sp, store_offset);
            store_offset -= 4;
        }
    }

    fn resolve_alias(&self, maybe_alias: VReg) -> VReg {
        if let Some(real) = self.aliases.get(&maybe_alias) {
            self.resolve_alias(*real)
        } else {
            maybe_alias
        }
    }

    fn emit_func_epilogue(&mut self, allocs: &FrameAllocations) {
        let stack_alloc_size = allocs.stack_allocation_size();
        assert!(stack_alloc_size <= 2048); // TODO: (again) allow bigger frames, see emit_func_prelude TODO

        // TODO lots of duplicate code with prolog, clean that up
        let frame_header_size = stack_alloc_size as i16
            + ((allocs
                .register_allocations
                .forward()
                .iter()
                .filter(|(_, reg)| reg.is_callee_saved())
                .count()
                + 1)
                * 4) as i16;

        let mut store_offset: i16 = frame_header_size - 4;

        self.emit_load_register(Register::Fp, Register::Sp, store_offset);
        store_offset -= 4;

        for (_, &reg) in allocs
            .register_allocations
            .forward()
            .iter()
            .filter(|(_, reg)| reg.is_callee_saved())
        {
            self.emit_load_register(reg, Register::Sp, store_offset);
            store_offset -= 4;
        }

        self.emit_stack_shift(frame_header_size);
        writeln!(self.text, "jr ra").expect("write failed");
    }

    pub fn compile_frame(&mut self, name: &str, frame: &FrameData) {
        writeln!(self.text, ".globl {}", name).unwrap();
        writeln!(self.text, "{}:", name).unwrap();
        let allocs = self.allocate_registers(frame);
        self.emit_func_prelude(&allocs);
        for op in &frame.operations {
            self.compile_op(&allocs, op.clone())
        }
        if frame.cell(frame.output).typ != TypeInfo::Unit {
            self.emit_load_vreg(&allocs, frame.output, &[Register::A0, Register::A1]);
        }
        self.emit_func_epilogue(&allocs);
    }

    pub fn get_free_temporaries(&mut self) -> HashSet<Register> {
        static TEMPORARY_REGISTER: [Register; 6] = [
            Register::T0,
            Register::T1,
            Register::T2,
            Register::T3,
            Register::T4,
            Register::T5,
        ];

        let mut free_temporaries = HashSet::from(TEMPORARY_REGISTER);
        for t in self.temporaries.values() {
            free_temporaries.remove(t);
        }

        free_temporaries
    }

    /// Allocate a temporary register
    pub fn take_temporary(&mut self, vreg: VReg) -> Register {
        // TODO: free a temporary if they're all in use
        let t = self
            .get_free_temporaries()
            .iter()
            .copied()
            .nth(0)
            .expect("No free temporaries");
        self.temporaries.insert(vreg, t);
        t
    }

    pub fn release_temporary(&mut self, r: VReg) {
        self.temporaries.remove(&r);
    }

    pub fn use_word(&mut self, vreg: VReg, alloc: &FrameAllocations) -> Register {
        let vreg_hw_register = alloc
            .register_allocations
            .forward()
            .get(&self.resolve_alias(vreg));

        if let Some(&vreg_hw_register) = vreg_hw_register {
            vreg_hw_register
        } else if let Some(stack_offset) = alloc.stack_offset_of(vreg) {
            assert!(stack_offset <= 2048);

            let t = self.take_temporary(vreg);
            self.emit_load_register(t, Register::Fp, -(stack_offset as i16));
            t
        } else {
            panic!(
                "could not find virtual register in stack frame register={:?}",
                vreg
            ); // TODO: error handling
        }
    }
    pub fn emit_arith_op(
        &mut self,
        allocs: &FrameAllocations,
        instr: &str,
        r: VReg,
        x: VReg,
        y: VReg,
    ) {
        let a_reg = self.use_word(x, allocs);
        let b_reg = self.use_word(y, allocs);
        let r_reg = self.use_word(r, allocs);

        writeln!(
            self.text,
            "{} {}, {}, {}",
            instr,
            r_reg.to_abi_name(),
            a_reg.to_abi_name(),
            b_reg.to_abi_name()
        )
        .expect("write failed");
        for r in [x, y] {
            self.release_temporary(r)
        }
    }
    pub fn compile_op(&mut self, allocs: &FrameAllocations, op: IrOp) {
        match op {
            IrOp::IAdd(r, x, y) => self.emit_arith_op(allocs, "add", r, x, y),
            IrOp::ISub(r, x, y) => self.emit_arith_op(allocs, "sub", r, x, y),
            IrOp::IMul(r, x, y) => self.emit_arith_op(allocs, "mul", r, x, y),
            IrOp::IDiv(r, x, y) => self.emit_arith_op(allocs, "div", r, x, y),

            IrOp::Call(_ret, name, params) => {
                let arg_registers = [
                    Register::A0,
                    Register::A1,
                    Register::A2,
                    Register::A3,
                    Register::A4,
                    Register::A5,
                    Register::A6,
                    Register::A7,
                ];
                let mut arg_reg_index = 0;
                for param in params {
                    arg_reg_index +=
                        self.emit_load_vreg(allocs, param, &arg_registers[arg_reg_index..])
                }
                writeln!(self.text, "jal zero,{}", name).expect("write failed");
            }
            IrOp::IStoreImm(l, val) => {
                let l_reg = self.use_word(l, allocs);
                writeln!(self.text, "li {}, {}", l_reg.to_abi_name(), val).expect("write failed");
            }
            IrOp::Eq(l, r) => {
                self.aliases.insert(l, r);
            }
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    fn emit_load_vreg(
        &mut self,
        frame: &FrameAllocations,
        vreg: VReg,
        registers: &[Register],
    ) -> usize {
        let vreg_phys = frame
            .register_allocations
            .forward()
            .get(&self.resolve_alias(vreg))
            .unwrap_or_else(|| todo!("load non-register allocated vregs"));
        writeln!(
            &mut self.text,
            "mv {}, {}",
            registers[0].to_abi_name(),
            vreg_phys.to_abi_name()
        )
        .unwrap();
        1
    }
}

impl Default for RiscVCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ir::{frame::FrameData, values::ValueCell, IrOp},
        riscv::compiler::Register,
        typecheck::TypeInfo,
        util::idvec::IdVec,
    };

    use super::RiscVCompiler;

    #[test]
    fn alloc_vregs_simple() {
        let mut registers = IdVec::new();
        let reg_cell = ValueCell {
            typ: TypeInfo::integer(0, 100),
        };

        let r = registers.push(reg_cell.clone());
        let a = registers.push(reg_cell.clone());
        let b = registers.push(reg_cell.clone());
        let c = registers.push(reg_cell);
        let frame = FrameData {
            registers,
            constants: Default::default(),
            inputs: vec![],
            output: r,
            operations: vec![IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)],
        };

        let compiler = RiscVCompiler::new();
        let allocs = compiler.allocate_registers(&frame);

        assert_eq!(
            allocs.register_allocations.reverse().get(&Register::A0),
            Some(&r)
        );
        assert_eq!(
            allocs.register_allocations.reverse().get(&Register::A1),
            Some(&a)
        );
        assert_eq!(
            allocs.register_allocations.reverse().get(&Register::A2),
            Some(&b)
        );
        assert_eq!(
            allocs.register_allocations.reverse().get(&Register::A3),
            Some(&c)
        );
    }

    #[test]
    fn emit_frame_setup_simple() {
        let mut registers = IdVec::new();
        let reg_cell = ValueCell {
            typ: TypeInfo::integer(0, 100),
        };

        let r = registers.push(reg_cell.clone());
        let a = registers.push(reg_cell.clone());
        let b = registers.push(reg_cell.clone());
        let c = registers.push(reg_cell);
        let frame = FrameData {
            registers,
            constants: Default::default(),
            inputs: vec![],
            output: r,
            operations: vec![IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)],
        };

        let mut compiler = RiscVCompiler::new();
        let allocs = compiler.allocate_registers(&frame);
        compiler.emit_func_prelude(&allocs);
        assert_eq!(
            compiler.text(),
            r"addi sp, sp, -20
sw fp, 16(sp)
sw a0, 12(sp)
sw a1, 8(sp)
sw a2, 4(sp)
sw a3, 0(sp)
"
        )
    }

    #[test]
    fn emit_add_op() {
        let mut registers = IdVec::new();
        let reg_cell = ValueCell {
            typ: TypeInfo::integer(0, 100),
        };

        let r = registers.push(reg_cell.clone());
        let a = registers.push(reg_cell.clone());
        let b = registers.push(reg_cell.clone());
        let c = registers.push(reg_cell);
        let frame = FrameData {
            registers,
            constants: Default::default(),
            inputs: vec![],
            output: r,
            operations: vec![IrOp::IAdd(r, a, b), IrOp::IAdd(r, c, r)],
        };
        let mut compiler = RiscVCompiler::new();
        compiler.compile_frame("test", &frame);
        assert_eq!(
            compiler.text(),
            r".globl test
test:
addi sp, sp, -20
sw fp, 16(sp)
sw a0, 12(sp)
sw a1, 8(sp)
sw a2, 4(sp)
sw a3, 0(sp)
add a0, a1, a2
add a0, a3, a0
lw fp, 16(sp)
lw a0, 12(sp)
lw a1, 8(sp)
lw a2, 4(sp)
lw a3, 0(sp)
addi sp, sp, 20
jr ra
"
        )
    }
}
