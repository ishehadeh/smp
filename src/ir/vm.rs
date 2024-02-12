use std::{collections::VecDeque, ops::Add};

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

impl From<usize> for Register {
    fn from(x: usize) -> Register {
        assert!(x < 32);

        use Register::*;

        #[rustfmt::skip]
        const LOOKUP: [Register; 31] = [
            Zero, Ra, Sp, Gp, Tp, T0, T1,
            T2, Fp, A0, A1, A2, A3, A4, A5,
            A6, A7, S2, S3, S4, S5, S6, S7,
            S8, S9, S10, S11, T3, T4, T5, T6,
        ];

        return *LOOKUP.get(x).unwrap();
    }
}

/// Intermediate representation
///
///

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    /// Push a number onto the stack
    PushI(u32),

    /// Push a register onto the stack
    Push(Register),

    /// Remove the top value from the stack and put it in a register
    Pop(Register),

    /// Pop the two top values of stack and add them, then push the result
    Add,

    /// Pop the two top values of stack and subtract the second value popped from the first, then push the result
    Sub,

    /// Pop the two top values of the stack and multiply them, then push the result
    Mul,

    /// Pop the top two values of the stack divide the second value popped by the first value popped, then push the result
    Div,

    /// Move an immediate value into a register
    LdI(Register, u32),

    /// Move the value stored at a memory address into a register.
    /// The address is equal to the value held in the register referenced second argument, and offset by the third argument
    Ld(Register, Register, i8),

    StI(u32, Register),
    St(Register, Register, i8),
}

#[derive(Debug, Clone)]
pub struct Vm {
    program: Vec<Op>,
    /// instruction pointer
    stack: VecDeque<u32>,
    memory: [u32; 1024],
    registers: [u32; 32],

    /// Instruction pointer
    ip: usize,
}

impl Vm {
    pub fn new<T: Into<Vec<Op>>>(program: T) -> Vm {
        let mut vm = Vm {
            program: program.into(),
            stack: VecDeque::new(),
            memory: [0; 1024],
            registers: [0; 32],

            ip: 0,
        };
        vm.reg_set(Register::Sp, vm.memory.len() as u32 - 1);
        vm
    }

    pub fn dbg_state(&self) {
        const STACK_VIEW_SIZE: u32 = 16;
        match self.program.get(self.ip - 1) {
            None => println!("OP <None>"),
            Some(op) => println!("OP {op:?}"),
        };

        println!("REGISTERS");
        for (reg_i, reg_val) in self.registers.iter().enumerate() {
            if reg_i % 6 == 0 {
                if reg_i > 0 {
                    println!()
                }
            } else {
                print!(" ");
            }

            print!("x{reg_i:02}={reg_val:04}");
        }
        println!();

        println!("STACK:");
        let start = self.reg_get(Register::Sp);
        for offset in 0..STACK_VIEW_SIZE {
            let mem_addr = (start + offset) as usize;
            if mem_addr >= self.memory.len() {
                break;
            }
            if offset % 4 == 0 {
                if offset > 0 {
                    println!();
                }

                print!("{mem_addr:04} |  ")
            }
            print!(" ");

            let val = self.memory[mem_addr];
            print!("{val:04}");
        }
        println!()
    }

    pub fn reg_set(&mut self, reg: Register, val: u32) {
        self.registers[reg as usize] = val;
    }

    pub fn reg_get(&self, reg: Register) -> u32 {
        return self.registers[reg as usize];
    }

    pub fn pop(&mut self) -> u32 {
        let sp = self.reg_get(Register::Sp);
        let val = self.memory[sp as usize];
        self.reg_set(Register::Sp, sp + 1);
        val
    }

    pub fn push(&mut self, val: u32) {
        let new_sp = self.reg_get(Register::Sp) - 1;
        self.reg_set(Register::Sp, new_sp);
        self.memory[new_sp as usize] = val;
    }

    /// Move to the next instruction
    /// Returns false if there are no instructions remaining
    pub fn step(&mut self) -> bool {
        let op = match self.program.get(self.ip) {
            None => return false,
            Some(op) => *op,
        };

        match op {
            // impl push/pop
            Op::PushI(val) => self.push(val),
            Op::Push(reg) => self.push(self.reg_get(reg)),
            Op::Pop(reg) => {
                let val = self.pop();
                self.reg_set(reg, val);
            }

            // impl load/store
            Op::LdI(reg, val) => self.reg_set(reg, val),
            Op::Ld(reg, ptr, offset) => self.reg_set(
                reg,
                self.memory[(self.reg_get(ptr) as isize + offset as isize) as usize],
            ),
            Op::StI(val, ptr) => self.memory[self.reg_get(ptr) as usize] = val,
            Op::St(val, ptr, offset) => {
                self.memory[(self.reg_get(ptr) as isize + offset as isize) as usize] =
                    self.reg_get(val)
            }

            // Math ops
            Op::Add => {
                let y = self.pop();
                let x = self.pop();
                self.push(x + y)
            }
            Op::Sub => {
                let y = self.pop();
                let x = self.pop();
                self.push(x - y)
            }
            Op::Mul => {
                let y = self.pop();
                let x = self.pop();
                self.push(x * y)
            }
            Op::Div => {
                let denom = self.pop();
                let num = self.pop();
                self.push(num / denom)
            }
        }

        self.ip += 1;
        true
    }
}
