use std::{collections::VecDeque, ops::Add};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    /// Instruction Pointer
    IP,
    /// stack pointer
    SP,
    R1,
    R2,
    R3,
    R4,
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
    registers: [u32; 6],
}

impl Vm {
    pub fn new<T: Into<Vec<Op>>>(program: T) -> Vm {
        let mut vm = Vm {
            program: program.into(),
            stack: VecDeque::new(),
            memory: [0; 1024],
            registers: [0; 6],
        };
        vm.reg_set(Register::SP, vm.memory.len() as u32 - 1);
        vm
    }

    pub fn dbg_state(&self) {
        const STACK_VIEW_SIZE: u32 = 16;
        match self.program.get(self.reg_get(Register::IP) as usize - 1) {
            None => println!("OP <None>"),
            Some(op) => println!("OP {op:?}"),
        };
        println!(
            "REG ip={} sp={} r1={} r2={} r3={} r4={}",
            self.reg_get(Register::IP),
            self.reg_get(Register::SP),
            self.reg_get(Register::R1),
            self.reg_get(Register::R2),
            self.reg_get(Register::R3),
            self.reg_get(Register::R4)
        );
        println!("STACK:");
        let start = self.reg_get(Register::SP);
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

    fn reg_index(reg: Register) -> usize {
        match reg {
            Register::IP => 0,
            Register::SP => 1,
            Register::R1 => 2,
            Register::R2 => 3,
            Register::R3 => 4,
            Register::R4 => 5,
        }
    }

    pub fn reg_set(&mut self, reg: Register, val: u32) {
        self.registers[Vm::reg_index(reg)] = val;
    }

    pub fn reg_get(&self, reg: Register) -> u32 {
        return self.registers[Vm::reg_index(reg)];
    }

    pub fn pop(&mut self) -> u32 {
        let sp = self.reg_get(Register::SP);
        let val = self.memory[sp as usize];
        self.reg_set(Register::SP, sp + 1);
        val
    }

    pub fn push(&mut self, val: u32) {
        let new_sp = self.reg_get(Register::SP) - 1;
        self.reg_set(Register::SP, new_sp);
        self.memory[new_sp as usize] = val;
    }

    /// Move to the next instruction
    /// Returns false if there are no instructions remaining
    pub fn step(&mut self) -> bool {
        let op = match self.program.get(self.reg_get(Register::IP) as usize) {
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

        self.reg_set(Register::IP, self.reg_get(Register::IP) + 1);
        true
    }
}
