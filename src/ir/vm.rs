use std::collections::VecDeque;

/// Intermediate representation
///
///

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    /// Push a number onto the stack
    Push(i32),

    /// Remove the top value from the stack
    Pop,

    /// Pop the two top values of stack and add them, then push the result
    Add,

    /// Pop the two top values of stack and subtract the second value popped from the first, then push the result
    Sub,

    /// Pop the two top values of the stack and multiply them, then push the result
    Mul,

    /// Pop the top two values of the stack divide the second value popped by the first value popped, then push the result
    Div,

    /// Push the value at a specific memory location onto the stack
    Load(i32),

    /// Pop the stack and place it into a given memory location
    Store(i32),
}

#[derive(Debug, Clone)]
pub struct Vm {
    program: Vec<Op>,
    /// instruction pointer
    ip: i32,
    stack: VecDeque<i32>,
    memory: [i32; 1024],
}

impl Vm {
    pub fn new<T: Into<Vec<Op>>>(program: T) -> Vm {
        Vm {
            program: program.into(),
            ip: 0,
            stack: VecDeque::new(),
            memory: [0; 1024],
        }
    }

    pub fn pop(&mut self) -> i32 {
        self.stack.pop_front().expect("stack is empty!")
    }

    pub fn push(&mut self, val: i32) {
        self.stack.push_front(val)
    }

    /// Move to the next instruction
    /// Returns false if there are no instructions remaining
    pub fn step(&mut self) -> bool {
        let op = match self.program.get(self.ip as usize) {
            None => return false,
            Some(op) => *op,
        };

        match op {
            // guard against bad ptrs
            Op::Load(ptr) | Op::Store(ptr) if !(0..1024).contains(&ptr) => {
                panic!("Pointer out of range {}", ptr)
            }

            // impl push/pop
            Op::Push(val) => self.push(val),
            Op::Pop => {
                self.pop();
            }

            // impl load/store
            Op::Load(ptr) => self.push(self.memory[ptr as usize]),
            Op::Store(_) => {
                let x = self.pop();
                self.push(x)
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
