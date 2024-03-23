use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::atomic::AtomicUsize,
};

use super::compiler::Register;
use crate::{
    parser::{
        ast::{self, InfixOp, XData},
        Ast,
    },
    typecheck::typetree::TypeTreeXData,
};
use std::fmt::Write;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub struct CompilerXData {
    result: ValueRef,
    operations: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueRef {
    None,
    Immediate(i16),
    Register(Register),

    // stack offset from the frame pointer
    Stack(i16),
}

pub type CompilerTree = Ast<CompilerXData>;

#[derive(Default, Clone, Debug)]
pub struct Compiler {
    scopes: Vec<Scope>,
    register_stack: VecDeque<Register>,
    stack_offset: usize,
    /// TODO: turn this into a write at some point
    pub out: String,
}

#[derive(Default, Clone, Debug)]

pub struct Scope {
    pub variables: HashMap<String, ValueRef>,
}

#[derive(Default, Clone, Debug)]
pub struct EvalResult {
    pub result: Option<ValueRef>,
    pub registers: HashSet<Register>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            scopes: vec![Default::default()],
            register_stack: VecDeque::from([
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
            ]),
            stack_offset: 0,
            out: String::new(),
        }
    }

    pub fn text(&self) -> &str {
        &self.out
    }

    pub fn get_slot(&mut self, size: usize) -> ValueRef {
        if size <= 4 {
            if let Some(r) = self.register_stack.pop_front() {
                return ValueRef::Register(r);
            }
        }

        assert!(self.stack_offset < 2024);
        let val = ValueRef::Stack(-1 * self.stack_offset as i16);
        self.stack_offset += size;
        val
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("no scope to pop");
    }

    pub fn write_slot(out: &mut String, slot: ValueRef, value: &[u8]) {
        match slot {
            ValueRef::None => panic!("cannot write to null ref"),
            ValueRef::Immediate(_) => panic!("slot is read-only, this should be unreachable"),
            ValueRef::Register(r) => {
                assert_eq!(value.len(), 4);
                writeln!(
                    out,
                    "li {}, {}",
                    r.to_abi_name(),
                    u32::from_le_bytes(value.try_into().unwrap())
                )
                .expect("write failed");
            }
            ValueRef::Stack(_offset) => {
                todo!("write_slot for ValueRef::Stack")
            }
        }
    }

    // save a register on the stack pass the returned ValueRef to load_register to restore it
    pub fn save_register(&mut self, out: &mut String, reg: Register) -> ValueRef {
        // add space in the frame for the stored register
        let offset = -1 * self.stack_offset as i16;
        let save_ref = ValueRef::Stack(offset);
        self.stack_offset += 4;
        writeln!(out, "sw {}, ({})fp", reg.to_abi_name(), offset).expect("write failed");
        save_ref
    }

    pub fn load_register(&mut self, reg: Register, slot: ValueRef) {
        match slot {
            ValueRef::None => panic!("cannot load reg with null value"),
            ValueRef::Stack(offset) => {
                writeln!(&mut self.out, "lw {}, ({})fp", reg.to_abi_name(), offset)
                    .expect("write failed");
            }
            ValueRef::Immediate(imm) => {
                writeln!(&mut self.out, "li {}, {}", reg.to_abi_name(), imm).expect("write failed");
            }
            ValueRef::Register(other) => {
                writeln!(
                    &mut self.out,
                    "mv {}, {}",
                    reg.to_abi_name(),
                    other.to_abi_name()
                )
                .expect("write failed");
            }
        }
    }

    pub fn slot_to_register(&mut self, slot: ValueRef) -> Register {
        if let ValueRef::Register(reg) = slot {
            reg
        } else {
            let next_reg = self
                .register_stack
                .pop_front()
                .unwrap_or_else(|| todo!("handle no free registers"));

            self.load_register(next_reg, slot);
            next_reg
        }
    }

    // retrieve a copy of a variable's type data, or return an error and with declt type Unit if no such variable exists
    pub fn get_var(&self, name: &str) -> ValueRef {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.variables.get(name) {
                return v.clone();
            }
        }

        panic!("variable not found, this likely indicates a bug in the compiler.\n All missing variables should be caught in the type checker")
    }

    pub fn free_slot(&mut self, slot: ValueRef) {
        if let ValueRef::Register(r) = slot {
            self.register_stack.push_back(r);
        }
    }

    pub fn set_var(&mut self, name: &str, data: ValueRef) {
        let top = self
            .scopes
            .last_mut()
            .expect("no scopes! (should be unreachable)");
        top.variables.insert(name.to_string(), data);
    }

    pub fn eval_ast(&mut self, ast: Ast<TypeTreeXData>) -> EvalResult {
        match ast {
            Ast::LiteralInteger(i) => self.eval_literal_integer(i),
            Ast::LiteralBool(b) => Compiler::eval_literal_bool(b),
            Ast::Ident(i) => EvalResult {
                result: Some(self.get_var(&i.symbol)),
                registers: HashSet::new(),
            },
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction(f) => self.eval_def_function(f),
            Ast::Block(b) => self.eval_block(b),
            Ast::StmtIf(i) => self.eval_stmt_if(i),
            Ast::ExprCall(c) => self.eval_expr_call(c),
            Ast::Expr(e) => self.eval_expr(e),
            Ast::StmtLet(l) => self.eval_stmt_let(l),
            Ast::DefType(_) => todo!(),
            Ast::Program(p) => {
                p.definitions.into_iter().for_each(|a| {
                    self.eval_ast(a);
                });
                EvalResult {
                    result: None,
                    registers: HashSet::new(),
                }
            }
        }
    }

    pub fn eval_literal_integer(&mut self, i: ast::LiteralInteger<TypeTreeXData>) -> EvalResult {
        if i.value.abs() < 2048 {
            EvalResult {
                result: Some(ValueRef::Immediate(i.value as i16)),
                registers: HashSet::new(),
            }
        } else {
            let result = self.get_slot(4);
            let mut operations = String::new();
            Compiler::write_slot(&mut self.out, result, &i.value.to_le_bytes());

            EvalResult {
                result: Some(result),
                registers: HashSet::new(),
            }
        }
    }

    pub fn eval_literal_bool(i: ast::LiteralBool<TypeTreeXData>) -> EvalResult {
        EvalResult {
            result: Some(ValueRef::Immediate(i.value as i16)),
            registers: HashSet::new(),
        }
    }

    pub fn gen_label(&mut self) -> String {
        static LABEL_COUNTER: AtomicUsize = AtomicUsize::new(0);

        let label_num = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        format!(".L{}", label_num)
    }

    pub fn eval_expr(&mut self, expr: ast::Expr<TypeTreeXData>) -> EvalResult {
        let lhs = self.eval_ast(*expr.lhs);
        let rhs = self.eval_ast(*expr.rhs);

        let mut operations = String::new();

        let lreg = self.slot_to_register(lhs.result.expect(""));
        let rreg = self.slot_to_register(rhs.result.expect(""));
        let lreg_s = lreg.to_abi_name();
        let rreg_s: &str = rreg.to_abi_name();
        let mut arith = |name: &str| {
            writeln!(&mut self.out, "{} {}, {}, {}", name, lreg_s, lreg_s, rreg_s)
                .expect("write failed")
        };

        match expr.op {
            InfixOp::Add => arith("add"),
            InfixOp::Sub => arith("sub"),
            InfixOp::Div => arith("div"),
            InfixOp::Mul => arith("mul"),
            InfixOp::CmpNotEqual => {
                arith("sub");
                writeln!(&mut self.out, "snez {}, {}", lreg_s, lreg_s).expect("write_failed");
            }
            InfixOp::CmpEqual => {
                arith("sub");
                writeln!(&mut self.out, "seqz {}, {}", lreg_s, lreg_s).expect("write_failed");
            }
            InfixOp::CmpLess => {
                writeln!(&mut self.out, "slt {}, {}, {}", lreg_s, lreg_s, rreg_s)
                    .expect("write_failed");
            }
        }
        self.register_stack.push_front(rreg);

        EvalResult {
            result: Some(ValueRef::Register(lreg)),
            registers: HashSet::new(),
        }
    }

    fn eval_stmt_let(&mut self, l: ast::StmtLet<TypeTreeXData>) -> EvalResult {
        todo!()
    }

    fn eval_stmt_if(&mut self, i: ast::StmtIf<TypeTreeXData>) -> EvalResult {
        todo!()
    }

    fn eval_expr_call(&mut self, c: ast::ExprCall<TypeTreeXData>) -> EvalResult {
        todo!()
    }

    fn eval_def_function(&mut self, f: ast::DefFunction<TypeTreeXData>) -> EvalResult {
        let arg_regs = [
            Register::A0,
            Register::A1,
            Register::A2,
            Register::A3,
            Register::A4,
            Register::A5,
            Register::A6,
            Register::A7,
        ];
        self.push_scope();
        writeln!(&mut self.out, "{}:", f.name);
        for (arg_reg_i, p) in f.params.iter().enumerate() {
            self.set_var(&p.name, ValueRef::Register(arg_regs[arg_reg_i]));
        }
        self.eval_ast(*f.body);
        self.pop_scope();
        EvalResult {
            result: None,
            registers: HashSet::new(),
        }
    }

    fn eval_block(&mut self, b: ast::Block<TypeTreeXData>) -> EvalResult {
        self.push_scope();

        b.statements
            .into_iter()
            .map(|s| self.eval_ast(s))
            .last()
            .clone()
            .unwrap_or(EvalResult {
                result: None,
                registers: HashSet::new(),
            })
    }
}
