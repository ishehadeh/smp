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
    typecheck::{typetree::TypeTreeXData, TypeInfo},
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

    pub fn write_slot(&mut self, slot: ValueRef, value: &[u8]) {
        match slot {
            ValueRef::Immediate(_) => panic!("slot is read-only, this should be unreachable"),
            ValueRef::Register(r) => {
                assert!(value.len() <= 4);
                let mut padded_value: [u8; 4] = [0; 4];
                for (i, x) in value.iter().enumerate() {
                    padded_value[i] = *x;
                }
                writeln!(
                    &mut self.out,
                    "li {}, {}",
                    r.to_abi_name(),
                    u32::from_le_bytes(padded_value)
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
    pub fn get_register(&mut self) -> Register {
        let next_reg = self
            .register_stack
            .pop_front()
            .unwrap_or_else(|| todo!("handle no free registers"));

        next_reg
    }
    pub fn slot_to_register(&mut self, slot: ValueRef) -> Register {
        if let ValueRef::Register(reg) = slot {
            reg
        } else {
            let next_reg = self.get_register();

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

    pub fn move_slot(&mut self, target: ValueRef, source: ValueRef) {
        match (target, source) {
            (ValueRef::Immediate(_), _) => panic!("cannot write to immediate slot"),
            (dest, ValueRef::Immediate(val)) => self.write_slot(dest, &val.to_le_bytes()),
            (ValueRef::Register(r_dest), source) => self.load_register(r_dest, source),
            (ValueRef::Stack(offset_dest), source) => {
                let r_src = self.slot_to_register(source);
                writeln!(
                    &mut self.out,
                    "sw {}, ({})fp",
                    r_src.to_abi_name(),
                    offset_dest
                )
                .expect("write failed");
            }
        }
    }

    pub fn eval_ast(&mut self, ast: &Ast<TypeTreeXData>) -> EvalResult {
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
                p.definitions.iter().for_each(|a| {
                    self.eval_ast(a);
                });
                EvalResult {
                    result: None,
                    registers: HashSet::new(),
                }
            }
        }
    }

    pub fn eval_literal_integer(&mut self, i: &ast::LiteralInteger<TypeTreeXData>) -> EvalResult {
        if i.value.abs() < 2048 {
            EvalResult {
                result: Some(ValueRef::Immediate(i.value as i16)),
                registers: HashSet::new(),
            }
        } else {
            let result = self.get_slot(4);
            self.write_slot(result, &i.value.to_le_bytes());

            EvalResult {
                result: Some(result),
                registers: HashSet::new(),
            }
        }
    }

    pub fn eval_literal_bool(i: &ast::LiteralBool<TypeTreeXData>) -> EvalResult {
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

    pub fn eval_expr(&mut self, expr: &ast::Expr<TypeTreeXData>) -> EvalResult {
        let lhs = self.eval_ast(expr.lhs.as_ref());
        let rhs = self.eval_ast(expr.rhs.as_ref());

        let lreg = self.slot_to_register(lhs.result.expect(""));
        let rreg = self.slot_to_register(rhs.result.expect(""));
        let lreg_s = lreg.to_abi_name();
        let rreg_s: &str = rreg.to_abi_name();
        let res_reg = self.get_register();
        let res_reg_s = res_reg.to_abi_name();
        let mut arith = |name: &str| {
            writeln!(
                &mut self.out,
                "{} {}, {}, {}",
                name, res_reg_s, lreg_s, rreg_s
            )
            .expect("write failed")
        };

        match expr.op {
            InfixOp::Add => arith("add"),
            InfixOp::Sub => arith("sub"),
            InfixOp::Div => arith("div"),
            InfixOp::Mul => arith("mul"),
            InfixOp::CmpNotEqual => {
                arith("sub");
                writeln!(&mut self.out, "snez {}, {}", res_reg_s, lreg_s).expect("write_failed");
            }
            InfixOp::CmpEqual => {
                arith("sub");
                writeln!(&mut self.out, "seqz {}, {}", res_reg_s, lreg_s).expect("write_failed");
            }
            InfixOp::CmpLess => {
                writeln!(&mut self.out, "slt {}, {}, {}", res_reg_s, lreg_s, rreg_s)
                    .expect("write_failed");
            }
        }
        self.register_stack.push_front(rreg);

        EvalResult {
            result: Some(ValueRef::Register(res_reg)),
            registers: HashSet::new(),
        }
    }

    fn eval_stmt_let(&mut self, l: &ast::StmtLet<TypeTreeXData>) -> EvalResult {
        let result = self.eval_ast(l.value.as_ref());
        self.set_var(&l.name, result.result.unwrap().clone());
        result
    }

    fn eval_stmt_if(&mut self, i: &ast::StmtIf<TypeTreeXData>) -> EvalResult {
        let res = self.eval_ast(i.condition.as_ref());
        let cond_reg = self.slot_to_register(res.result.unwrap());
        let end_label = self.gen_label();
        let result = if let Some(ast_else_) = &i.else_ {
            let if_result = self.get_slot(i.xdata().current_type().get_size());
            let false_label = self.gen_label();

            writeln!(
                &mut self.out,
                "beq {}, zero, {}",
                cond_reg.to_abi_name(),
                false_label,
            )
            .expect("write failed");
            let res_true = self.eval_ast(i.body.as_ref());
            self.move_slot(if_result, res_true.result.unwrap());
            writeln!(&mut self.out, "j {}", end_label).expect("write failed");
            writeln!(&mut self.out, "{}:", false_label).expect("write failed");
            let res_false = self.eval_ast(ast_else_.as_ref());
            self.move_slot(if_result, res_false.result.unwrap());
            if_result
        } else {
            writeln!(
                &mut self.out,
                "beqz {}, zero, {}",
                cond_reg.to_abi_name(),
                end_label,
            )
            .expect("write failed");
            self.eval_ast(i.body.as_ref()).result.unwrap()
        };
        writeln!(&mut self.out, "{}:", end_label).expect("write failed");

        EvalResult {
            result: Some(result), // TODO,
            registers: HashSet::new(),
        }
    }

    fn eval_expr_call(&mut self, c: &ast::ExprCall<TypeTreeXData>) -> EvalResult {
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

        // take her to help borrow checker
        let return_ty = c.xdata().declared_type.clone();
        for (arg_reg_i, arg) in c.paramaters.iter().enumerate() {
            let result = self.eval_ast(&arg);
            self.load_register(arg_regs[arg_reg_i], result.result.unwrap());
        }
        writeln!(&mut self.out, "jal {}", &c.function_name).expect("write failed");
        if return_ty != TypeInfo::Unit {
            EvalResult {
                result: Some(ValueRef::Register(arg_regs[0])),
                registers: HashSet::new(),
            }
        } else {
            EvalResult {
                result: None,
                registers: HashSet::new(),
            }
        }
    }

    fn eval_def_function(&mut self, f: &ast::DefFunction<TypeTreeXData>) -> EvalResult {
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
        writeln!(&mut self.out, ".globl {}", f.name).expect("write failed");
        writeln!(&mut self.out, "{}:", f.name).expect("write failed");
        for (arg_reg_i, p) in f.params.iter().enumerate() {
            self.set_var(&p.name, ValueRef::Register(arg_regs[arg_reg_i]));
        }
        let res = self.eval_ast(f.body.as_ref());
        if let Some(out) = res.result {
            self.load_register(Register::A0, out);
        }
        writeln!(&mut self.out, "ret").expect("write failed");
        self.pop_scope();
        EvalResult {
            result: None,
            registers: HashSet::new(),
        }
    }

    fn eval_block(&mut self, b: &ast::Block<TypeTreeXData>) -> EvalResult {
        self.push_scope();

        let res = b
            .statements
            .iter()
            .map(|s| self.eval_ast(s))
            .last()
            .clone()
            .unwrap_or(EvalResult {
                result: None,
                registers: HashSet::new(),
            });
        self.pop_scope();
        res
    }
}
