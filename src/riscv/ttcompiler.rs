use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet, VecDeque},
    iter, result,
    sync::atomic::AtomicUsize,
};

use super::{asmgen::AssemblyWriter, compiler::Register};
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
    result: Slot,
    operations: String,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    Immediate(i16),
    Register(Register),

    // stack offset from the frame pointer
    Stack(usize),
}

pub type CompilerTree = Ast<CompilerXData>;

#[derive(Default, Clone, Debug)]
pub struct Compiler {
    scopes: Vec<Scope>,
    register_stack: VecDeque<Register>,
    stack: StackState,
}

#[derive(Default, Clone, Debug)]

pub struct StackState {
    bytes: Vec<bool>,
}

impl StackState {
    pub fn reset(&mut self) {
        self.bytes.clear();
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        let mut first_free_ind = None;
        let mut offset_size = None;
        for (i, &is_free) in self.bytes.iter().enumerate() {
            if is_free {
                if first_free_ind.is_none() {
                    first_free_ind = Some(i)
                }
            } else {
                if let Some(offset) = first_free_ind {
                    let cur_block_size = i - offset;
                    let block_fits_better = offset_size
                        .map(|(_, best_block_size)| cur_block_size < best_block_size)
                        .unwrap_or(true);
                    if cur_block_size >= size && block_fits_better {
                        offset_size = Some((i, cur_block_size))
                    }
                }
                first_free_ind = None;
            }
        }

        if let Some((offset, size)) = offset_size {
            offset
        } else {
            let offset = self.bytes.len();
            self.bytes.extend((0..size).map(|_| false));
            offset
        }
    }

    pub fn free(&mut self, offset: usize, size: usize) {
        for i in offset..offset + size {
            self.bytes[i] = true;
        }
    }

    fn size(&self) -> usize {
        self.bytes.len()
    }
}

#[derive(Default, Clone, Debug)]

pub struct Scope {
    pub variables: HashMap<String, Slot>,
}

#[derive(Default, Clone, Debug)]
pub struct EvalResult {
    pub result: Option<Slot>,
    pub buffer: AssemblyWriter,
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
            stack: StackState::default(),
        }
    }

    pub fn get_slot(&mut self, size: usize) -> Slot {
        if size <= 4 {
            if let Some(r) = self.register_stack.pop_front() {
                return Slot::Register(r);
            }
        }

        let offset = self.stack.alloc(size);
        Slot::Stack(offset)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("no scope to pop");
    }

    pub fn write_slot(&mut self, buffer: &mut AssemblyWriter, slot: Slot, value: &[u8]) {
        match slot {
            Slot::Immediate(_) => panic!("slot is read-only, this should be unreachable"),
            Slot::Register(r) => {
                assert!(value.len() <= 4);
                let mut padded_value: [u8; 4] = [0; 4];
                for (i, x) in value.iter().enumerate() {
                    padded_value[i] = *x;
                }
                buffer.li(r, u32::from_le_bytes(padded_value))
            }
            Slot::Stack(_offset) => {
                todo!("write_slot for ValueRef::Stack")
            }
        }
    }

    // save a register on the stack pass the returned ValueRef to load_register to restore it
    pub fn save_register(&mut self, buffer: &mut AssemblyWriter, reg: Register) -> Slot {
        // add space in the frame for the stored register
        let offset = self.stack.alloc(4);
        let save_ref = Slot::Stack(offset);
        assert!(offset < 2048);
        buffer.sw(reg, offset as i16, Register::Sp);
        save_ref
    }

    pub fn load_register(&mut self, buffer: &mut AssemblyWriter, dest: Register, slot: Slot) {
        match slot {
            Slot::Stack(offset) => {
                assert!(offset < 2048);
                buffer.lw(dest, offset as i16, Register::Sp);
            }
            Slot::Immediate(imm) => {
                buffer.addi(dest, Register::Zero, imm);
            }
            Slot::Register(src) => {
                buffer.mv(dest, src);
            }
        }
    }

    pub fn get_var_regs(&self) -> HashSet<Register> {
        self.scopes
            .iter()
            .flat_map(|x| {
                x.variables
                    .values()
                    .filter_map(|x| {
                        if let Slot::Register(r) = x {
                            Some(r)
                        } else {
                            None
                        }
                    })
                    .copied()
            })
            .collect()
    }

    pub fn get_register(&mut self, state: &AssemblyWriter, prefer: &[Register]) -> Register {
        let register_list = [
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
        ];
        let var_regs: HashSet<Register> = self.get_var_regs();

        let next_reg = prefer
            .iter()
            .chain(register_list.iter())
            .find(|r| {
                !var_regs.contains(r)
                    && !state.get_regs_write().contains(r)
                    && !state.get_regs_read().contains(r)
            })
            .unwrap_or_else(|| todo!("handle no free registers"));

        *next_reg
    }

    pub fn slot_to_register(
        &mut self,
        state: &mut AssemblyWriter,
        prefer: &[Register],
        slot: Slot,
    ) -> Register {
        if let Slot::Register(reg) = slot {
            reg
        } else {
            let next_reg = self.get_register(state, prefer);

            self.load_register(state, next_reg, slot);
            next_reg
        }
    }

    // retrieve a copy of a variable's type data, or return an error and with declt type Unit if no such variable exists
    pub fn get_var(&self, name: &str) -> Slot {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.variables.get(name) {
                return v.clone();
            }
        }

        panic!("variable not found, this likely indicates a bug in the compiler.\n All missing variables should be caught in the type checker")
    }

    pub fn free_slot(&mut self, slot: Slot) {
        if let Slot::Register(r) = slot {
            self.register_stack.push_back(r);
        }
    }

    pub fn set_var(&mut self, name: &str, data: Slot) {
        let top = self
            .scopes
            .last_mut()
            .expect("no scopes! (should be unreachable)");
        top.variables.insert(name.to_string(), data);
    }

    pub fn del_var(&mut self, name: &str) {
        let top = self
            .scopes
            .last_mut()
            .expect("no scopes! (should be unreachable)");
        top.variables.remove(name);
    }

    pub fn move_slot(&mut self, state: &mut AssemblyWriter, target: Slot, source: Slot) {
        match (target, source) {
            (Slot::Immediate(_), _) => panic!("cannot write to immediate slot"),
            (dest, Slot::Immediate(val)) => self.write_slot(state, dest, &val.to_le_bytes()),
            (Slot::Register(r_dest), source) => self.load_register(state, r_dest, source),
            (Slot::Stack(offset_dest), source) => {
                let r_src = self.slot_to_register(state, &[], source);
                assert!(offset_dest < 2048);
                state.sw(r_src, offset_dest as i16, Register::Fp);
            }
        }
    }

    pub fn eval_ast(&mut self, ast: &Ast<TypeTreeXData>) -> EvalResult {
        match ast {
            Ast::LiteralInteger(i) => self.eval_literal_integer(i),
            Ast::LiteralBool(b) => Compiler::eval_literal_bool(b),
            Ast::Ident(i) => EvalResult {
                result: Some(self.get_var(&i.symbol)),
                buffer: Default::default(),
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
                let mut buffer = AssemblyWriter::new();
                p.definitions.iter().for_each(|a| {
                    buffer.include_ref(&self.eval_ast(a).buffer);
                });
                EvalResult {
                    result: None,
                    buffer,
                }
            }
        }
    }

    pub fn eval_literal_integer(&mut self, i: &ast::LiteralInteger<TypeTreeXData>) -> EvalResult {
        let mut buffer = AssemblyWriter::default();
        if i.value.abs() < 2048 {
            EvalResult {
                result: Some(Slot::Immediate(i.value as i16)),
                buffer,
            }
        } else {
            let result = self.get_slot(4);
            self.write_slot(&mut buffer, result, &i.value.to_le_bytes());

            EvalResult {
                result: Some(result),
                buffer,
            }
        }
    }

    pub fn eval_literal_bool(i: &ast::LiteralBool<TypeTreeXData>) -> EvalResult {
        EvalResult {
            result: Some(Slot::Immediate(i.value as i16)),
            buffer: Default::default(),
        }
    }

    pub fn gen_label(&mut self) -> String {
        static LABEL_COUNTER: AtomicUsize = AtomicUsize::new(0);

        let label_num = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        format!(".L{}", label_num)
    }

    pub fn eval_expr(&mut self, expr: &ast::Expr<TypeTreeXData>) -> EvalResult {
        let lhs = self.eval_ast(expr.lhs.as_ref());
        // slot to make sure the left register isn't overwritten by the rhs
        // this is a big hack, it really needs to be rethought
        let hold_var_name = self.gen_label();
        self.set_var(&hold_var_name, lhs.result.unwrap());

        let rhs = self.eval_ast(expr.rhs.as_ref());

        self.del_var(&hold_var_name);

        let mut buffer = AssemblyWriter::from([lhs.buffer, rhs.buffer].into_iter());
        let lreg = self.slot_to_register(&mut buffer, &[], lhs.result.expect(""));
        let rreg = self.slot_to_register(&mut buffer, &[], rhs.result.expect(""));

        let res_reg = self.get_register(&buffer, &[lreg]);

        match expr.op {
            InfixOp::Add => buffer.add(res_reg, lreg, rreg),
            InfixOp::Sub => buffer.sub(res_reg, lreg, rreg),
            InfixOp::Div => buffer.div(res_reg, lreg, rreg),
            InfixOp::Mul => buffer.mul(res_reg, lreg, rreg),
            InfixOp::CmpNotEqual => {
                buffer.sub(res_reg, lreg, rreg);
                buffer.snez(res_reg, res_reg);
            }
            InfixOp::CmpEqual => {
                buffer.sub(res_reg, lreg, rreg);
                buffer.snez(res_reg, res_reg);
            }
            InfixOp::CmpLess => {
                buffer.slt(res_reg, lreg, rreg);
            }
        }

        EvalResult {
            result: Some(Slot::Register(res_reg)),
            buffer,
        }
    }

    fn eval_stmt_let(&mut self, l: &ast::StmtLet<TypeTreeXData>) -> EvalResult {
        let result = self.eval_ast(l.value.as_ref());
        self.set_var(&l.name, result.result.unwrap().clone());
        result
    }

    fn eval_stmt_if(&mut self, i: &ast::StmtIf<TypeTreeXData>) -> EvalResult {
        let res = self.eval_ast(i.condition.as_ref());

        let mut buffer = AssemblyWriter::new();
        let cond_reg = self.slot_to_register(&mut buffer, &[], res.result.unwrap());
        let end_label = self.gen_label();
        buffer.include_ref(&res.buffer);
        let result = if let Some(ast_else_) = &i.else_ {
            let if_result = self.get_slot(i.xdata().current_type().get_size());
            let false_label = self.gen_label();

            buffer.beq(cond_reg, Register::Zero, &false_label);

            let res_true = self.eval_ast(i.body.as_ref());
            buffer.include(res_true.buffer);
            self.move_slot(&mut buffer, if_result, res_true.result.unwrap());

            buffer.j(&end_label);
            buffer.label(&false_label);

            let res_false = self.eval_ast(ast_else_.as_ref());
            buffer.include(res_false.buffer);
            self.move_slot(&mut buffer, if_result, res_false.result.unwrap());

            if_result
        } else {
            buffer.beq(cond_reg, Register::Zero, &end_label);
            let res_true = self.eval_ast(i.body.as_ref());
            buffer.include(res_true.buffer);
            res_true.result.unwrap()
        };

        buffer.label(&end_label);

        EvalResult {
            result: Some(result),
            buffer,
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
        // FIXME: this is so ineficient.
        let var_regs = self.get_var_regs();
        let arg_reg_set = HashSet::from(arg_regs);
        let arg_regs_to_save: HashSet<_> = arg_reg_set.intersection(&var_regs).collect();
        let mut buffer = AssemblyWriter::new();

        // take here to help borrow checker

        let mut arg_evals_buffer = AssemblyWriter::new();
        let mut epilog_buffer = AssemblyWriter::new();
        let return_ty = c.xdata().declared_type.clone();
        for (arg_reg_i, arg) in c.paramaters.iter().enumerate() {
            let result = self.eval_ast(&arg);
            arg_evals_buffer.include(result.buffer);
            if let Some(slot) = result.result {
                self.load_register(&mut arg_evals_buffer, arg_regs[arg_reg_i], slot);
                if arg_regs_to_save.contains(&arg_regs[arg_reg_i]) {
                    let slot = self.save_register(&mut buffer, arg_regs[arg_reg_i]);
                    self.load_register(&mut epilog_buffer, arg_regs[arg_reg_i], slot);
                }
            }
        }

        buffer.include(arg_evals_buffer);
        buffer.call(&c.function_name);

        if return_ty != TypeInfo::Unit {
            let result = if arg_regs_to_save.contains(&Register::A0) {
                // TODO same for a1
                let result = self.get_slot(4);
                self.move_slot(&mut buffer, result, Slot::Register(Register::A0));
                result
            } else {
                Slot::Register(Register::A0)
            };
            buffer.include(epilog_buffer);

            EvalResult {
                result: Some(result),
                buffer,
            }
        } else {
            buffer.include(epilog_buffer);
            EvalResult {
                result: None,
                buffer,
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

        let mut buffer = AssemblyWriter::new();

        self.push_scope();
        buffer.d_globl(&f.name);
        buffer.label(&f.name);

        for (arg_reg_i, p) in f.params.iter().enumerate() {
            self.set_var(&p.name, Slot::Register(arg_regs[arg_reg_i]));
        }
        let res = self.eval_ast(f.body.as_ref());

        let mut prolog = AssemblyWriter::new();
        let mut epilog = AssemblyWriter::new();

        let mutated_callee_saved_regs = res
            .buffer
            .get_regs_write()
            .iter()
            .filter(|r| r.is_callee_saved())
            .chain(&[Register::Ra, Register::Fp])
            .copied();

        for r in mutated_callee_saved_regs {
            let offset = self.stack.alloc(4);

            prolog.sw(r, offset as i16, Register::Sp);
            epilog.lw(r, offset as i16, Register::Sp);
        }

        buffer.addi(Register::Sp, Register::Sp, -(self.stack.size() as i16));
        buffer.include(prolog);

        buffer.include(res.buffer);
        if let Some(out) = res.result {
            self.load_register(&mut buffer, Register::A0, out);
        }

        buffer.include(epilog);
        buffer.addi(Register::Sp, Register::Sp, self.stack.size() as i16);
        buffer.jr(Register::Ra);

        self.pop_scope();
        self.stack.reset();
        EvalResult {
            result: Some(Slot::Register(Register::A0)),
            buffer,
        }
    }

    fn eval_block(&mut self, b: &ast::Block<TypeTreeXData>) -> EvalResult {
        self.push_scope();
        let mut buffer = AssemblyWriter::new();
        let mut result = None;
        for s in &b.statements {
            let stmt_result = self.eval_ast(&s);
            buffer.include(stmt_result.buffer);
            result = stmt_result.result;
        }

        EvalResult { buffer, result }
    }
}
