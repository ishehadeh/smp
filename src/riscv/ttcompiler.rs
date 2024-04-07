use std::{
    collections::{HashSet, VecDeque},
    sync::atomic::AtomicUsize,
};

use thiserror::Error;

use super::{asmgen::AssemblyWriter, scope::ScopeManager, Register, Slot, Value, ValueMap};
use crate::{
    parser::{
        ast::{self, InfixOp, XData},
        Ast,
    },
    typecheck::{types::RecordType, typetree::TypeTreeXData, TypeInfo},
};

#[derive(Error, Debug, Clone)]
pub enum CompileError {
    #[error("incompatible value types: (dest = {:?}, source = {:?})", dest, src)]
    IncompatibleValueType { dest: Value, src: Value },

    #[error(
        "incompatible value map structure: (dest = {:?}, source = {:?})",
        dest,
        src
    )]
    IncompatibleValueMapStructure { dest: Value, src: Value },
}

#[derive(Default, Clone, Debug)]
pub struct Compiler {
    scopes: ScopeManager,
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

    /// Allocate `size` bytes on the stack and return the offset from the bottom of the stack (in bytes)
    /// Note that if this address should be subtracted from the frame pointer, since the RISC-V stack grows down
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
            self.bytes
                .iter_mut()
                .skip(offset)
                .take(size)
                .for_each(|free| *free = false);
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
pub struct EvalResult {
    pub result: Option<Value>,
    pub buffer: AssemblyWriter,
}

const TEMPORARY_REGISTERS: [Register; 4] = [Register::T0, Register::T1, Register::T2, Register::T3];
impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            scopes: Default::default(),
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

    pub fn copy_value(
        &mut self,
        state: &mut AssemblyWriter,
        dest: &Value,
        src: &Value,
    ) -> Result<(), CompileError> {
        match (dest, src) {
            (Value::Slot(dest), Value::Slot(src)) => {
                self.copy_slot(state, dest, src);
                Ok(())
            }
            (Value::Map(dest_map), Value::Map(src_map)) => {
                for (key, src_field) in src_map.values.iter() {
                    let dest_field = dest_map.values.get(key).ok_or_else(|| {
                        CompileError::IncompatibleValueMapStructure {
                            dest: dest.clone(),
                            src: src.clone(),
                        }
                    })?;
                    self.copy_value(state, dest_field, src_field)?;
                }
                Ok(())
            }
            _ => Err(CompileError::IncompatibleValueType {
                dest: dest.clone(),
                src: src.clone(),
            }),
        }
    }

    pub fn get_slot(&mut self, size: usize) -> Slot {
        if size <= 4 {
            if let Some(r) = self.register_stack.pop_front() {
                return Slot::Register(r);
            }
        }

        let offset = self.stack.alloc(size);
        Slot::Indirect {
            offset,
            base: Box::new(Slot::Register(Register::Sp)),
        }
    }

    pub fn write_slot(&mut self, buffer: &mut AssemblyWriter, slot: &Slot, value: &[u8]) {
        match slot {
            Slot::Immediate(_) => panic!("slot is read-only, this should be unreachable"),
            &Slot::Register(r) => {
                assert!(value.len() <= 4);
                let mut padded_value: [u8; 4] = [0; 4];
                for (i, x) in value.iter().enumerate() {
                    padded_value[i] = *x;
                }
                buffer.li(r, u32::from_le_bytes(padded_value))
            }
            Slot::Indirect { base, offset } => {
                assert!(value.len() <= 4);

                let base_reg = self.slot_to_register(buffer, &TEMPORARY_REGISTERS, base.as_ref());
                let r = self.get_register(buffer, &TEMPORARY_REGISTERS);
                for (i, word) in value.chunks(4).enumerate() {
                    let word_offset = i * 4 + offset;
                    if word.len() == 4 {
                        let val: [u8; 4] = word.try_into().unwrap();
                        buffer.li(r, u32::from_le_bytes(val));
                        buffer.sb(r, word_offset.try_into().unwrap(), base_reg);
                    } else {
                        for (j, byte) in word.iter().enumerate() {
                            buffer.li(r, *byte as u32);
                            buffer.sb(r, (word_offset + j).try_into().unwrap(), base_reg);
                        }
                    }
                }
            }
        }
    }

    // save a register on the stack pass the returned ValueRef to load_register to restore it
    pub fn save_register(&mut self, buffer: &mut AssemblyWriter, reg: Register) -> Slot {
        // add space in the frame for the stored register
        const REGISTER_WIDTH: usize = 4;

        let offset = self.stack.alloc(REGISTER_WIDTH);
        let save_ref = Slot::Indirect {
            offset,
            base: Box::new(Slot::Register(Register::Sp)),
        };
        assert!(offset < 2048);
        buffer.sw(reg, offset as i16, Register::Sp);
        save_ref
    }

    pub fn load_register(&mut self, buffer: &mut AssemblyWriter, dest: Register, slot: &Slot) {
        match slot {
            Slot::Indirect { offset, base } => {
                assert!(*offset < 2048);
                let base_reg = self.slot_to_register(buffer, &TEMPORARY_REGISTERS, base.as_ref());
                buffer.lw(dest, *offset as i16, base_reg);
            }
            &Slot::Immediate(imm) => {
                buffer.addi(dest, Register::Zero, imm);
            }
            &Slot::Register(src) => {
                buffer.mv(dest, src);
            }
        }
    }

    pub fn get_var_regs(&self) -> HashSet<Register> {
        self.scopes
            .all_values()
            .flat_map(|v| v.slots())
            .filter_map(|x| {
                if let Slot::Register(r) = x {
                    Some(r)
                } else {
                    None
                }
            })
            .copied()
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
        slot: &Slot,
    ) -> Register {
        if let &Slot::Register(reg) = slot {
            reg
        } else {
            let next_reg = self.get_register(state, prefer);

            self.load_register(state, next_reg, &slot);
            next_reg
        }
    }

    pub fn free_slot(&mut self, slot: Slot) {
        if let Slot::Register(r) = slot {
            self.register_stack.push_back(r);
        }
    }

    pub fn copy_slot(&mut self, state: &mut AssemblyWriter, target: &Slot, source: &Slot) {
        match (target, source) {
            (Slot::Immediate(_), _) => panic!("cannot write to immediate slot"),
            (dest, Slot::Immediate(val)) => self.write_slot(state, dest, &val.to_le_bytes()),
            (Slot::Register(r_dest), source) => self.load_register(state, *r_dest, source),
            (
                Slot::Indirect {
                    offset: offset_dest,
                    base: base_dest,
                },
                source,
            ) => {
                let r_src = self.slot_to_register(state, &TEMPORARY_REGISTERS, source);
                let r_dest_base =
                    self.slot_to_register(state, &TEMPORARY_REGISTERS, base_dest.as_ref());
                assert!(*offset_dest < 2048);
                state.sw(r_src, *offset_dest as i16, r_dest_base);
            }
        }
    }

    pub fn eval_ast(&mut self, ast: &Ast<TypeTreeXData>) -> EvalResult {
        match ast {
            Ast::LiteralInteger(i) => self.eval_literal_integer(i),
            Ast::LiteralBool(b) => Compiler::eval_literal_bool(b),
            Ast::Ident(i) => EvalResult {
                result: Some(self.scopes.must_get(&i.symbol).clone()),
                buffer: Default::default(),
            },
            Ast::Repaired(_) => todo!(),
            Ast::DefFunction(f) => self.eval_def_function(f),
            Ast::Block(b) => self.eval_block(b),
            Ast::StmtIf(i) => self.eval_stmt_if(i),
            Ast::ExprCall(c) => self.eval_expr_call(c),
            Ast::Expr(e) => self.eval_expr(e),
            Ast::StmtLet(l) => self.eval_stmt_let(l),
            Ast::DefType(_) => EvalResult::default(), // handled when scanning decls
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
            Ast::FieldAccess(f) => {
                let mut object_res = self.eval_ast(&f.object);
                let field_val = object_res
                    .result
                    .expect("cannot use field access on empty result")
                    .as_map()
                    .expect("cannot use field access on slot")
                    .values
                    .get(&f.field.symbol)
                    .expect("field not found")
                    .clone();
                object_res.result = Some(field_val);
                object_res
            }
            Ast::StructLiteral(l) => self.eval_literal_struct(l),
        }
    }

    pub fn eval_literal_integer(&mut self, i: &ast::LiteralInteger<TypeTreeXData>) -> EvalResult {
        let mut buffer = AssemblyWriter::default();
        if i.value.abs() < 2048 {
            EvalResult {
                result: Some(Slot::Immediate(i.value as i16).into()),
                buffer,
            }
        } else {
            let result = self.get_slot(4);
            self.write_slot(&mut buffer, &result, &i.value.to_le_bytes());

            EvalResult {
                result: Some(result.into()),
                buffer,
            }
        }
    }

    pub fn eval_literal_bool(i: &ast::LiteralBool<TypeTreeXData>) -> EvalResult {
        EvalResult {
            result: Some(Slot::Immediate(i.value as i16).into()),
            buffer: Default::default(),
        }
    }

    pub fn eval_literal_struct(&mut self, l: &ast::StructLiteral<TypeTreeXData>) -> EvalResult {
        let mut buffer = AssemblyWriter::new();
        let struct_value_map =
            if let Value::Map(m) = self.type_info_to_value(&mut buffer, l.xdata().current_type()) {
                m
            } else {
                unreachable!();
            };
        for member in &l.members {
            let val_result = self.eval_ast(&member.value);
            let val_result_val = val_result.result.unwrap();
            let val_slot = val_result_val
                .as_slot()
                .expect("TODO: records within records");
            let struct_slot_val = struct_value_map.values.get(&member.field.symbol).unwrap();
            let struct_slot = struct_slot_val
                .as_slot()
                .expect("TODO: records within records");

            self.copy_slot(&mut buffer, struct_slot, val_slot);
        }

        EvalResult {
            result: Some(struct_value_map.into()),
            buffer,
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
        self.scopes.set(&hold_var_name, lhs.result.clone().unwrap());

        let rhs = self.eval_ast(expr.rhs.as_ref());

        self.scopes.unset(&hold_var_name);

        let mut buffer = AssemblyWriter::from([lhs.buffer, rhs.buffer].into_iter());
        let lhs_val = lhs.result.expect("lhs in expression had no result");
        let lhs_slot = lhs_val
            .as_slot()
            .expect("expected a word-sized scalar on expression lhs");
        let rhs_val = rhs.result.expect("rhs in expression had no result");
        let rhs_slot = rhs_val
            .as_slot()
            .expect("expected a word-sized scalar on expression rhs");
        let lreg = self.slot_to_register(&mut buffer, &[], lhs_slot);
        let rreg = self.slot_to_register(&mut buffer, &[], rhs_slot);

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
            InfixOp::Assign => todo!(),
        }

        EvalResult {
            result: Some(Slot::Register(res_reg).into()),
            buffer,
        }
    }

    fn type_info_to_value(&mut self, buffer: &mut AssemblyWriter, ty: &TypeInfo) -> Value {
        match ty {
            TypeInfo::Unit => todo!(),
            TypeInfo::Scalar(_) => self.get_slot(4).into(), // FIXME: doesn't work for float64, could also adjust size for ints
            TypeInfo::Union(_) => todo!(),
            TypeInfo::Record(s) => self.struct_to_value(buffer, s),
            TypeInfo::TyRef(_) => panic!("Reference type found during compilation, these should be resolved by the typechecker"),
        }
    }

    fn type_info_to_value_memory(
        &mut self,
        buffer: &mut AssemblyWriter,
        base: Slot,
        offset: usize,
        ty: &TypeInfo,
    ) -> Value {
        match ty {
            TypeInfo::Unit => todo!(),
            TypeInfo::Scalar(_) => Slot::Indirect {
                base: Box::new(base),
                offset,
            }
            .into(),
            TypeInfo::Union(_) => todo!(),
            TypeInfo::Record(s) => self.struct_to_value_memory(buffer, base, offset, s),         
           TypeInfo::TyRef(_) => panic!("Reference type found during compilation, these should be resolved by the typechecker"),
        }
    }

    fn struct_to_value(&mut self, buffer: &mut AssemblyWriter, s: &RecordType) -> Value {
        let mut val = ValueMap::default();
        for field in &s.fields {
            val.values.insert(
                field.name.clone(),
                self.type_info_to_value(buffer, &field.type_info),
            );
        }

        val.into()
    }

    fn struct_to_value_memory(
        &mut self,
        buffer: &mut AssemblyWriter,
        base: Slot,
        offset: usize,
        s: &RecordType,
    ) -> Value {
        let mut val = ValueMap::default();
        let mut field_offset = offset;
        for field in &s.fields {
            val.values.insert(
                field.name.clone(),
                self.type_info_to_value_memory(
                    buffer,
                    base.clone(),
                    field_offset,
                    &field.type_info,
                ),
            );
            field_offset += field.type_info.get_size()
        }

        val.into()
    }

    fn eval_stmt_let(&mut self, l: &ast::StmtLet<TypeTreeXData>) -> EvalResult {
        let result = self.eval_ast(l.value.as_ref());
        self.scopes.set(&l.name, result.result.clone().unwrap());
        result
    }

    fn eval_stmt_if(&mut self, i: &ast::StmtIf<TypeTreeXData>) -> EvalResult {
        let res = self.eval_ast(i.condition.as_ref());

        let mut buffer = AssemblyWriter::new();
        let cond_val = res.result.unwrap();
        let cond_slot = cond_val
            .as_slot()
            .clone()
            .expect("condition resulted in a value tuple, this should be unreachable");
        let cond_reg = self.slot_to_register(&mut buffer, &[], cond_slot);
        let end_label = self.gen_label();
        buffer.include_ref(&res.buffer);
        let result = if let Some(ast_else_) = &i.else_ {
            let if_result = self.get_slot(i.xdata().current_type().get_size());
            let false_label = self.gen_label();

            buffer.beq(cond_reg, Register::Zero, &false_label);

            let res_true = self.eval_ast(i.body.as_ref());
            let res_false = self.eval_ast(ast_else_.as_ref());

            buffer.include(res_true.buffer);
            self.copy_slot(
                &mut buffer,
                &if_result,
                res_true
                    .result
                    .unwrap()
                    .as_slot()
                    .expect("TODO: support tuple types as if results"),
            );

            buffer.j(&end_label);
            buffer.label(&false_label);

            buffer.include(res_false.buffer);
            self.copy_slot(
                &mut buffer,
                &if_result,
                res_false
                    .result
                    .unwrap()
                    .as_slot()
                    .expect("TODO: support tuple types as if results"),
            );

            if_result.into()
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

        let mut arg_evals_buffer = AssemblyWriter::new();
        let mut epilog_buffer = AssemblyWriter::new();
        let return_ty = c.xdata().declared_type.clone();

        let mut arg_reg_iter = arg_regs.iter().copied();
        if return_ty.get_size() > 8 || return_ty.is_record() {
            let arg_reg = arg_reg_iter.next().expect("ran out of argument registers");
            let return_value_offset = self.stack.alloc(return_ty.get_size());
            arg_evals_buffer.addi(arg_reg, Register::Sp, -(return_value_offset as i16));
            self.type_info_to_value_memory(
                &mut buffer,
                Slot::Register(Register::Sp),
                return_value_offset,
                &return_ty,
            )
        } else {
            // TODO: support dword return values
            Slot::Register(Register::A0).into()
        };

        for arg in c.paramaters.iter() {
            let result = self.eval_ast(&arg);
            let arg_reg = arg_reg_iter.next().expect("ran out of argument registers");

            arg_evals_buffer.include(result.buffer);
            if let Some(arg_val) = result.result {
                match arg_val {
                    Value::Map(_) => {
                        let arg_ty = arg.xdata().current_type();
                        let offset = self.stack.alloc(arg_ty.get_size());
                        // create a new layout for the variable in memory, in the allocated stack space
                        assert!(offset < 2048);
                        dbg!(&self.stack);
                        arg_evals_buffer.addi(arg_reg, Register::Sp, -(offset as i16));
                        let mem_arg_value = self.type_info_to_value_memory(
                            &mut arg_evals_buffer,
                            Slot::Register(arg_reg),
                            0,
                            arg_ty,
                        );
                        self.copy_value(&mut arg_evals_buffer, &mem_arg_value, &arg_val)
                            .unwrap();
                    }
                    Value::Slot(slot) => self.load_register(&mut arg_evals_buffer, arg_reg, &slot),
                }
                if arg_regs_to_save.contains(&arg_reg) {
                    let slot = self.save_register(&mut buffer, arg_reg);
                    self.load_register(&mut epilog_buffer, arg_reg, &slot);
                }
            }
        }

        buffer.include(arg_evals_buffer);
        buffer.call(&c.function_name);

        if return_ty != TypeInfo::Unit {
            let result = if arg_regs_to_save.contains(&Register::A0) {
                // TODO same for a1
                let result = self.get_slot(4);
                self.copy_slot(&mut buffer, &result, &Slot::Register(Register::A0));
                result.clone()
            } else {
                Slot::Register(Register::A0)
            };
            buffer.include(epilog_buffer);

            EvalResult {
                result: Some(result.into()),
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

        self.scopes.push();
        buffer.d_globl(&f.name);
        buffer.label(&f.name);

        for (arg_reg_i, p) in f.params.iter().enumerate() {
            let arg_reg_slot = Slot::Register(arg_regs[arg_reg_i]);
            let arg_ty = p.xdata().current_type();
            let value = match arg_ty {
                TypeInfo::Unit => continue,
                TypeInfo::Scalar(_) => arg_reg_slot.into(),
                TypeInfo::Union(_) => todo!("union function args"),
                TypeInfo::Record(_) => {
                    self.type_info_to_value_memory(&mut buffer, arg_reg_slot, 0, arg_ty)
                }
                TypeInfo::TyRef(_) => panic!("Reference type found during compilation, these should be resolved by the typechecker"),
            };
            self.scopes.set(&p.name, value);
        }
        let res = self.eval_ast(f.body.as_ref());

        let mut prolog = AssemblyWriter::new();
        let mut epilog = AssemblyWriter::new();

        let mut mutated_callee_saved_regs: Vec<_> = res
            .buffer
            .get_regs_write()
            .iter()
            .copied()
            .filter(|r| r.is_callee_saved())
            .collect();
        // this is literal just so we can get nice diffs between asm outputs of compiler versions
        // it should be optional or removed
        mutated_callee_saved_regs.sort();

        // TODO: only push these if a child function is called
        mutated_callee_saved_regs.push(Register::Ra);
        mutated_callee_saved_regs.push(Register::Fp);

        for r in mutated_callee_saved_regs {
            let offset = self.stack.alloc(4);

            prolog.sw(r, offset as i16, Register::Sp);
            epilog.lw(r, offset as i16, Register::Sp);
        }

        buffer.addi(Register::Sp, Register::Sp, -(self.stack.size() as i16));
        buffer.include(prolog);

        buffer.include(res.buffer);
        if let Some(Value::Slot(out_slot)) = res.result {
            self.load_register(&mut buffer, Register::A0, &out_slot);
        }

        buffer.include(epilog);
        buffer.addi(Register::Sp, Register::Sp, self.stack.size() as i16);
        buffer.jr(Register::Ra);

        self.scopes.pop();
        self.stack.reset();
        EvalResult {
            result: Some(Slot::Register(Register::A0).into()),
            buffer,
        }
    }

    fn eval_block(&mut self, b: &ast::Block<TypeTreeXData>) -> EvalResult {
        self.scopes.push();
        let mut buffer = AssemblyWriter::new();
        let mut result = None;
        for s in &b.statements {
            let stmt_result = self.eval_ast(&s);
            buffer.include(stmt_result.buffer);
            result = stmt_result.result;
        }
        self.scopes.pop();

        EvalResult { buffer, result }
    }
}
