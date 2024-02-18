use super::vm::{Op, Register};
use crate::parser::AnonType;
use crate::parser::{Ast, InfixOp};
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
pub struct IntegerType {
    // TODO: allow for integers larger than a single pointer
    // FIXME: sync these field names with the AST integer range names
    pub lo: usize,
    pub hi: usize,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    IntegerType(IntegerType),
}

impl TypeInfo {
    pub fn get_size(&self) -> usize {
        match self {
            &TypeInfo::IntegerType(_) => 4,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub typ: String,
    pub stack_space: usize,
    pub offset: usize,
}

#[derive(Debug, Clone, Default)]
pub struct FrameInfo {
    variables: HashMap<String, VariableInfo>,
    operations: Vec<Op>,
}

impl FrameInfo {
    pub fn compile(&self) -> Vec<Op> {
        let space_needed = self.local_variable_size();
        let mut ops = Vec::new();
        // save frame
        ops.push(Op::Push(Register::Sp));
        ops.push(Op::Pop(Register::Fp));

        // shift stack above frame
        ops.push(Op::Push(Register::Sp));
        ops.push(Op::PushI(space_needed as u32 / 4));
        ops.push(Op::Sub);
        ops.push(Op::Pop(Register::Sp));
        ops.extend(&self.operations);
        ops
    }

    pub fn local_variable_size(&self) -> usize {
        return self.variables.values().map(|a| a.stack_space).sum();
    }
}

#[derive(Default, Clone, Debug)]
pub struct Compiler {
    // TODO: multiple scopes
    types: HashMap<String, TypeInfo>,
    stackframes: VecDeque<FrameInfo>,
    anon_type_counter: usize,
}

#[derive(Clone, Debug)]
pub enum CompileError {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler::default()
    }

    pub fn push_frame(&mut self) {
        self.stackframes.push_back(FrameInfo::default())
    }

    pub fn frame(&self) -> &FrameInfo {
        self.stackframes.back().expect("no stack frames!")
    }

    pub fn frame_mut(&mut self) -> &mut FrameInfo {
        self.stackframes.back_mut().expect("no stack frames!")
    }

    pub fn add_anon_type(&mut self, t: TypeInfo) -> String {
        let anon_type_name = format!("#anon-{}", self.anon_type_counter);
        self.anon_type_counter += 1;
        self.types.insert(anon_type_name.clone(), t);
        anon_type_name
    }

    pub fn alloc_local(&mut self, var_name: &String, typ_name: String) -> &VariableInfo {
        let typ = self.types.get(&typ_name).unwrap();
        let typ_size = typ.get_size();
        // TODO: don't hardcode word size
        // padding to fit into a word
        let size = typ_size + (typ_size % 4) / 4;
        let current_stack_offset: usize =
            self.frame().variables.values().map(|a| a.stack_space).sum();
        self.frame_mut().variables.insert(
            var_name.clone(),
            VariableInfo {
                typ: typ_name,
                stack_space: size,
                offset: current_stack_offset,
            },
        );

        self.frame().variables.get(var_name).unwrap()
    }

    pub fn ast_to_type_name(&mut self, node: &AnonType) -> String {
        match node {
            AnonType::TypeReference { name, parameters } => return name.clone(),
            AnonType::IntegerRange {
                inclusive_low,
                inclusive_high,
            } => self.add_anon_type(TypeInfo::IntegerType(IntegerType {
                lo: inclusive_low.parse().unwrap(),
                hi: inclusive_high.parse().unwrap(),
            })),
            AnonType::StructBody { members: _ } => todo!(),
        }
    }

    pub fn add_node(&mut self, node: &Ast) -> Result<(), CompileError> {
        match node {
            Ast::Ident(_) => unimplemented!(),
            Ast::Repaired(_) | Ast::Error => panic!("error in ast!"),
            Ast::Number(x) => self.frame_mut().operations.push(Op::PushI(*x)),
            Ast::Expr { lhs, op, rhs } => {
                let _ = self.add_node(lhs)?;
                let _ = self.add_node(rhs)?;
                let frame = self.frame_mut();
                match op {
                    InfixOp::Add => frame.operations.push(Op::Add),
                    InfixOp::Sub => frame.operations.push(Op::Sub),
                    InfixOp::Mul => frame.operations.push(Op::Mul),
                    InfixOp::Div => frame.operations.push(Op::Div),
                    InfixOp::CmpNotEqual => todo!(),
                    InfixOp::CmpEqual => todo!(),
                }
            }
            Ast::DefFunction {
                name: _,
                params: _,
                return_type: _,
                body: _,
            } => todo!(),
            Ast::DefType { name, typ } => match typ {
                AnonType::TypeReference { name, parameters } => todo!(),
                AnonType::StructBody { members } => todo!(),
                AnonType::IntegerRange {
                    inclusive_low,
                    inclusive_high,
                } => {
                    // TODO: type scopes
                    self.types.insert(
                        name.clone(),
                        TypeInfo::IntegerType(IntegerType {
                            lo: inclusive_low.parse().unwrap(),
                            hi: inclusive_high.parse().unwrap(), // FIXME: these should be parsed with the rest of the AST
                        }),
                    );
                }
            },
            Ast::Block {
                statements,
                returns: _,
            } => {
                // TODO implement returns
                for stmt in statements {
                    self.add_node(stmt)?;
                }
            }
            Ast::StmtLet {
                name,
                value_type,
                value,
            } => {
                let typ = self.ast_to_type_name(value_type);
                let &VariableInfo {
                    offset,
                    stack_space,
                    typ: _,
                } = self.alloc_local(name, typ);

                // compute = expr...
                self.add_node(value).unwrap();

                // copy resulting value from computation in stack frame
                let frame = self.stackframes.back_mut().expect("no stack frames!");
                for word in 0..(stack_space / 4) {
                    frame.operations.push(Op::Pop(Register::T1));
                    frame.operations.push(Op::St(
                        Register::T1,
                        Register::Fp,
                        -((offset / 4) as i8 + word as i8),
                    ));
                }
            }
            Ast::Program { definitions } => todo!(),
            Ast::StmtIf {
                condition,
                body,
                else_,
            } => todo!(),
            Ast::ExprCall {
                function_name,
                paramaters,
            } => todo!(),
        }

        Ok(())
    }

    pub fn program(&self) -> &[Op] {
        todo!("actually compile the program")
    }

    pub fn into_program(self) -> Vec<Op> {
        todo!("actually compile the program")
    }
}
