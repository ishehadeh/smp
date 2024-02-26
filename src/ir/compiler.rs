use super::vm::{Cond, Op, Register};
use crate::parser::AnonType;
use crate::parser::{Ast, InfixOp};
use crate::typecheck::{ScalarType, TypeInfo};
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub typ: TypeInfo,
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
        let mut ops = vec![
            // save frame
            Op::Push(Register::Sp),
            Op::Pop(Register::Fp),
            // shift stack above frame
            Op::Push(Register::Sp),
            Op::PushI(space_needed as u32 / 4),
            Op::Sub,
            Op::Pop(Register::Sp),
        ];

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
pub enum CompileError {
    TypeError { left: TypeInfo, right: TypeInfo },
    ArithmeticIncompatible { left: TypeInfo, right: TypeInfo },
}

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

    pub fn alloc_local(&mut self, var_name: &String, typ: TypeInfo) -> &VariableInfo {
        let typ_size = typ.get_size();
        // TODO: don't hardcode word size
        // padding to fit into a word
        let size = typ_size + (typ_size % 4) / 4;
        let current_stack_offset: usize =
            self.frame().variables.values().map(|a| a.stack_space).sum();
        self.frame_mut().variables.insert(
            var_name.clone(),
            VariableInfo {
                typ,
                stack_space: size,
                offset: current_stack_offset,
            },
        );

        self.frame().variables.get(var_name).unwrap()
    }

    pub fn expr_type(&mut self, e: &Ast) -> Result<TypeInfo, CompileError> {
        match e {
            Ast::Ident(a) => Ok(self.frame().variables.get(a).unwrap().typ.clone()),
            Ast::Number(n) => Ok(TypeInfo::integer(*n as usize, *n as usize)),
            Ast::Expr { lhs, op, rhs } => {
                let ltype = self.expr_type(lhs)?;
                let rtype = self.expr_type(rhs)?;

                match (&ltype, &rtype) {
                    (TypeInfo::Scalar(s1), TypeInfo::Scalar(s2)) => match (s1, s2) {
                        (ScalarType::Float32, ScalarType::Float64)
                        | (ScalarType::Float64, ScalarType::Float32) => {
                            Ok(TypeInfo::Scalar(ScalarType::Float64))
                        }
                        (ScalarType::Integer(x), ScalarType::Integer(y)) => match op {
                            InfixOp::Add => Ok(TypeInfo::integer(x.lo + y.lo, x.hi + y.hi)),
                            InfixOp::Sub => Ok(TypeInfo::integer(x.lo - y.lo, x.hi - y.hi)),
                            InfixOp::Div => Ok(TypeInfo::integer(x.lo / y.lo, x.hi / y.hi)),
                            InfixOp::Mul => Ok(TypeInfo::integer(x.lo * y.lo, x.hi + y.hi)),
                            InfixOp::CmpNotEqual => todo!(),
                            InfixOp::CmpEqual => todo!(),
                        },
                        (ScalarType::Character, ScalarType::Character) => {
                            Ok(TypeInfo::Scalar(ScalarType::Character))
                        }
                        _ => Err(CompileError::ArithmeticIncompatible {
                            left: ltype,
                            right: rtype,
                        }),
                    },
                    _ => Err(CompileError::ArithmeticIncompatible {
                        left: ltype,
                        right: rtype,
                    }),
                }
            }
            _ => todo!(),
        }
    }

    pub fn add_node(&mut self, node: &Ast) -> Result<(), CompileError> {
        match node {
            Ast::Ident(varname) => {
                let frame: &mut FrameInfo = self.stackframes.back_mut().expect("no stack frames!");
                let var = frame.variables.get(varname).unwrap();
                for word in 0..(var.stack_space / 4) {
                    frame.operations.push(Op::Ld(
                        Register::T1,
                        Register::Fp,
                        -((var.offset / 4) as i8 + word as i8),
                    ));
                    frame.operations.push(Op::Push(Register::T1));
                }
            }
            Ast::Repaired(_) | Ast::Error => panic!("error in ast!"),
            Ast::Number(x) => self.frame_mut().operations.push(Op::PushI(*x)),
            Ast::Expr { lhs, op, rhs } => {
                self.add_node(lhs)?;
                self.add_node(rhs)?;
                let frame = self.frame_mut();
                match op {
                    InfixOp::Add => frame.operations.push(Op::Add),
                    InfixOp::Sub => frame.operations.push(Op::Sub),
                    InfixOp::Mul => frame.operations.push(Op::Mul),
                    InfixOp::Div => frame.operations.push(Op::Div),
                    InfixOp::CmpNotEqual => frame.operations.push(Op::Xor),
                    InfixOp::CmpEqual => frame.operations.push(Op::Sub),
                }
            }
            Ast::DefFunction {
                name: _,
                params: _,
                return_type: _,
                body: _,
            } => todo!(),
            Ast::DefType { name, typ } => match typ {
                AnonType::TypeReference {
                    name: _,
                    parameters: _,
                } => todo!(),
                AnonType::StructBody { members: _ } => todo!(),
                AnonType::IntegerRange {
                    inclusive_low,
                    inclusive_high,
                } => {
                    // TODO: type scopes
                    self.types.insert(
                        name.clone(),
                        TypeInfo::integer(
                            inclusive_low.parse().unwrap(),
                            inclusive_high.parse().unwrap(),
                        ),
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
                let typ = TypeInfo::from_ast(value_type);
                let val_type = self.expr_type(value)?;
                // TODO no intersect and unit are different things, intersect should return some kind of None or 0 type.
                if val_type.intersect(&typ) == TypeInfo::Unit {
                    return Err(CompileError::TypeError {
                        left: typ,
                        right: val_type,
                    });
                }

                let &VariableInfo {
                    offset,
                    stack_space,
                    typ: _,
                } = self.alloc_local(name, typ);

                // compute = expr...
                self.add_node(value).unwrap();

                // copy resulting value from computation in stack frame
                let frame: &mut FrameInfo = self.stackframes.back_mut().expect("no stack frames!");
                for word in 0..(stack_space / 4) {
                    frame.operations.push(Op::Pop(Register::T1));
                    frame.operations.push(Op::St(
                        Register::T1,
                        Register::Fp,
                        -((offset / 4) as i8 + word as i8),
                    ));
                }
            }
            Ast::Program { definitions: _ } => todo!(),
            Ast::StmtIf {
                condition,
                body,
                else_,
            } => {
                self.add_node(condition)?;

                let branch_index = {
                    let frame = self.stackframes.back_mut().expect("no stack frames!");
                    frame.operations.push(Op::Pop(Register::T1));
                    frame.operations.push(Op::LdI(Register::T2, 0));

                    // we'll set this later -- save the position
                    frame.operations.push(Op::Nop);
                    frame.operations.len() - 1
                };

                self.add_node(body)?;
                {
                    let frame = self.stackframes.back_mut().expect("no stack frames!");
                    let offset = frame.operations.len() - branch_index;
                    assert!(offset <= i16::MAX as usize);
                    frame.operations[branch_index] =
                        Op::Bc(Cond::Ne, Register::T1, Register::T2, offset as i16);
                }

                if let Some(else_) = else_ {
                    let skip_else_ip = {
                        let frame = self.stackframes.back_mut().expect("no stack frames!");
                        frame.operations.push(Op::Nop);
                        frame.operations.len() - 1
                    };

                    self.add_node(else_)?;

                    {
                        let frame = self.stackframes.back_mut().expect("no stack frames!");
                        let offset = frame.operations.len() - skip_else_ip;
                        assert!(offset <= i16::MAX as usize);
                        frame.operations[skip_else_ip] = Op::B(offset as i16);
                    }
                }
            }
            Ast::ExprCall {
                function_name: _,
                paramaters: _,
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
