use crate::parser::{Ast, InfixOp};

use super::vm::Op;

#[derive(Default, Clone, Debug)]
pub struct Compiler {
    ops: Vec<Op>,
}

#[derive(Clone, Debug)]
pub enum CompileError {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler::default()
    }

    pub fn add_node(&mut self, node: &Ast) -> Result<(), CompileError> {
        match node {
            Ast::Ident(_) => unimplemented!(),
            Ast::Repaired(_) | Ast::Error => panic!("error in ast!"),
            Ast::Number(x) => self.ops.push(Op::Push(*x)),
            Ast::Expr { lhs, op, rhs } => {
                let _ = self.add_node(lhs);
                let _ = self.add_node(rhs);
                match op {
                    InfixOp::Add => self.ops.push(Op::Add),
                    InfixOp::Sub => self.ops.push(Op::Sub),
                    InfixOp::Mul => self.ops.push(Op::Mul),
                    InfixOp::Div => self.ops.push(Op::Div),
                }
            },
            Ast::DefFunction { name, params, return_type, body } => todo!(),
            Ast::DefType { name, typ } => todo!(),
            Ast::Block { statements: _ } => todo!(),
            Ast::StmtLet { name, return_type, value } => todo!(),
        }

        Ok(())
    }

    pub fn program(&self) -> &[Op] {
        &self.ops
    }

    pub fn into_program(self) -> Vec<Op> {
        self.ops
    }
}
