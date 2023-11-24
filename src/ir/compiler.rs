use crate::parser::ast;

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

    pub fn add_node(&mut self, node: &ast::Node) -> Result<(), CompileError> {
        match node {
            ast::Node::Ident(_) => unimplemented!(),
            ast::Node::Number(x) => self.ops.push(Op::Push(*x)),
            ast::Node::Expr { lhs, op, rhs } => {
                let _ = self.add_node(lhs);
                let _ = self.add_node(rhs);
                match op {
                    ast::InfixOp::Add => self.ops.push(Op::Add),
                    ast::InfixOp::Sub => self.ops.push(Op::Sub),
                    ast::InfixOp::Mul => self.ops.push(Op::Mul),
                    ast::InfixOp::Div => self.ops.push(Op::Div),
                }
            }
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
