use std::collections::HashMap;

use crate::{
    parser::Ast,
    typecheck::{typetree::TypeInterpreter, FunctionDeclaration, TypeInfo, TypeTree},
};

use super::{
    frame::{FrameCompiler, FrameData},
    CompileError,
};

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<String, TypeInfo>,
    pub functions: HashMap<String, FunctionDeclaration>,
}

#[derive(Debug, Default)]
pub struct IrCompiler {
    pub declarations: Declarations,
    pub functions: HashMap<String, FrameData>,
}

impl IrCompiler {
    pub fn new() -> IrCompiler {
        IrCompiler::default()
    }

    pub fn scan_declarations<'a>(
        &mut self,
        program: impl Iterator<Item = &'a Ast>,
    ) -> Result<(), CompileError> {
        // this function was refactored out of the struct, it should probably be removed at some point
        self.declarations = crate::util::ast::scan_declarations(program);
        Ok(())
    }

    pub fn compile(&mut self, ast: Ast) -> Result<(), CompileError> {
        self.scan_declarations(std::iter::once(&ast))?;

        let mut type_interp = TypeInterpreter::new(self.declarations.clone());
        let type_tree = type_interp.eval_ast(ast);
        match type_tree {
            Ast::Program(prog) => self.compile_functions(prog.definitions.iter()),
            _ => panic!("expected top level program"), // TODO: normal error here
        }
    }

    pub fn compile_functions<'a>(
        &mut self,
        program: impl Iterator<Item = &'a TypeTree>,
    ) -> Result<(), CompileError> {
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::DefFunction(func) => {
                    // TODO: params, return type
                    let decl = self.declarations.functions.get(&func.name).unwrap();
                    let mut framecc = FrameCompiler::from_function_declaration(decl);
                    framecc.compile(&func.body)?;
                    // TODO handle output
                    self.functions
                        .insert(func.name.clone(), framecc.into_frame());
                }
                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::LiteralInteger(_)
                | Ast::LiteralBool(_)
                | Ast::Ident(_)
                | Ast::Repaired(_)
                | Ast::Block(_)
                | Ast::StmtIf(_)
                | Ast::ExprCall(_)
                | Ast::StmtLet(_)
                | Ast::DefType(_)
                | Ast::Expr(_)
                | Ast::Program(_) => continue,
            }
        }

        Ok(())
    }
}
