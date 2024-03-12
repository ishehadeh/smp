use std::collections::HashMap;

use crate::{
    parser::Ast,
    typecheck::{FunctionDeclaration, TypeInfo},
};

use super::{
    frame::{FrameCompiler, FrameData},
    CompileError,
};

#[derive(Debug, Default)]
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
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::DefFunction {
                    name,
                    params,
                    return_type,
                    body: _body,
                } => {
                    // TODO: params, return type
                    self.declarations.functions.insert(
                        name.clone(),
                        FunctionDeclaration {
                            paramaters: params
                                .iter()
                                .map(|p| (p.name.clone(), TypeInfo::from_ast(&p.typ)))
                                .collect(),
                            returns: TypeInfo::from_ast(return_type),
                        },
                    );
                }

                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::Number(_)
                | Ast::Ident(_)
                | Ast::Error
                | Ast::Repaired(_)
                | Ast::Block { .. }
                | Ast::StmtIf { .. }
                | Ast::ExprCall { .. }
                | Ast::DefType { .. }
                | Ast::StmtLet { .. }
                | Ast::Expr { .. }
                | Ast::Program { .. } => continue,
            }
        }

        Ok(())
    }

    pub fn compile_functions<'a>(
        &mut self,
        program: impl Iterator<Item = &'a Ast>,
    ) -> Result<(), CompileError> {
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::DefFunction {
                    name,
                    params: _params,
                    return_type: _return_type,
                    body,
                } => {
                    // TODO: params, return type
                    let decl = self.declarations.functions.get(name).unwrap();
                    let mut framecc = FrameCompiler::from_function_declaration(decl);
                    framecc.compile(body)?;
                    // TODO handle output
                    self.functions.insert(name.clone(), framecc.into_frame());
                }
                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::Number(_)
                | Ast::Ident(_)
                | Ast::Error
                | Ast::Repaired(_)
                | Ast::Block { .. }
                | Ast::StmtIf { .. }
                | Ast::ExprCall { .. }
                | Ast::StmtLet { .. }
                | Ast::DefType { .. }
                | Ast::Expr { .. }
                | Ast::Program { .. } => continue,
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::ir::{compiler::FunctionDeclaration, frame::FrameCompiler, ops::IrOp};
    use crate::parser::lexer::Lexer;
    use crate::parser::InfixOp;
    use crate::parser::{grammar, Ast, ParseError};
    use crate::typecheck::TypeInfo;

    #[cfg(test)]
    pub fn must_parse_expr(s: &str) -> Ast {
        let lexer = Lexer::new(s);
        let mut recovered_errors = Vec::new();
        let result = grammar::ExprParser::new().parse(&mut recovered_errors, lexer);

        let mut errors: Vec<_> = recovered_errors
            .into_iter()
            .map(|e| ParseError::from(e.error))
            .collect();
        match result {
            Ok(v) => return v,
            Err(e) => {
                errors.push(ParseError::from(e));
            }
        };

        panic!("parser encountered errors: {:?}", errors);
    }

    #[test]
    fn add_arith_op() {
        let mut frame_compiler = FrameCompiler::from_function_declaration(&FunctionDeclaration {
            paramaters: vec![],
            returns: TypeInfo::Unit,
        });
        let l = frame_compiler.allocate_register(TypeInfo::integer(0, 100));
        let r = frame_compiler.allocate_register(TypeInfo::integer(0, 100));

        let result = frame_compiler.add_op(InfixOp::Add, l, r).unwrap();
        let t = &frame_compiler.get_frame().cell(result).typ;
        assert_eq!(t.clone(), TypeInfo::integer(0, 100));
    }

    #[test]
    fn compile_simple_expr() {
        let mut frame_compiler = FrameCompiler::from_function_declaration(&FunctionDeclaration {
            paramaters: vec![],
            returns: TypeInfo::Unit,
        });

        frame_compiler
            .compile_expr(&must_parse_expr("1 + 2"))
            .expect("compile expr failed");
        assert!(matches!(
            &frame_compiler.get_frame().operations[..],
            &[
                IrOp::IStoreImm(a, 1),
                IrOp::IStoreImm(b, 2),
                IrOp::IAdd(_, a_, b_)
            ] if a_ == a && b_ == b
        ));
    }
}
