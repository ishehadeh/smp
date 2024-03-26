use std::collections::HashMap;

use crate::{
    parser::Ast,
    typecheck::{FunctionDeclaration, TypeInfo},
};

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<String, TypeInfo>,
    pub functions: HashMap<String, FunctionDeclaration>,
}

pub fn scan_declarations<'a>(program: impl Iterator<Item = &'a Ast>) -> Declarations {
    let mut functions = HashMap::new();
    let mut types = HashMap::new();
    for toplevel_ast_node in program {
        match toplevel_ast_node {
            Ast::Program(p) => {
                let child_decls = scan_declarations(p.definitions.iter());
                functions.extend(child_decls.functions.into_iter());
                types.extend(child_decls.types.into_iter());
            }
            Ast::DefFunction(func) => {
                functions.insert(
                    func.name.clone(),
                    FunctionDeclaration {
                        paramaters: func
                            .params
                            .iter()
                            .map(|p| (p.name.clone(), TypeInfo::from_ast(&p.typ)))
                            .collect(),
                        returns: TypeInfo::from_ast(&func.return_type),
                    },
                );
            }

            // TODO: types

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
            | Ast::Expr(_) => continue,
        }
    }

    Declarations { functions, types }
}
