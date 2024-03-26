use std::collections::HashMap;

use crate::{
    parser::{ast::AnonType, Ast},
    typecheck::{FunctionDeclaration, ScalarType, TypeInfo},
};

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<String, TypeInfo>,
    pub functions: HashMap<String, FunctionDeclaration>,
}

impl Declarations {
    pub fn from_ast<X: std::fmt::Debug + Clone>(program: &Ast<X>) -> Declarations {
        Declarations::from_ast_iter(std::iter::once(program))
    }

    pub fn from_ast_iter<'a, X: std::fmt::Debug + Clone + 'a>(
        program: impl Iterator<Item = &'a Ast<X>>,
    ) -> Declarations {
        let mut decls = Declarations::default();
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::Program(p) => {
                    let child_decls = Declarations::from_ast_iter(p.definitions.iter());
                    decls.functions.extend(child_decls.functions.into_iter());
                    decls.types.extend(child_decls.types.into_iter());
                }
                Ast::DefFunction(func) => {
                    let paramaters = func
                        .params
                        .iter()
                        .map(|p| (p.name.clone(), decls.eval_anon_type(&p.typ)))
                        .collect();
                    let returns = decls.eval_anon_type(&func.return_type);

                    decls.functions.insert(
                        func.name.clone(),
                        FunctionDeclaration {
                            paramaters,
                            returns,
                        },
                    );
                }

                Ast::DefType(t) => {
                    let mut typ = decls.eval_anon_type(&t.typ);
                    decls.types.insert(t.name.clone(), typ);
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
                | Ast::Expr(_) => continue,
            }
        }

        decls
    }

    pub fn eval_anon_type(&self, t: &AnonType) -> TypeInfo {
        match t {
            AnonType::IntegerRange {
                inclusive_low,
                inclusive_high,
            } => TypeInfo::integer(
                inclusive_low.parse().unwrap(),
                inclusive_high.parse().unwrap(),
            ),
            // TODO: unit keyword type
            AnonType::TypeReference {
                name,
                parameters: _,
            } if name == "unit" => TypeInfo::Unit,
            AnonType::TypeReference {
                name,
                parameters: _,
            } => self.types.get(name).expect("type not found!").clone(),
            AnonType::Bool => TypeInfo::Scalar(ScalarType::Boolean(None)),
            a => panic!("TODO: from_ast for {:?}", a),
        }
    }
}
