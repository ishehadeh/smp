use crate::parser::{parse, Ast};

pub struct AstInterpreter {
    tree: Ast,
}

impl AstInterpreter {
    pub fn new(program: &str) -> AstInterpreter {
        AstInterpreter {
            tree: parse(program).ast,
        }
    }
}
