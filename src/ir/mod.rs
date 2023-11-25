pub mod compiler;
pub mod vm;

#[cfg(test)]
mod test {
    use crate::{ir::vm::Op, parser::ast::parse};

    use super::{compiler, vm};

    fn compile(s: &str) -> Vec<vm::Op> {
        let ast = parse(s).into_result().expect("failed to compile");
        let mut compiler = compiler::Compiler::new();
        compiler.add_node(&ast).expect("failed to compile");
        compiler.into_program()
    }

    // execute the program and get the top value from the stack
    fn execute(s: &str) -> i32 {
        let mut vm = vm::Vm::new(compile(s));
        while vm.step() {
            // ...
        }

        vm.pop()
    }

    #[test]
    fn simple_compile() {
        assert_eq!(compile("1 + 1"), [Op::Push(1), Op::Push(1), Op::Add]);
        assert_eq!(
            compile("(1 / 100) + 1"),
            [Op::Push(1), Op::Push(100), Op::Div, Op::Push(1), Op::Add]
        );
    }

    #[test]
    fn simple_execute() {
        assert_eq!(execute("1 + 1"), 2);
        assert_eq!(execute("(3 * 100) + (5 - 2)"), 303);
    }
}
