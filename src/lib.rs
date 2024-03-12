pub mod ir;
pub mod parser;
pub mod riscv;
pub mod span;
pub mod typecheck;
pub mod util;

pub fn greeting() -> &'static str {
    "Hello, World!"
}

#[cfg(test)]
mod test {
    use crate::greeting;

    #[test]
    fn greets_world() {
        assert_eq!(greeting(), "Hello, World!");
    }
}
