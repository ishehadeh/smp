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
