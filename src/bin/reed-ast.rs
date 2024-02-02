use std::env;
pub fn main() {
    let result = reed::parser::parse(&env::args().nth(1).unwrap());
    println!("ast = {:?}", result.ast);
    println!("errors = {:?}", result.errors);
}