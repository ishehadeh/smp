use std::env;
use std::io;
use std::io::Read;

pub fn main() {
    let mut program = String::new();
    if let Some(arg1) = env::args().nth(1) {
        program.push_str(&arg1);
    } else {
        io::stdin().read_to_string(&mut program).unwrap();
    };
    let result = howlite::parser::parse(&program);
    println!("ast = {:#?}", result.ast);
    println!("errors = {:#?}", result.errors);
}
