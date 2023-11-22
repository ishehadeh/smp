#[derive(pest_derive::Parser)]
#[grammar = "src/parser/grammar.pest"]
pub struct ReedParser;

#[cfg(test)]
mod test {
    use super::{ReedParser, Rule};
    use pest::Parser;

    #[test]
    fn test() {
        panic!("{:#?}", ReedParser::parse(Rule::program, "1 + 1 + 1"));
    }
}
