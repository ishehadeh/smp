# Parsing

## Tools

- [lalrpop](https://github.com/lalrpop/lalrpop) LR(1) parser generator
    + PRO: error recovery
    + PRO: close(ish)ly related to parsing layed out by aho
    + PRO: a few languages already implemented using it
    - CON: ugly language
    

- [pest](https://pest.rs/) PEG parser generator
    + PRO: fantastic tooling
    + PRO: (supposed) very fast
    + PRO: grammars are prettier (to me at least)
    - CON: couldn't find similar programming languages implemented in pest
    - CON: precedence handled seperatedly after parsing
    - CON: no error recovery? (I can't find anything about it)

### Results

lalrpop seems like its been used in more projects similar to mine which will be useful for examples.
Furthermore, it gives me some peace of mind that it supports the features I'll need as my language grows.

I enjoy Pest's polish, especially when it comes to tooling.
But, I can't find any references about error recovery, which is important in long program files. 
It also is a PEG parser, which the Compilers book doesn't cover.
I'm sure I could find another resource on working with this type of parser, but it would be easier to use a parser generator that is covered by the book.

## References

### lalrpop Examples

These are fairly complete examples that have (pretty) active development by a number of contributors.
I feel pretty same that they're a decent reference.
- [Gluon](https://github.com/gluon-lang/gluon/blob/main/parser/src/grammar.lalrpop)
- [lalrpop](https://github.com/lalrpop/lalrpop/blob/master/lalrpop/src/parser/lrgrammar.lalrpop)
- [RustPython](https://github.com/RustPython/Parser/blob/main/parser/src/python.lalrpop)
- [Nickel](https://github.com/tweag/nickel/blob/master/core/src/parser/grammar.lalrpop)
