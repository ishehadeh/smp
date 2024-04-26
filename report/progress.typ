#import "@preview/charged-ieee:0.1.0": ieee

#let abstract = [
  We introduce the Howlite programming language. Where it's from, and where it's going.
]

#show: ieee.with(
  title: [The Howlite Programming Language - First Semester Progress],
  authors: (
    (
      name: "Ian Shehadeh",
      department: [Computer Science],
      organization: [St. Mary's College of Maryland],
      location: [St. Mary's City, Maryland],
      email: "irshehadeh@smcm.edu",
    ),
  ),
  bibliography: bibliography("refs.bib"),
)

= Introduction
We present Howlite, a programming language which targets the RISC-V instruction set. It is a small language, with features optimized for writing low-level performance critical systems.

== Project Timeline & Objectives
This project is designed to be finished in two semesters.
By the end of the second semester, Howlite will be capable of self-hosting (i.e. writing a Howlite compiler in Howlite).

Ideally, all the features outlined below will be implemented, but some may be dropped in order to prioritize creating a _complete_ language.
For example, right now the language is missing several operators, and integer types are only unsigned.
While these omissions have not caused issues at this stage of development, these features will take priority going forward.

== Language Objective
We use Howlite to study expressive modeling of data, with minimal constraints. Howlite's type system supports common patterns in embedded programming, while allowing the programmer to freely and explicitly manage memory and program behavior.

In practice this means a strong support for _subtype polymorphism_, _numeric subtypes_, and explicit behavior with regard
to how data structures are encoded. The syntax and semantics are inspired by TypeScript's@ts type system around JavaScript@js Objects.

= Language Design <lang-design>

== Syntax
Howlite's syntax is designed to be clear, concise and immediately legible to people familiar with C-like
languages. We use curly-braces to mark blocks, square brackets for arrays, and semicolons as statement terminators. To maintain clarity Howlite's syntax keeps symbols within the same context as much as possible. For example colons always relate type signatures. But this consistency is broken if it would require straying too far from mainstream language syntax

Aside from C, Howlite's syntax takes inspiration from several new languages. The type syntax was inspired by TypeScript@ts, hints were taken from Rust@rust on bringing ML niceties to C-like syntax, and notes were taken from Go@go on how to implement a small easily-parsed grammar.

== Semantics

Howlite is an imperative, eagerly-evaluated, statically typed, and lexically scoped language. The language is designed to make program behavior obvious and easily controlled. Being a high-level language, abstractions are a given, but we aim to keep behavior consistent whenever some abstraction is applied. This is relatively easy to achieve for the Howlite compiler, since it targets a single architecture and is non-optimizing.

This section walks through a number of common Howlite expressions. It is not a full specification, or even proper documentation, but should give context for the following sections.

=== Variables
Variables are declared with a `let` statement.
```hlt
let a: <TYPE> = <VALUE>;
let mut b: <TYPE> = <VALUE>;
```
All variables must be immediately initialized. Identifiers may not be redeclared. Only variables declared with "`mut`" may be reassigned or modified.
The syntax was taken from Rust@rust, TypeScript@ts has a similar binding syntax. We prefer this syntax over the C type-first declarations because it clearly defines when an variable is declared. Furthermore, because type names are arbitrary, they may be difficult for humans to parse. Thus, we precede every type with ':' throughout the language.

=== Numbers
Number types in Howlite are expressed as an inclusive range.
The integers in this range must be representable in at most two registers on the target machine.
`i` may be `0`, `255` or any integer between the two:
```
let i: 0..255 = 127;
```

Integer literals support radix prefixes: `0x` (hexadecimal), `0b` (binary), `0o` (octal).
Numbers may also include underscores as separators. This syntax was mostly lifted from Rust@rust, although there are a number of languages with similar integer literals.

There are two built-in EEE-754 floating point types, `float32` and `float64`. Float literal syntax is identical to C.

=== Null
The `null` type has a single value called `null`.
```hlt
let nothing: null = null;
```
This is identitcal to TypeScript's@ts `null` type, and Rust's@rust unit type "`()`".
We chose "`null`" as the keyword because it is common in popular languages today: JavaScript@js[4.4.16], Java@null_java, C@null_c and C++@null_cxx.

=== Structures
Structures are similar to C `struct`s.
```hlt
let pair: { a: bool, b: bool } = struct {
  a: true,
  b: false
};
```
Howlite supports several operators which can control a structure's layout in memory:
```hlt
{
  a: bool,
  @align(4) {
    b1: 0..5
    b2: 5..10
  }
  c: u16
}
```
This defines a structure which takes up 4 bytes for `a` (by default all structs are 32-bit aligned), then packs `b1`, and `b2` into a single byte. `c` directly follows this byte, and is padded by another byte, so following fields may return to the default alignment.

=== References
Howlite does not support pointers with integer semantics like C. Instead it has opaque "reference" types.
```hlt
let tuple_ref: &Tuple = &tuple_val;
```
By default, the only operation that may be performed on a reference is access: `tuple_ref.first`, and dereference: `*tuple_ref`. Dereference is used when an operation must be performed on the referenced value itself, for example: assignment or addition. References cannot be `null`. They may be downgraded with a cast: `(tuple_ref : uint)`. Upgrading a reference requires using the compiler built-in function `@toReference`.

=== Arrays and Slice
Arrays are semantically similar to C arrays but differ slightly in syntax:
```hlt
let array: [bool; 4] = [true, false, true, false];
```

Slices are similar, but their length is unknown at compile time.
Furthermore, slices are a type of pointer, so they are expressed as:
```
let slice: &[bool] = &array.[1..3]
```
`slice` refers to the elements of `array` at indices 1, 2, and 3.
We chose to place the type inside the brackets, unlike C-style arrays because it looks better when combined with number ranges. This syntax also disambiguates arrays from type parameters.

The `.` in the array index syntax disambiguates calling a function pointer in an array from calling a function with a type parameter.

=== Characters & Strings
Characters and strings both have dedicated types and syntax:
```hlt
let h: char = 'h';
let greet: &string = ...;
```
The literal syntax is identical to C.
"&string" is a subtype of "&[0..255]", which is understood to be valid UTF-8 encoding of a series of Unicode characters.

=== Unions
Howlite supports C-like union types. Unions are constructed from two or more _subtypes_, separated by `|`: `type U = bool | null`.
`U` may be a `bool` or `null`. Unlike dynamic languages, C++'s `std::variant`@cxx_variant Howlite does not know the current type of `U` at runtime.

To ensure unions are not accessed incorrectly, only operation is valid on a subtype only if it is
1. Valid on _all_ subtypes
2. And, has the same definition on all subtypes

For example `bool` and `null` equality is defined differently (`== : (bool, bool) -> bool` vs `== : (null, null) -> bool`) so `U` has no valid `==` operator.
`if` statements may narrow the union by checking values of specific fields:
```hlt
let v: { a: 1..1, b: null } | { a: 2..2, b: bool } = { a: 1, b: null };
if v.a == 1 {
  /* v : { a: 1..1, b: null } */
}
```

=== Expressions
Howlite expressions look similar to expressions in most C-adjacent languages.
The language supports infix, postfix and unary operators.
There are function calls, variable declarations, assignment and so on.
It also supports the usual flow control: `while`, `for`, `if`, and `switch`.

All blocks and flow control have a value and type.
The value is the last statement in the executed block.
The type is the union of all possible branch's value's types.
For example:
```hlt
let a: char | float = if cond {
  'a'
} else {
  1.2
}
```
This feature was lifted from Rust@rust, which itself was inspired by OCaml@ocaml.

For loops are always given an explicit range.
There is no other form.
```hlt
for let i: uint in a..b { ... }
```

While loops are nearly identical to C.

Note that like in Go and Rust, parentheses may be omitted from flow control condition expressions.

Syntactically switch statements are identical to C, but they break after each case.
They are intended as syntactic sugar for long if-else chains.
```hlt
switch value {
  case 1: ... // implicit break
  case 2: ... // implicit break
  // etc...
  default: ...
}
```
=== Type Parameters
```
type MyArray[T: any] = {
  elements: &rw[T],
  length: uint
}
```

There are two kinds of type parameters in Howlite, "Boxed" and "Any". An "Any" type may only be referenced. They cannot
be inlined into the structure. For example, the following definition is illegal:
```hlt
type Pair[L: any, R: any] = { left: L, right: R }
```
`L` and `R` may only be referenced, so
```hlt
type Pair[L: any, R: any] = { left: &L, right: &R }
```
is allowed.

Box types may be included, with the limitation they may be no larger than the system's register size. For example:
```
type Vec2[T: boxed] = { x: T, y: T };
```

This limited parametric polymorphism gives the programmer the ability to get strong typing, without specializing the
data structure. This is especially useful for parameterized procedures don't need to be specialized either, simplifying the calling
convention and allowing easy, strongly-typed interoperation with other languages.

We have adapted these types from _Cyclone_@ref_cyclone, they are described by Dan Grossman in _Quantified types in an imperative language_@cyclone_types

= Compiler Architecture <hltc-arch>
The compiler is organized into three stages:

1. Parsing
2. Type checking
3. Code generation

Unlike many compilers Howlite does not have an intermediate representation. We chose to omit this pass because Howlite is non-optimizing and targets a single architecture, so it would have considerably slowed development for little benefit.

The compiler is written in Rust. We chose Rust because the author had the most experience with this language. Rust also has a number of other programming languages written in it, which we used as a reference. Specifically, we looked at _Gluon_@ref_gluon, _Gleam_@ref_gleam, and _RustPython_@ref_rspy. We also read through sections of the _Tiny C Compiler_@ref_tcc, because it compiles directly to assembler. The _Rust_@ref_rustc and _Swift_@ref_swiftc compilers were referenced for their abstract syntax tree representations.

== Parsing
The lexer is generated by the Rust library _logos_@lib_logos, and the parser by _lalrpop_@lib_lalrpop. _lalrpop_ is an LR(1) parser generator
for Rust. We did not write a custom recursive descent parser to speed development.

We chose _lalrpop_ because it is well-supported and has a few complex grammars we can use as reference (_Gluon_@ref_gluon, _Gleam_@ref_gleam, and _RustPython_@ref_rspy). By using a generated parser we trade faster development for worse error handling and a lower performance ceiling. But since precise, well formatted errors, and quick compile times are not stated goals of the project, the ability to rapidly prototype the grammar was well-worth the tradeoff.

The generated parser directly emits an abstract syntax tree (AST). Errors are just another node in the AST. Keeping errors siloed in the AST has the advantage it forces each pass to explicitly handle them as they are encountered. So, a program may be compiled as much as possible, with each pass trying to work around the invalid nodes. Information typically kept in the parse tree is not encoded in the abstract syntax tree. Some languages keep individual tokens (e.g. _Rust_@ref_rustc), or "trivia" nodes (e.g. _Swift_@ref_swiftc), alongside the abstract tree contents.

Howlite's AST is a parameterized type. Each node in the AST has a field containing arbitrary data.
We'll discuss this more in the next section, but initially this field has the unit type. In further iterations of the
parser it may be used to hold error information and syntax trivia (such as whitespace, keywords, etc.).

== Typechecking

Typechecking is performed via recursive descent of the AST. The type checker annotates each node with type information. The type checker accepts the tree produced by the parser, that is - a tree with no extra data attached. It produces a tree with type information attached to each node. Errors are included alongside this type information, but they don't preclude each node getting a type. So in the case of an invalid operation, like `1 + "a"`, the type checker will add an error, then choose a valid type that the parent node may reference.

At the time of writing, the Howlite type checker is lacking. While we are confident in the broad strokes outlined above, it's difficult to give more specifics. Especially because many language features are not fully implemented like polymorphism and support for integer range types. @curr-impl will elaborate on the current implementation further. The current architecture is _just enough_ to allow for well-formed code generation.

== Code Generation
Similar to type checking the code generator works by recursively crawling the AST. Unlike type checking it does not
annotate each node (i.e. creating a new AST). Instead, each node is transformed into a structure containing.

1. The compiled assembly
2. Error information (if any)
3. Where to find the result of any computation performed by the given node.

For example, the node corrosponding to `1 + 1` may return:
```s
li a0, 1          ; move literal '1' into register a0
li a1, 1          ; move literal '1' into register a1
add a0, a0, a1   ; a0 = a0 + a1
```
The fundamental limitation of this approach is that the returned code is opaque to the calling procedure, This limitation blocks a number of optimizations. Again, like with our use of a parser generator we chose this method because it allows for quick development and prototyping. Runtime performance is not a goal of the project.

The code generator emits a RISC-V assembly string, which is expected to be passed to an external assembler and linker. The compiler includes a structure holds a buffer a block of assembler. Each individual instruction has a method which adds it to the buffer. This allows the structure to track which registers have been read or mutated. With this information, when the parent node receives the child's assembler block, it is able to determine which registers must be saved if used around the block.

There is no clever register allocation. If a register is needed the compiler checks which registers are free, and allocates the first unused one it finds. If no registers are free it allocates space on the stack.

= Current Implementation <curr-impl>

The focus of this semester was creating a small core language which compiles to RISC-V assembler. We have built a solid foundation which can be
iterated on next semester.

The most notable omissions are in the type checker. Currently, it doesn't handle any kind of polymorphism, and only has
basic support for numbers.

The compiler is well partitioned, each step outlined in @hltc-arch is clearly separated. An AST is generated by the
parser, passed through the type checker then transformed into an assembler file. Each module has minimal dependencies on
any other.

== Syntax Progress
Type expressions support a subset of the possible scalars: `unit`, `bool`, and integer types. Support for number
(i.e. integer) ranges (`0..10`) has been established, although negative numbers are not included. Container types,
arrays and structures have been fully implemented in syntax. Named references to other types have also been implemented,
which may include type parameters. The most notable omission in the current type expression grammar is that there is no
union operator, or any kind of reference type.

Most top level definitions have been implemented: `type`, `func`, and `extern func`. Each of the these definitions also has support for type parameters. There is no syntax for module imports or exports.

Expression Support for integer comparison operators, basic arithmetic operations, and
assignment. Bindings are supported via `let` and `let mut`. Identifiers and may be accessed and called. Literals have been implemented for integers, booleans, structures and arrays. Basic branching via `if`/`else` and `while` has been implemented, but there is no `else if`, `switch` or `for` expressions.

Comments have not been implemented.
== Type Checking Progress
The most significant progress in the type checker is crawling the AST, adding type annotations. The actual _checking_ of
these annotations is limited.

Simple types are checked, for example:

```
let a: bool = 5    // this causes an error
```

But all statements aren't checked, for example function calls and return expressions are ignored by the type cheker.

The fundamental architecture of the type checker is solid. The type checker derives a type based on _the child types of the current node_ and the specifics in of _current AST node_. Making this information long-lived and tightly coupled is useful. This architecture has been essential in debugging the type checker and compiler. This pattern could also help provide better error messages in the future, although that is mostly out of scope for the project.

Despite our belief in the architecture, the current implementation has a few critical flaws:

=== Performance
During type checking _the entire_ AST is copied while annotating it with type information. Alternative representations of
the AST could work around this limitation. For example, we could extend the AST presented in _Compilers_@compilers.
This implementation has each node added to an array, and children are referenced using an index into this
array. Then, extension data could be stored in a separate, equally-sized array with each element corresponding to the AST Node at the same index, in effect we turn the AST into a structure of arrays.

=== Monolithic
The current AST walker type resides in a nearly 1,000 line file, with no clear way to break it into seseparateparts.
Again, there are a few paths to remedy this, but similar to the performance issue it will require a large project refactor.

Both these points are to say: while the current type checker provides a rough outline of what we hope the final product
will look like, there will likely need to be a large restructuring and rewrite of the system.

== Code Generation
Most features are fairly easily implemented in the code generator. Expressions that can be parsed, and annotated
with types are able to transformed into assembler. The only notable omissions are around container types: Structure
accessories are not implemented, and there is only limited support for the RISC-V calling convention when working with
values larger than the register width.

There is no support for integers greater than the register size anywhere in the language.

_ASIDE_: The actual integer representations used through the compiler are brittle, and badly needs to be re-thought. The
compiler just uses the pointer-sized signed integers everywhere, which may lead to non-representable numbers
(pointer-sized unsigned integers) or overflow, even before considering integers wider than a pointer.

Consequently, multi-register function arguments are an afterthought. Arrays and structures are always put on the stack,
even when they are small enough that the calling convention requires they be passed using registers. It's worth noting
multi-register return values are not supported either, although this is less critical, since it's not as common to
return large values and, only two registers are used for the return value.

Although there are missing ppiecesin the compiler, we don't expect filling these gaps will require a major shift, like the type checker.

== Documentation
There is some documentation. But most notes are out of date and irrelevant; One
could count the number of documented functions on at-most one hand; And there have been enough changes that what few comments exist cause more harm than good.

== Tests

Howlite's test suite isn't lacking to the same degree as documentation, although it's not far off. There are a few
broken tests scattered throughout the code. Running the compiler's unit tests will fail, they are long out of date. We
gave up on maintaining a solid set of unit tests due to how rapidly the repository was shifting.

As alluded to previously, a number of example programs were developed, which serve two purposes:

=== Tracking Impact
When compiling source test programs, the result of each compiler pass is written to a file, which is stored in version
control. This way, we can see the result of changes to the type checker and code generation over the course of
development.

=== Verify Changes
As a side effect of tracking compiler output in version control, we can immediately observe any change since the last
commit by simply running `git diff HEAD tests/`. This isn't a replacement for real tests, since it requires the
programmer correctly verify the changes are intended (which they have failed to do a number of times). But it allows
rapid iteration and prototyping without constantly updating expected test results.

= Roadmap

This semester has been spent learning how to make a compiler. The challenge came from fitting each piece into place. We
answered seemingly simple questions, like _what does the AST look like_? _what information is retained from parsing_? _How is
type information stored_? and _how is code generated_?

By answering these questions we built a foundation, The compiler architecture has mostly been decided. Now, we are free to iterate on this design and implement more complex language features. We are confident that the language outlined in @lang-design will be completed over the next 7 months.

The remaining tasks have been split into a Major and Minor To-do list. The major tasks will likely be long-lived, and require consideration as the compiler ddevelops They may also need to some sections of the compiler's design to be reconsidered. The Minor To-do list are small tasks that can be quickly implemented without too uch consideration. Most of these have a well established design, and partial support in the compiler already.

== The Major To-Do List

=== Addition Research
During the course of implementing the current version of Howlite we became aware of Copy-and-Patch@cp compilation and Hindley-Milner type systems@hindley_types@milner_types. While neither of these systems are directly applicable its worth reading through the material. Especially since our type checker is one of the largest areas needing improvement.

=== Reference Types
The most sorely missing feature in the core language, which prevents Howlite for being used in conjunction with C
libraries is the lack of support for any kind of indirection. We expect this to also be a fairly significant change that
will riple throughout code generation and type checking so it's first on the to-do list.

=== Modules
Modules have not been designed. We have some rudimentary ideas of how they may fit into the language, but nothing solid.

=== Union Types
Union types have some parts implemented in the type checker, but there is no syntax for them, and the type checking is fairly rudimentary.

=== Polymorphism Types
Although the syntax is implemented, parametric polymorphism is unsupported in the type checker. Subtype polymorphism is partially implemented, but broken.

=== Integer Range Types
While partially implemented, integer range types are unchecked.
This will be one of the last additions to the language, since it weaves its way into just about every system.

== The Minor To-Do List

These are partially implemented features or ones which we expect to be relatively easy to implement. None should require major design changes.

1. Operator Precedence
2. Logical, bitwise, and arithmetic with assignment, binary operators
3. Unary operators
4. Core types: Reference/Length pairs (slices), strings, enumerables, floating point numbers, and characters.
5. Structure encoding control (`@align`, `@pad` etc.)
6. Literals: characters, floats, radix-prefix integers.

== Refactoring, Documentation
Once reference types are implemented we plan to slow development to write documentation, and try to refactor some sections of the compiler that have become overly complex.

When iterating quickly this is usually an afterthough, leading to giant files all part of a tangled web of dependencies.
Periodically stopping and cleaning up help speed future development.

= Summary
