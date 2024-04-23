#import "@preview/charged-ieee:0.1.0": ieee

#let abstract = [
  We introduce the Howlite programming language. Where it's from, and where it's going.
]

#show: ieee.with(title: [The Howlite Programming Language - First Semester Progress], authors: ((
  name: "Ian Shehadeh",
  department: [Computer Science],
  organization: [St. Mary's College of Maryland],
  location: [St. Mary's City, Maryland],
  email: "irshehadeh@smcm.edu",
),), bibliography: bibliography("refs.bib"))

= Introducing Howlite

The Howlite (abbr. _hlt_) programming language is a small systems programming language aimed at embedded devices and
very low-level performance critical systems.

== Project Goals

= Language Design

Before describing the language, consider this example of a non-recursive fuction to get the `n`#super[th] number in the
fibonacci sequence.

```
type uint16 = 0..65535;

func fib(n: uint16): uint16 {
    let mut memory: [uint16; 2] = [ 0, 1 ];
    let mut acc: uint16 = 0;

    let i: uint16 = 0;
    while i < n {
        acc = memory.[0] + memory.[1];
        memory.[0] = memory.[1];
        memory.[1] = acc;
        i = i + 1;
    };

    acc
}
```

We'll use parts of this example throughout this section.

== Syntax

Howlite's syntax is designed to be clear, concise and most importantly: immediately legible to those familiar with "c-like"
languages. We use curly-braces to mark blocks, square brackets for arrays, and semicolons as statement terminators.

The departures from this well-established lineage is more notable.

=== Reversing Binding/Type Order
In Howlite, variable types, parameter types and function return types are the last part of the declaration, as opposed
to C, where they lead. For example consider the equivalent snippets:

#figure(caption: "Binding Syntax Comparison")[
#grid(
  columns: (2fr, 18fr),
  rows: (auto, auto),
  gutter: 8pt,
  align: left,
  [*c*],
  `uint16 fib(uint16 n)`,
  [*hlt*],
  [`func fib(n: uint16): uint16`],
)

#grid(
  columns: (2fr, 18fr),
  rows: (auto, auto),
  gutter: 8pt,
  align: left,
  [*c*],
  `uint16 acc = 0`,
  [*hlt*],
  [`let mut acc: uint16 = 0`],
)
]

We prefer this prefix notation because it prioritizes _alignment_ and _orders information_ from most general to least
right-to-left.

Ordering of information is important when reading long series of declarations. When every mutable variable is prefixed
with "`let mut`" and every immutable variable prefixed with "`let`", it makes it easy to quickly find variable names
when scanning a function declaration. Every line starting with "let" binds a name to some value. If the `let` is
followed by `mut` than that binding my be mutated. Throughout the language keywords are used first, to express the
purpose of the following text.

Furthermore, this helps with aligning similar statements into columns. All mutable variable names start in the same
location; All immutable variable names start in the same location. The programmer knows exactly where to look for name,
or the type.

The `:` is also a device reused throughout the language for denoting type information. For any text, if is preceded by a
colon, the programmer immediately knows it is a type.

/* TODO: this section could use more work, I need to take a second pass to clarify */

== Type System
The primary goal of Howlite as a project is to study expressive modeling of data, with minimal constraints. We create a
type system which allows modeling of common patterns in embedded programming, while avoiding constraints that force the
programmer into a specific pattern. For example, regions, borrow-checking, or linear types.

In practice this means a strong support for _subtype-polymorphism_, numeric subtypes, and explicit behavior with regard
to how data structures are encoded. The syntax and semantics are inspired by TypeScripts type system around JavaScript
Objects.

=== Type Parameters
```
type MyArray[T: any] {
  elements: &rw[T],
  length: uint
}
```

There are two kinds of type parameters in Howlite, "Boxed" and "Any". An "Any" type may only be referenced. They cannot
be inlined into the structure. For example, the following definition is illegal:

```
type Pair[L: any, R: any] {
  left: L,
  right: R
}
```

Box types may be included, with the limitation they may be no larger than the system's register size.

This limited parametric polymorphism give's the programmer the ability to get strong typing, without specializing the
datastructure. More importantly, parameterized procedures don't need to be specialized either, simplifying the calling
convention and allowing easy, strongly-typed interop with other languages.

For example: Say we want to write a definition for FreeRTOS `xTaskCreate`

#par(justify: false)[
```
type TaskFunction[P: any] = func(param: &rw P): unit;
type CStr = &0..127;
type TaskHandle = uint;

uint
xTaskCreate[P: any]( pvTaskCode: TaskFunction[P],
                     pcName: CStr,
                     uxStackDepth: uint32,
                     pvParameters: &P,
                     uxPriority: uint,
                     taskHandle: &w TaskHandle );
```
]\

=== Subtype Polymorphism
When passing by reference, a superset of the required type can be used instead of the requested type. The key limitation
is obvious: the types have to be _identical_ - meaning the size, alignment and order of the fields all must be
identical. To compensate for this limitation Howlite enforces strong rules on how types are encoded, and allows the
programmer to make explicit exceptions.

=== Type Encoding
If not otherwise specificed, all fields are aligned to 32 bit boundaries. They are encoded in the order specified. The
programmer may change field order with "decorators". Support is planned for the following decorators:

- `@align(bits)`, aligns all included fields in by `bits`.
- `@order(n)`, make this field the `n`'th in the encoded structure, no matter where it appears in the declaration. All
  following fields are shifted up by 1.
- `@size(bits)`, force the field to `bits` wide, throws an error if the type cannot be encoded in the specified number of
  bits.

= Compiler Architecture
The compiler is organized into three stages:

1. Parsing
2. Typechecking
3. Code generation

There is no intermediate representation, to ease implementation.

The compiler is written in Rust. We chose rust because the author had the most expierence with this language, and it has
a number of other programming languages also use rust for there compiler. This gives us a good set of references while
also causing minimal friction.

== Parsing

The lexer is generated by the Rust library "logos", and the parser by "lalrpop". "lalrpop" is an LR(1) parser generator
for Rust. we chose not write a custom recursive descent parser to speed development.

The generated parser directly emits an abstract syntax tree (AST). Errors are added directly into the AST, they have
they're own node type. Our AST is a parameterized type. Each node in the AST has a field containing arbitrary data.
We'll discuss this more in the next section, but initially this field has the unit type. In further iterations of the
parser it may be used to hold error information and syntax trivia (such as whitespace, keywords, etc.).

== Typechecking

Typechecking is performed via recursive descent of the AST. The typecher annotates each node with type information.
There is no type inference.

This part of the compiler is the biggest, and least developed. The current implementation is mostly in a single, rough
1,000 line file. Every node has a transformer method, which first recurses to the child nodes, by calling their
transformers, then based on the result add's its own type information.

== Code Generation

Similar to type checking the code generator works by recursively crawling the AST. Unlike type checking it does not
annotate each node (i.e. creating a new AST). Instead each node is transformed into a structure containing.

1. The compiled assembly
2. Error information (if any)
3. Where to find the result of any computation performed by the given node.

For example, the node corrosponding to `1 + 1` may return:

```s
li a0, 1
li a1, 1
add a0, a0, a1
```

The fundamental limitation of this approach is that the returned code is complete opaque to the calling procedure.

_Aside_: Technically, this is not true, the result _could_ be inspect, if the text was parsed and interpreted. But there
would be _much_ better ways to accomplish any benefits of that technique.

This limitation blocks a number of optimisations. However, the goal of this compiler is to get implement a complete
programming language. So, optimization is an afterthought.

Some care is taken to not emit completely absurd code. Immediates may be encoded as the result of an an expression. So,
with a small change to the compiler it would be possible to use immediate-arithmetic instructions, for example: Instead
of the above we could emit ```s
li a0, 1
add a0, a0, a1
```

= Current Implementation

The focus for this semester was to get a small core language to compile. This builds a solid foundation, which can be
iterated on next semester.

The most notable omissions are in the typechecker. Currently it doesn't handle any kind of polymorphism, and only has
basic support for numbers.

The compiler is well partitioned, each step outlined in section III is clearly seperated. An AST is generated by the
parser, passed through the typechecker then transformed into an assembler file. Each module has minimal dependencies on
any other.

== Syntax Progress
Type expressions support a subset of the possible scalars: `unit` and `bool`, and integer types. Support for number
(i.e. integer) ranges (`0..10`) has been established, although negative numbers are not included. Container types,
arrays and structures have been fully implemented in syntax. Named references to other types have also been implemented,
which may include type parameters. The most notable omission in the current type expression grammar is that there is no
union operator, or any kind of reference type.

Most of the top level definitions have been implemented: `type`, `func`, and `extern func`. The most notable omission is
proper module syntax. Each of the these definitions also has support for type parameters.

Expression syntax is lacking. There is only support for integer comparison operators, basic arithmetic operations, and
assignment. Bindings are supported via `let`, and may be referenced by name. Container type access is also supported..
Basic branching via `if` and `while` (`if`, `while`, and `let`, all have a value and type, allowing them to be used in
expressions).\
\
== Type Checking Progress
The most significant progress in the typechecker is crawling the AST, adding type annotations. The actual _checking_ of
these annotations is limited.

Simple types are checked, for example: ```
let a: bool = 5
``` But all statements aren't checked, for example function calls and returns. Typechecking is by far the pass with the
most holes.

The fundamental architecture of the type checker is solid; Attaching type information to AST nodes, then recursively
resolving types, has been useful for debugging and clarity of type checker code. We hope this pattern will also be
useful for providing clear and detailed error information, in the future.

Despite our belief in the architecture, the current implementation has a few critical flaws:

=== Performance
During type checking _the entire_ AST is copied in annotating it with type information. Alternative representations of
the AST could work around this limitation. We could extended the AST presented in _Compilers (Aho, 2007)_ /* TODO cite */.
In this implementation each node would be added to an array, and children would be referenced using an index into this
array. Then, extension data could be stored in a seperate, equally-sized array with each element corrosponding to the
AST Node at the same index (i.e. A Structure of Arrays)

=== Monolithic
The current AST walker type resides in a nearly 1,000 line file, with no clear way to break it into seperate parts.
Again, there are a few paths to remedy this, but similar to the performance issue it will require a fairly large
restructuring.

Both these points are two say: while the current type checker provides a rough outline of what we hope the final product
will look like, therewill likely need to be a large restructuring and rewrite of the system.

== Code Generation
Most features are fairly easily implemented in the code generator. Most expressions that can be parsed, and annotated
with types are able to transformed into assembler. The only notable omissions are around container types: Structure
accessories are not implemented, and there is only limited support for the RISC-V calling convention when working with
values larger than the register width.

There is no support for integers greater than the register size anywhere in the language

_ASIDE_: The actual integer representations used through the compiler are brittle, and badly needs to be re-thought. The
compiler just uses the pointer-sized signed integers everywhere, which may lead to unrepresentable numbers
(pointer-sized unsigned integers) or overflow, even before considering integers wider than a pointer.

Consequently, multi-register function arguments are an afterthough. Arrays and structures are always put on the stack,
even when they are small enough that the calling convention requires they be passed using registers. It's worth noting
multi-register return values are not supported either, although this is less critical, since 1) It's not as common to
return large values and 2) Only two registers are used for the return value.

== Documentation
There is documentation, though it entirely lives in the author's head. Most notes are out of date and irrelevant; One
could count the number of documented functions on a single hand (or, possibly, no hands); And there have been enough
changes that what few comments exist cause more harm than good.

= Roadmap

= Summary
