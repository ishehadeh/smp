#import "@preview/charged-ieee:0.1.0": ieee

#let abstract = [
  We introduce the Howlite programming language.
  Where it's from, and where it's going.
]

#show: ieee.with(
  title: [The Howlite Programming Language - First Semester Progress],
  authors: (
    (
      name: "Ian Shehadeh",
      department: [Computer Science],
      organization: [St. Mary's College of Maryland],
      location: [St. Mary's City, Maryland],
      email: "irshehadeh@smcm.edu"
    ),
  ),
  bibliography: bibliography("refs.bib"),
)

= Introducing Howlite

The Howlite (abbr. _hlt_) programming language is a small systems programming language aimed at embedded devices and very low-level performance critical systems.

== Project Goals

= Language Design

Before describing the language, consider this example of a non-recursive fuction to get the `n`#super[th] number in the fibonacci sequence.

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

Howlite's syntax is designed to be clear, concise and most importantly: immediately legible to those familiar with "c-like" languages. We use curly-braces to mark blocks, square brackets for arrays, and semicolons as statement terminators.

The departures from this well-established lineage is more notable.

=== Reversing Binding/Type Order
In Howlite, variable types, parameter types and function return types are the last part of the declaration, as opposed to C, where they lead. For example consider the equivalent snippets:


#figure(caption: "Binding Syntax Comparison")[
  #grid(
      columns: (2fr, 18fr),
      rows: (auto, auto),
      gutter: 8pt,
      align: left,
      [*c*], `uint16 fib(uint16 n)`,
      [*hlt*], [`func fib(n: uint16): uint16`]
  )


  #grid(
      columns: (2fr, 18fr),
      rows: (auto, auto),
      gutter: 8pt,
      align: left,
      [*c*], `uint16 acc = 0`,
      [*hlt*], [`let mut acc: uint16 = 0`]
  )
]

We prefer this prefix notation because it prioritizes _alignment_ and _orders information_ from most general to least right-to-left.

Ordering of information is important when reading long series of declarations. When every mutable variable is prefixed with "`let mut`" and every immutable variable prefixed with "`let`", it makes it easy to quickly find variable names when scanning a function declaration. Every line starting with "let" binds a name to some value. If the `let` is followed by `mut` than that binding my be mutated. Throughout the language keywords are used first, to express the purpose of the following text.

Furthermore, this helps with aligning similar statements into columns. All mutable variable names start in the same location; All immutable variable names start in the same location. The programmer knows exactly where to look for name, or the type.

The `:` is also a device reused throughout the language for denoting type information. For any text, if is preceded by a colon, the programmer immediately knows it is a type.

/* TODO: this section could use more work, I need to take a second pass to clarify */

== Type System
The primary goal of Howlite as a project is to study expressive modeling of data, with minimal constraints. We create a type system which allows modeling of common patterns in embedded programming, while avoiding constraints that force the programmer into a specific pattern. For example, regions, borrow-checking, or linear types.

In practice this means a strong support for _subtype-polymorphism_, numeric subtypes, and explicit behavior with regard to how data structures are encoded. The syntax and semantics are inspired by TypeScripts type system around JavaScript Objects. 

=== Type Parameters
```
type MyArray[T: any] {
  elements: &rw[T],
  length: uint
}
```

There are two kinds of type parameters in Howlite, "Boxed" and "Any". An "Any" type may only be referenced. They cannot be inlined into the structure.
For example, the following definition is illegal:

```
type Pair[L: any, R: any] {
  left: L,
  right: R
}
```

Box types may be included, with the limitation they may be no larger than the system's register size.

This limited parametric polymorphism give's the programmer the ability to get strong typing, without specializing the datastructure.
More importantly, parameterized procedures don't need to be specialized either, simplifying the calling convention and allowing easy, strongly-typed interop with other languages.

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
When passing by reference, a superset of the required type can be used instead of the requested type. The key limitation is obvious: the types have to be _identical_ - meaning the size, alignment and order of the fields all must be identical. To compensate for this limitation Howlite enforces strong rules on how types are encoded, and allows the programmer to make explicit exceptions.


=== Type Encoding
If not otherwise specificed, all fields are aligned to 32 bit boundaries. They are encoded in the order specified. The programmer may change field order with "decorators". Support is planned for the following decorators:

- `@align(bits)`, aligns all included fields in by `bits`.
- `@order(n)`, make this field the `n`'th in the encoded structure, no matter where it appears in the declaration. All following fields are shifted up by 1.
- `@size(bits)`, force the field to `bits` wide, throws an error if the type cannot be encoded in the specified number of bits.

= Compiler Architecture

= Current Implementation

= Roadmap

= Summary
