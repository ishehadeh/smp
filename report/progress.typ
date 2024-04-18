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
= Compiler Architecture

= Current Implementation

= Roadmap

= Summary
