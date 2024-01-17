# Semantics of ReedLang

## Feature: Polymorphism

The programming language Cyclone takes an interesting approach to low-level polymorphism.

Type variables may be one of two __Kinds__:

- *Boxed Types* are types which fit within a `uintptr_t`
- *Any Types* are any type

*Any* type variables can only be indirected, while *Boxed* types can be used literally.


This is a useful concept, because it allows all versions of a polymorphic entity to share a single implementation.
For example:

<!-- TODO: Ask Simon / research more about existential vs universally quantified types... -->

```c
// In this imaginary C-like language we declare an existentially quantified struct over T, which is a type of kind Boxed
struct pair<T: Boxed> {
    T a;
    T b;
}

pair<T> swap<T>(pair: pair<T>) {
    pair<T> swapped = (Pair<T>){ .b = pair.a, .a = pair.b };
    return swapped;
}
```

Because `T` is boxed (i.e. `uintptr_t`), this program could always be reduced to the following, no matter what T is.

```C
struct pair_impl {
    uintptr_t a;
    uintptr_t b;
}

pair_impl swap_impl(pair_impl pair) {
    pair_impl swapped;
    swapped.b = pair.a
    swapped.a = pair.b
    return swapped;
}
```

Examples and concepts adapted from[^1]

## Citations

[^1]: Grossman, Dan. “Quantified Types in an Imperative Language.” ACM Transactions on Programming Languages and Systems 28, no. 3 (May 2006): 429–75. https://doi.org/10.1145/1133651.1133653.
