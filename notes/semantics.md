# Semantics of Howlite

## Feature: Polymorphism

The programming language Cyclone takes an interesting approach to low-level polymorphism.

Type variables may be one of two __Kinds__:

- *Boxed Types* are types which fit within a `uintptr_t`
- *Any Types* are any type

*Any* type variables can only be indirected, while *Boxed* types can be used literally.


This is a useful concept, because it allows all versions of a polymorphic entity to share a single implementation.
For example:

<!-- TODO: Ask Simon / research more about existential vs universally quantified types... -->
<!-- TODO: Re-read section 4.4, I don't *completely* understand it, but I think the "no withness changes" solution makes the most sense -->

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

```c
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


## Feature: Layouts & Types

sorta related: [^2], p 133

- **Layout**: A Layout (or storage type) is the literal encoding of data in memory and on disk. (TODO: this could also be called a *shape*? or just *encoding*)
- **Types**: A type is a restriction on the possible values of a variable.

Control the actual bits values are compiled into is an important feature for a low-level language.

> Exmaple: say your writing to memory-mapped IO ports, the location and physical representation are important
> ```c
>   // the layout for writing to an 8-bit 
>   struct my_mmio {
>       bit_t flag_a;  
>       bit_t flag_b;  
>       bit_t flag_c;  
>       bit_t flag_d;  
>       uint4_t value;  
>   }
>   
>   static volatile struct my_mmio* MY_MMIO = 0xDEADBEEF
> ```

Rarely, though do the higher-level **Types** align perfectly with these values.
For example, each of `flag_` struct members above would make much more sense as a boolean, not a single bit value.
But, it doesn't necessarily make sense for a boolean to always be stored in a single bit.
Often, it'll be stored in a register, or it could be packed into a byte.

I hope to implement integer range tracking, while allowing the specific encoding of an integer to be specificed separately.

## Immutability


Struct members and variables can be marked as immutable. An immutable value disallows any assignment to the value or any interior values

```c
struct test {
    variable a: int;
    readonly b: int;
};

variable ex1: test;
test.a = 2 // ok
test.b = 3 // not ok

readonly ex2: test;
ex2.a = 2 // not ok
ex2.b = 3 //not ok
```


## Linearity

i.e. Use-once variables.

## Modules

TODO

## Name Aliasing

Howlite does not support namespaces - exactly.

One of the primary goals of this language is easy interoperation with other languages.
As a result, the names (and calling conventions (TODO)) of compiled functions and values should be transparent.

But namespaces are nice! So, Howlite implements a similar scheme to Zig:
Modules and type members are syntactic sugar for `<Container>_Member`.

## References

"Reference" types are an opaque reference to a location in memory.
Unlike C pointers, references are *Non-nullable*, *Non-numeric* types.

References can be cast to integer types in order to do arithmetic on them.

## Stupid Monads; Intelligent Goto

```txt

type Error[T: Boxed] = // ...

func unwrap[T](e: Error[T])

func error(): unit {

}

```

## Bibliography

[^1]: Grossman, Dan. “Quantified Types in an Imperative Language.” ACM Transactions on Programming Languages and Systems 28, no. 3 (May 2006): 429–75. https://doi.org/10.1145/1133651.1133653.
[^2]: Krishnamurthi, Shriram. “Programming Languages: Application and Interpretation,” n.d.
