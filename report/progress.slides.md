% Howlite
% Ian Shehadeh
% April 29, 2024


## Syntax

- C-like with a dash of Typescript and Rust
- Stick to popular conventions
- Abbreviations: "bool", "int", etc.
- Symbols: "+", "-", "<", ">", etc.


-----

```
func fib(): u16 {
    let N: u16 = 10;
    let mut memory: [u16; 2] = [ 0, 1 ];
    let mut acc: u16 = 0;

    let i: u16 = 0;
    while i < N {
        acc = (memory.[0] + memory.[1]);
        memory.[0] = memory.[1];
        memory.[1] = acc;
        i = (i + 1);
    };

    acc
}
```

---

## Semantics

- Imperative
- Eager
- Typing: Static
- Scopes: Lexical

---

# Language Tour
## Expressions

--- 

```
if 1 + 2 == 3 {
  ...
} else {
  panic("???")
}
```

---


```
let a: bool = {
  let a: 0..0 = 0;
  1 == a
};
```

---

```
for let i: uint in 0..10 {
    arr.[i]
}

for a < b {
  arr.[i]
}
```

---

```
switch value {
  case 1: ... // implicit break
  case 2: ... // implicit break
  // etc...
  Default: ...
}
```

---

# Types

--- 

## Integers

```
let dec: 0..255 = 128;
let hex: 0..255 = 0x80;
let bin: 0..255 = 0b1000_0000;
let oct: 0..255 = 0o200;
```

---

## Null

```
let nothing: null = null;
```

---

## Boolean

```
let b: bool = true
```

---

## IEEE 754 Floats

```
let e: float64 = 2718e-3;
let pi: float32 = 3.14;
```

---

## References

```
let b_ref: &bool = &b;
```

---

## Arrays

```
let array: [bool; 4] = [true, false, true, false];

if array.[0] == array.[3] { /* ... */ }
```

---

## Slices (Fat Pointers)

```
let slice: &[bool] = &array.[1..3];
```

---

## Characters

```
let h: char = 'h';
```

---

## Strings

```
let greet: &string = "hello ";
```


---

## Structures

```
type A[T: boxed] = {
  a: T,
  @align(4) {
    b1: 0..5,
    b2: 5..10
  },
  c: u16
};
```


---

```
let a: A[0..2] = struct { a: 2, b1: 0, b2: 5, c:  0 }

a.a + a.b1 + a.b2
```

---

## Unions

```
type U = bool | null;

let a: U = true;
if a == true { }      /* type error */
```

----

```
type V = { a: 1..1, b: null } 
       | { a: 2..2, b: bool };

let v : V = struct { a: 1, b: null };
if v.a == 1 {
  /* v : { a: 1..1, b: null } */
}
```

---

```
type TaskFunction[P: any] = func(param: &mut P): null;
type CStr = &[0..127];
type TaskHandle = uint;
extern func
xTaskCreate[P: any]( pvTaskCode: TaskFunction[P],
                     pcName: CStr,
                     uxStackDepth: uint32,
			         pvParameters: &mut P, 
			         uxPriority: uint, 
			         taskHandle: &mut TaskHandle );
```

---

# Compiler


```dot
digraph Compiler {
    bgcolor="transparent"
    rankdir="LR"
    P [label="Parser", shape=square]
    T [label="Typechecker", shape=square]
    C [label="Code Generation", shape=square]
    L [label="Assemble/Link", shape=square]
    P -> T [label="AST"]
    T -> C [label="Type Annotated AST"]
    C -> L [label="Assembler"]
}
```

---

## Typecheck

```dot
digraph TC {
    bgcolor="transparent"
    node[shape=record];

    subgraph cluster_0 {
        label="[1] AST for 1 + 1"
        peripheries=0
        margin=20

        a[label="+"]
        a_1[label="1"]
        a_2[label="1"]
        a -> a_1
        a -> a_2
    }

    subgraph cluster_1 {
        label="[2] Type Child Nodes"
        peripheries=0
        margin=20

        b[label="+"]
        b_1[label="1 : int"]
        b_2[label="1 : int"]
        b -> b_1
        b -> b_2
    }

    subgraph cluster_2 {
        label="[3] Type Parent"
        peripheries=0
        margin=20

        c[label="+ : int"]
        c_1[label="1 : int"]
        c_2[label="1 : int"]
        c -> c_1
        c -> c_2
    }
}
```

---

### Type Error

```dot
digraph TCBad {
    bgcolor="transparent"
    node[shape=record];

    subgraph cluster_0 {
        label="[1] AST for 1 + 'a'"
        peripheries=0
        margin=20

        a[label="+"]
        a -> "1"
        a -> "'a'"
    }

    subgraph cluster_1 {
        label="[2] Type Child Nodes"
        peripheries=0
        margin=20

        b[label="+"]
        b_1[label="1 : int"]
        b_2[label="'a' : char"]
        b -> b_1
        b -> b_2
    }

    subgraph cluster_2 {
        label="[3] Note Error"
        peripheries=0
        margin=20

        c[label="+ : int [error]"]
        c_1[label="1 : int"]
        c_2[label="'a' : char"]
        c -> c_1
        c -> c_2
    }
}
```

---

## Code Generation

```dot
digraph CG {
    bgcolor="transparent"
    node[shape=record];

    subgraph cluster_0 {
        label="[1] Typed AST for 1 + 1"
        peripheries=0
        margin=20

        a[label="+ : int"]
        a_1[label="1 : int"]
        a_2[label="1 : int"]
        a -> a_1
        a -> a_2
    }

    subgraph cluster_1 {
        label="[2] Compile LHS"
        peripheries=0
        margin=20

        b[label="+ : int"]
        b_1[label="li a0, 1"]
        b_2[label="1 : int"]
        b->b_1[headlabel="a0" dir=back labelangle=60.0, labeldistance=1.2]
        b->b_2
    }

    subgraph cluster_2 {
        label="[3] Compile RHS"
        peripheries=0
        margin=20

        c[label="+ : int"]
        c_1[label="li a0, 1"]
        c_2[label="li a1, 1"]
        c->c_1[headlabel="a0" dir=back labelangle=60.0, labeldistance=1.5]
        c->c_2[headlabel="a1" dir=back labelangle=-60.0, labeldistance=1.5]
    }

    subgraph cluster_3 {
        label="[4] Compile '+'"
        peripheries=0
        margin=10
        d[label="+ : int"]
        d_1[label="\<removed\>"]
        d_2[label="\<removed\>"]
        d->d_1[dir=back]
        d->d_2[dir=back]
        d[label="li a0, 1\nli a1, 1\nadd a0, a0, a1"]
    }
}
```

---

# This Semester

- Language Design
- Compiler Architecture
- Base Language

---

## Unimplemented

- Unions
- References
- Mutability Guarentees
- Type narrowing
- Polymorphism
- Modules (!)

---

## Next Steps

- Cleanup codebase
- Design modules
- Constraint solving and unification
- Copy & patch compilation
- Write a thorough test suite

---

# Questions