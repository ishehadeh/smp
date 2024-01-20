# Syntax


## Examples

```

type Hello = {
    variable member1: type;
    readonly member2: type;
    constant abc: type = 1 + 1;
};

// Hello is a subtype of HelloExt
type HelloExt = {
    varaible member1: type;
    readonly member2: type;
    constant abc: type = 1 + 1;

    variable secret_member: type = abc
}

// integer range type, that is distinct from <0 .. 5>
type Test = new <0 .. 5>;

func hello() {
    variable member: type = value;
    variable h: HelloExt = {...}
    // { member1: type = a; member2: type b; }
    // ^ subtype of Hello
    h = { member1: type = a; member2: type b; }

    // distinct types must be qualified;
    const a: Test = Test 3;
    variable b: <0 .. 3> = a    // illegal
}

```