# Generics

Generic parameters make type, module, and socket definitions reusable at
many sizes/types. Parameters are declared in square brackets after the
definition name, each written `name : kind`.

## Rules

- Generic parameters may be declared on **every** definition form:
  `struct type`, `union type`, `enum type`, `builtin type`, `mod`, and
  `socket`.
- Parameter list syntax (shared by all forms):
  `[Name : Kind]`, comma-separated: `[A : Type, B : Type]`.
- There are exactly **two kinds**:
  - `Type` — the parameter stands for a whole type (`Bit`, `Word[4]`, ...).
  - `Nat` — the parameter is a number literal, used for widths (`Word[n : Nat]`).
  Any other kind name is an error.
- Parameter names are in scope only inside the body of the declaring
  definition (member types, port types, channel types).
- References supply **type arguments** in `[...]`, comma-separated, in
  declaration order: a `Nat` parameter takes a number literal, a `Type`
  parameter takes a full type expression (so arguments nest arbitrarily).
- The number and kinds of the arguments are checked at every reference site.
- A generic definition is analyzed once, generically; concrete types exist
  only at the instantiation sites.
- Mark generic definitions `pub` when other packages instantiate them.

## Generic type definitions

```virdant
pub struct type Pair[A : Type, B : Type] {
    fst : A
    snd : B
}

pub union type Either[L : Type, R : Type] {
    Left(l : L)
    Right(r : R)
}

pub enum type Opt[T : Type] {
    None
    Some(value : T)
}

builtin type Word[n : Nat] {}
```

## Type arguments at use sites

```virdant
wire pair : Pair[Word[4], Bit]
reg pair : Pair[Word[4], Bit] {
    it <= ${ fst = a, snd = b }
}
```

Arguments nest to arbitrary depth — a `Type` argument is a full type
expression:

```virdant
wire nested : Valid[Pair[Word[4], Bit]]
nested := @Valid(${ fst = a, snd = b })
```

Enum payload construction uses `@Name(...)` for payload variants and
`#Name` for bare variants, as usual (see ./types-and-patterns.md):

```virdant
wire opt : Opt[Word[4]]
opt := match b {
    with true => @Some(a)
    with false => #None
}
```

## Generic modules

A generic `mod` is a template. It is not emitted to Verilog on its own and
has no concrete port widths until instantiated.

```virdant
mod Reg[T : Type] {
    incoming clk : Clock
    incoming d : T
    reg slot : T {
        it <= d
    }
    outgoing q : T
    q := slot
}
```

Inside the body, `on` is implicit when the module has exactly one `Clock`
component (here the `incoming clk`), same as for non-generic modules.

Instantiate with type arguments after the module name:

```virdant
mod r8 of Reg[Word[8]] {
    it.clk := clk
    it.d := inp
}
mod r16 of Reg[Word[16]] {
    it.clk := clk
    it.d := wide
}
```

Inside an instantiation's it-block the parameters are substituted:
`it.d` of `r8` is `Word[8]`, of `r16` is `Word[16]`. Connecting a
`Word[8]` signal to `r16.d` is rejected:
`Wrong type: Expected Word[16] but found Word[8]`.

## Mono modules

- Each distinct combination of (module definition, type arguments) is a
  **mono module**.
- Two instances with identical arguments share one mono module; a generic
  module that is never instantiated emits nothing.
- Verilog generation emits exactly one Verilog module per mono module.
  The Verilog name carries a hash of the type arguments, and a `//`
  comment above it records the readable origin:

```verilog
// generics_instances::Reg[Word[8]]
module \generics_instances::Reg#bde2499a (
    input  wire             clk,
    input  wire      [7:0]  d,
    output wire      [7:0]  q
);
    ...
endmodule

// generics_instances::Reg[Word[16]]
module \generics_instances::Reg#e5a295e4 (
    ...
```

## Reference-site checks (actual error messages)

- **Wrong arity** — `incoming inp : Pair[Bit]` against
  `Pair[A : Type, B : Type]`:
  `Type Pair takes 2 type arguments, but 1 was given`
- **`Type` parameter given a number** — `Valid[8]`:
  `Type argument mismatch: parameter T of Valid expects a type, but found a Nat literal`
- **`Nat` parameter given a type** — `Word[Bit]`:
  `Type argument mismatch: parameter n of Word expects a Nat, but found a type`
- **Unbound parameter** — referencing `T` outside its declaring definition
  (e.g. an `incoming inp : T` port in a non-generic module):
  `Unresolved type T`
- **Parameter takes arguments** — `T[Bit]` inside `struct type Box[T : Type]`:
  `Parameter T takes no type arguments`
- **Unknown kind** — `struct type S[X : Foo]`:

```text
Unknown kind Foo: generic parameters must be declared as `: Type` or `: Nat`
```

The same checks apply at module and socket instantiation sites, and the
messages use the package-qualified name:

```virdant
client socket p of Pipe          // Pipe is generic
mod f of Keep                    // Keep is generic
```

reports:

```text
Type builtin::Pipe takes 1 type argument, but 0 were given
Type arity::Keep takes 1 type argument, but 0 were given
```

## Restrictions

- An `ext mod` cannot have generic parameters — this is a hard rule, not
  a missing feature. External modules parameterize through `@parameter`
  annotations instead (see ./modules.md):
  `ext mod BadCell[T : Type]` reports
  `ext mod ext_mod::BadCell cannot have generics: ext mods do not
  support type parameters (use @parameter annotations instead)`
- An ext-mod **instantiation** cannot take type arguments either, even
  when the ext mod declares none: `mod f of Cell[Word[8]]` reports
  `ext mod ext_mod::Cell cannot have generics` (same error).
- A generic module cannot be the top: `[prog] top` in `virdant.toml` must
  name a concrete module — a generic definition has no instantiation.
  `pub mod`s may be generic; only tops may not. Building a project whose
  `[prog] top` names a generic module reports
  `top module top::Fifo cannot have generic parameters:
  a top module must be a concrete instantiation`
  at the bitstream/platform stage.

## Cross-references

- Generic sockets and type arguments on socket instances: ./sockets.md
- Instances, it-blocks, ext mods, `@parameter`: ./modules.md
- User-defined types and pattern matching on generic payloads:
  ./types-and-patterns.md
