# Types and Patterns

Beyond the built-in types (`Bit`, `Word[n]`, `Clock`, `Reset`, `Valid[T]`),
Virdant lets you define your own types with `enum type`, `struct type`,
and `union type`.

## Enum types

An enum type maps named values to concrete bit patterns.
The values are known at compile time and can be used in `match` expressions.

```virdant
enum type Phase {
    Green  = 1w4
    Yellow = 2w4
    Red    = 4w4
    Walk   = 8w4
}
```

Access an enum variant with the `#` prefix:

```virdant
wire p : Phase
p := #Green
```

Match on enum variants with `#`:

```virdant
out := match phase {
    with #Green  => x + y
    with #Yellow => 2
    with #Red    => 0
    with #Walk   => 5
}
```

Enum variant names begin with an uppercase letter (camel case, same as
module and type names).

### One-hot encoding by default

When NO enumerant specifies a value, the enumerants are one-hot
encoded: enumerant `i` (in declaration order) gets the value `1 << i`,
and the width of the enum type is the number of enumerants.

```virdant
enum type State {
    Idle   // value 1 (1 << 0)
    Busy   // value 2 (1 << 1)
    Done   // value 4 (1 << 2)
}
```

* `State` above is exactly 3 bits wide: `word(state)` yields `Word[3]`
  with `#Idle == 1`, `#Busy == 2`, `#Done == 4`.
* Prefer the one-hot default for state-machine-like enums:
  - The all-zeros word is never a valid enumerant, so a signal
    explicitly initialized to zero can never be confused with a
    legitimate enum value.
  - Testing which enumerant a signal holds needs only a single bit.
* An enum that specifies values explicitly is unaffected by the
  default.

### Explicit values: all or nothing

* Either give EVERY enumerant a value, or NONE (which defaults to
  one-hot). Giving values to only some enumerants is an error
  (`EnumPartiallySpecified`).
* The enum's width is taken from the FIRST enumerant's literal.
  Write the first value with an explicit width (`1w4`); the remaining
  values may be bare literals:

```virdant
enum type Opcode {
    Add = 1w3      // explicit width pins the enum at Word[3]
    Sub = 2        // bare literals from here on
    Nop = 0
    Xor = 5
}
```

* If the first literal's width is not inferrable, the enum's width
  cannot be determined; write `A = 0wN` instead of `A = 0`.
* Every enumerant must have a distinct value. Duplicates are an error
  even when written in different bases: `A = 15w4` and `B = 0x0F`
  report `Duplicate enum value: enumerants of Code share value 15`.

## Payload enumerants

An enumerant may carry parameters, like a union variant:

```virdant
enum type Opt[T : Type] {
    None
    Some(value : T)
}
```

* An enumerant WITH parameters is constructed with `@`, exactly like
  a union variant: `o := @Some(14w4)`.
* An enumerant WITHOUT parameters is constructed with `#`, and may
  also be written with an empty argument list: `#None` or `@None()`.
* Using `#Name` on a parameter-carrying enumerant is an error:
  `o := #Some` reports `Ctor Some requires arguments: use @Some(...)`.
* Match parameter-carrying enumerants with `with @Name(pattern)`,
  which binds the parameters positionally. Use `_` to discard a
  payload value:

```virdant
wire inner : Word[4]
inner := match opt {
    with @Some(v) => v
    with #None    => 0
}
```

* An exhaustive match must cover every enumerant, with or without
  parameters; payload enumerants must be covered with their
  constructor pattern (`@Some(v)`, not `#Some`).
* In hardware, an enum whose enumerants carry no parameters is a tag
  of the enum's width. When some enumerants carry parameters, the
  representation is a tag followed by the widest payload, like a
  union.

## `cast()` for enum conversion

Convert between an enum type and its underlying `Word[n]` type with
the `cast()` function:

```virdant
wire opcode : Opcode
opcode := cast(opcode_bits)      // Word[4] -> Opcode
```

Conversion works in both directions.
The underlying width is determined by the enum's value widths.

## Struct types

A struct type bundles several named fields into a single value.

```virdant
struct type Color {
    red   : Word[8]
    green : Word[8]
    blue  : Word[8]
}
```

Struct field names begin with a lowercase letter (snake case).

### Struct literals (`${}`)

Create a struct value with `${}` syntax.
Inside, assign each field with `field = expr`.

```virdant
color := ${
    red   = 0,
    green = 128,
    blue  = 255,
}
```

Trailing commas are allowed but not required.
Put each field on its own line for readability.
Align the `=` signs if it makes the values easier to scan.

### Field access (`->`)

Access a struct field with the `->` operator:

```virdant
red := color->green
```

The `->` operator is a projection that extracts a named field.
It also works on socket channels and may be used for chaining
(e.g., `obj->field_a->sub_field`).

## Union types

A union type (also called a tagged union or sum type) holds one of
several variants, each optionally carrying payload data.

```virdant
union type UartState {
    Idle()
    Start(pulse : Word[11])
    Bit(bit : Word[3], pulse : Word[11])
    Stop(pulse : Word[11])
}
```

Variant names begin with an uppercase letter (camel case).
Payload field names begin with a lowercase letter (snake case).

### Constructor expressions (`@Variant(args)`)

Create a union value with `@Variant(args)`:

```virdant
wire s : UartState
s := @Idle()
s := @Start(1250w11)
s := @Bit(0, 1250w11)
```

### Pattern matching on unions

Use `match` with `@Variant(args)` patterns to destructure a union.
Each arm introduces variables for the payload fields.

```virdant
ready := match state {
    with @Idle() => true
    with @Start(pulse) => false
    with @Bit(bit, pulse) => false
    with @Stop(pulse) => false
}
```

The `else` arm catches any unlisted variant:

```virdant
valid := match state {
    with @Done(result) => true
    else => false
}
```

### Sub-patterns and variable binding

A pattern arm can inspect the payload values directly.

Concrete sub-patterns match exact values:

```virdant
out := match u {
    with @Nothing()         => false
    with @Pair(true, false) => true
    else                    => false
}
```

Variable sub-patterns bind the payload to a name that can be used
in the arm's expression:

```virdant
out := match u {
    with @Nothing()         => false
    with @Pair(x, false)    => x
    else                    => false
}
```

You can mix concrete and variable sub-patterns in the same arm,
as shown with `@Pair(x, false)` above.

Use `_` to discard a payload value you do not need:
`with @Pair(_, false) => true`.

### Using `@Invalid()` and `@Valid(x)` with `Valid[T]`

The built-in type `Valid[T]` is an optional value that wraps any type
`T` with a validity bit. It is an ordinary generic payload enum
defined in the builtin package --- no special compiler support:

```virdant
pub enum type Valid[T : Type] {
    Invalid
    Valid(value : T)
}
```

```virdant
wire v1 : Valid[Bit]
v1 := @Valid(true)
v1 := @Invalid()

got_it := match v1 {
    with @Valid(x) => x
    with @Invalid() => false
}
```

* `Valid[T]` is one-hot encoded like every enum: its total hardware
  width is `width(T) + 2` --- a 2-bit tag distinguishing `@Valid`
  from `@Invalid`, above the payload.
* `#Invalid` also matches; `@Invalid()` is the empty constructor
  pattern for it.

## Dynamic indexing (`dyn`)

Index a word by an expression with the `dyn` keyword:

```virdant
wire arr : Word[8]
wire idx : Word[3]
wire out : Bit

out := arr[dyn idx]
```

* The subject must be `Word[n]` and the index `Word[k]`, with the
  constraint `n == 2^k` (`n = 8, k = 3` is valid; `n = 8, k = 2` is a
  type error).
* The result type is `Bit`.
* Indices are zero-based: `data[0]` is the least significant bit.
* `Word[1]` indexed by `Word[0]` is valid (`2^0 = 1`).
* Use constant indexing (`data[3]`) or ranges (`data[7..4]`) when the
  index is known at compile time; see ./literals-and-builtins.md.

## Type ascription (`expr : Type`)

You can annotate an expression with its type using the `:` operator.
This is useful when the type cannot be inferred or to document intent.

```virdant
it := 0w8 : Word[8]
it := @Valid(3) : Valid[Word[8]]
```

## Patterns that parse but are not supported

* The grammar also admits bare `Ident` and `dontcare` patterns, but
  the match-coverage checker does not yet handle them correctly.
* Do NOT use bare-Ident or bare-`dontcare` patterns in `match` arms.
* To discard a payload value in a constructor pattern, use `_`:
  `with @Some(_) => true`.
