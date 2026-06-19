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
    case #Green  => x + y
    case #Yellow => 2
    case #Red    => 0
    case #Walk   => 5
}
```

Enum variant names begin with an uppercase letter (camel case, same as
module and type names).

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
    case @Idle() => true
    case @Start(pulse) => false
    case @Bit(bit, pulse) => false
    case @Stop(pulse) => false
}
```

The `else` arm catches any unlisted variant:

```virdant
valid := match state {
    case @Done(result) => true
    else => false
}
```

### Sub-patterns and variable binding

A pattern arm can inspect the payload values directly.

Concrete sub-patterns match exact values:

```virdant
out := match u {
    case @Nothing()         => false
    case @Pair(true, false) => true
    else                    => false
}
```

Variable sub-patterns bind the payload to a name that can be used
in the arm's expression:

```virdant
out := match u {
    case @Nothing()         => false
    case @Pair(x, false)    => x
    else                    => false
}
```

You can mix concrete and variable sub-patterns in the same arm,
as shown with `@Pair(x, false)` above.

### Using `@Invalid()` and `@Valid(x)` with `Valid[T]`

The built-in type `Valid[T]` is a union with two variants:

```virdant
wire v1 : Valid[Bit]
v1 := @Valid(true)
v1 := @Invalid()

got_it := match v1 {
    case @Valid(x) => x
    case @Invalid() => false
}
```

## Type ascription (`expr : Type`)

You can annotate an expression with its type using the `:` operator.
This is useful when the type cannot be inferred or to document intent.

```virdant
it := 0w8 : Word[8]
it := @Valid(3) : Valid[Word[8]]
```
