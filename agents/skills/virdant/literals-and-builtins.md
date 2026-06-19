# Literals and Built-in Functions

This document covers the syntax for numeric literals and the built-in
functions that operate on `Bit` and `Word[n]`.

## Numeric literals

A plain integer literal (e.g., `0`, `42`, `100000`) infers a width
from its context.

```virdant
wire n : Word[4]
n := 0               // width inferred as 4 from the type
```

To specify the width explicitly, append `w` followed by the bit width:

```virdant
wire n : Word[4]
n := 0w4             // explicit: Word[4]

wire m : Word[32]
m := 100w32           // explicit: Word[32]
```

## Base prefixes

Binary literals use the `0b` prefix:

```virdant
wire b : Word[4]
b := 0b1010
```

Hexadecimal literals use the `0x` prefix:

```virdant
wire h : Word[8]
h := 0xfe
```

You can combine a base prefix with an explicit width suffix:

```virdant
wire b : Word[4]
b := 0b1010w4

wire h : Word[8]
h := 0xfew8
```

## Underscore separators

Use underscores to group digits for readability.
They are ignored by the compiler.

```virdant
wire n : Word[32]
n := 100_000

wire b : Word[7]
b := 0b00_101_11

wire h : Word[32]
h := 0xcafe_babe
```

## `word(...)` constructor

The `word()` function concatenates bits and sub-words into a wider word.
Arguments are listed from most-significant to least-significant.

```virdant
// Concatenate four bits into Word[4]
sum := word(s3, s2, s1, s0)

// Prepend a bit onto a word (shift-left-and-insert pattern)
r <= word(r[3..0], serial_in)
```

The width of the result is the sum of the widths of all arguments.
Each argument must be `Bit` or `Word[n]`.

## `zext()` and `sext()`

Zero-extend or sign-extend a word to a wider target.

- `zext(x)` — zero-extend.
  Pads the most-significant bits with `0`.
- `sext(x)` — sign-extend.
  Pads the most-significant bits with the original most-significant bit.

```virdant
wire z : Word[8]
z := zext(0x8w4)    // result: 0x08

wire s : Word[8]
s := sext(0x8w4)    // result: 0xf8 (0x8 = 0b1000, MSB is 1)
```

The target width is inferred from the context of the assignment.

## `cast()`

Reinterpret the bit pattern of a `Word[n]` as an enum type, or vice versa:

```virdant
opcode := cast(opcode_bits)   // Word[4] -> Opcode
```

See `types-and-patterns.md` for more details.

## `dontcare`

Use `dontcare` to indicate that a signal's value is intentionally
unspecified.
This suppresses warnings about unconnected outputs and may enable
optimizations.

```virdant
wire bit_x : Bit {
    it := dontcare
}

wire word_x : Word[8] {
    it := dontcare
}
```

`dontcare` can be assigned to any wire (of any type) and to outgoing
ports.

It's especially useful as the RHS of `when` and `match` cases where the value is irrelevant or unused.

## `mux()` ternary selector

`mux(sel, a, b)` returns `a` when `sel` is true and `b` when `sel` is false.
Use it for simple ternary selection instead of a `when` expression.

```virdant
r <= mux(reset, 0, r + 1)

state <= mux(data_valid, @Start(1250w11), @Idle())
```

Both `a` and `b` must have the same type.

## Operators on `Bit` and `Word[n]`

Virdant provides the usual operators:

- `Bit`: `!` (not), `&&` (and), `||` (or), `^^` (xor)
- `Word[n]`: `&` (and), `|` (or), `^` (xor), `+`, `-`, `==`, `!=`,
  `<`, `>`, `<=`, `>=`, `<<`
- `any(x)` — true if any bit in `x` is set.
  Equivalent to `x != 0`.
- `word(...)` — concatenation (see above).
