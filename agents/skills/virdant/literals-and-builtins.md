# Literals and Built-in Functions

This document covers the syntax for numeric literals and the built-in
functions that operate on `Bit` and `Word[n]`.

## String literals

A string literal is text in double quotes.  Between the quotes, any byte
except `"` and `\\` stands for itself; a backslash introduces a C-style
escape: `\\a \\b \\e \\f \\n \\r \\t \\v`, `\\\\ \\' \" \\?`, octal `\\ooo` of one
to three octal digits (`\\0` is the NUL byte), and hex `\\xNN` of exactly
two hex digits (`\\x0d` is carriage return).  Escapes are decoded where
string bytes are consumed — e.g. by `@image_inline` (see `./memory.md`).

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

### Over-wide inferred literals are errors, not truncation

If an inferred-width literal's value does not fit the inferred width,
it is a hard compile error (`DoesntFit`) — the value is never
silently truncated:

```virdant
wire n : Word[4]
n := 16              // ERROR: 16 needs 5 bits (Value doesn't fit)
n := 15              // OK: 15 is the Word[4] maximum
```

If you want a wider value, widen the type; if you want truncation,
do it explicitly with `trunc` (see below).

The behavior differs for explicit-width literals: `0xffw4` declares
itself as `Word[4]` and the value is silently truncated to it
(it evaluates to `0xf`).
Prefer writing the value to fit the width you declare — an
over-wide `w` suffix is usually a typo.

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
Make use of them for strange groupings of bits
(such as RISC-V's 5.2 opcodes)
or for 32- or 64-bit addresses
(such as 0x8000_0000 or 0x8000_0000_0000_0000).

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

## `zext()`, `sext()`, and `trunc()`

Change the width of a word. The target width is inferred from the
context of the assignment.

- `zext(x)` — zero-extend.
  Pads the most-significant bits with `0`.
- `sext(x)` — sign-extend.
  Pads the most-significant bits with the original most-significant bit.
- `trunc(x)` — truncate.
  Keeps the low bits of the word, dropping the most-significant bits.
  Truncating to a width at least as wide as the input is a no-op.

```virdant
wire z : Word[8]
z := zext(0x8w4)    // result: 0x08

wire s : Word[8]
s := sext(0x8w4)    // result: 0xf8 (0x8 = 0b1000, MSB is 1)

wire t : Word[4]
t := trunc(0xabw8)  // result: 0xb (keeps the low 4 bits)
```

The target width is inferred from the context of the assignment.

## `asclock()`

Convert a `Bit` signal to a `Clock`.

- `asclock(x)` — reinterpret a `Bit` as a `Clock`.
  Useful when you need to connect a `Bit` signal to a `Clock` port.

```virdant
wire bit_sig : Bit
wire clk : Clock
clk := asclock(bit_sig)
```

## `cast()`

Reinterpret the bit pattern of a `Word[n]` as an enum type, or vice versa:

```virdant
opcode := cast(opcode_bits)   // Word[4] -> Opcode
```

`trycast(v)` is the checked variant: it converts a `Word[n]` into a
`Valid[E]` (for an enum type `E` as wide as the word), `@Valid(e)`
when the bits decode cleanly, `@Invalid()` otherwise.

See ./types-and-patterns.md for more details.

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

It's especially useful as the RHS of `if` and `match` arms where the value is irrelevant or unused.

## `?` holes

`?` is a hole: a placeholder that typechecks as whatever type the
context requires, but leaves the driver unimplemented.
It produces a warning (`UnfilledHole`), not an error.

Use `?` while scaffolding — when you need the design to typecheck
but have not written the real logic yet — and fill in the holes
before finishing:

```virdant
mod WithHole {
    outgoing out : Word[4]
    out := ?      // WARNING: Unfilled hole: ? : Word[4]
}
```

Unlike `dontcare`, a hole is a deliberate "not written yet" marker
the compiler reminds you about.

## `tap` and tristate reads

Use the unary `tap` operator to read a tristate `bus` or `net`
component (`net sda : Bit` then `tap sda`); a plain reference to a
net is a compile error (`NetReadWithoutTap`).
See ./tristate.md.

## `mux()` ternary selector

`mux(sel, a, b)` returns `a` when `sel` is true and `b` when `sel` is false.
Use it for simple ternary selection instead of a `if` expression.
See ./driver-blocks.md for when a full `if` block is preferable.

```virdant
r <= mux(reset, 0, r + 1)

state <= mux(data_valid, @Start(1250w11), @Idle())
```

Both `a` and `b` must have the same type.

## Unary operators: `-`, `~`, `!`

- `-x` — arithmetic negation (two's complement) of a `Word[n]`:
  equivalent to `0 - x` with wraparound.
- `~x` — bitwise NOT of a `Word[n]`: inverts every bit.
- `!x` — logical NOT of a `Bit`.

Unary operators have the highest precedence and are right-associative.

```virdant
wire neg : Word[8]
neg := -value       // two's-complement negation

wire inv : Word[8]
inv := ~value       // invert all bits

wire not_bit : Bit
not_bit := !ready
```

## Operators on `Bit` and `Word[n]`

Virdant provides the usual operators:

- `Bit`: `!` (not), `&&` (and), `||` (or), `^^` (xor)
- `Word[n]`: `&` (and), `|` (or), `^` (xor), `-`, `~`, `==`, `!=`,
  `<`, `>`, `<=`, `>=`
- `any(x)` — true if any bit in `x` is set. Equivalent to `x != 0`.
- `all(x)` — true if every bit in `x` is set. Equivalent to `!any(~x)`.
- `word(...)` — concatenation (see above).

See below for the `+` family, which has wrapping and widening forms.

## Arithmetic: `+`/`-` (wrapping) and `+.`/`-.` (widening)

- `a + b` and `a - b` are modular: the result wraps to the operand
  width, so overflow/underflow information is lost.
- `a +. b` (add with carry) and `a -. b` (subtract with borrow)
  require the same `Word[n]` operands but produce a `Word[n+1]`: the
  low `n` bits are the wrapped result and the most-significant bit
  is the carry-out (for `+.`) or borrow-out (for `-.`).

```virdant
wire wrapped : Word[4] {
    it := 0xfw4 + 0x1w4     // wraps to 0x0
}

wire carried : Word[5] {
    it := 0xfw4 +. 0x1w4    // 0x10: carry-out set in the top bit
}

wire diff : Word[5] {
    it := count -. 0x1w4    // top bit is the borrow-out (count is Word[4])
}
```

Prefer `+.`/`-.` when you need to detect overflow or chain a carry
between additions.

## Bit shifting

Virdant does **not** have `<<` or `>>` operators.
Bit shifting must be done with `word()` and bit slicing.

### Shift left by `n` bits (multiply by 2^n)

Drop the top `n` bits and append `n` zero bits at the bottom.
The slice keeps the bottom `8-n` bits: `x[8-n..0]` (the top
slice index is exclusive, so `x[5..0]` holds bits 4..0).

```virdant
// x is Word[8], shift left by 3 into Word[8]
result := word(x[5..0], 0w3)
```

The slice `x[5..0]` keeps the bottom 5 bits of the original word and
discards bits 7..5 (the bits shifted out and lost).
Then `0w3` pads the result with 3 zero bits at the bottom (the LSB side).

### Shift right by `n` bits (divide by 2^n)

Drop the bottom `n` bits and prepend `n` zero bits at the top.

```virdant
// x is Word[8], shift right by 2 into Word[8]
result := word(0w2, x[8..2])
```

The slice `x[8..2]` keeps the top 6 bits (discarding bits 1..0).
`0w2` pads the result with 2 zero bits at the top (the MSB side).

### Variable-distance shifts

Variable-distance shifts require an `if` or `match` expression:

```virdant
// x is Word[8], shift left by a variable amount (0..3)
// shift_amount is Word[2], so the four arms are exhaustive.
reg result : Word[8] {
    it <= match shift_amount {
        with 0 => x
        with 1 => word(x[7..0], 0w1)
        with 2 => word(x[6..0], 0w2)
        with 3 => word(x[5..0], 0w3)
    }
}
```

### Signed / arithmetic right shift

For an arithmetic (sign-extending) right shift, replicate the MSB
instead of zero-padding:

```virdant
// Arithmetic right shift of x (Word[8]) by 2
result := word(x[7], x[7], x[8..2])
```

The term `x[7]` is the MSB of `x` (a `Bit`).
Repeating it twice prepends two copies of the sign bit,
which is the same as sign-extending 6 bits to 8.
