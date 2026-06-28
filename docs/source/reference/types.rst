Types
=====
Types in Virdant describe the kinds of values that may flow across components
and be stored in registers.
Virdant has a strong, static type system that catches many classes of errors at
compile time.

Every component in a Virdant design has a type annotation.
The type determines what operations are valid on the value and how it is
represented in hardware.


Built-in Types
--------------
Virdant provides several built-in types that serve as the primitive building
blocks of hardware designs.


Clock
~~~~~
`Clock` represents a clock signal.
A clock is a 1-bit signal that oscillates at a fixed frequency in standard
operation.

Every `reg` declaration must have a clock associated with it.
The register latches its next value on the rising edge of that clock.

`Clock` is an opaque type.
There are no operations that can manipulate or inspect values of type `Clock`.
Clocks may only be passed through ports or associated with registers using the
`on` keyword.

.. code-block:: virdant

    mod Top {
        incoming clock : Clock
        // ...
    }


Bit
~~~
`Bit` is a 1-bit value.
It is analogous to the boolean type found in programming languages.
Its two values are `true` and `false`.

.. code-block:: virdant

    wire ready : Bit
    ready := true

`Bit` supports the following operations:

* Logical: `!` (not), `&&` (and), `||` (or), `^^` (xor)
* Comparison: `==`, `!=`


Word
~~~~
`Word[n]` is an `n`-bit unsigned integer value.
`n` must be a compile-time constant natural number.

When used as a number, it is interpreted as an unsigned integer in 2's complement
representation.

.. code-block:: virdant

    wire data : Word[8]
    data := 0xff

`Word[n]` supports the following operations:

* Arithmetic: `+`, `-` (addition and subtraction)
* Bitwise: `&`, `|`, `^`, `~` (and, or, xor, not)
* Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
* Indexing: `w[i]`, `w[i..j]` (bit and range selection)
* Concatenation: `word(...)` builtin function

Operations are always fully defined.
Overflow in addition and subtraction wraps around (modular arithmetic).


Valid
~~~~~
`Valid[T]` is an optional value that wraps any type `T` with a validity bit.
The wire or register carrying the value may be valid (holding a value of type
`T`) or invalid (holding no value).
The total hardware width is `width(T) + 1`, where the extra bit signals
whether the value is valid.

.. code-block:: virdant

    wire v1 : Valid[Bit]
    v1 := @Valid(true)

    wire v2 : Valid[Bit]
    v2 := @Invalid()

Two constructors are provided:

* `@Valid(value)` --- wraps a valid value of type `T`.
* `@Invalid()` --- represents an invalid (absent) value.

Values of type `Valid[T]` are deconstructed using `match`:

.. code-block:: virdant

    wire got_it : Bit
    got_it := match v1 {
        case @Valid(x) => x
        case @Invalid() => false
    }

The `Valid[T]` type is useful for representing handshake signals,
optional values, or data that may not yet be ready.


Struct Types
------------
A struct type is a user-defined type that bundles several named fields together.

Struct types are declared with the `struct type` keywords:

.. code-block:: virdant

    struct type Color {
        red   : Word[8]
        green : Word[8]
        blue  : Word[8]
    }

Each field has a name and a type.
Struct types are nominal: two struct types with the same structure are
distinct types.

Struct values are constructed with the `$` syntax:

.. code-block:: virdant

    wire pixel : Color
    pixel := ${ red = 0, green = 0, blue = 0 }

Fields are accessed with the `->` projection operator:

.. code-block:: virdant

    wire r : Word[8]
    r := pixel->red

Struct types support structural pattern matching in `match` expressions.
For details, see the :doc:`patterns` chapter.


Union Types
-----------
A union type, also known as an algebraic data type, represents a value that may
take one of several alternative forms.
Each alternative, called a variant, may carry associated data of its own.

Union types are declared with the `union type` keywords:

.. code-block:: virdant

    union type MyUnion {
        Foo()
        Bar(x : MyEnum)
    }

A variant with no associated data is written with empty parentheses `()`.
A variant with associated data lists the parameters inside the parentheses,
similar to a function parameter list.

Union values are constructed using the `@` syntax:

.. code-block:: virdant

    wire u : MyUnion
    u := @Bar(#Baz)

Union values are deconstructed using `match` expressions:

.. code-block:: virdant

    wire result : MyEnum
    result := match u {
        case @Foo()   => #Baz
        case @Bar(x)  => x
    }

Union types must be matched exhaustively.
If a variant is not covered by any arm of a `match`, the compiler will report
an error.


Enum Types
----------
An enum type is a fixed set of named constant values.
All values in an enum have the same bit width.

Enum types are declared with the `enum type` keywords,
followed by the width in bits:

.. code-block:: virdant

    enum type Opcode width 7 {
        OP      = 0b01_100_11
        OP_IMM  = 0b00_100_11
        LOAD    = 0b00_000_11
        STORE   = 0b01_000_11
    }

Each variant is assigned a constant expression that fits within the declared
width.

Enum values are referenced with the `#` syntax:

.. code-block:: virdant

    wire op : Opcode
    op := #OP

Enum values may be compared with `==` and `!=`.
They may be matched in `match` expressions:

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        case #OP     => 1
        case #OP_IMM => 2
        case #LOAD   => 3
        case #STORE  => 4
    }

Enum types also participate in the `word()` and `any()` builtin functions.


Generics and Parameterized Types
--------------------------------
Types may be parameterized by a natural number.
This is how `Word[n]` supports arbitrary bit widths.

.. code-block:: grammar

    GenericsParams :=
          "[" Nat "]"
        | "[" Type "]"
    Generics      := "[" Ident ":" Kind "]"

When a type is parameterized by another type (like `Valid[T]`), the
`[Type]` form of `GenericsParams` is used.
The earlier `[Nat]` form is used for width parameters like `Word[n]`.

User-defined types may also be generic.
A `builtin type` declaration may specify generic parameters:

.. code-block:: virdant

    builtin type Word[n : Width] {}

A generic type parameter has a name and a kind (such as `Width` for bit widths).
When instantiating a generic type, the parameter is provided in square brackets:

.. code-block:: virdant

    wire data : Word[32]


Type Expressions
----------------
A type expression in Virdant consists of an ofness (a simple name like `Word`
or a fully-qualified name like `buffer::BufferType`) followed by an optional
generic parameter in square brackets:

.. productionlist::
    type: `Ofness` `GenericsParams`?
    ofness: `Ident` | `Ident` "::" `Ident`


Kind System
-----------
Types are classified into kinds.
A kind describes the family of types that a generic parameter ranges over.

The kind `Width` corresponds to natural numbers used as bit widths.
Additional kinds may be introduced by builtin type declarations.