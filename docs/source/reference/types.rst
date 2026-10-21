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
Like every enum, `Valid` is one-hot encoded: the total hardware width is
`width(T) + 2`, where the 2-bit tag distinguishes `@Valid` from `@Invalid`
above the payload.

Unlike `Clock`, `Bit`, and `Word[n]`, which are built into the compiler,
`Valid[T]` is an ordinary generic enum type defined in the builtin package:

.. code-block:: virdant

    enum type Valid[T : Type] {
        Invalid
        Valid(value : T)
    }

It requires no special compiler support.
See `Generics and Parameterized Types`_ for the full generics story.

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


Built-in Modules
----------------
In addition to built-in types, Virdant ships a small library of
built-in modules in `lib/builtin.vir`.
These modules are available to every design without an import.


Sync
~~~~
`Sync` synchronizes a single `Bit` signal from one clock domain onto another.
It safely samples an asynchronous input, such as a button press,
into a synchronous design.

The `Sync` module exposes the following ports:

`inp` (incoming `Bit`)
    The asynchronous input signal on a foreign clock domain.

`clock` (incoming `Clock`)
    The clock of the destination domain.
    The synchronized output is updated on this clock.

`out` (outgoing `reg` of `Bit` on `clock`)
    The synchronized output.

Internally `Sync` is two back-to-back flip-flops on `clock`.
The first register may go metastable when `inp` changes near the clock edge,
but it has a full cycle to settle before the second register samples it.
The output of the second register is therefore stable.

.. code-block:: virdant

    mod Top {
        incoming clock  : Clock
        incoming button : Bit

        mod sync of Sync {
            it.clock := clock
            it.inp   := button
        }

        outgoing led    : Bit {
            it := sync.out // `sync.out` is safe to use on `clock`.
        }
    }


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
Each named constant is called an *enumerant*.
Enum types are declared with the `enum type` keywords:

.. code-block:: virdant

    enum type Opcode {
        Add = 1w4
        Sub = 2
        And = 4
        Or  = 8
    }

Each enumerant may be given an explicit value with `=`.
Values are constant expressions.
The width of the enum type is taken from the first enumerant's literal:
write the first value with an explicit width (`1w4` above);
the remaining values may be bare literals.

If the first literal's width is not inferrable, the enum's width cannot be
determined:

.. code-block:: virdant

    enum type Bad {
        A = 0
        B = 1
    }

reports ``Enum type's first enumerant does not have an inferrable width.
Add an explicit width, e.g. `= 0wN`.`` --- write ``A = 0w4`` instead.

Every enumerant must have a distinct value.
Two enumerants that evaluate to the same value are an error, even when the
values are written in different bases:

.. code-block:: virdant

    enum type Code {
        A = 15w4
        B = 0x0F
    }

reports ``Duplicate enum value: enumerants of Code share value 15``.

Enum values are referenced with the `#` syntax:

.. code-block:: virdant

    wire op : Opcode
    op := #Add

Enum values may be compared with `==` and `!=`.
They may be matched in `match` expressions:

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        case #Add => 1
        case #Sub => 2
        case #And => 3
        case #Or  => 4
    }

Enum types also participate in the `word()` and `any()` builtin functions.

In hardware, an enum whose enumerants carry no parameters is represented
as a tag of the enum's width.
When some enumerants carry parameters (see `Payload Enumerants`_ below),
the representation is a tag followed by the widest payload, like a union.


One-Hot Encoding by Default
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When *no* enumerant specifies a value, the enumerants are one-hot encoded:
enumerant `i` (in declaration order) gets the value `1 << i`, and the width
of the enum type is the number of enumerants.

.. code-block:: virdant

    enum type State {
        Idle   // value 1 (1 << 0)
        Busy   // value 2 (1 << 1)
        Done   // value 4 (1 << 2)
    }

    mod Ctl {
        incoming state : State
        outgoing code : Word[3] // State is exactly 3 bits wide.
        code := word(state)     // #Idle == 1, #Busy == 2, #Done == 4.
    }

One-hot is the default for two reasons:

* The all-zeros word is never a valid enumerant, so a zero-initialized
  register or an uninitialized wire can never be confused with a
  legitimate enum value.
* Testing which enumerant a signal holds, and comparing it against a
  constant, only requires checking a single bit.

Giving some enumerants explicit values and leaving others unspecified is
ambiguous, and therefore an error --- either give every enumerant a value,
or none:

.. code-block:: virdant

    enum type Color {
        Red
        Green = 1
        Blue
    }

reports ``Enum type Color specifies values for some but not all enumerants.
Either give every enumerant a value, or none (which defaults to one-hot
encoding).``


Payload Enumerants
~~~~~~~~~~~~~~~~~~
An enumerant may carry parameters, like a union variant:

.. code-block:: virdant

    enum type Opt[T : Type] {
        None
        Some(value : T)
    }

An enumerant that carries parameters is constructed with the `@` syntax,
exactly like a union variant:

.. code-block:: virdant

    wire o1 : Opt[Word[4]]
    o1 := @Some(14w4)

An enumerant without parameters uses the `#` syntax, and may also be
constructed with an empty argument list:

.. code-block:: virdant

    wire o2 : Opt[Word[4]]
    o2 := #None

    wire o3 : Opt[Word[4]]
    o3 := @None()

Using `#Name` on an enumerant that carries parameters is an error:

.. code-block:: virdant

    o1 := #Some

reports ``Ctor Some requires arguments: use @Some(...)``.

Payload enumerants are matched with `case @Name(pattern)`, which binds the
parameters to names inside the arm:

.. code-block:: virdant

    wire inner : Word[4]
    inner := match o1 {
        case @Some(v) => v
        case #None    => 0
    }

Matching an enum exhaustively requires covering every enumerant, with or
without parameters; a missing enumerant is reported like a missing union
variant (``Non-exhaustive match ... not covered: #None``).

The builtin `Valid[T]` type is exactly such a generic payload enum
(see `Valid`_ above):

.. code-block:: virdant

    enum type Valid[T : Type] {
        Invalid
        Valid(value : T)
    }

For the pattern side, see the :doc:`patterns` chapter.


Generics and Parameterized Types
--------------------------------
Type definitions may declare *generic parameters*, making them
parameterized (generic) types.
This is how `Word[n]` supports arbitrary bit widths and how `Valid[T]` wraps
an arbitrary type.

Every type definition form may declare generic parameters: `struct type`,
`union type`, `enum type`, and `builtin type`.
The parameter list is written in square brackets after the type name.
Parameters are comma-separated, and each parameter is written `name : kind`:

.. code-block:: virdant

    struct type Pair[A : Type, B : Type] {
        fst : A
        snd : B
    }

    union type Either[L : Type, R : Type] {
        Left(l : L)
        Right(r : R)
    }

    enum type Opt[T : Type] {
        None
        Some(value : T)
    }

    builtin type Word[n : Nat] {}

The parameter list syntax is shared by all definition forms;
the two available kinds are described in `Kind System`_ below:

.. code-block:: grammar

    Generics := "[" KindParamList "]"
    KindParamList := (KindParam ",")* KindParam
    KindParam := Ident ":" Kind

Module and socket definitions accept the same generic parameter lists;
see :doc:`modules` and :doc:`sockets`.

Generic parameter names are in scope inside the body of the declaring
definition, where member types may reference them by name (`fst : A` and
`snd : B` above).
The definition is analyzed once, generically, with the parameters as
placeholders; actual types are substituted only at the use sites.

Type Arguments
~~~~~~~~~~~~~~

A reference to a generic type supplies *type arguments* in square brackets,
comma-separated and in the same order as the declared parameters:
`Pair[Word[4], Bit]` supplies `Word[4]` for `A` and `Bit` for `B`.

A parameter of kind `Nat` takes a number literal, as in the familiar
`Word[8]`.
A parameter of kind `Type` takes a full type expression, so type arguments
nest to arbitrary depth:

.. code-block:: virdant

    reg pair : Pair[Word[4], Bit] on clock {
        it <= ${ fst = a, snd = b }
    }

    // The argument to Valid is itself a full type expression.
    wire nested : Valid[Pair[Word[4], Bit]]
    nested := @Valid(${ fst = a, snd = b })

Checks at the Reference Site
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The number and the kinds of the type arguments are checked at each
reference site.

*Wrong arity.* A reference must supply exactly one argument per declared
parameter:

.. code-block:: virdant

    incoming inp : Pair[Bit]

reports ``Type Pair takes 2 type arguments, but 1 was given``.

*Kind mismatch, `Type` parameter given a number.* A `Type` parameter stands
for a whole type, not a number literal:

.. code-block:: virdant

    incoming inp : Valid[8]

reports ``Type argument mismatch: parameter T of Valid expects a type, but
found a Nat literal``.

*Kind mismatch, `Nat` parameter given a type.* A `Nat` parameter stands for
a number literal:

.. code-block:: virdant

    incoming inp : Word[Bit]

reports ``Type argument mismatch: parameter n of Word expects a Nat, but
found a type``.

*Unbound parameter.* A generic parameter name is only in scope inside its
declaring definition; referencing it anywhere else is an unresolved type:

.. code-block:: virdant

    struct type Box[T : Type] {
        value : T
    }

    export mod Top {
        incoming inp : T
    }

reports ``Unresolved type T``.

*Parameters take no arguments.* A generic parameter stands for a single
type and cannot itself be applied to type arguments:

.. code-block:: virdant

    struct type Box[T : Type] {
        value : T[Bit]
    }

reports ``Parameter T takes no type arguments``.


Type Expressions
----------------
A type expression in Virdant consists of an ofness (a simple name like `Word`
or a fully-qualified name like `buffer::BufferType`) followed by an optional
comma-separated list of type arguments in square brackets:

.. productionlist::
    type: `Ofness` `GenericsParams`?
    ofness: `Ident` | `Ident` "::" `Ident`
    genericsparams: "[" `GenericsArgList` "]"
    genericsarglist: (`GenericsArg` ",")* `GenericsArg`
    genericsarg: `Nat` | `Type`

The argument list is matched against the generic parameters of the
referenced type in declaration order.
Each argument is either a number literal (for a `Nat`-kind parameter) or a
full type expression (for a `Type`-kind parameter), so type arguments nest
arbitrarily: `Valid[Pair[Word[4], Bit]]`.


Kind System
-----------
Types are classified into kinds.
A kind describes what a generic parameter stands for.

Virdant has exactly two kinds:

* `Type` --- the parameter stands for a whole type, such as `Bit`,
  `Word[4]`, or a user-defined type.
* `Nat` --- the parameter is a number literal, used for bit widths
  (as in `Word[n : Nat]`).

Any other kind name is a compile-time error.
Declaring a parameter with an unknown kind:

.. code-block:: virdant

    struct type S[X : Foo] {
        value : X
    }

reports:

.. code-block:: text

    Unknown kind Foo: generic parameters must be declared as `: Type` or `: Nat`
