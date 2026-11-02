Patterns
========
Patterns are used in `match` expressions and `match` statements to
destructure values and bind their components to names.

.. code-block:: grammar

    Pat :=
        "#" Ident
        | "@" Ident
        | "@" Ident "(" PatList ")"
        | WordLit
        | BitLit
        | Ident
        | "dontcare"

The `#`, `@`, and literal patterns described below are the fully
supported forms.
The grammar also admits bare `Ident` and `dontcare` patterns, but the
match-coverage checker does not yet handle them correctly, so they are
not recommended for use yet.

.. _patlist:

.. code-block:: grammar

    PatList :=
        (Pat ",")+ Pat ","?
        | Pat ","?


Enum Patterns
-------------
Enum patterns match a value against a specific enumerant.

.. code-block:: grammar

    Pat :=
        "#" Ident

The `#` syntax selects a named enumerant of an enum type.

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        with #OP     => 0x33
        with #OP_IMM => 0x13
        with #LOAD   => 0x03
        with #STORE  => 0x23
    }

The pattern `#OP` matches only the `OP` enumerant of the enum type.
The scrutinee and the patterns must belong to the same enum type.


Enumerant Constructor Patterns
------------------------------
An enumerant that carries parameters (a *payload enumerant*) is matched
with the `@` constructor syntax, like a union variant.
For how payload enumerants are declared, see :doc:`types`.

.. code-block:: virdant

    enum type Opt[T : Type] {
        None
        Some(value : T)
    }

    wire inner : Word[4]
    inner := match opt {
        with @Some(v) => v   // `v` is bound to the payload of `Some`.
        with #None    => 0
    }

The pattern list matches the enumerant's parameters positionally.
Each parameter binds a name that is in scope only within the matched arm,
like a union binding; `_` discards a payload value.

An enumerant without parameters is matched with `#Name`, and may also be
matched with an empty constructor pattern `@Name()`:

.. code-block:: virdant

    wire tagged : Bit
    tagged := match opt {
        with @Some(_) => true
        with @None()  => false
    }

Using `#Name` on an enumerant that carries parameters is an error;
the compiler reports ``Ctor Some requires arguments: use @Some(...)``.

The builtin `Valid[T]` is the canonical example of a payload enum;
see `Valid Patterns`_ below.


Union Patterns
--------------
Union patterns match a value against a specific variant of a union type,
optionally binding the variant's associated data to names.

.. code-block:: grammar

    Pat :=
        "@" Ident
        | "@" Ident "(" PatList ")"

The `@` syntax selects a variant of a union type.

For a variant with no associated data:

.. code-block:: virdant

    wire result : Bit
    result := match maybe {
        with @Nothing() => false
        with @Just(_)   => true
    }

For a variant with associated data, the pattern binds each field to a name:

.. code-block:: virdant

    wire decoded : MyEnum
    decoded := match u {
        with @Foo()      => #Baz
        with @Bar(x)     => x
    }

The bound names behave like variables that are in scope only within the
matched arm's expression or statement block.
They are not registers or wires --- they are read-only bindings to the
components of the matched union value.

Valid Patterns
--------------
The builtin `Valid[T]` type supports two patterns for deconstructing
optional values:

.. code-block:: grammar

    Pat := "@" "Valid" "(" PatList ")" | "@" "Invalid" "(" ")"

* `@Valid(x)` matches a valid value and binds the inner value to `x`.
* `@Invalid()` matches an invalid (absent) value.

.. code-block:: virdant

    wire got_it : Bit
    got_it := match v {
        with @Valid(x) => x
        with @Invalid() => false
    }

These patterns are checked against the `Valid[T]` type by the compiler.
The scrutinee must be of type `Valid[T]` for either pattern to apply.

`Valid[T]` is a payload enum: `@Valid(x)` is the constructor pattern for
its `Valid` enumerant and `@Invalid()` the empty constructor pattern for
`Invalid` (`#Invalid` also matches).
See :doc:`types` for how payload enumerants are declared and constructed.


Literal Patterns
----------------
Literal patterns match a value against a specific constant.

.. code-block:: grammar

    Pat :=
        WordLit
        | BitLit

.. code-block:: virdant

    wire parity : Bit
    parity := match 2w2 {
        with 0 => false
        with 1 => true
        with 2 => false
        with 3 => true
    }

Literal patterns support both `Bit` literals (`true`, `false`) and
`Word` literals (`0`, `42w8`, `0xff`).


Exhaustiveness
--------------
Match expressions and statements must be exhaustive.
This means that every possible value of the scrutinee type must be covered
by at least one pattern.

For enum types, exhaustiveness is checked by ensuring every enumerant
appears in at least one arm --- including payload enumerants, which must
be covered with their constructor pattern (``@Some(v)``, not ``#Some``).

.. code-block:: virdant

    enum type State {
        Idle = 0w2
        Busy = 1
        Done = 2
    }

    // Error: missing variant 'Done'
    wire next : State
    next := match state {
        with #Idle => #Busy
        with #Busy => #Done
    }

For union types, exhaustiveness is checked by ensuring every variant appears
in at least one arm.

For literal patterns, exhaustiveness requires that every value of the word's
bit width is covered, which is often impractical.
In such cases, an `else` arm should be used as a catch-all.

.. code-block:: virdant

    wire parity : Bit
    parity := match 2w2 {
        with 0 => false
        with 1 => true
        with 2 => false
        with 3 => true
    }
    // All 4 values of Word[2] are covered, so this is exhaustive


The else Arm
------------
Both `match` expressions and `match` statements support an `else` arm
that matches any value not covered by the preceding `with` arms.

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        with #OP     => 1
        with #OP_IMM => 2
        else => 0
    }

The `else` arm must appear last, after all `with` arms.
Only one `else` arm is allowed per match.


Patterns in Match Statements
----------------------------
Patterns are used identically in both `match` expressions and
`match` statements.
In a match statement, the arm body is a block of module body statements
instead of a single expression.

.. code-block:: virdant

    match union_val {
        with @Foo() {
            out := 0
            flag := true
        }
        with @Bar(x) {
            out := x
            flag := false
        }
    }

Inside the arm body, bindings from the pattern (like `x` in the
`@Bar(x)` arm) are available as read-only values.