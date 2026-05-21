Patterns
========
Patterns are used in `match` expressions and `match` statements to
destructure values and bind their components to names.

.. code-block:: grammar

    Pat :=
        "#" Ident
        | "@" Ident
        | "@" Ident "(" ArgList ")"
        | WordLit
        | BitLit


Enum Patterns
-------------
Enum patterns match a value against a specific enum variant.

.. code-block:: grammar

    Pat :=
        "#" Ident

The `#` syntax selects a named variant of an enum type.

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        case #OP     => 0x33
        case #OP_IMM => 0x13
        case #LOAD   => 0x03
        case #STORE  => 0x23
    }

The pattern `#OP` matches only the ``OP`` variant of the enum type.
The scrutinee and the patterns must belong to the same enum type.


Union Patterns
--------------
Union patterns match a value against a specific variant of a union type,
optionally binding the variant's associated data to names.

.. code-block:: grammar

    Pat :=
        "@" Ident
        | "@" Ident "(" ArgList ")"

The `@` syntax selects a variant of a union type.

For a variant with no associated data:

.. code-block:: virdant

    wire result : Bit
    result := match maybe {
        case @Nothing() => false
        case @Just(_)   => true
    }

For a variant with associated data, the pattern binds each field to a name:

.. code-block:: virdant

    wire decoded : MyEnum
    decoded := match u {
        case @Foo()      => #Baz
        case @Bar(x)     => x
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

    Pat := "@" "Valid" "(" ArgList ")" | "@" "Invalid" "(" ")"

* `@Valid(x)` matches a valid value and binds the inner value to `x`.
* `@Invalid()` matches an invalid (absent) value.

.. code-block:: virdant

    wire got_it : Bit
    got_it := match v {
        case @Valid(x) => x
        case @Invalid() => false
    }

These patterns are checked against the `Valid[T]` type by the compiler.
The scrutinee must be of type `Valid[T]` for either pattern to apply.


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
        case 0 => false
        case 1 => true
        case 2 => false
        case 3 => true
    }

Literal patterns support both `Bit` literals (`true`, `false`) and
`Word` literals (``0``, `42w8`, `0xff`).


Exhaustiveness
--------------
Match expressions and statements must be exhaustive.
This means that every possible value of the scrutinee type must be covered
by at least one pattern.

For enum types, exhaustiveness is checked by ensuring every variant appears
in at least one arm.

.. code-block:: virdant

    enum type State width 2 {
        Idle = 0
        Busy = 1
        Done = 2
    }

    // Error: missing variant 'Done'
    wire next : State
    next := match state {
        case #Idle => #Busy
        case #Busy => #Done
    }

For union types, exhaustiveness is checked by ensuring every variant appears
in at least one arm.

For literal patterns, exhaustiveness requires that every value of the word's
bit width is covered, which is often impractical.
In such cases, an `else` arm should be used as a catch-all.

.. code-block:: virdant

    wire parity : Bit
    parity := match 2w2 {
        case 0 => false
        case 1 => true
        case 2 => false
        case 3 => true
    }
    // All 4 values of Word[2] are covered, so this is exhaustive


The else Arm
------------
Both `match` expressions and `match` statements support an `else` arm
that matches any value not covered by the preceding `case` arms.

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        case #OP     => 1
        case #OP_IMM => 2
        else => 0
    }

The `else` arm must appear last, after all `case` arms.
Only one `else` arm is allowed per match.


Patterns in Match Statements
----------------------------
Patterns are used identically in both `match` expressions and
`match` statements.
In a match statement, the arm body is a block of module body statements
instead of a single expression.

.. code-block:: virdant

    match union_val {
        case @Foo() => {
            out := 0
            flag := true
        }
        case @Bar(x) => {
            out := x
            flag := false
        }
    }

Inside the arm body, bindings from the pattern (like `x` in the
`@Bar(x)` case) are available as read-only values.