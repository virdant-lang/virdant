Expressions
===========
Expressions in Virdant represent combinational logic.
They compute values from inputs without side effects or state.

The expression language includes literals, references, arithmetic,
bitwise operations, comparisons, conditionals, pattern matching, struct
construction, function calls, indexing, and projection.


Grammar
-------
The full expression grammar, from highest to lowest precedence:

.. code-block:: grammar

    Expr := If | Match | Let | ExprStruct | ExprBinOpLogical

    If := "if" "{" IfArm* "}"
    IfArm        := "when" Expr "=>" Expr
                    | "when" Expr ArmBlock
                    | "when" Expr If
                    | "when" Expr Match
                    | "when" Expr Let
                    | "else" "=>" Expr
                    | "else" ArmBlock
                    | "else" If
                    | "else" Match
                    | "else" Let
    Match        := "match" Expr "{" MatchArm* "}"
    MatchArm     := "with" Pat "=>" Expr
                    | "with" Pat ArmBlock
                    | "with" Pat If
                    | "with" Pat Match
                    | "with" Pat Let
                    | "else" "=>" Expr
                    | "else" ArmBlock
                    | "else" If
                    | "else" Match
                    | "else" Let
    Let          := "let" LetBindingList ArmBlock
                    | "let" LetBindingList "=>" Expr
                    | "let" LetBindingList If
                    | "let" LetBindingList Match
    LetBindingList   := LetBinding | LetBindingList "," LetBinding
    LetBinding       := Ident ":" Type "=" Expr | Ident "=" Expr
    ExprStruct       := "$" "{" AssignList "}"
    ExprBinOpLogical := ExprBinOpLogical ( "&&" | "||" | "^^" ) ExprBinOpCompare | ExprBinOpCompare
    ExprBinOpCompare := ExprBinOpCompare ( "<" | "<=" | ">" | ">=" | "==" | "!=" ) ExprBinOpAdditive | ExprBinOpAdditive
    ExprBinOpAdditive:= ExprBinOpAdditive ( "+" | "-" | "&" | "|" | "^" ) ExprUnOp | ExprUnOp
    ExprUnOp         := ( "-" | "~" | "!" ) ExprUnOp | "tap" Path | ExprAscription
    ExprAscription   := ExprPrimary ":" Type | ExprPrimary
    ExprPrimary      := Ofness "(" ArgList? ")"
                       | ExprPrimary "->" Ident
                       | ExprPrimary "[" Index "]"
                       | ExprPrimary "[" Index ".." Index "]"
                       | ExprPrimary "[" "dyn" Expr "]"
                       | "@" Ident "(" ArgList? ")"
                       | ExprAtom
    ExprAtom         := Path | WordLit | BitLit | "Str" | "#" Ident | "@" Ident | "?" | "dontcare" | "(" Expr ")"
    Index            := "Nat"


Precedence and Associativity
----------------------------
Expression operators are listed below in order of decreasing precedence,
from tightest to loosest binding.

.. list-table::
    :header-rows: 1

    * - Category
      - Operators
      - Associativity
    * - Unary
      - `-` `~` `!`
      - Right-to-left
    * - Multiplicative / Additive / Bitwise
      - `+` `+.` `-` `-.` `&` `|` `^`
      - Left-to-right
    * - Comparison
      - `<` `<=` `>` `>=` `==` `!=`
      - Left-to-right
    * - Logical
      - `&&` `||` `^^`
      - Left-to-right
    * - Postfix
      - `->` `[]` `[..]`, function call
      - Left-to-right


Literals
--------
Literals represent constant values directly in the expression syntax.

.. code-block:: virdant

    // Bit literals
    true
    false

    // Word literals with explicit width
    42w8
    0xffw16
    0b1010w4

    // Word literals with inferred width (context required)
    42
    0xff
    0b1010

    // Enum tag reference
    #Idle
    #OP

For more details on literals, see :doc:`grammar`.


Paths
-----
A path refers to a named component, port, or instance field.

.. code-block:: grammar

    Path :=
        Ident ("." Ident)*
        | "it" ("." Ident)*

.. code-block:: virdant

    counter        // a local register or wire
    ha1.sum        // port on a submodule instance
    memory.mem.addr // port on a socket instance

The special path `it` refers to the enclosing declaration's target
(available inside declaration blocks).


Struct Construction
-------------------
Struct values are constructed using the `$` syntax.

.. code-block:: grammar

    ExprStruct :=
        "$" "{" AssignList "}"

    AssignList :=
        (Assign ",")* Assign ","?
        | Expr ","?
        |

    Assign :=
        Ident "=" Expr

.. code-block:: virdant

    wire color : Color
    color := ${ red = 0, green = 0, blue = 0 }

Fields may be assigned in any order.
All fields must be assigned.

If a struct type has many fields, you may also use a single expression
as the struct value if the types are compatible (positional construction).


Field Projection
----------------
Fields of struct values are accessed with the `->` operator.

.. code-block:: grammar

    ExprPrimary :=
        ExprPrimary "->" Ident

.. code-block:: virdant

    wire r : Word[8]
    r := color->red

Projection is left-associative, so nested projections like
`outer->inner->field` are parsed as `(outer->inner)->field`.


Indexing and Slicing
--------------------
Individual bits and ranges of bits may be extracted from `Word[n]` values.

.. code-block:: grammar

    ExprPrimary :=
        ExprPrimary "[" Index "]"
        | ExprPrimary "[" Index ".." Index "]"
        | ExprPrimary "[" "dyn" Expr "]"

.. code-block:: virdant

    // Bit selection (constant index)
    bit := data[3]

    // Range selection (inclusive of both bounds)
    nibble := data[7..4]

    // Dynamic bit selection (index by expression, prefixed with `dyn`)
    bit := data[dyn idx]

Indices are zero-based.
`data[0]` is the least significant bit.
`data[7..4]` extracts bits 7, 6, 5, and 4.

Dynamic Indexing
~~~~~~~~~~~~~~~~
When the index is an expression rather than a constant literal,
the `dyn` keyword prefixes the index expression.

The array must be of type `Word[n]` and the index of type `Word[k]`,
and the constraint `n == 2^k` must hold.
The result type is `Bit`.

.. code-block:: virdant

    // n = 8, k = 3, 2^3 = 8 -- valid
    out := arr[dyn idx]

    // n = 1, k = 0, 2^0 = 1 -- valid
    out := single_bit_arr[dyn zero_idx]

If the constraint is violated, the compiler reports a type error.

.. code-block:: virdant

    // Error: n = 8, k = 2, 2^2 = 4 != 8
    out := arr[dyn wrong_width]


Union Construction
------------------
Union values are constructed using the `@` syntax with the variant name.

.. code-block:: grammar

    ExprPrimary :=
        "@" Ident "(" ArgList? ")"

For a variant with no associated data, use empty parentheses:

.. code-block:: virdant

    wire u : MyUnion
    u := @Foo()

For a variant with associated data:

.. code-block:: virdant

    wire u : MyUnion
    u := @Bar(#Baz)

If the union type has multiple variants with the same name, the type context
disambiguates which union type is intended.

The builtin Valid type also uses the `@` constructor syntax:

.. code-block:: virdant

    wire v : Valid[Word[8]]
    v := @Valid(0xff)

    wire invalid : Valid[Word[8]]
    invalid := @Invalid()

For more details, see the :doc:`types` chapter on Valid[T].


If Expressions
--------------
If expressions select between two or more branches based on conditions.

.. code-block:: grammar

    If := "if" "{" IfArm* "}"

    IfArm :=
        "when" Expr "=>" Expr
        | "when" Expr ArmBlock
        | "when" Expr If
        | "when" Expr Match
        | "when" Expr Let
        | "else" "=>" Expr
        | "else" ArmBlock
        | "else" If
        | "else" Match
        | "else" Let

.. code-block:: virdant

    wire max : Word[8]
    max := if {
        when a > b => a
        else => b
    }

    // Multi-branch if
    wire sel : Word[8]
    sel := if {
        when op == 0 => a & b
        when op == 1 => a | b
        when op == 2 => a + b
        else => a - b
    }

All branches must have the same type.
Each condition must be of type `Bit`.
The `if` expression requires an `else` arm.

Arm Syntax
~~~~~~~~~~
Each `when` clause may use one of five forms:

1. Bare expression: `when cond => expr`
2. Block: `when cond { stmts }`
   (blocks must contain a single expression to be valid in expression context)
3. Nested `if`: `when cond if { ... }`
4. Nested `match`: `when cond match expr { ... }`
5. Let binding: `when cond let x = expr => ...`

The same forms apply to the `else` arm.
A `let` arm body follows the same terminator rules as any other `let`.


Let Expressions
---------------
A `let` expression binds immutable names to values for use within a
scope. In expression position, the body must *produce* a value: either
with `=>`, or by chaining directly into an `if` or `match` whose arms
produce values. Only the `{ ... }` block form is a style error here,
since a block performs rather than produces:

.. code-block:: grammar

    Let :=
        "let" LetBindingList ArmBlock
        | "let" LetBindingList "=>" Expr
        | "let" LetBindingList If
        | "let" LetBindingList Match

.. code-block:: virdant

    out := let scaled = a + a => scaled + b

    // Chained form: also legal in expression position.
    out := let done = a == 0w8 if {
        when done => 0w8
        else => 0w8
    }

Bindings may be untyped (the type is inferred) or carry an explicit
type, and several bindings may be given in a comma-separated list:

.. code-block:: virdant

    out := let lo = a, hi : Word[8] = b => hi + lo

Bindings are ordered: each binding may reference the bindings before
it. Within the body, a bound name shadows any component of the same
name, and the innermost binding wins: a match arm's pattern variables
shadow `let` bindings from outside the match, while a `let` written
inside the arm shadows the arm's pattern variables.

For untyped bindings, the binding's type is inferred from its first
use in the body: a widthless literal RHS must be anchored by a use
site that fixes its width.

The terminator-style rule is the same as for statement `let`s
(see :doc:`statements`): `=>` means the scope produces a value, `{`
means the scope performs statements. In expression position, the `=>`
form is the idiomatic one.


Match Expressions
-----------------
Match expressions perform pattern matching on a value.

.. code-block:: grammar

    Match := "match" Expr "{" MatchArm* "}"

    MatchArm :=
        "with" Pat "=>" Expr
        | "with" Pat ArmBlock
        | "with" Pat If
        | "with" Pat Match
        | "with" Pat Let
        | "else" "=>" Expr
        | "else" ArmBlock
        | "else" If
        | "else" Match
        | "else" Let

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        with #OP     => 1
        with #OP_IMM => 2
        with #LOAD   => 3
        else         => 0
    }

The match expression evaluates the scrutinee, compares it against each pattern
in order, and returns the expression from the first matching arm.

The `else` arm matches any value not covered by the preceding arms.
If no `else` arm is present, the match must be exhaustive.

Arm Syntax
~~~~~~~~~~
Each `with` arm may use one of five forms:

1. Bare expression: `with pat => expr`
2. Block: `with pat { stmts }`
   (blocks must contain a single expression to be valid in expression context)
3. Nested `if`: `with pat if { ... }`
4. Nested `match`: `with pat match expr { ... }`
5. Let binding: `with pat let x = expr => ...`

The same forms apply to the `else` arm.
A `let` arm body follows the same terminator rules as any other `let`.

For more details on patterns, see :doc:`patterns`.


Function Calls
--------------
Functions (both user-defined and builtin) are called with the standard
function call syntax.

.. code-block:: grammar

    ExprPrimary :=
        Ofness "(" ArgList? ")"
        | "@" Ident "(" ArgList? ")"

.. code-block:: virdant

    result := add(x, y)
    zero := !any(result)

The function name may be a simple identifier or a fully-qualified name.
The arguments must match the function's parameter list in number and type.

Builtin Functions
~~~~~~~~~~~~~~~~~
Virdant provides several builtin functions,
including type conversion and reduction operators.

**mux(cond, a, b)** - Multiplexer

Selects between two values based on a condition.
Returns `a` if `cond` is true, otherwise returns `b`.

.. code-block:: virdant

    // Basic usage
    out := mux(enable, data, 0)

    // Equivalent to:
    out := if {
        when enable => data
        else => 0
    }

The `mux` function is desugared to an `if` expression during compilation.
All three arguments must be provided:

- `cond` must be of type `Bit`
- `a` and `b` must have the same type
- The result type matches the type of `a` and `b`

**asclock(x)** - Convert a `Bit` to a `Clock`

Reinterprets a `Bit` signal as a `Clock`.
The subject :vir:`x` must be of type `Bit`,
and the result is of type `Clock`.

.. code-block:: virdant

    wire bit_sig : Bit
    wire clk : Clock
    clk := asclock(bit_sig)

Since `Clock` is opaque —
clocks may only be passed through ports or used in a register's `on` clause —
:vir:`asclock` is the only way to obtain a `Clock` from an ordinary signal.
Its main use is clocking a register on a `Bit` signal,
because the expression after `on` must be of type `Clock`:

.. code-block:: virdant

    mod ClockFromBit {
        incoming clk_bit : Bit

        wire clk : Clock
        clk := asclock(clk_bit)

        reg counter : Word[8] on clk {
            it <= it + 1w8
        }

        outgoing count : Word[8] {
            it := counter
        }
    }

It can also be applied inline in the `on` clause itself:

.. code-block:: virdant

    reg r : Bit on asclock(clk_bit) {
        it <= !it
    }

The remaining builtin functions are:

- :vir:`any(w)` / :vir:`all(w)` — unary reductions over a :vir:`Word`,
  yielding a :vir:`Bit` that is set if any (respectively all) bits are set.
- :vir:`sext(w)` / :vir:`zext(w)` / :vir:`trunc(w)` — change the width of a
  :vir:`Word`, sign-extending, zero-extending, or truncating it;
  the target width is checked against the expected type.
- :vir:`cast(x)` — reinterpret the bit pattern of a :vir:`Word[n]` as an enum
  type, or vice versa.


Unary Operators
---------------
.. list-table::
    :header-rows: 1

    * - Operator
      - Description
    * - `- expr`
      - Arithmetic negation (two's complement)
    * - `~ expr`
      - Bitwise NOT (invert all bits)
    * - `! expr`
      - Logical NOT (for `Bit` values)
    * - `tap path`
      - Read a tristate `bus` or `net`, asserting it is driven

Unary operators have the highest precedence and are right-associative.

.. code-block:: virdant

    wire neg : Word[8]
    neg := -value

    wire inverted : Word[8]
    inverted := ~value

    wire not_bit : Bit
    not_bit := !ready


The tap Operator
~~~~~~~~~~~~~~~~
`tap` reads a tristate component (`bus` or `net`, see :doc:`modules`).
Because a tristate net may be floating, a plain reference to one is a
compile error (``NetReadWithoutTap``); `tap` asserts that the net is
driven at the point of the read and evaluates to the net's value:

.. code-block:: virdant

    net sda : Bit
    sda #- true

    seen := tap sda

Applying `tap` to anything other than a `bus` or `net` is an error
(``TapNotOnNet``). Like the other unary operators, `tap` has the
highest precedence and is right-associative.


Binary Operators
----------------

Arithmetic Operators
~~~~~~~~~~~~~~~~~~~~

.. list-table::
    :header-rows: 1

    * - Operator
      - Description
    * - `a + b`
      - Addition (modular, wraps on overflow)
    * - `a - b`
      - Subtraction (modular, wraps on underflow)
    * - `a +. b`
      - Addition with carry: like `a + b`, but the result is
        `Word[n+1]` and its most-significant bit is the carry-out
    * - `a -. b`
      - Subtraction with borrow: like `a - b`, but the result is
        `Word[n+1]` and its most-significant bit is the borrow-out

Operands must be of type `Word[n]` with the same `n`.  The `+` and
`-` operators wrap their results to `n` bits; `+.` and `-.` instead
widen the result to `n+1` bits, so no information is lost:

.. code-block:: virdant

    // 0xf + 0x1 wraps to 0x0 ...
    wire wrapped : Word[4] {
        it := 0xfw4 + 0x1w4
    }

    // ... while `+.` keeps the carry out in the top bit.
    wire carried : Word[5] {
        it := 0xfw4 +. 0x1w4   // 0x10, carry bit set
    }

Bitwise Operators
~~~~~~~~~~~~~~~~~

.. list-table::
    :header-rows: 1

    * - Operator
      - Description
    * - `a & b`
      - Bitwise AND
    * - `a | b`
      - Bitwise OR
    * - `a ^ b`
      - Bitwise XOR

Operands must be of type `Word[n]` with the same `n`.

Comparison Operators
~~~~~~~~~~~~~~~~~~~~

.. list-table::
    :header-rows: 1

    * - Operator
      - Description
    * - `a < b`
      - Less than
    * - `a <= b`
      - Less than or equal
    * - `a > b`
      - Greater than
    * - `a >= b`
      - Greater than or equal
    * - `a == b`
      - Equal
    * - `a != b`
      - Not equal

Comparison operators return `Bit`.
Operands must have the same type.

Logical Operators
~~~~~~~~~~~~~~~~~

.. list-table::
    :header-rows: 1

    * - Operator
      - Description
    * - `a && b`
      - Logical AND (for `Bit` values)
    * - `a || b`
      - Logical OR (for `Bit` values)
    * - `a ^^ b`
      - Logical XOR (for `Bit` values)

Logical operators require `Bit` operands and return `Bit`.


Type Ascriptions
----------------
An expression may be annotated with an explicit type using the `:` operator.

.. code-block:: grammar

    ExprAscription :=
        ExprPrimary ":" Type

.. code-block:: virdant

    value : Word[16]

Type ascriptions are useful for disambiguating overloaded expressions or
providing type hints to the compiler.


Dontcare
--------
The `dontcare` expression represents an unknown or unused value.

.. code-block:: virdant

    wire x : Word[8]
    x := dontcare


The ? Expression
----------------
The `?` token may be used in certain contexts where the compiler can
infer the intended value.

.. code-block:: virdant

    // Used to ask the compiler to fill in a value
    wire x : Word[8]
    x := ?