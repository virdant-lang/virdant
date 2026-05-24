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

    Expr :=
        ExprIf | ExprMatch | ExprStruct | ExprBinOpLogical

    ExprIf           := "if" Expr "{" Expr "}" ("else" "if" Expr "{" Expr "}")* ("else" "{" Expr "}")?
    ExprMatch        := "match" Expr "{" ExprMatchArm* "}"
    ExprStruct       := "$" "{" AssignList "}"
    ExprBinOpLogical := ExprBinOpLogical ( "&&" | "||" | "^^" ) ExprBinOpCompare | ExprBinOpCompare
    ExprBinOpCompare := ExprBinOpCompare ( "<" | "<=" | ">" | ">=" | "==" | "!=" ) ExprBinOpAdditive | ExprBinOpAdditive
    ExprBinOpAdditive:= ExprBinOpAdditive ( "+" | "-" | "&" | "|" | "^" ) ExprUnOp | ExprUnOp
    ExprUnOp         := ( "-" | "~" | "!" ) ExprUnOp | ExprAscription
    ExprAscription   := ExprPrimary ":" Type | ExprPrimary
    ExprPrimary      := Ofness "(" ArgList ")"
                       | ExprPrimary "->" Ident
                       | ExprPrimary "[" Index "]"
                       | ExprPrimary "[" Index ".." Index "]"
                       | ExprPrimary "[" "dyn" Expr "]"
                       | "@" Ident "(" ArgList ")"
                       | ExprAtom
    ExprAtom         := Path | WordLit | BitLit | "Str" | "#" Ident | "@" Ident | "?" | "dontcare" | "(" Expr ")"


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
      - `+` `-` `&` `|` `^`
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
    color := ${ red = 0, red = 0, blue = 0 }

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
        "@" Ident "(" ArgList ")"

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
If expressions select between two or more branches based on a condition.

.. code-block:: grammar

    ExprIf :=
        "if" Expr "{" Expr "}"
        ("else" "if" Expr "{" Expr "}")*
        ("else" "{" Expr "}")?

.. code-block:: virdant

    wire max : Word[8]
    max := if a > b { a } else { b }

    // Multi-branch if
    wire sel : Word[8]
    sel := if op == 0 { a & b }
           else if op == 1 { a | b }
           else if op == 2 { a + b }
           else { a - b }

All branches must have the same type.
The condition must be of type `Bit`.
Curly braces are required around each branch expression, even for single
expressions.


Match Expressions
-----------------
Match expressions perform pattern matching on a value.

.. code-block:: grammar

    ExprMatch :=
        "match" Expr "{" ExprMatchArm* "}"

    ExprMatchArm :=
        "case" Pat "=>" Expr
        | "else" "=>" Expr

.. code-block:: virdant

    wire decoded : Word[8]
    decoded := match op {
        case #OP     => 1
        case #OP_IMM => 2
        case #LOAD   => 3
        else         => 0
    }

The match expression evaluates the scrutinee, compares it against each pattern
in order, and returns the expression from the first matching arm.

The `else` arm matches any value not covered by the preceding arms.
If no `else` arm is present, the match must be exhaustive.

For more details on patterns, see :doc:`patterns`.


Function Calls
--------------
Functions (both user-defined and builtin) are called with the standard
function call syntax.

.. code-block:: grammar

    ExprPrimary :=
        Ofness "(" ArgList ")"
        | "@" Ident "(" ArgList ")"

.. code-block:: virdant

    result := add(x, y)
    zero := !any(result)

The function name may be a simple identifier or a fully-qualified name.
The arguments must match the function's parameter list in number and type.


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

Unary operators have the highest precedence and are right-associative.

.. code-block:: virdant

    wire neg : Word[8]
    neg := -value

    wire inverted : Word[8]
    inverted := ~value

    wire not_bit : Bit
    not_bit := !ready


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

Operands must be of type `Word[n]` with the same `n`.

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