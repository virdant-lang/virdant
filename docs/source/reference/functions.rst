Functions
=========
Functions in Virdant declare reusable combinational logic that computes a value
from its inputs.
Functions are pure and have no side effects.
They are instantiated as combinational hardware at every call site.


Function Definitions
--------------------
A function definition gives a name to a combinational expression
parameterized by typed arguments.

.. code-block:: grammar

    FnDef :=
        DocString "fn" Ident ParamList "->" Type "{" Expr "}"

    ParamList :=
        "(" Params? ")"

    Params :=
        Param | Params "," Param

    Param :=
        DocString Ident ":" Type

The function body is a single expression enclosed in curly braces.

.. code-block:: virdant

    fn add(x : Word[8], y : Word[8]) -> Word[8] {
        x + y
    }

    fn max(a : Word[8], b : Word[8]) -> Word[8] {
        if a > b { a } else { b }
    }

The expression in the function body must have the declared return type.
Functions may use any expression form, including `if` expressions,
`match` expressions, and struct constructors.


Calling Functions
-----------------
Functions are called using the standard function call syntax:
the function name followed by arguments in parentheses.

.. code-block:: virdant

    mod Top {
        incoming clock : Clock
        outgoing out : Word[8]

        reg counter : Word[8] on clock
        counter <= counter + 1

        out := add(counter, counter)
    }

Each function call instantiates the function's logic as combinational hardware
at that point in the design.
The arguments are connected as inputs to the instantiated logic, and the
return value is the output.


Builtin Functions
-----------------
Virdant provides several builtin functions that are defined by the compiler.

`word(...)`
    Concatenates bits or words into a larger word.
    May also be used to convert an enum value to its underlying `Word`
    representation.

    .. code-block:: virdant

        // Concatenating individual bits into a word
        sum := word(s3, s2, s1, s0)

        // Converting an enum to its word value
        wire n : Word[7]
        n := word(opcode)

`any(...)`
    Returns `true` if any bit in the argument is 1.

    .. code-block:: virdant

        zero := !any(result)


Pure Combinational Semantics
----------------------------
Functions are **not** inlined textually.
Each call creates a separate instantiation of the combinational logic.

This means that if you call the same function twice with different arguments,
you get two independent copies of the logic.
There is no sharing of hardware between function calls at different call sites.

Functions cannot:

* Contain state (no registers)
* Have side effects (no assignments to external signals)
* Contain module instances
* Contain socket instances

Functions are equivalent to a block of combinational expressions given a name.
They serve as a documentation and reuse mechanism for combinational logic
patterns.