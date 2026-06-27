Statements
==========
Statements are the declarations that appear inside a module body.
Unlike expressions, which compute values, statements declare the structure
and behavior of a module.

The full set of module body statements includes:

* Component declarations (ports, wires, registers)
* Driver statements (assignments)
* Instance declarations
* Socket instance declarations
* Conditional statements (when)
* Match statements
* Unused declarations

Statements are order-independent within a module body.
Virdant processes them as a set of declarations, not as a sequence of
imperative steps.


Component Declarations
----------------------
Component declarations introduce named signals into a module.

.. code-block:: grammar

    ModDefStmtComponent :=
        DocString Annotations "incoming" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "outgoing" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "outgoing" "wire" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "outgoing" "reg" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "wire" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "reg" Ident ":" Type OnClause? ItBlock?

.. code-block:: virdant

    mod Example {
        // Port declarations
        incoming clock : Clock
        incoming inp   : Word[8]
        outgoing out   : Word[8]

        // Internal wire
        wire sum : Word[8]

        // Register
        reg counter : Word[8] on clock

        // Outgoing register port
        outgoing reg status : Word[8] on clock
    }

For a detailed description of each component type, see :doc:`modules`.


Driver Statements
-----------------
Driver statements assign values to components.

.. code-block:: grammar

    ModDefStmtDriver :=
        Path ":=" Expr
        | Path "<=" Expr
        | Path ":=:" Path

Combinational assignment :=
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `:=` operator drives a combinational signal continuously.

.. code-block:: virdant

    out := a + b
    sum := ha1.sum

The target of `:=` must be a combinational signal (wire, port, or
combinational output of a submodule).

Sequential assignment <=
~~~~~~~~~~~~~~~~~~~~~~~~
The `<=` operator drives a register.
The value is latched on the next rising edge of the register's clock.

.. code-block:: virdant

    reg counter : Word[8] on clock
    counter <= counter + 1

The target of `<=` must be a `reg` declaration.

Bidirectional connection :=:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `:=:` operator connects two signals bidirectionally.

.. code-block:: virdant

    memory.mem :=: core.mem

Both sides of `:=:` must be socket instances of compatible socket types.
The operator connects every port in the socket, respecting the direction
of each port relative to each instance.


Instance Declarations
---------------------
Instance declarations instantiate a submodule inside the current module.

.. code-block:: grammar

    ModDefStmtInstance :=
        DocString Annotations "mod" Ident "of" Ofness ItBlock?

The instance name must be unique within the enclosing module.
Ports on the instance are accessed via dot notation.

.. code-block:: virdant

    mod ha1 of HalfAdder
    ha1.a := a
    ha1.b := b

An instance may have an optional `it` block for grouping connections:

.. code-block:: virdant

    mod ha1 of HalfAdder {
        it.a := a
        it.b := b
    }

The type (after `of`) must be a module definition visible in the current
scope, either defined in the same package or imported from another package.


Socket Instance Declarations
----------------------------
Socket instances declare either a client or server role for a socket type.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString Annotations "client" "socket" Ident "of" Ofness ItBlock?
        | DocString Annotations "server" "socket" Ident "of" Ofness ItBlock?

.. code-block:: virdant

    client socket mem of Mem
    unused mem.data
    mem.addr := 0

For details on sockets, see :doc:`sockets`.


Conditional Statements
----------------------
Conditional statements allow a module body to contain different sets of
statements depending on conditions.

.. code-block:: grammar

    ModDefStmtWhen := "when" "{" ModDefStmtWhenArm* "}"

    ModDefStmtWhenArm :=
        "case" Expr "=>" Expr
        | "case" Expr ModDefStmtBlock
        | "case" Expr ModDefStmtWhen
        | "case" Expr ModDefStmtMatch
        | "else" "=>" Expr
        | "else" ModDefStmtBlock
        | "else" ModDefStmtWhen
        | "else" ModDefStmtMatch

.. code-block:: virdant

    when {
        case reset {
            r <= 0
            state := #Idle
        }
        case enable {
            r <= r + 1
            state := #Active
        }
        else {
            r <= r
            state := #Idle
        }
    }

Each condition must be of type `Bit`.
The `else` arm is optional in statement context,
though it is recommended for completeness.

Arm Syntax
~~~~~~~~~~
Each arm may use one of four forms:

1. Bare expression: `case cond => expr` (rarely used in statement context)
2. Block: `case cond { stmts }`
3. Nested `when`: `case cond when { ... }`
4. Nested `match`: `case cond match expr { ... }`

The same forms apply to the `else` arm.

Conditional statements are different from `when` expressions:
statements can contain declarations, assignments, instances, and other
module body constructs, while `when` expressions only compute values.


Match Statements
----------------
Match statements perform pattern-based dispatch in the module body.

.. code-block:: grammar

    ModDefStmtMatch :=
        "match" Expr "{" ModDefStmtMatchArm* "}"

    ModDefStmtMatchArm :=
        "case" Pat "=>" Expr
        | "case" Pat ModDefStmtBlock
        | "case" Pat ModDefStmtWhen
        | "case" Pat ModDefStmtMatch
        | "else" "=>" Expr
        | "else" ModDefStmtBlock
        | "else" ModDefStmtWhen
        | "else" ModDefStmtMatch

.. code-block:: virdant

    match state {
        case #Idle => {
            r <= 0
            out := ready
        }
        case #Active => {
            r <= r + 1
            out := busy
        }
        else => {
            r <= r
            out := idle
        }
    }

Arm Syntax
~~~~~~~~~~
Each arm may use one of four forms:

1. Bare expression: `case pat => expr` (rarely used in statement context)
2. Block: `case pat { stmts }`
3. Nested `when`: `case pat when { ... }`
4. Nested `match`: `case pat match expr { ... }`

The same forms apply to the `else` arm.

The `else` arm is optional but recommended for catch-all behavior.
If no `else` arm is present and the match is not exhaustive,
the compiler will report an error.

For more details on patterns, see :doc:`patterns`.


The Unused Statement
--------------------
The `unused` statement declares that a signal is intentionally not used.
This suppresses compiler warnings about unused components or ports.

.. code-block:: grammar

    ModDefStmtUnused :=
        "unused" Path

.. code-block:: virdant

    mod Core {
        client socket mem of Mem
        unused mem.data           // data output is intentionally unused
        mem.addr := 0
    }

The path must refer to an existing signal, port, or instance field.


on Clause
---------
The `on` clause associates a register with a clock signal.

.. code-block:: grammar

    OnClause :=
        "on" Expr

.. code-block:: virdant

    reg r : Word[8] on clock

The expression after `on` must evaluate to a value of type `Clock`.

Every `reg` declaration must have an `on` clause.


it Block
--------
An `it` block groups statements under a declaration.
Inside the block, the keyword `it` refers to the enclosing declaration.

.. code-block:: grammar

    ItBlock :=
        ModDefStmtBlock

    ModDefStmtBlock :=
        "{" ModDefStmt* "}"

.. code-block:: virdant

    reg r : Word[8] on clock {
        it <= inp
        out := it
    }

The `it` block is syntactic sugar that avoids repeating the declaration name.
It is purely a matter of style --- the above is equivalent to:

.. code-block:: virdant

    reg r : Word[8] on clock
    r <= inp
    out := r