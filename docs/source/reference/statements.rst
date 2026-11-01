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
* Conditional statements (`when`)
* Match statements
* Let bindings
* Dependency declarations (`dependson`)
* Unused declarations
* Initial declarations

Statements are order-independent within a module body.
Virdant processes them as a set of declarations, not as a sequence of
imperative steps.


Component Declarations
----------------------
Component declarations introduce named signals into a module.

.. code-block:: grammar

    ModDefStmtComponent :=
        DocString? Annotations "incoming" Ident ":" Type OnClause? ItBlock?
        | DocString? Annotations "outgoing" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString? Annotations "bus" Ident ":" Type ItBlock?
        | DocString? Annotations "net" Ident ":" Type ItBlock?
        | DocString? Annotations "outgoing" "wire" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString? Annotations "outgoing" "reg" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString? Annotations "wire" Ident ":" Type OnClause? ItBlock?
        | DocString? Annotations "reg" Ident ":" Type OnClause? ItBlock?

    DefaultClause := "default" Expr

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
Tristate components (`bus` and `net`) are described in :doc:`modules`.


Driver Statements
-----------------
Driver statements assign values to components.

.. code-block:: grammar

    ModDefStmtDriver :=
        Path ":=" Expr
        | Path "<=" Expr
        | Path "#=" Expr
        | Path "#-" Expr
        | Path "#=#" Path
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


Tristate Drivers
~~~~~~~~~~~~~~~~
Three driver operators work on tristate components (`bus` and `net`,
see :doc:`modules`). Unlike `:=` and `<=`, they carry a *strength*,
and a tristate component may have any number of drivers.

Tristate drive `#=`
^^^^^^^^^^^^^^^^^^^
The `#=` operator drives a bus or net at full strength.
If the driver is guarded by a `when` or `match` arm that does not fire,
the component is undriven in that case and floats (high impedance):

.. code-block:: virdant

    bus sda : Bit {
        when {
            case pull {
                it #= false      // drive low strongly
            }
            // no `else`: when not pulling, the line floats
        }
    }

Weak drive `#-`
^^^^^^^^^^^^^^^
The `#-` operator drives a bus or net *weakly*, like a pull resistor.
Any strong driver (`#=`) overrides a weak one on the same net.
In generated Verilog, a weak drive is emitted as
`assign (weak1, weak0) ...`:

.. code-block:: virdant

    net sda : Bit
    sda #- true          // weak pull-up: idle-high

Net attachment `#=#`
^^^^^^^^^^^^^^^^^^^^
The `#=#` operator attaches a submodule's `bus` port to a `bus` or
`net` in the current module, joining them into one electrically shared
line. Both operands must be paths to tristate components:

.. code-block:: virdant

    net sda : Bit
    mod master of Participant
    master.sda #=# sda

When several drivers share a net, a strong driver wins over weak ones;
if no driver is active, the net floats.
A `when` or `match` guarding only tristate drivers does not need to be
exhaustive: an undriven case leaves the net floating rather than
holding its previous value.


Instance Declarations
---------------------
Instance declarations instantiate a submodule inside the current module.

.. code-block:: grammar

    ModDefStmtInstance :=
        DocString? Annotations "mod" Ident "of" Ofness GenericsParams? ItBlock?

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
        DocString? Annotations "client" "socket" Ident "of" Ofness GenericsParams? ItBlock?
        | DocString? Annotations "server" "socket" Ident "of" Ofness GenericsParams? ItBlock?

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

    When := "when" "{" WhenArm* "}"

    WhenArm :=
        "case" Expr "=>" Expr
        | "case" Expr ArmBlock
        | "case" Expr When
        | "case" Expr Match
        | "case" Expr Let
        | "else" "=>" Expr
        | "else" ArmBlock
        | "else" When
        | "else" Match
        | "else" Let

    ArmBlock :=
        "{" ModDefStmt* "}"
        | "{" ArmBlockExpr "}"

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
            state := #Idle
        }
    }

Each condition must be of type `Bit`.
The `else` arm is optional in statement context,
though it is recommended for completeness.

Arm Syntax
~~~~~~~~~~
Each arm may use one of five forms:

1. Bare expression: `case cond => expr` (rarely used in statement context)
2. Block: `case cond { stmts }`
3. Nested `when`: `case cond when { ... }`
4. Nested `match`: `case cond match expr { ... }`
5. Let binding: `case cond let x = expr { ... }`

The same forms apply to the `else` arm.

Conditional statements are different from `when` expressions:
statements can contain declarations, assignments, instances, and other
module body constructs, while `when` expressions only compute values.


Match Statements
----------------
Match statements perform pattern-based dispatch in the module body.

.. code-block:: grammar

    Match := "match" Expr "{" MatchArm* "}"

    MatchArm :=
        "case" Pat "=>" Expr
        | "case" Pat ArmBlock
        | "case" Pat When
        | "case" Pat Match
        | "case" Pat Let
        | "else" "=>" Expr
        | "else" ArmBlock
        | "else" When
        | "else" Match
        | "else" Let

    ArmBlock :=
        "{" ModDefStmt* "}"
        | "{" ArmBlockExpr "}"

.. code-block:: virdant

    enum type StateE {
        Idle
        Run
        Halt
    }

    reg state : StateE on clock
    reg r : Word[8] on clock
    outgoing out : Word[8]

    match state {
        case #Idle {
            r <= 0w8
            out := 0w8
            state <= #Run
        }
        case #Run {
            r <= r + 1w8
            out := r
        }
        else {
            out := 0w8
        }
    }

Arm Syntax
~~~~~~~~~~
Each arm may use one of five forms:

1. Bare expression: `case pat => expr` (rarely used in statement context)
2. Block: `case pat { stmts }`
3. Nested `when`: `case pat when { ... }`
4. Nested `match`: `case pat match expr { ... }`
5. Let binding: `case pat let x = expr { ... }`

The same forms apply to the `else` arm.

The `else` arm is optional but recommended for catch-all behavior.
If no `else` arm is present and the match is not exhaustive,
the compiler will report an error.

For more details on patterns, see :doc:`patterns`.


Let Bindings
------------
A `let` statement binds names to values for use within a scope.
Bindings may be untyped (the type is inferred) or carry an explicit type,
and several bindings may be given in a comma-separated list:

.. code-block:: grammar

    Let :=
        "let" LetBindingList ArmBlock
        | "let" LetBindingList "=>" Expr
        | "let" LetBindingList When
        | "let" LetBindingList Match

    LetBindingList :=
        LetBinding
        | LetBindingList "," LetBinding

    LetBinding :=
        Ident ":" Type "=" Expr
        | Ident "=" Expr

.. code-block:: virdant

    let x = a {
        out := x + x
    }

    let y : Word[8] = a + b {
        out := y
    }

    let lo = a, hi = b {
        out := hi + lo
    }

A `let` may be chained directly into a `when` or `match`, in which case
the bindings are in scope throughout the chained arms:

.. code-block:: virdant

    let x = a match op {
        case #Add {
            out := x + b
        }
        else {
            out := x
        }
    }

A `let` may also appear as the body of a `when` or `match` arm
(`case cond let x = expr { ... }`).


Terminator Style
~~~~~~~~~~~~~~~~
A `let` body may be either a block (`{ ... }`) or a value (`=> expr`).
The two spellings carry intent, and the compiler enforces the pairing:

* `{` means *this scope performs*: it runs driver statements.
* `=>` means *this scope produces*: it evaluates to a value.

A `let` used as a statement must terminate in a block; a `let` used as
an expression must produce a value with `=>`. A statement `let` whose
block contains nothing but a single bare expression is also flagged,
since the scope produces a value that is then discarded:

.. code-block:: virdant

    // Error: a statement `let` must use `{ ... }` instead of `=>`
    let x = a => x

    // OK
    let x = a {
        out := x
    }

The chained forms (`let ... when { ... }` and `let ... match { ... }`)
are legal in both statement and expression position and are never
flagged by the style check.


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


The Initial Statement
---------------------
The `initial` statement declares the value a register holds when the design
starts up (for example, after an FPGA is configured).

.. code-block:: grammar

    ModDefStmtInitial :=
        "initial" Path "<=" Expr

.. code-block:: virdant

    mod Top {
        incoming clock : Clock

        reg counter : Word[3] on clock {
            initial it <= 7

            when {
                case it > 0 {
                    it <= it - 1
                }
            }
        }
    }

The target must be a `reg` or `outgoing reg`,
and the value should be a constant expression of the register's type.
A register without an `initial` statement starts at 0 in the generated Verilog.

An `initial` statement is not a driver:
a register with an `initial` statement still needs its usual `<=` driver.
Each register may have at most one `initial` statement,
and `initial` may not appear inside a `when` or `match` block.


on Clause
---------
The `on` clause associates a register with a clock signal.

.. code-block:: grammar

    OnClause :=
        "on" Expr

.. code-block:: virdant

    reg r : Word[8] on clock

The expression after `on` must evaluate to a value of type `Clock`.

Every `reg` declaration must have an `on` clause,
unless the module declares *exactly one* component of type `Clock`.
In that case the `on` clause is optional on every `reg` declaration,
and an omitted `on` clause is equivalent to writing `on` with that
component:

.. code-block:: virdant

    mod Counter {
        incoming clock : Clock

        // No `on` clause needed: `clock` is the module's only
        // Clock-typed component.
        reg count : Word[8]
        count <= count + 1
    }

If the module declares zero or more than one component of type `Clock`,
every `reg` declaration needs an explicit `on` clause.


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
