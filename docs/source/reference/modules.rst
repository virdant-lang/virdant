Modules
=======
Modules are the primary building blocks of Virdant designs.
A module defines a hardware block with ports, internal components, and
behavioral statements.


Module Body
-----------
The body of a module definition is a sequence of statements that declare
ports, components, instances, and control flow.

.. code-block:: grammar

    ModDefStmt :=
        ModDefStmtComponent
        | ModDefStmtDriver
        | ModDefStmtInstance
        | ModDefStmtSocket
        | ModDefStmtIf
        | ModDefStmtMatch
        | ModDefStmtUnused

The order of statements within a module body does not matter.
Virdant processes the module body as a set of declarations, not as a sequence
of imperative statements.


Ports
-----
Ports are the interface between a module and the outside world.
They are declared with a direction, a name, and a type.

.. code-block:: grammar

    ModDefStmtComponent :=
        DocString "incoming" Ident ":" Type OnClause? ItBlock?
        | DocString "outgoing" Ident ":" Type OnClause? ItBlock?
        | DocString "outgoing" "wire" Ident ":" Type OnClause? ItBlock?
        | DocString "outgoing" "reg" Ident ":" Type OnClause? ItBlock?
        | DocString "wire" Ident ":" Type OnClause? ItBlock?
        | DocString "reg" Ident ":" Type OnClause? ItBlock?

.. code-block:: virdant

    mod Passthrough {
        incoming inp : Word[8]
        outgoing out : Word[8]

        out := inp
    }

`incoming` ports receive values from outside the module.
`outgoing` ports send values from the module to the outside.


Wires
-----
A `wire` declaration creates an internal combinational signal.

.. code-block:: virdant

    wire sum : Word[8]
    sum := a + b

A wire may optionally have an `on` clause and a block, making it a clocked wire.
Wires may also be given an initial value through an `it` block.

.. code-block:: virdant

    wire w : Word[8] {
        w := a + b
    }


Registers
---------
A `reg` declaration creates a sequential (stateful) element.
Every register must have a clock associated with it via the `on` keyword.

.. code-block:: virdant

    reg counter : Word[8] on clock
    counter <= counter + 1

Registers update their value on the rising edge of the associated clock.
The value assigned to a register uses the `<=` assignment operator,
indicating a non-blocking (clocked) assignment.

A register may have an associated block for additional statements:

.. code-block:: virdant

    reg counter : Word[8] on clock {
        it <= if reset { 0 } else { it + 1 }
    }

Inside the register's block, the special identifier `it` refers to the
register itself.


Assignments
-----------
Virdant provides three assignment operators for driving signals.

`:=` (combinational assignment)
    Drives a combinational signal.
    The target is continuously driven by the expression.

    .. code-block:: virdant

        out := a + b          // combinational: out always equals a + b

`<=` (sequential assignment)
    Drives a register.
    The target updates on the next clock edge.

    .. code-block:: virdant

        reg r : Word[8] on clock
        r <= a + b            // sequential: r updates on next clock edge

`:=:` (bidirectional connection)
    Connects two signals bidirectionally.
    Both sides drive each other.

    .. code-block:: virdant

        memory.mem :=: core.mem


Module Instances
----------------
Module instances allow you to compose larger designs from smaller modules.

.. code-block:: grammar

    ModDefStmtInstance :=
        DocString "mod" Ident "of" Ofness ItBlock?

.. code-block:: virdant

    mod ha1 of HalfAdder
    ha1.a := a
    ha1.b := b
    sum := ha1.sum
    carry := ha1.carry

The `mod` keyword introduces an instance, followed by a name for the instance,
the `of` keyword, and the type (module name) being instantiated.

Ports on the instance are accessed using dot notation: `<instance>.<port>`.

An instance may optionally have an `it` block for grouping connections:

.. code-block:: virdant

    mod ha1 of HalfAdder {
        it.a := a
        it.b := b
    }
    sum := ha1.sum
    carry := ha1.carry


Socket Instances
----------------
Sockets provide a way to group related ports and connect them as a unit.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString "client" "socket" Ident "of" Ofness ItBlock?
        | DocString "server" "socket" Ident "of" Ofness ItBlock?

.. code-block:: virdant

    client socket mem of Mem {
        it.addr := 0
        unused it.data
    }

A socket instance must specify its role: either `client` or `server`.
The role determines the direction of the individual ports within the socket.

For more details on sockets, see :doc:`sockets`.


Conditional Statements
----------------------
Conditional statements allow a module to contain different components
depending on a condition.

.. code-block:: grammar

    ModDefStmtIf :=
        ModDefStmtIfStart ModDefStmtIfMiddle* ModDefStmtIfEnd?

    ModDefStmtIfStart :=
        "if" Expr ModDefStmtBlock

    ModDefStmtIfMiddle :=
        "else" "if" Expr ModDefStmtBlock

    ModDefStmtIfEnd :=
        "else" ModDefStmtBlock

.. code-block:: virdant

    if reset {
        r <= 0
        state := Idle
    } else {
        r <= r + 1
        state := Active
    }

Conditional statements select between different sets of module body statements
based on a condition expression.


Match Statements
----------------
Match statements enable pattern-based conditional logic in module bodies.

.. code-block:: grammar

    ModDefStmtMatch :=
        "match" Expr "{" ModDefStmtMatchArm* "}"

    ModDefStmtMatchArm :=
        "case" Pat "=>" ModDefStmtBlock
        | "else" "=>" ModDefStmtBlock

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

The `else` arm catches any value not matched by the preceding `case` arms.
If no `else` arm is present, the match must be exhaustive.


Unused Declarations
-------------------
The `unused` keyword indicates that a signal is intentionally not used.
This suppresses compiler warnings about unused signals.

.. code-block:: grammar

    ModDefStmtUnused :=
        "unused" Path

.. code-block:: virdant

    unused mem.data

`unused` takes a path that may refer to a component, a port, or a field
of an instance.


The it Identifier
-----------------
Inside certain blocks (such as register blocks and instance blocks),
the special identifier `it` refers to the enclosing declaration.

For a register `it` refers to the register itself.
For an instance `it` refers to the instance.

.. code-block:: virdant

    reg counter : Word[8] on clock {
        it <= if reset { 0 } else { it + 1 }
    }

    mod fifo of Fifo {
        it.clock := clock
        it.data := data
    }

The `it` identifier is only available within the block of a declaration
that provides it.


On Clause
---------
The `on` clause associates a register with a clock signal.

.. code-block:: virdant

    reg r : Word[8] on clock

Every register must have exactly one `on` clause.
The clock signal referenced in the `on` clause must be of type `Clock`.

The register latches its input value on the rising edge of the named clock.
All registers driven by the same clock form a single clock domain.


Driver Blocks
-------------
Driver blocks are groups additional module body
statements under a declaration.
The keyword `it` inside the block refers to the enclosing declaration.

.. code-block:: virdant

    reg r : Word[8] on clock {
        it <= 0
        out := it
    }

Blocks are optional.
When present, they provide a scoped grouping for related assignments and
connections.
