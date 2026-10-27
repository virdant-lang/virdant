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
        | ModDefStmtMatch
        | ModDefStmtUnused
        | ModDefStmtDependsOn

The order of statements within a module body does not matter.
Virdant processes the module body as a set of declarations, not as a sequence
of imperative statements.


Ports
-----
Ports are the interface between a module and the outside world.
They are declared with a direction, a name, and a type.

.. code-block:: grammar

    ModDefStmtComponent :=
        DocString Annotations "incoming" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "outgoing" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString Annotations "outgoing" "wire" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString Annotations "outgoing" "reg" Ident ":" Type DefaultClause? OnClause? ItBlock?
        | DocString Annotations "wire" Ident ":" Type OnClause? ItBlock?
        | DocString Annotations "reg" Ident ":" Type OnClause? ItBlock?

    DefaultClause := "default" Expr

.. code-block:: virdant

    mod Passthrough {
        incoming inp : Word[8]
        outgoing out : Word[8]

        out := inp
    }

`incoming` ports receive values from outside the module.
`outgoing` ports send values from the module to the outside.

A `default` clause on an `outgoing` port is only allowed in a platform
definition; in a module it is an error (``DefaultOutsidePlatformPort``).
See :doc:`platforms` for how platform defaults synthesize ports a
module does not declare.


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
        it <= when {
            case reset => 0
            else => it + 1
        }
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
        DocString Annotations "mod" Ident "of" Ofness GenericsParams? ItBlock?

.. code-block:: virdant

    mod ha1 of HalfAdder
    ha1.a := a
    ha1.b := b
    sum := ha1.sum
    carry := ha1.carry

The `mod` keyword introduces an instance, followed by a name for the instance,
the `of` keyword, and the type (module name) being instantiated.
When the module being instantiated is a generic module, the type arguments
follow the module name in square brackets, as in :vir:`mod r8 of Reg[Word[8]]`.
See `Generic Modules`_ below.

Ports on the instance are accessed using dot notation: `<instance>.<port>`.

An instance may optionally have an `it` block for grouping connections:

.. code-block:: virdant

    mod ha1 of HalfAdder {
        it.a := a
        it.b := b
    }
    sum := ha1.sum
    carry := ha1.carry


Generic Modules
---------------
A module definition may declare *generic parameters*: a comma-separated
list in square brackets after the module name, each parameter written
`name : kind`, exactly like a generic type definition (see :doc:`types`).
The two kinds are :vir:`Type` (the parameter stands for a whole type) and
:vir:`Nat` (a number literal, as in :vir:`Word[n : Nat]`).

.. code-block:: virdant

    mod Reg[T : Type] {
        incoming clk : Clock
        incoming d : T
        reg slot : T on clk {
            it <= d
        }
        outgoing q : T
        q := slot
    }

Generic parameters are in scope inside the body of the declaring module:
port and component types may reference them by name
(:vir:`d : T` and :vir:`slot : T` above).
A generic module is a template: it is not emitted to Verilog on its own
and has no concrete port widths until it is instantiated.


Instantiating a Generic Module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An instantiation of a generic module supplies *type arguments* after the
module name, comma-separated and in the same order as the declared
parameters:

.. code-block:: virdant

    pub mod Top {
        incoming clk : Clock
        incoming a : Word[8]
        incoming wide : Word[16]

        mod r8 of Reg[Word[8]] {
            it.clk := clk
            it.d := a
        }
        mod r16 of Reg[Word[16]] {
            it.clk := clk
            it.d := wide
        }

        outgoing out8 : Word[8]
        outgoing out16 : Word[16]
        out8 := r8.q
        out16 := r16.q
    }

Inside the it-block of an instantiation the parameters are substituted
with the arguments: `it.d` of :vir:`r8` has type :vir:`Word[8]`, while
`it.d` of :vir:`r16` has type :vir:`Word[16]`.
Connecting a :vir:`Word[8]` signal to :vir:`r16` is therefore rejected:

.. code-block:: virdant

    mod r16 of Reg[Word[16]] {
        it.clk := clk
        it.d := a            // a : Word[8] --- wrong width for T = Word[16]
    }

reports ``Wrong type: Expected Word[16] but found Word[8]``.

The same generic module may be instantiated several times with different
arguments in one design.
Each distinct combination of a module definition and concrete type
arguments is a *mono module* (a monomorphized instantiation);
two instances with identical arguments share one mono module.

The number and the kinds of the type arguments are checked at the
instantiation site, so diagnostics point at the arguments as written:

*Wrong arity.* :vir:`mod f of Reg` reports ``Type Reg takes 1 type
argument, but 0 were given``.

*Wrong kind.* :vir:`mod f of Reg[8]` reports ``Type argument mismatch:
parameter T of Reg expects a type, but found a Nat literal``.


Restrictions
~~~~~~~~~~~~
Two hard rules restrict where generic parameters may appear.

An :vir:`ext mod` cannot have generic parameters: external modules
parameterize through `@parameter` annotations instead
(see :doc:`annotations`).
The rule cuts both ways: an ext-mod instantiation cannot take type
arguments either, even when the ext mod declares none.
:vir:`ext mod BadCell[T : Type]` reports ``ext mod BadCell cannot have
generics: ext mods do not support type parameters (use @parameter
annotations instead)``.

A generic module cannot be a top: the top module named by `[prog] top`
in `Virdant.toml` must be a concrete instantiation, and a generic
definition has none.  (`pub mod`s may be generic.)
Building a project whose `[prog] top` names a generic module reports
``top module top::Fifo cannot have generic parameters: a top module must
be a concrete instantiation``.


Monomorphized Verilog Generation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Verilog generation emits exactly one Verilog module per distinct mono
module --- one per (definition, type arguments) combination.
Instantiations with identical arguments share the emitted module;
a generic module that is never instantiated emits nothing.

Mono module names carry a hash of the type arguments, so instantiations
at different arguments never collide, and a ``//`` comment above each
emitted declaration records the readable origin.
For :vir:`Top` above, the generated Verilog contains:

.. code-block:: verilog

    // top::Reg[Word[8]]
    module \top::Reg#bde2499a (
        input  wire             clk,
        input  wire      [7:0]  d,
        output wire      [7:0]  q
    );
        /* ... */
    endmodule

    // top::Reg[Word[16]]
    module \top::Reg#e5a295e4 (
        input  wire             clk,
        input  wire     [15:0]  d,
        output wire     [15:0]  q
    );
        /* ... */
    endmodule

and :vir:`Top` wires each instance up to its mono module by that name:

.. code-block:: verilog

    \top::Reg#bde2499a  r8(
        .clk(\r8.clk ),
        .d  (\r8.d ),
        .q  (\r8.q )
    );


Socket Instances
----------------
Sockets provide a way to group related ports and connect them as a unit.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString Annotations "client" "socket" Ident "of" Ofness ItBlock?
        | DocString Annotations "server" "socket" Ident "of" Ofness ItBlock?

.. code-block:: virdant

    client socket mem of Mem {
        it.addr := 0
        unused it.data
    }

A socket instance must specify its role: either `client` or `server`.
The role determines the direction of the individual ports within the socket.

For more details on sockets, see :doc:`sockets`.


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
        it <= when {
            case reset => 0
            else => it + 1
        }
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


dependson Statements
--------------------
The `dependson` statement declares a combinational dependency between two
signals that the compiler cannot infer on its own.

.. code-block:: grammar

    ModDefStmtDependsOn :=
        Path "dependson" Path

The first path is the dependent signal and the second is the dependee:
changes to the dependee combinationally affect the dependent.
Virdant adds a combinational edge from the dependent to the dependee in
the module's dependency graph.

.. code-block:: virdant

    ext mod PLL {
        incoming clk_in  : Clock
        outgoing clk_out : Clock {
            it dependson clk_in
        }
    }

In this example the PLL is an `ext mod`: its body is opaque to Virdant,
so the compiler cannot see that `clk_out` is derived from `clk_in`.
The `dependson` statement tells Virdant about that dependency.

A standalone `dependson` (outside an `it` block) is also allowed:

.. code-block:: virdant

    ext mod ExternalMux {
        incoming sel    : Bit
        incoming a      : Bit
        incoming b      : Bit
        outgoing result : Bit

        result dependson sel
        result dependson a
        result dependson b
    }

For internal modules, `dependson` documents a dependency chain without
requiring the full driver expression:

.. code-block:: virdant

    mod Internal {
        incoming clk : Clock
        incoming inp : Word[8] on clk

        reg stored : Word[8] on clk
        stored <= inp

        outgoing out_val : Word[8] on clk {
            it dependson stored
            it := stored
        }
    }

Use `dependson` sparingly.
It only adds edges to the dependency graph; it does not create a driver.
The dependent signal still needs a real driver (`:=`, `<=`, or `:=:`).


Combinational Cycle Detection
------------------------------
Virdant builds a dependency graph for every module and checks it for
combinational cycles.
Each `:=` and `:=:` driver adds combinational edges from its target to
every signal read on the right-hand side.
Each `dependson` statement adds a combinational edge from its dependent
to its dependee.

The checker stitches the module's own graph together with the already
built graphs of its immediate submodule instances, prefixing each
submodule path with the instance name.
A cycle is reported at the module that closes it --- the lowest module
whose own local edge completes a loop --- so each loop is reported
exactly once.

.. code-block:: virdant

    mod Loop {
        wire a : Word[8] on clk
        wire b : Word[8] on clk
        wire c : Word[8] on clk

        a := 1
        b := 2
        c := 3

        a dependson b
        b dependson c
        c dependson a
    }

The three `dependson` edges form a cycle `a -> b -> c -> a`.
Although each wire also has a literal driver (a self-loop, which is
skipped), the `dependson` edges alone close a combinational loop and
the compiler reports an error.

Cycles that cross module boundaries are detected the same way:

.. code-block:: virdant

    mod Loop2 {
        mod loop_a of LoopA
        mod loop_b of LoopB

        loop_a.inp := loop_b.out
        loop_b.inp := loop_a.out
    }

Here `loop_a.inp` depends on `loop_b.out`, which depends on
`loop_b.w`, which depends on `loop_b.inp`, which depends on
`loop_a.out` --- a stitched combinational loop reported at `Loop2`.
