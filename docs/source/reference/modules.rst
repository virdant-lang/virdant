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

    ModDefStmtCore :=
        ModDefStmtComponent
        | ModDefStmtMemory
        | ModDefStmtDriver
        | ModDefStmtIndexedDriver
        | ModDefStmtInstance
        | ModDefStmtSocket
        | ModDefStmtUnused
        | ModDefStmtInitial
        | ModDefStmtDependsOn

    ModDefStmt :=
        ModDefStmtCore
        | If
        | Match
        | Let

The order of statements within a module body does not matter.
Virdant processes the module body as a set of declarations, not as a sequence
of imperative statements.


Ports
-----
Ports are the interface between a module and the outside world.
They are declared with a direction, a name, and a type.

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
A register is associated with a clock via the `on` keyword;
when the module declares exactly one component of type `Clock`,
the `on` clause may be omitted and the module's unique clock is used
implicitly.

.. code-block:: virdant

    reg counter : Word[8] on clock
    counter <= counter + 1

Registers update their value on the rising edge of the associated clock.
The value assigned to a register uses the `<=` assignment operator,
indicating a non-blocking (clocked) assignment.

A register may have an associated block for additional statements:

.. code-block:: virdant

    reg counter : Word[8] on clock {
        it <= if {
            when reset => 0
            else => it + 1
        }
    }

Inside the register's block, the special identifier `it` refers to the
register itself.


Memories
--------
A `memory` declaration creates a module-local, clocked, word-addressable
memory (a behavioral RAM or ROM) with dynamic indexing and explicit write
drivers. The depth is written in brackets after the component name —
mirroring how the memory is read — and the element type follows the colon:

.. code-block:: grammar

    ModDefStmtMemory :=
        DocString? Annotations "memory" Ident "[" Expr "]" ":" Type
                       OnClause? MemoryBlock?

    MemoryBlock := "{" MemoryBlockStmt* "}"

    MemoryBlockStmt :=
        ModDefStmtDriver
        | ModDefStmtIndexedDriver
        | ModDefStmtUnused
        | ModDefStmtInitial
        | If
        | Match
        | Let
        | ModDefStmtOnRegion

    ModDefStmtOnRegion := "on" Expr ModDefStmtBlock

.. code-block:: virdant

    mod Ram {
        incoming clock : Clock
        incoming write_data : Word[32]
        incoming addr : Word[10]
        incoming write_enable : Bit
        outgoing read_data : Word[32] dependson addr {
            it := mem[dyn addr]
        }

        memory mem[1024] : Word[32] {
            if {
                when write_enable {
                    it[dyn addr] <= write_data
                }
            }
        }
    }

`memory mem[1024] : Word[32]` reads as "a memory `mem` with 1024 slots, each
a `Word[32]`".

Shape rules:

* Both the depth and the element width must be literal powers of two; no
  generics are supported in these positions.
* A dynamic index must be *exactly* `Word[k]` where `depth == 2^k` — a wider
  address must be narrowed explicitly, never truncated silently.
* Static indices read and write one word (`mem[3]`); they must be in range.
* A memory is readable only through an index: a bare `mem` or a slice of a
  memory is an error.

Clocking and writes:

* A memory has one clock slot, resolved exactly like a `reg`'s: the `on`
  clause may be omitted when the module declares exactly one `Clock`.
* A write is an indexed latched driver: `mem[dyn idx] <= data` (the first
  indexed left-hand side in the language; `:=` is accepted with a warning).
* Inside a memory body, writes live either in an `on <CLOCK> { ... }`
  region (for multi-clock modules) or at the top level of the body (the
  same single-clock inference as `reg`); only writes to the memory are
  allowed inside an `on` region. A blockless memory may be written from a
  guarded module-level write site.
* A memory has exactly one write site, and sibling `when` clauses whose
  guards are not exclusive may not both write it — use `when`/`else` or
  `match` arms, whose arms are disjoint.

Reads are combinational and usable anywhere an expression is; multiple reads
at different addresses in one cycle are legal (multiple asynchronous read
ports). A write commits on the clock edge, and a read of an address written
in the same cycle sees the *old* value (read-before-write). Words never
written read as unspecified (pseudorandom in simulation). An `outgoing` port
that combinationally reads a memory declares `dependson` naming the index's
incoming ports: the only combinational dependency a read introduces is on
the index expression.

Memories lower to behavioral Verilog (inferred as block RAM by synthesis
tools):

.. code-block:: verilog

    reg [31:0] mem [0:1023];

    always @(posedge clock) begin
        if (write_enable) begin
            mem[addr] <= write_data;
        end
    end

    assign read_data = mem[addr];

Initialization: the `@image("path")` annotation loads a raw binary file
word-packed in declaration order (most-significant byte first within each
word), and `@image_hex("path")` a text hex file with one word per line (à
la `$readmemh`). Paths resolve relative to the project root — the
directory where `virdant.toml` is found. A file shorter than the depth leaves the remaining words
unspecified; a longer file is an error. A memory with an image is a ROM: it
has no write site, needs no clock, and writes to it are rejected. On
platforms annotated `@noinitial`, memory images are disallowed, exactly like
`initial` statements.

Tristate Nets
-------------
Tristate components model wires that more than one driver can drive,
where undriven means *floating* (high impedance) rather than
holding a value. There are two kinds:

* `bus` declares a bidirectional port. It is visible to the parent
  module, and it appears as an `inout` port in generated Verilog.
* `net` declares an internal tristate net. It is not a port; it is
  visible only within the module.

.. code-block:: virdant

    mod Participant {
        incoming pull : Bit

        bus sda : Bit {
            if {
                when pull {
                    it #= false
                }
            }
        }
    }

A `bus` or `net` may not have an `on` clause or a `default` clause.
It may have an `it` block, which is the usual place to put its drivers.

Reading a tristate component is different from reading a wire or
register: because the value may be floating, a plain reference is a
compile error (``NetReadWithoutTap``). Reads must go through the `tap`
operator, which asserts that the net is driven at that point:

.. code-block:: virdant

    seen := tap sda

See :doc:`statements` for the tristate driver operators (`#=`, `#-`,
and `#=#`) and :doc:`expressions` for `tap`.

Tristate components may have any number of drivers. When several drivers
are active at once, a strong driver (`#=`) wins over weak ones (`#-`);
if no driver is active, the net floats. An `if` or `match` guarding
only tristate drivers does not need to be exhaustive: an undriven arm
leaves the net floating rather than holding its previous value.

In generated Verilog, a `bus` port becomes an `inout wire`, a strong
drive becomes a plain `assign`, a weak drive becomes
`assign (weak1, weak0)`, and `#=#` becomes a `tran` connecting the two
sides.


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

        memsrv.mem :=: core.mem


Module Instances
----------------
Module instances allow you to compose larger designs from smaller modules.

.. code-block:: grammar

    ModDefStmtInstance :=
        DocString? Annotations "mod" Ident "of" Ofness GenericsParams? ItBlock?

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
in `virdant.toml` must be a concrete instantiation, and a generic
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
        DocString? Annotations "client" "socket" Ident "of" Ofness GenericsParams? ItBlock?
        | DocString? Annotations "server" "socket" Ident "of" Ofness GenericsParams? ItBlock?

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
        with #Idle {
            r <= 0w8
            out := 0w8
            state <= #Run
        }
        with #Run {
            r <= r + 1w8
            out := r
        }
        else {
            out := 0w8
        }
    }

The `else` arm catches any value not matched by the preceding `with` arms.
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
        it <= if {
            when reset => 0
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

The clock signal referenced in the `on` clause must be of type `Clock`.

Every `reg` declaration must have an `on` clause,
unless the module declares *exactly one* component of type `Clock`.
In that case the `on` clause is optional on every `reg` declaration,
and an omitted `on` clause is equivalent to writing `on` with that
component (see :doc:`statements`).
If the module declares zero or more than one component of type `Clock`,
every `reg` declaration needs an explicit `on` clause.
When the module does declare exactly one component of type `Clock`,
an explicit `on` clause that names that clock is redundant:
it is equivalent to omitting it, and the compiler emits a warning
encouraging the implicit form.
An `on` clause that references any other clock (for example, a
submodule's clock output) is not redundant and never warns.

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


The `dependson` List
--------------------
The `dependson` list is a modifier on an `outgoing` (or `outgoing wire`)
component declaration: the module's declaration of which `incoming` ports
the port *may* combinationally depend on.

.. code-block:: grammar

    DependsonList :=
        "dependson" Path ("," Path)*

.. code-block:: virdant

    pub mod Top {
        incoming inp : Bit

        outgoing out : Bit dependson inp {
            it := inp
        }
    }

Multiple entries are comma separated:

.. code-block:: virdant

    pub mod Top {
        incoming inp_0 : Bit
        incoming inp_1 : Bit

        outgoing out : Bit dependson inp_0, inp_1 {
            it := inp_0 && inp_1
        }
    }

The list is part of the module's *public interface*: it tells clients
what the port may depend on without looking inside the module.  A
declaration is a statement of *potential*, not of actuality: `dependson`
does not create a combinational dependency.  It only promises the port
may depend on the listed ports, and it must list every `incoming` port
the port can combinationally depend on (directly, through internal
signals, or through submodule declarations).

* An entry must resolve to an `incoming` port of the module.
  Anything else (an outgoing port, a register, an internal signal, a
  path into an instance) is an error.
* An undeclared dependency is an error: a port whose drivers (or whose
  submodules' declared potential) reach an `incoming` port that is not
  in its `dependson` list is rejected.
* A declared entry the port does not actually depend on is an
  *unused* `dependson`: a warning.  (There is one exception: an `ext
  mod`'s body is opaque, so its declarations cannot be checked and are
  never flagged as unused.)

Only `outgoing` and `outgoing wire` may carry the list.  The grammar
accepts it on any component kind so the checker can diagnose each
invalid placement precisely:

* `outgoing reg`: a registered output cannot change in the same cycle,
  so a declared dependency would be permanently false --- an error.
* `incoming`: an error.
* `bus`: tristate components are not yet covered by `dependson` --- an
  error.
* `wire`, `reg`, `net`: internal signals may not declare `dependson` ---
  an error.

Socket channels may declare `dependson` as well (see :doc:`sockets`).
A list on a `cosi` channel applies when the socket is instantiated as
a `client`, and a list on a `soci` channel as a `server`; it places
the same obligation on the containing module as if the `dependson` were
declared on the resulting port.  In the receiving role the list is
inert, and socket-derived obligations are exempt from the unused
warning (a protocol such as Wishbone legitimately varies its latency):

.. code-block:: virdant

    socket Wishbone {
        cosi cyc : Bit
        cosi stb : Bit
        soci ack : Bit dependson cyc, stb
    }


Combinational Cycle Detection
------------------------------
Virdant checks every module for combinational cycles over the module's
*potentiality graph*, whose edges are:

* the module's driver-derived combinational edges (`:=` targets
  depending on the signals their right-hand sides read, including the
  per-channel edges of `:=:` socket links);
* the module's own `dependson` lists and socket obligations;
* the `dependson` lists and socket obligations declared by the
  module's immediate submodule instances (the declarations are the
  boundary contract: Virdant never looks inside a child module).

Sequential (`<=`) edges are never traversed: a clock edge breaks the
combinational path.

A cycle in this graph is an error even when no actual combinational
path exists today.  `dependson` declares *potential*: a designer must
be free to make any declared dependency actual at any time without
breaking clients, so the cycle check operates on the declared relation.

Each cycle is reported once, at the module whose own local edge closes
it --- a loop living entirely inside a submodule is reported there, and
a loop spanning a parent and its children is reported at the parent:

.. code-block:: virdant

    mod Loop2 {
        mod loop_a of LoopA
        mod loop_b of LoopB

        loop_a.inp := loop_b.out
        loop_b.inp := loop_a.out
    }

    mod LoopA {
        incoming inp : Bit
        outgoing out : Bit dependson inp {
            it := inp
        }
    }

Here `loop_a.inp` depends on `loop_b.out`, which (by `LoopB`'s
declaration) may depend on `loop_b.inp`, which depends on `loop_a.out`,
which may depend on `loop_a.inp` --- a cycle in the `dependson`
relation, reported at `Loop2`.
