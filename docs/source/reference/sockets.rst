Sockets
=======
Sockets are a way for Virdant modules to expose a set of ports as a named group.
You may connect all the ports together in a single bidirectional assignment,
rather than wiring each port individually.


Socket Definitions
------------------
A socket definition is a top-level declaration that names a group of ports
with their types and directions.

.. code-block:: grammar

    SocketDef :=
        DocString? Annotations "pub"? "socket" Ident Generics? "{" SocketDefStmt* "}"

    SocketDefStmt :=
        DocString? Annotations "outgoing" Ident ":" Type
        | DocString? Annotations "incoming" Ident ":" Type

.. code-block:: virdant

    socket Mem {
        outgoing addr : Word[16]
        incoming data : Word[8]
    }

The direction keywords have the following meanings, from the perspective of a
forward socket instance:

`outgoing`
    For a forward instance, this port is an output.
    For a reverse instance, this port is an input.

`incoming`
    For a forward instance, this port is an input.
    For a reverse instance, this port is an output.

Thus `outgoing` ports are driven by the forward side and received by the
reverse side, while `incoming` ports are driven by the reverse side and
received by the forward side.


Generic Socket Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A socket definition may declare *generic parameters*, exactly like type
and module definitions (see :doc:`types`).
The parameters are a comma-separated `name : kind` list in square brackets
after the socket name, and the parameter names are in scope in the channel
types of the body:

.. code-block:: virdant

    socket Bus[T : Type] {
        outgoing req : T
        incoming rsp : T
    }

Both channels of `Bus` have type `T`.
The definition is analyzed once, generically, with the parameters unbound;
concrete channel types exist only per instantiation, which supplies the
actual types (see `Type Arguments`_ below).


Socket Instances
----------------
Socket instances are declared inside module bodies.
Each instance must specify its role: `forward` or `reverse`.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString? Annotations "forward" "socket" Ident "of" Ofness GenericsParams? ItBlock?
        | DocString? Annotations "reverse" "socket" Ident "of" Ofness GenericsParams? ItBlock?

.. code-block:: virdant

    mod Core {
        forward socket mem of Mem
        unused mem.data
        mem.addr := 0
    }

    mod Memory {
        reverse socket mem of Mem
        unused mem.addr
        mem.data := 10
    }

A forward socket instance uses `outgoing` ports as outputs (driven by the
forward side) and `incoming` ports as inputs (received by the forward side).
A reverse socket instance reverses these directions.


Type Arguments
~~~~~~~~~~~~~~

An instance of a generic socket supplies *type arguments* after the socket
definition name, in square brackets and in the same order as the declared
parameters:

.. code-block:: virdant

    mod Master {
        incoming d : Word[8]
        forward socket bus of Bus[Word[8]] {
            it.req := d
        }
        unused bus.rsp
    }

    mod Slave {
        reverse socket bus of Bus[Word[8]] {
            it.rsp := it.req
        }
    }

The channel types of the instance are the definition's channel types with
the arguments substituted: both `bus.req` and `bus.rsp` have type
`Word[8]` in the examples above, so `Slave` may assign `it.rsp` straight
from `it.req`.

The arguments are checked at the instantiation site: a `Type` parameter
takes a type expression and a `Nat` parameter a number literal, in exactly
the number declared.
Instantiating the builtin `Pipe` with no arguments:

.. code-block:: virdant

    forward socket p of Pipe

reports ``Type builtin::Pipe takes 1 type argument, but 0 were given``.
Instantiating `Bus` at a number:

.. code-block:: virdant

    forward socket bus of Bus[8]

reports ``Type argument mismatch: parameter T of Bus expects a type, but
found a Nat literal``.


Bidirectional Connections
-------------------------
A pair of socket instances (one forward, one reverse) may be connected
bidirectionally using the `:=:` operator.
This connects every port in the socket at once, with the appropriate
direction for each instance.

.. code-block:: virdant

    mod Top {
        mod core of Core
        mod memsrv of Memory

        memsrv.mem :=: core.mem
    }

The `:=:` operator connects every port in the `Mem` socket between
the `core` and `memsrv` instances.
The `addr` port is driven by `core` and received by `memsrv`.
The `data` port is driven by `memsrv` and received by `core`.

This is equivalent to manually wiring each port:

.. code-block:: virdant

    memsrv.mem.addr := core.mem.addr
    core.mem.data := memsrv.mem.data


Mono-Socket Matching
~~~~~~~~~~~~~~~~~~~~

When the connected sockets are generic, `:=:` matches on the *mono
socket*: the socket definition together with its type arguments.
A connection joins two instances of the same socket definition
instantiated at the same type arguments.
Two instances of the same socket definition at different arguments are
*different* mono sockets and do not connect.

The arguments written at a socket instantiation inside a generic module
compose with the arguments of the enclosing module instances:

.. code-block:: virdant

    mod Master[W : Type] {
        incoming d : W
        forward socket bus of Bus[W] {
            it.req := d
        }
        unused bus.rsp
    }

    mod Slave[W : Type] {
        reverse socket bus of Bus[W] {
            it.rsp := it.req
        }
    }

    pub mod Top {
        incoming inp : Word[8]

        mod master of Master[Word[8]] {
            it.d := inp
        }
        mod slave of Slave[Word[8]] {
            master.bus :=: it.bus
        }

        outgoing out : Word[8]
        out := slave.bus.rsp
    }

`master.bus` instantiates `Bus` at `W`, and the instance `master` supplies
`Word[8]` for `W`, so `master.bus` is the mono socket `Bus[Word[8]]` ---
and so is `it.bus` inside `slave`'s it-block.
The connection therefore checks.
Connections written inside a module instance's it-block are checked the
same way: `it` resolves to the instance, and the instance's arguments
compose with the socket's.

A mismatch is an error whose diagnostic names both instantiations and
points at the connection.
If `master`'s socket instantiates `Link[Word[8]]` while `slave`'s
instantiates `Link[Word[9]]`:

.. code-block:: virdant

    slave.pipe :=: master.pipe

reports:

.. code-block:: text

    Cannot connect Link[Word[9]] to Link[Word[8]]: mismatched socket instantiations


Accessing Socket Ports
----------------------
Individual ports on a socket instance are accessed through a two-level path:

.. code-block:: virdant

    <instance>.<socket_instance>.<port>

.. code-block:: virdant

    core.mem.addr := 0
    result := memsrv.mem.data

The first component is the module instance name, the second is the socket
instance name, and the third is the port name within the socket definition.


Sockets as Interfaces
---------------------
Sockets are useful for defining standard bus interfaces that can be reused
across multiple modules.

For example, a TileLink UL interface or a Wishbone bus can be defined as a
socket and then instantiated by any module that speaks that protocol.

.. code-block:: virdant

    socket TileLinkUL {
        outgoing a_address  : Word[32]
        outgoing a_opcode   : Word[3]
        outgoing a_size     : Word[2]
        outgoing a_data     : Word[32]
        outgoing a_mask     : Word[4]
        incoming d_data     : Word[32]
        incoming d_opcode   : Word[3]
        incoming d_size     : Word[2]
    }

By defining the interface as a socket, the compiler can verify that all
connections are complete and correctly oriented.


Builtin Sockets
---------------
Virdant ships a single builtin socket, in the `builtin` package of the
standard library (`lib/builtin.vir`): :vir:`Pipe[T]`, a ready/valid
handshake channel.

.. code-block:: virdant

    pub socket Pipe[T : Type] {
        outgoing data  : Valid[T]
        incoming ready : Bit
    }

The two ports form a handshake pair:

* :vir:`outgoing data : Valid[T]` --- the payload.
  :vir:`@Valid(v)` carries a value `v` of type `T`, while :vir:`@Invalid()`
  marks the pipe as empty.
  The forward side drives `data` and the reverse side receives it.

* :vir:`incoming ready : Bit` --- readiness.
  The reverse side drives `ready` and the forward side receives it.

A :vir:`forward socket` instance of :vir:`Pipe[T]` therefore drives
:vir:`data` and reads :vir:`ready`, while a :vir:`reverse socket` instance
reads :vir:`data` and drives :vir:`ready`.

Like any generic socket, :vir:`Pipe[T]` is instantiated with explicit type
arguments, e.g. :vir:`of Pipe[Word[8]]`.
And like everything in the builtin package, it is loaded implicitly by
every design: no :vir:`import` is needed, and builtin items are referenced
unqualified.
The name `builtin` is a keyword, so it cannot be written as a package
qualifier: `builtin::Pipe[...]` is not valid Virdant.


A Producer/Consumer Pair
~~~~~~~~~~~~~~~~~~~~~~~~
The example below is a compact end-to-end pair over :vir:`Pipe[Word[8]]`:
a producer drives a :vir:`Valid` payload into its end of the pipe, a
consumer asserts readiness, and a top module connects the two with
:vir:`:=:`.

.. code-block:: virdant

    // A producer: pushes a Valid payload into its end of the pipe.
    mod Producer {
        // The payload the producer pushes into the pipe.
        incoming d : Word[8]

        forward socket pipe of Pipe[Word[8]] {
            it.data := @Valid(d)
        }

        // The consumer's readiness, received back through the pipe.
        outgoing ready : Bit
        ready := pipe.ready
    }

    // A consumer: asserts readiness and forwards the received payload.
    mod Consumer {
        reverse socket pipe of Pipe[Word[8]] {
            it.ready := true
        }

        // The payload received from the producer.
        outgoing data : Valid[Word[8]]
        data := pipe.data
    }

    pub mod Top {
        incoming d : Word[8]

        mod producer of Producer {
            it.d := d
        }
        mod consumer of Consumer

        consumer.pipe :=: producer.pipe

        // The consumer's readiness flows back to the producer through the pipe.
        outgoing ready : Bit
        ready := producer.ready

        // The producer's payload, unwrapped where the argument is concrete.
        outgoing valid : Bit
        outgoing data : Word[8]
        valid := match consumer.data {
            with @Valid(dontcare) => true
            with @Invalid() => false
        }
        data := match consumer.data {
            with @Valid(payload) => payload
            with @Invalid() => 0
        }
    }

The :vir:`:=:` connection joins the two socket instances port by port:
the producer's :vir:`@Valid(d)` payload appears on the consumer's
:vir:`pipe.data`, and the consumer's readiness flows back to the producer's
:vir:`pipe.ready`.

The `dependson` List on Channels
--------------------------------
A socket channel may carry a `dependson` list, naming channels of the
opposite direction word of the same socket:

.. code-block:: virdant

    socket Wishbone {
        outgoing cyc : Bit
        outgoing stb : Bit
        incoming ack : Bit dependson cyc, stb
    }

When the socket is instantiated in the channel's driving role (`forward`
for `outgoing` channels, `reverse` for `incoming` channels), the list places the
same obligation on the containing module as if the `dependson` were
declared on the resulting port: the module's drive on that channel may
combinationally depend on its reads of the listed channels.  In the
receiving role the list is inert.

Unlike `dependson` on module ports, a socket channel's list never
produces an unused warning when the drive does not exercise it:
protocols such as Wishbone and TileLink support same-cycle responses but
also arbitrary delays, so the declared potential may legitimately stay
dormant.  It is still part of the contract: the containing module must
not drive the channel with anything outside the declared set, and cycles
in the composed `dependson` relation (for example, both sides of a link
declaring same-cycle responses) are errors.  See :doc:`modules`.

The list covers same-socket channels only.  Dependencies on other
sockets' channels or on plain `incoming` ports are declared with
`where` clauses attached to the socket instance declaration (see
:doc:`modules`, "The `where` Clause on Socket Instances"):

.. code-block:: virdant

    mod PassThrough {
        forward socket out of Pipe[Word[8]]
            where it.data dependson inp.data
        {
            it.data := inp.data
        }

        reverse socket inp of Pipe[Word[8]]
            where it.ready dependson out.ready
        {
            it.ready := out.ready
        }
    }

A driven channel's declared set — its socketdef obligations plus its
`where` entries — must be exhaustive over every incoming component the
drive can combinationally reach; an undeclared one is an error.
