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
        DocString Annotations "socket" Ident Generics? "{" SocketDefStmt* "}"

    SocketDefStmt :=
        DocString Annotations "cosi" Ident ":" Type
        | DocString Annotations "soci" Ident ":" Type

.. code-block:: virdant

    socket Mem {
        cosi addr : Word[16]
        soci data : Word[8]
    }

The direction keywords have the following meanings, from the perspective of a
client socket instance:

`cosi`
    **C**\ lient-**O**\ ut, **S**\ erver-**I**\ n.
    For a client instance, this port is an output.
    For a server instance, this port is an input.

`soci`
    **S**\ erver-**O**\ ut, **C**\ lient-**I**\ n.
    For a client instance, this port is an input.
    For a server instance, this port is an output.

Thus `cosi` ports are driven by the client and received by the server,
while `soci` ports are driven by the server and received by the client.


Generic Socket Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A socket definition may declare *generic parameters*, exactly like type
and module definitions (see :doc:`types`).
The parameters are a comma-separated `name : kind` list in square brackets
after the socket name, and the parameter names are in scope in the channel
types of the body:

.. code-block:: virdant

    socket Bus[T : Type] {
        cosi req : T
        soci rsp : T
    }

Both channels of `Bus` have type `T`.
The definition is analyzed once, generically, with the parameters unbound;
concrete channel types exist only per instantiation, which supplies the
actual types (see `Type Arguments`_ below).


Socket Instances
----------------
Socket instances are declared inside module bodies.
Each instance must specify its role: `client` or `server`.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString Annotations "client" "socket" Ident "of" Ofness GenericsParams? ItBlock?
        | DocString Annotations "server" "socket" Ident "of" Ofness GenericsParams? ItBlock?

.. code-block:: virdant

    mod Core {
        client socket mem of Mem
        unused mem.data
        mem.addr := 0
    }

    mod Memory {
        server socket mem of Mem
        unused mem.addr
        mem.data := 10
    }

A client socket instance uses `cosi` ports as outputs (driven by the client)
and `soci` ports as inputs (received by the client).
A server socket instance reverses these directions.


Type Arguments
~~~~~~~~~~~~~~

An instance of a generic socket supplies *type arguments* after the socket
definition name, in square brackets and in the same order as the declared
parameters:

.. code-block:: virdant

    mod Master {
        incoming d : Word[8]
        client socket bus of Bus[Word[8]] {
            it.req := d
        }
        unused bus.rsp
    }

    mod Slave {
        server socket bus of Bus[Word[8]] {
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

    client socket p of Pipe

reports ``Type builtin::Pipe takes 1 type argument, but 0 were given``.
Instantiating `Bus` at a number:

.. code-block:: virdant

    client socket bus of Bus[8]

reports ``Type argument mismatch: parameter T of Bus expects a type, but
found a Nat literal``.


Bidirectional Connections
-------------------------
A pair of socket instances (one client, one server) may be connected
bidirectionally using the `:=:` operator.
This connects every port in the socket at once, with the appropriate
direction for each instance.

.. code-block:: virdant

    mod Top {
        mod core of Core
        mod memory of Memory

        memory.mem :=: core.mem
    }

The `:=:` operator connects every port in the `Mem` socket between
the `core` and `memory` instances.
The `addr` port is driven by `core` and received by `memory`.
The `data` port is driven by `memory` and received by `core`.

This is equivalent to manually wiring each port:

.. code-block:: virdant

    memory.mem.addr := core.mem.addr
    core.mem.data := memory.mem.data


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
        client socket bus of Bus[W] {
            it.req := d
        }
        unused bus.rsp
    }

    mod Slave[W : Type] {
        server socket bus of Bus[W] {
            it.rsp := it.req
        }
    }

    export mod Top {
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
    result := memory.mem.data

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
        cosi a_address  : Word[32]
        cosi a_opcode   : Word[3]
        cosi a_size     : Word[2]
        cosi a_data     : Word[32]
        cosi a_mask     : Word[4]
        soci d_data     : Word[32]
        soci d_opcode   : Word[3]
        soci d_size     : Word[2]
    }

By defining the interface as a socket, the compiler can verify that all
connections are complete and correctly oriented.


Builtin Sockets
---------------
Virdant ships a single builtin socket, in the `builtin` package of the
standard library (`lib/builtin.vir`): :vir:`Pipe[T]`, a ready/valid
handshake channel.

.. code-block:: virdant

    socket Pipe[T : Type] {
        cosi data  : Valid[T]
        soci ready : Bit
    }

The two ports form a handshake pair:

* :vir:`cosi data : Valid[T]` --- the payload.
  :vir:`@Valid(v)` carries a value `v` of type `T`, while :vir:`@Invalid()`
  marks the pipe as empty.
  The client drives `data` and the server receives it.

* :vir:`soci ready : Bit` --- readiness.
  The server drives `ready` and the client receives it.

A :vir:`client socket` instance of :vir:`Pipe[T]` therefore drives
:vir:`data` and reads :vir:`ready`, while a :vir:`server socket` instance
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

        client socket pipe of Pipe[Word[8]] {
            it.data := @Valid(d)
        }

        // The consumer's readiness, received back through the pipe.
        outgoing ready : Bit
        ready := pipe.ready
    }

    // A consumer: asserts readiness and forwards the received payload.
    mod Consumer {
        server socket pipe of Pipe[Word[8]] {
            it.ready := true
        }

        // The payload received from the producer.
        outgoing data : Valid[Word[8]]
        data := pipe.data
    }

    export mod Top {
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
            case @Valid(dontcare) => true
            case @Invalid() => false
        }
        data := match consumer.data {
            case @Valid(payload) => payload
            case @Invalid() => 0
        }
    }

The :vir:`:=:` connection joins the two socket instances port by port:
the producer's :vir:`@Valid(d)` payload appears on the consumer's
:vir:`pipe.data`, and the consumer's readiness flows back to the producer's
:vir:`pipe.ready`.