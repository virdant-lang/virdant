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
        DocString Annotations "socket" Ident "{" SocketDefStmt* "}"

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


Socket Instances
----------------
Socket instances are declared inside module bodies.
Each instance must specify its role: `client` or `server`.

.. code-block:: grammar

    ModDefStmtSocket :=
        DocString Annotations "client" "socket" Ident "of" Ofness ItBlock?
        | DocString Annotations "server" "socket" Ident "of" Ofness ItBlock?

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

The `:=:` operator connects every port in the ``Mem`` socket between
the `core` and `memory` instances.
The `addr` port is driven by `core` and received by `memory`.
The `data` port is driven by `memory` and received by `core`.

This is equivalent to manually wiring each port:

.. code-block:: virdant

    memory.mem.addr := core.mem.addr
    core.mem.data := memory.mem.data


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