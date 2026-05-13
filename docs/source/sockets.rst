Sockets
=======
Sockets are a way for Virdant modules to expose a set of ports as a group.
You may then connect all the ports together in a single statement, rather than doing so individually.


Declaring a Socket
------------------
We declare a socket in Virdant with the `socket` keyword.
This is a top-level definition, on the same level as `mod`\s and `type` definitions.
The socket definition gives a name to the group of ports we wish to use together.

.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :lines: 1-4

Inside a socket definition, we give the name and type of each port, as well as a direction.
We declare the direction of each port using the `cosi` and `ciso` keywords.
A `cosi` port is client-out, server-in, while `ciso` is the reverse.

Inside of a module definition, we can declare instances of this socket.
Such instances are declared by giving their name and the name of the socket definition,
as well as their role: either `client` or `server`.


Declaring Instances of a Socket
-------------------------------
Here is a snippet showing an example of each:

.. code-block:: virdant

    mod Core {
        client socket mem of Mem
        // ...
    }

    mod Memory {
        server socket mem of Mem
        // ...
    }

By declaring the socket instance, the module gets a copy of each port in the definition.
The effective directions are easy to think about:
`cosi` ports on a `client` socket instance act like output ports,
while a `cosi` port on a `server` acts like an input port,
and similarly for the other two combinations.

You access these ports using dot notation:

.. code-block:: virdant

    mod Core {
        client socket mem of Mem
        mem.addr := 0
    }

    mod Memory {
        server socket mem of Mem
        mem.data := 0
    }

(Of course, instead of assigning them to just `0`,
a real `Core` or `Memory` would have the logic to drive actual values to the sockets).

Just like with ports, module definitions are obligated to drive the ports of their sockets.
If the socket is a client, then the obligation is to drive all of the `cosi` ports,
and dually, if the socket is a server, then the module is obligated to drive all of the `ciso` ports.

Also like with ports, module definitions may make use of the incoming signals from the sockets.
Client sockets may use `ciso` ports in expressions, and similarly, servers may use `cosi` ports in them.


Bulk Connect
------------
Socket instances may be connected together with a single statement.
This is sometimes called "bulk connect", since it allows you to connect many wires "in bulk".
This bidirectional driver statement is written `:=:`.

The most common pattern is when one module connects together the sockets of two of its children.
Given the socket definition `Mem` and the module definitions `Core` and `Memory` above,
we can define a new module `Top` which instantiates the two modules,
and then connects their sockets like this:

.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :dedent:
    :lines: 19-24

This is essentially equivalent to the following:

.. code-block:: virdant

    mod Top {
        mod core of Core
        mod memory of Memory

        memory.mem.addr := core.mem.addr
        core.mem.data := memory.mem.data
    }


Note that each driver statement connects the two ports of the same name together,
but that which drives which is worked out automatically.
This is exceedingly useful for larger interfaces, such as `Wishbone`_ or `AHB Lite`_.

.. _Wishbone: https://cdn.opencores.org/downloads/wbspec_b4.pdf
.. _AHB Lite: https://www.eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf


Rules of Socket Connectivity
----------------------------
As mentioned, the most common configuration is for a parent to connect the client socket of a child module
to the server socket of another child module.
However, this is not the only possible configuration.

The rules for socket connectivity depend on two properties of both sockets involved:

The *role* of the socket is whether it has been declared as a client or a server.
This is indicated by the `client` or `server` keyword.

The *perspective* is whether the socket is being viewed from the inside or the outside of the module it is declared in.
When referring to a socket in the same module in which it was declared, this is an interior view of the socket.
When referring to a socket declared in a submodule, this is an exterior view of the socket.
In the earlier example, both `memory.mem` and `core.mem` are seen from an exterior view.

Given these definitions, the following connections are legal:

.. list-table:: legal socket configurations
  :widths: 25 10 25 50
  :header-rows: 1

  * - Left hand
    -
    - Right hand
    - Explanation

  * - exterior server
    - `:=:`
    - exterior client
    - the common configuration

  * - interior client
    - `:=:`
    - exterior client
    - forwarding a client up to its parent

  * - exterior server
    - `:=:`
    - interior server
    - forwarding a server up from its child

  * - interior client
    - `:=:`
    - interior server
    - loopback configuration (rarely used)
