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
We declare the direction of each port using the `mosi` and `miso` keywords.
A `mosi` port is master-out, slave-in, while `miso` is the reverse.

Inside of a module definition, we can declare instances of this socket.
Such instances are declared by giving their name and the name of the socket definition,
as well as their role: either `master` or `slave`.


Declaring Instances of a Socket
-------------------------------
Here is a snippet showing an example of each:

.. code-block:: virdant

    mod Core {
        master socket mem of Mem;
        // ...
    }

    mod Memory {
        slave socket mem of Mem;
        // ...
    }

By declaring the socket instance, the module gets a copy of each port in the definition.
The effective directions are easy to think about:
`mosi` ports on a `master` socket instance acts like an output port,
while a `mosi` port on a `slave` acts like an input port,
and similarly for the other two combinations.

You access these ports using dot notation:

.. code-block:: virdant

    mod Core {
        master socket mem of Mem;
        mem.addr := 0;
    }

    mod Memory {
        slave socket mem of Mem;
        mem.data := 0;
    }

(Of course, instead of assigning them to just `0`,
a real `Core` or `Memory` would have the logic to drive actual values to the sockets).

Just like with ports, module definitions are obligated to drive the ports of its sockets.
If the socket is a master, then the obligation is to drive all of the `mosi` ports,
and dually, if the socket is a slave, it the module is obligated to drive all of the `miso` ports.

Also like with ports, module definitions may make use of the incoming signals from the sockets.
Master sockets may use `miso` ports in expressions, and similarly, slaves may use `mosi` ports in them.


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
    :lines: 20-25

This is essentially equivalent to the following:

.. code-block:: virdant

    mod Top {
        mod core of Core;
        mod memory of Memory;

        memory.mem.addr := core.mem.addr;
        core.mem.data := memory.mem.data;
    }


Note that each driver statement connects the two ports of the same name together,
but that which drives which is worked out automatically.
This is exceedingly useful for larger interfaces, such as `Wishbone`_ or `AHB Lite`_.

.. _Wishbone: https://cdn.opencores.org/downloads/wbspec_b4.pdf
.. _AHB Lite: https://www.eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf


Rules of Socket Connectivity
----------------------------
As mentioned, the most common configuration is for a parent to connect the master socket of a child module
to the slave socket of another child module.
However, this is not the only possible configuration.

The rules for socket connectivity depend on two properties of both sockets involved:

The *role* of the socket is whether it has been declared as a master or a slave.
This is indicated by the `master` or `slave` keyword.

The *perspective* is whether the socket is being viewed from the inside or the outside of the module it is declared in.
When referring to a socket in the same module it was declared, this is an interior view of the socket.
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

  * - exterior slave
    - `:=:`
    - exterior master
    - the common configuration

  * - interior master
    - `:=:`
    - exterior master
    - forwarding a master up to its parent

  * - exterior slave
    - `:=:`
    - interior slave
    - forwarding a slave up from its child

  * - interior master
    - `:=:`
    - interior slave
    - loopback configuration (rarely used)
