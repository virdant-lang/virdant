Sockets
=======
Sockets are a way for Virdant modules to expose a set of ports as a group.
You may then connect all the ports together in a single bidirectional driver statement, rather than doing so individually.

We declare a socket in Virdant with the `socket` keyword.

.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :lines: 1-4

Inside a socket definition, we declare the direction of each port using the `mosi` and `miso` keywords.
A `mosi` port is master-out, slave-in, while `miso` is the reverse.

Then, inside of a module definition, we can declare a socket with either:

.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :dedent:
    :lines: 7

or

.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :dedent:
    :lines: 14

depending on whether you want to declare that your module is a master or a slave.

In either case, declaring the socket gives you access to all of its ports with dotted notation:
in this example, `mem.addr` and `mem.data`.

If the socket is a master, the module is obligated to drive all of the `mosi` ports,
and similarly, if the socket is a slave, it the module is obligated to drive all of the `miso` ports.

So imagine if we have two module definitions:

.. code-block:: virdant

    mod Core {
        master socket mem of Mem;
        // mem.addr := ...
    }

    mod Memory {
        slave socket mem of Mem;
        // mem.data := ...
    }

Then a common parent module can connect the two sockets together with the socket connect operator.
This is written `:=:`, with the slave socket on the left hand side and the master on the right hand side:


.. literalinclude:: /examples/sockets.vir
    :language: virdant
    :dedent:
    :lines: 20-25

This is equivalent to the following:

.. code-block:: virdant

    mod Top {
        mod core of Core;
        mod memory of Memory;

        memory.mem.addr := core.mem.addr;
        core.mem.data := memory.mem.data;
    }
