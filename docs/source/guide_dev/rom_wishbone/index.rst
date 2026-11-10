Read-Only Memory and the Wishbone Bus
=====================================
So far, every wire we have connected has been a single port.
Real designs talk through whole *interfaces*: a bundle of related signals
that always travel together.

In this example, we introduce *sockets*, Virdant's mechanism for grouping
signals into a reusable interface.
Our example is a small read-only memory (ROM) which serves a fixed message
over a trimmed-down `Wishbone <https://opencores.org/wishbone>`_ bus,
one of the most common open bus protocols.

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :caption: rom_wishbone.vir
    :language: virdant


The Socket Definition
---------------------
A `socket` groups related signals into named *channels*.
Each channel is declared with one of two direction keywords:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 11-17

- `cosi` means **co**-client, **si**-server:
  the channel flows *from* the client *to* the server.
  Think of it as the request direction.
- `soci` means **so**-server, **ci**-client:
  the channel flows *from* the server *to* the client.
  Think of it as the response direction.

Our `Wishbone` socket carries three request channels and two response channels:

.. list-table::
    :header-rows: 1

    * - Channel
      - Direction
      - Description
    * - `cyc`
      - `cosi`
      - a transaction is in progress
    * - `stb`
      - `cosi`
      - a request is strobed this cycle
    * - `addr`
      - `cosi`
      - which word to read
    * - `ack`
      - `soci`
      - the server accepted the request
    * - `data`
      - `soci`
      - the word read from the memory

The `soci` channels carry `dependson` lists.
Just like the `dependson` on an `outgoing` port,
they declare which channels of the *opposite* direction
the server's drive may read in the same cycle.
Here, both `ack` and `data` may depend on `cyc` `stb` and `addr`.

.. note::

    Compared to Verilog, a socket is like a SystemVerilog `interface`:
    one declaration, reused everywhere, instead of a pile of
    individually-drifting port lists.


The ROM Server
--------------
A module picks up a *role* with `server socket` or `client socket`.
`Rom` is the server:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 20-31
    :dedent:

Inside a socket driver block, `it` refers to the socket's channels.
The request channels arrive as *incoming* signals (`it.cyc` `it.stb` `it.addr`),
and the response channels are *outgoing* signals we drive (`it.ack` `it.data`).

The server is purely combinational:
it asserts `it.ack` whenever a strobed transaction is in progress,
and drives `it.data` with the word at `it.addr`.
Because the ROM never writes, the `memory` statement needs neither a write
site nor a clock.

The memory itself is initialized with `@image_inline`:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 28-30
    :dedent:

`@image_inline` packs the bytes of the string literal into the memory,
two characters per `Word[16]`.
So address 0 holds `"He"`, address 1 holds `"ll"`, and so on.
The remaining 248 words of the 256-word memory are left unspecified,
which is exactly what we want for a ROM that we only ever read at the
beginning.

.. note::

    In Verilog you would reach for an `initial` block or `$readmemh`.
    In Virdant, the `memory` statement and its `@image` `@image_hex`
    and `@image_inline` annotations are first-class:
    the compiler knows this is a ROM, and lowers it to an inferred
    block RAM with initial contents.


The Client
----------
`Reader` takes the client role:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 35-51
    :dedent:

The directions flip:
the `cosi` channels are now outgoing signals the client drives,
and the `soci` channels are incoming.

Our reader is about as small as a bus client can be.
It holds `cyc` and `stb` high forever, presents one fixed address,
and forwards whatever the server sends back on `data`.

Since the ROM answers combinationally, the client has nothing to wait for,
so it simply never looks at `ack`.
A channel we deliberately ignore is silenced with `unused`:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 48-50
    :dedent:

.. note::

    A real Wishbone client would walk through addresses one per cycle,
    using `ack` to know when each word is ready.
    That needs a clock and a small state machine;
    our combinational reader keeps the spotlight on the socket itself.


Wiring It Together
------------------
`Top` instantiates both modules and connects them with the bulk connect
operator `:=:`:

.. literalinclude:: /guide_dev/rom_wishbone/src/rom_wishbone.vir
    :language: virdant
    :lines: 55-64
    :dedent:

One statement connects the whole interface:
`:=:` wires every channel of the two sockets, matching each `cosi` to its
`cosi` and each `soci` to its `soci`, with the directions resolved for us.

If we later add a channel to `Wishbone` -- say, a `soci err` signal --
the bulk connect picks it up automatically,
and every module that does not drive or read it will point out exactly
where it needs an `unused`.


Seeing It Run
-------------
The demo script settles the combinational logic and asserts that the
message really made it through the bus:

.. literalinclude:: /guide_dev/rom_wishbone/rom_wishbone.poke
    :language: text

Address 0 holds `"He"`, packed as ``0x4865`` --
and that is exactly what `data` carries back to the client.
