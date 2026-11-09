A Full Wishbone Example
=======================
In this lesson, we will put two big features together:
:tooltip:`Wishbone (a popular bus protocol)` over **sockets**,
talking to a **real RAM** declared with the `memory` statement.

A `Wishbone` module drives seven signals at once: `cyc` `stb` `we` `addr`
`write_data` on the request side, and `ack` `read_data` on the response side.
Instead of declaring seven separate ports and wiring them one by one,
we will group them into a socket, define a server (a RAM) and a client
(a tiny master), and connect them with a single line.


The Socket
----------
.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The Wishbone socket
    :language: virdant
    :lines: 4-15
    :dedent:

A socket definition lists its signals as *channels*, each with one of
two direction keywords:

- `cosi` — Client-Out, Server-In: the client drives, the server reads.
- `soci` — Server-Out, Client-In: the server drives, the client reads.

So in `Wishbone`, the client raises `cyc` and pulses `stb` to make a
request, while the server answers with `ack` and `read_data`.

The `soci` channels also carry `dependson` lists.
Just like on an `outgoing` port, a `dependson` list on a channel
declares which signals of the *opposite* direction the server's drive
on that channel may combinationally depend on.
These lists are part of the socket's contract:
they apply to whichever module instantiates the socket as a `server`.

.. note::

    The `dependson` list declares *potential*, not actuality.
    Later, our RAM's `ack` will be driven by a register instead of
    combinationally from `cyc` and `stb`.
    That is fine: the list promises what the server *may* depend on,
    and bus protocols legitimately vary their latency,
    so an unused socket `dependson` entry is never a warning.


The RAM Server
--------------
.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The server side
    :language: virdant
    :lines: 17-61
    :dedent:

`RamServer` is a Wishbone *slave*: a 256-word x 16-bit RAM
with `server socket wishbone of Wishbone` as its front door.
Inside the module, the socket's `cosi` channels (`cyc` `stb` `we` `addr`
`write_data`) behave like incoming ports,
and the `soci` channels (`ack` `read_data`) behave like outgoing ports.

The RAM itself is declared with the `memory` statement:

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The memory's one write site
    :language: virdant
    :lines: 28-36
    :dedent:

`memory ram[256] : Word[16]` declares 256 slots, each a `Word[16]`.
Writes use the indexed form `it[dyn wishbone.addr] <= wishbone.write_data`:
the `dyn` keyword marks a *dynamic* index,
one that is only known at runtime (here, whatever address the master
put on the bus this cycle).

This `if` block is the memory's **one write site**.
A memory may have exactly one place where it is written,
guarded here by the full Wishbone write handshake:
`cyc && stb && we`.
When all three are high, the write commits on the next clock edge.

Reads, on the other hand, are combinational and need no guard.
`ram[dyn addr]` can be used anywhere an expression is expected,
including directly inside the socket's driver block:

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: Reading through the socket
    :language: virdant
    :lines: 22-26
    :dedent:

Because a memory read's only combinational dependency is on the *index*,
the server latches the requested address into `addr_latch` on the
request cycle, and presents `ram[dyn addr_latch]` on the next cycle.
A read therefore takes one cycle of latency:
request on cycle *t*, data on cycle *t + 1*.

The server acknowledges with `ack_pending`, a one-cycle pulse
registered on the clock edge after each strobed request.
A strobed request (write *or* read) on cycle *t* is acknowledged on
cycle *t + 1* — and for a read, `read_data` is valid in that same
acknowledged cycle.

.. note::

    In Verilog, this module would be an `always @(posedge)` block
    guarding a `reg [...] ram [0:255]` array, plus hand-wired
    `assign` statements for the ack and the read data.
    In Virdant, the `memory` statement carries the write protocol
    *inside its declaration*, and the compiler checks that
    there is exactly one write site.


The Master
----------
.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The master side
    :language: virdant
    :lines: 64-112
    :dedent:

`WishboneMaster` is a Wishbone *master*:
it owns `client socket wishbone of Wishbone` and drives the `cosi`
channels, while reading the `soci` channels as ordinary inputs.

The master runs a tiny reg-based program: write one word, then read it
back.  The program is a one-hot `enum type` with a state for each step:

- `#WriteRequest` — assert the write strobe,
- `#WriteWaitAck` — wait for the server's `ack`,
- `#ReadRequest` — assert the read strobe,
- `#ReadWaitAck` — wait for `ack`, then capture `read_data`,
- `#Done` — park with `cyc` low.

Because the enumerants carry no explicit values, they are one-hot
encoded: `#WriteRequest` is `1`, `#WriteWaitAck` is `2`, and so on.

The program counter is the `phase` register:

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The tiny program
    :language: virdant
    :lines: 114-132
    :dedent:

Each `when` clause fires at most once per pass through the program,
because the server's `ack` is a one-cycle pulse.
Inside the `phase` register's driver block, `it` refers to the
register being driven, so the guards read `it == #WriteRequest` and so on.

The socket's driver block turns the program into bus signals:

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: Driving the bus
    :language: virdant
    :lines: 89-96
    :dedent:

`cyc` stays high from the first request until the program is `#Done`.
`stb` is high only on the two single-cycle request phases,
and `we` distinguishes the write request from the read request.
The address and the write data are constants:
the master always works on word 42, always writes `0xBEEF`.

Finally, the capture logic samples the readback
on the read's acknowledged cycle:

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: Capturing the readback
    :language: virdant
    :lines: 134-152
    :dedent:

Both registers key off the same event —
`phase == #ReadWaitAck && wishbone.ack` —
so `readback` and `finished` rise together, exactly one cycle after
the read's `ack` appeared on the bus.


The Top Module
--------------
.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: The top module
    :language: virdant
    :lines: 155-182
    :dedent:

The top module instantiates one master and one server,
and connects them with the bulk connect operator `:=:`.

.. literalinclude:: /guide/memory_wishbone/src/memory_wishbone.vir
    :caption: One line, seven channels
    :language: virdant
    :lines: 180-181
    :dedent:

`cpu.wishbone :=: ram.wishbone` connects every channel of the two
sockets, mapping each `cosi` channel of the client to the matching
`cosi` channel of the server, and likewise for `soci`.
If we ever add or rename a channel in the `Wishbone` socket definition,
this line does not need to change —
the compiler re-checks the connection for us.

.. note::

    In Verilog, adding a signal to a bus interface means editing every
    instantiation site.  With sockets and `:=:`,
    the connection site never mentions the individual channels.


Running the Demo
----------------
The demo script `memory_wishbone.poke` steps the design cycle by cycle
and asserts the handshake at every step:

.. literalinclude:: /guide/memory_wishbone/memory_wishbone.poke
    :caption: memory_wishbone.poke
    :language: text

Each `tick` advances one clock cycle.
On the first tick's edge, the RAM commits the write of `0xBEEF`
into word 42, and the server latches its one-cycle `ack`.
On the third tick's edge, the server latches the read address,
and `read_data` presents the word — the very word that was written
two cycles earlier.  On the fourth tick's edge, the master captures
the readback into `result`, and `readback` reads `0xBEEF` at the top
level.

Run it yourself with::

    vir poke memory_wishbone.poke --script

If every assertion passes, the script exits 0 —
proof that the write reached the RAM,
and that the readback came back out through the Wishbone socket.
