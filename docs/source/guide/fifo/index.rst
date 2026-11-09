A First-In, First-Out Queue
===========================
In this example, we will build a FIFO: a first-in, first-out queue
that lets one part of a design hand bytes to another part at a
different pace.

Along the way, this lesson introduces three things at once:

- the builtin generic socket `Pipe[T]` and the valid/ready handshake,
- socket driver blocks, which drive a socket's channels through `it`,
- and `dependson`, Virdant's declaration of what an output may
  combinationally depend on.

The whole design is four modules: a `Fifo` with a depth-4 ring buffer,
a `Producer` that pushes a few known bytes, a `Consumer` that drains
them, and a `Top` that wires the three together.

.. literalinclude:: /guide/fifo/src/fifo.vir
    :caption: fifo.vir
    :language: virdant


The `Pipe[T]` socket
--------------------
A *socket* groups related signals into a reusable interface.
The builtin `Pipe[T]` socket is the simplest useful one: a stream of
optional values with a ready signal flowing the other way.

.. code-block:: virdant

    pub socket Pipe[T : Type] {
        cosi data  : Valid[T]
        soci ready : Bit
    }

Each channel has a direction word:

- `cosi` (client-out, server-in): the *client* side drives it.
- `soci` (server-out, client-in): the *server* side drives it.

So in a `Pipe`, the client pushes `data` and the server answers with
`ready`.

`Pipe` is *generic*: the `[T : Type]` parameter says what the stream
carries, and every instantiation fills it in.
Our FIFO moves bytes, so both of its sockets are `Pipe[Word[8]]`.
Another design could just as well use `Pipe[Word[32]]` or
`Pipe[SomeStruct]`, and the compiler treats each as its own distinct
socket.

The `Valid[T]` type
-------------------
The stream's payload type is `Valid[T]`: an optional value.
It is an ordinary builtin enum with two constructors:

.. code-block:: virdant

    pub enum type Valid[T : Type] {
        Invalid
        Valid(value : T)
    }

`@Valid(0xA1w8)` means "here is a byte", and `@Invalid()` means "there
is nothing to send this cycle".
That is exactly what a stream needs: a value that is sometimes there
and sometimes not, with no separate valid wire to forget to update.

To look inside a `Valid[T]`, match on it.
This is how the FIFO unwraps an incoming byte:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 86-91

The `with @Valid(data) => data` arm binds the payload to the name
`data`.
When nothing is being pushed, the stream is `@Invalid()`, and the arm
produces a harmless placeholder (`0w8`: a zero word of width 8).

.. note::

    A `Valid[T]` costs two bits more than `T`: a one-hot tag
    distinguishing `@Valid` from `@Invalid`, stored above the payload.

Sockets as module ports
-----------------------
A module uses a socket by declaring a port with a role:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 12-18

- `server socket inp` — the FIFO plays the *server* role on `inp`:
  `inp.data` (a `cosi` channel) arrives *into* the module, and
  `inp.ready` (a `soci` channel) leaves it.
- `client socket outp` — the FIFO plays the *client* role on `outp`:
  the FIFO drives `outp.data` and reads `outp.ready`.

Each role flips the direction of the channels, but the types line up:
both sockets are `Pipe[Word[8]]`, so a client on one module can be
wired to a server on another.

The valid/ready handshake
-------------------------
`Pipe` is a valid/ready handshake.
The producer places a value on `data` and holds it there; the consumer
asserts `ready` when it is willing to take a value.
A *transfer* happens on the clock edge where `data` is `@Valid` and
`ready` is high — neither side changes its offer before that edge.

Two rules make this work:

- `ready` may depend on anything except the `data` being offered in
  the same cycle (or you would create a combinational loop).
- `data` must stay put until the transfer happens.

We name the "a transfer happens now" condition a *fires* wire:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 93-105

`inp_fires` says a push is happening: the incoming stream is
`@Valid` and the FIFO has room.
`out_fires` says a pop is happening: the outgoing stream is `@Valid`
and the downstream consumer is ready.

.. note::

    Compared to Verilog, nothing here is special: `data` and `ready`
    are just signals grouped in a socket.
    The handshake is a convention the compiler type-checks for shape,
    but the *protocol* discipline is yours to keep.

Socket driver blocks
--------------------
Both socket declarations above attach a driver block, and inside it
`it` refers to the socket being driven.
This keeps the handshake logic right next to the port it belongs to:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 12-18

`it.ready := count < 4w3` drives the outgoing `ready` channel straight
from the occupancy count: the FIFO is willing to accept a byte
whenever it is not full.
`it.data := out_data` drives the outgoing `data` channel from the
`out_data` wire defined below.

You can read the channels of either role inside the module:
`inp.data` and `outp.ready` appear throughout the module body, with
the direction each role implies.

.. note::

    `it` inside a socket driver block is the same idea as `it` inside
    an outgoing port's driver block: "the thing being driven".
    Prefer it over spelling the socket name out again.

The ring buffer
---------------
The FIFO's storage is a four-word `memory` plus three registers:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 28-34

The write site is guarded by `inp_fires`, so a byte is written exactly
when a push handshake completes, into slot `tail`.
The `dyn` keyword indexes the memory by a runtime value — here the
`tail` register, a `Word[2]` (four slots, so addresses are two bits
wide).

.. note::

    `memory slots[4] : Word[8]` reads as "a memory named `slots` with
    four slots, each a `Word[8]`".
    The `[4]` belongs to the component, mirroring how reads look:
    `slots[dyn head]`.

The three registers form the ring:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 36-73

`head` advances on a pop, `tail` advances on a push, and `count`
tracks how many bytes are parked (it rises on a push without a pop,
falls on a pop without a push, and holds when both happen in the same
cycle).
Every register starts at zero (`initial it <= 0` seeds the registers
for simulation) and returns to zero under `reset`.

`when` arms inside one `if` block share one driver; here the guards
are written so they never overlap.

Offering data, with a shortcut
------------------------------
The value offered on `outp` is the heart of the FIFO:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 75-84

When the FIFO is empty, `inp.data` is forwarded combinationally.
This is a deliberate optimization: a push into an *empty* FIFO can be
consumed by the downstream consumer in the very same cycle — the byte
never parks at all.
Otherwise the byte at `slots[dyn head]` is offered as `@Valid(...)`.

Notice the if-expression arms use `=>`: in *expression* position,
conditional arms produce values.
(In *statement* position — like the register drivers above — arms are
`{ ... }` blocks that perform.)

`dependson`: declaring combinational reach
------------------------------------------
One output in this design reads an input combinationally:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 255-260

`held` is true when a byte is stuck in the FIFO because the drain is
shut.
Its driver reads the incoming `drain_enable` port in the same cycle,
so the port declares `dependson drain_enable`.

A `dependson` list does not *create* a dependency — it *declares* the
dependency a port is allowed to have.
It is part of the module's public interface: the list must name every
incoming port the output can combinationally reach, and the compiler
checks that promise both ways (an undeclared reach is an error; a
declared but unused entry is a warning).

Why don't the FIFO's sockets need `dependson` lists?
Because for socket channels, the declaration lives on the *socket
definition*, not on the module.
A channel may carry its own list — for example, a response channel
that reads the request channels of the same socket — and that
obligation then applies to every module that drives the channel in
the owning role.
The builtin `Pipe` declares no such lists, so our FIFO owes none,
even though `it.data` reads `inp.data` and `it.ready` reads `count`
(a register, which propagates no obligations).

.. note::

    An `outgoing reg` never needs a `dependson` list: a registered
    output cannot change in the same cycle, so it cannot depend on
    anything combinationally.
    That is why the `Consumer`'s counters below are plain outputs.

The producer
------------
The `Producer` pushes four known bytes — `0xA1`, `0x2B`, `0xC7`,
`0x44` — one per cycle, but only while the FIFO says it is accepting:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 114-119

While `sending`, the client's `data` channel is `@Valid(payload)`;
otherwise it is `@Invalid()`.
The payload comes from a match over the byte index:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 134-149

`push_fires` is the client side of the handshake: an offer that the
FIFO's `ready` accepts.
Only then does the `sent` counter advance.
If the FIFO is full, `outp.ready` drops, `push_fires` stays low, and
the producer holds its offer — the handshake doing its job.

The consumer
------------
The `Consumer` accepts everything offered to it — but only while
`drain_enable` is high:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 162-178

With `drain_enable` low, the server's `it.ready` is low, and the
FIFO's `out_fires` goes quiet: bytes pile up inside the FIFO.
That is backpressure, flowing right-to-left through the `:=:`
connections we are about to see.
The consumer also counts every accepted byte and remembers the most
recent one, so the demo can check that bytes really made it through:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 180-204

Wiring it all up
----------------
`Top` instantiates the three modules and connects the sockets:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 214-231

`:=:` is the socket bulk-connect operator: it wires every channel of
one socket to its counterpart on the other, mapping the `cosi` and
`soci` directions correctly.
`producer.outp :=: fifo.inp` connects a client to a server — one
`Pipe[Word[8]]` handshake — and likewise for the FIFO's output.

For the demo, `Top` also exposes a few observation ports, reading
through its instances — including the FIFO's socket channels
themselves:

.. literalinclude:: /guide/fifo/src/fifo.vir
    :language: virdant
    :dedent:
    :lines: 233-253

`fifo_peek` shows the FIFO's current offer as a `Valid[Word[8]]`,
`fifo_ready` shows whether the FIFO would accept a push right now,
and the two consumer counters prove bytes arrived end to end.

The demo
--------
The `fifo.poke` script resets the design with the drain shut, lets
the producer fill the FIFO, then opens the drain and watches it
empty:

.. literalinclude:: /guide/fifo/fifo.poke
    :caption: fifo.poke
    :language: virdant

Run it with `vir poke fifo.poke --script` from this directory.
The script asserts, cycle by cycle:

- one byte parks per cycle while the drain is shut, and `fifo_count`
  climbs 1, 2, 3, 4, with `fifo_peek` always showing the oldest byte;
- at four bytes the FIFO is full: `fifo_ready` is low, nothing has
  been drained, and `held` is high;
- raising `drain_enable` drops `held` *combinationally* — the
  `dependson drain_enable` dependency at work — before any clock edge;
- the consumer then pops one byte per cycle, and `drained_count` and
  `last_drained` confirm all four bytes arrived, in order.

When the queue empties, `fifo_peek` returns to `#Invalid()` and the
system is quiescent again.
