Echo
====
Now that we have seen counters and FIFOs,
let's put a few pieces together and build a real peripheral:
a UART echo.

A UART sends bytes one bit at a time over a single wire.
When the line is idle, it is held high.
To send a byte, the transmitter pulls the line low for one bit period (the "start" bit),
then sends the eight bits of the byte one at a time, least significant bit first,
and finally releases the line high again (the "stop" bit).

Our project is a `Top` module which echoes bytes back:
whatever byte arrives on its `rx` port is transmitted back out of its `tx` port.

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant

This module is small,
because the interesting logic lives in two submodules:
a UART transmitter (`UartSender`) and a UART receiver (`UartReceiver`),
both in `src/uart.vir`.
We will look at the transmitter first, then the receiver.


The Top module
--------------
`Top` declares two serial pins: `tx`, the outgoing line,
and `rx`, the incoming line.

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 7-8

`Top` does not implement the UART protocol itself.
Instead, it instantiates the two submodules and wires them together:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 10-20

The `sender` drives the `tx` line,
and the `receiver` listens on the `rx` line.
The last line is the whole point of this example:
`sender.inp :=: receiver.out` connects two sockets directly to each other.
The receiver produces bytes on its `out` socket,
and the sender consumes them from its `inp` socket,
so every byte that arrives on `rx` is handed straight to the sender
and echoed back out on `tx`.


The transmitter
---------------
The transmitter, `UartSender`, is a `reverse` behind a `Pipe` socket,
just like the FIFO we built earlier.
Here is the whole file, transmitter and receiver together:

.. literalinclude:: src/uart.vir
    :caption: uart.vir
    :language: virdant

The state of the transmitter is described by a union type, `UartState`:
`Idle`, `Start`, `Data`, or `Stop`.
The `Data` state carries its own payload, the index of the bit currently being sent.

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 1-6

The `tx` line is driven by a `match` on that state:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 15-22

While `Idle` or `Stop`, the line idles high.
During `Start`, it is pulled low.
During `Data`, we send one bit of `buffer` at a time,
selected with `buffer[dyn index]`.

The timing comes from a baud timer, a simple countdown register:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 30-44

Each bit lasts `baud_limit` + 1 cycles.
Here it is `8`, so every bit is held for nine cycles:
slow enough to watch comfortably in a waveform,
and exactly the shape of a real UART at any baud rate.

The transitions between states are the last statement in `UartSender`:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 46-71

Whenever the baud timer has run out, we advance:
`Idle` latches a byte from the `inp` socket into `buffer` and moves to `Start`,
`Start` moves to `Data(0)`,
`Data` counts its bit index up to `7` and then moves to `Stop`,
and `Stop` returns to `Idle`.
That is the entire protocol.


The receiver
------------
The receiver, `UartReceiver`, faces the other direction:
it watches the `rx` line and assembles the bits it sees back into a byte.

Its output is a `forward` socket, `out`,
whose `data` simply presents the contents of a register:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 78-80

That register, `hold`, holds one received byte as a `Valid[Word[8]]`:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 88-108

While `hold` is `@Invalid`, there is nothing to deliver.
When a byte arrives, `hold` latches `@Valid(buffer)` and keeps presenting it
until the consumer (here, the sender) takes it,
at which point `hold` drops back to `@Invalid`.
This is exactly the handshake a `Pipe` socket wants.

How do we know when a byte has arrived?
The receiver runs the same kind of countdown as the transmitter:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 110-132

The trick is in the `@Idle` arm.
While the line idles high, the timer holds at `0`, waiting.
The moment `rx` drops low, the timer loads `half_period`,
which is half of a bit period.
Sampling half a bit period after the falling edge
lands the receiver near the middle of the start bit,
and every following bit is one full bit period apart,
so every sample lands near the middle of its bit too.

The `byte_done` wire sums up the completion condition:
we are in `Stop`, the line is high (a valid stop bit), and the timer has run out.

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 84-86

The state machine itself mirrors the transmitter's,
but running backwards:

.. literalinclude:: src/uart.vir
    :language: virdant
    :dedent:
    :lines: 138-168

In `Idle`, a falling edge on `rx` starts a frame.
In `Start`, we check the line again:
if it has gone back high, that was a glitch, not a start bit,
and we return to `Idle`.
Otherwise the line is confirmed low,
and each `Data` step shifts the current `rx` value into the top of `buffer`
with `buffer <= word(rx, buffer[8..1])`,
since UART sends the least significant bit first.
After eight bits, `Stop` waits for the line to go high,
`byte_done` fires, and the finished byte is latched into `hold`.


Waveform
--------
Let's land this example by watching it run in the waveform viewer.
The design does not run by itself this time:
we have to play the role of the far-end UART
and send a byte into `rx` ourselves.
The `run.poke` script sends the byte `0x41` (`A`),
one bit at a time, nine cycles per bit:

.. code-block:: console

    $ vir poke run.poke

And here is the echo in action:

.. vcd:: trace.vcd

    label Clock
    signal top.clock:clk

    label Input
    signal top.rx

    label Receiver
    signal top.receiver.state:uart::UartState
    signal top.receiver.buffer:bin

    label Sender
    signal top.sender.state:uart::UartState
    signal top.sender.buffer:dec

    label Serial output
    signal top.tx

Follow `rx` on the left half:
it idles high, drops low for the start bit,
shifts through the eight data bits of `0x41`,
and returns high for the stop bit.
Meanwhile `receiver.buffer` shifts those same bits in one by one,
ending at `01000001` = `A`,
and `hold` goes `@Valid`.
From there the sender takes over:
it latches the byte into `sender.buffer` (`65`, the ASCII code of `A`),
and you can watch it re-play the identical frame out on `tx`
on the right half of the waveform.

.. note::

    You can hover over the waveform diagram with your mouse.
    The yellow line shows you what the value of every signal is
    at that exact moment in time.

A UART may not look like much,
but it is the classic "hello, world" of FPGA design:
with it, your board can talk back to your computer.
