UART
====
Now that we have seen counters and FIFOs,
let's put a few pieces together and build a real peripheral:
a UART transmitter.

A UART sends bytes one bit at a time over a single wire, `tx`.
When the line is idle, it is held high.
To send a byte, the transmitter pulls the line low for one bit period (the "start" bit),
then sends the eight bits of the byte one at a time,
and finally releases the line high again (the "stop" bit).

Our project is a `Top` module which streams the message `"Hello, world!"` out of its `tx` port,
one character at a time.

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant

This module is larger than the previous examples,
but it is built out of the same pieces we have already seen:
ports, drivers, registers, submodules, and `match`.


The Top module
--------------
`Top` declares two outgoing ports: `tx`, the serial line itself,
and `exit`, which tells the test harness when we are done.

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 8-13

When the character rom produces a `0` (the terminator of our message)
and the sender is ready for another byte,
`exit` becomes `@Valid(0)` and the simulation ends.

`Top` does not implement the UART protocol itself.
Instead, it instantiates two submodules and wires them together:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 15-28

The `sender` is the actual UART transmitter,
and `char_rom` is a small lookup table holding the message.
Whenever the sender is ready, `Top` feeds it the current character from the rom.

A register `char_index` walks through the message:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 34-45

Each time a character is handed to the sender (`inp_fires`),
`char_index` increments.
When it reaches the `0` terminator, `sending` drops and the message is done.


The state machine
-----------------
The interesting part is `UartSender`.
Like our FIFO, it is a `server` behind a `Pipe` socket:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 48-60

The state of the transmitter is described by a union type, `UartState`:
`Idle`, `Start`, `Data`, or `Stop`.
The `Data` state carries its own payload, the index of the bit currently being sent.

The `tx` line is driven by a `match` on that state:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 62-69

While `Idle` or `Stop`, the line idles high.
During `Start`, it is pulled low.
During `Data`, we send one bit of `buffer` at a time,
selected with `buffer[dyn index]`.

The timing comes from a baud timer, a simple countdown register:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 77-91

Each bit lasts `baud_limit` + 1 cycles.
Here it is `8`, so every bit is held for nine cycles:
slow enough to watch comfortably in a waveform,
and exactly the shape of a real UART at any baud rate.

The transitions between states are the last statement in `UartSender`:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 93-118

Whenever the baud timer has run out, we advance:
`Idle` latches a byte into `buffer` and moves to `Start`,
`Start` moves to `Data(0)`,
`Data` counts its bit index up to `7` and then moves to `Stop`,
and `Stop` returns to `Idle`.
That is the entire protocol.


The character rom
-----------------
`CharRom` is a read-only memory holding the message:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 121-131

The `@image_inline` attribute embeds the string directly in the design:
`"Hello, world!\r\n"` followed by a `0` terminator.
The `index` port selects one character at a time,
and `Top` advances the index as each character is sent.


Waveform
--------
Here is the transmitter in action,
sending the first few characters of the message:

.. vcd:: trace.vcd

    label Clock
    signal top.clock:clk

    label Sender
    signal top.sender.state:uart::UartState
    signal top.sender.buffer:dec

    label Serial output
    signal top.tx

Watch `top.tx`:
it idles high, drops low for the start bit,
toggles through the eight data bits,
and returns high for the stop bit, over and over,
once per character of the message.

.. note::

    You can hover over the waveform diagram with your mouse.
    The yellow line shows you what the value of every signal is
    at that exact moment in time.

A UART may not look like much,
but it is the classic "hello, world" of FPGA design:
with it, your board can talk back to your computer.
