Echo
====
:tooltip:`UART (Universal Asynchronous Receiver/Transmitter)` is a simple protocol
used to communicate over just two wires.

In this example, we will look at `Echo`, a server which takes whatever data is send over UART
and echoes it back unmodified.


Top
---
The `Top` module for our server is quite simple:

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant

We see `Top` is a top module for the `virt::Virt` platform.
Other than `clock`, it declares two ports with type `Bit`: `rx` and `tx`,
which are the standard ports for receiving and sending data over UART respectively.

We don't define `exit`, so our simulation will run indefinitely (or until we type `Ctrl+C`).

We also don't define `debug`.
`debug` as a tool is meant to be for simulations only,
but I don't know of any hardware that supports anything like it.
It's an easy way to get output on the same cycle that a problem occurs.

In this example, all of the output we see on the screen is
but rather a realistic UART output that you will frequently be using in real designs!


Running the Echo Server
-----------------------
If you have Virdant installed, run it like this, and then start typing:

.. code-block:: console

    $ vir new hello
    Created project 'hello'
    $ cd hello/
    $ vir run
    Hello, Virdant!

.. tip::

    Use `Ctrl+C` to exit!

The demo may seem like it doesn't do much.
But it's important to note that `vir run` actually *disables* your terminal's loopback.

You can confirm that our echo server is, in fact, hard at work if you delete everything and run it:

.. code-block:: virdant
    :caption: top.vir

    import virt

    pub mod Top for virt::Virt {}

Now if you try running the demo with `vir run`, you can type all you want, but nothing appears!


`Sync`
------
`Top` contains three submodules.
The first is rather special: `Sync` is a builtin module.
It's role is hard to appreciate at first,
but we'll present it here so you will know about it when you need it.

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 10-13
    :dedent:

In our Virdant designs, all signals change in sync with the clock.
But pins like `rx` are often driven from outside of your FPGA,
and they are *not* synchronized with the clock.

In hardware design, it is incredibly dangerous to use signals
that are not synchronized to your clock.
The reason is that 99.9999% of the time -- your design works perfectly.

But that other 0.0001% of the time...

To avoid these difficult to debug errors, Virdant provides `Sync`.
Simply put, you feed `Sync` two inputs:
`clock`, the clock you wish to synchronize with,
and `inp`, the unsynchronized signal.
Then the `out` port will give you a synchronized copy of `inp`.


UART Submodules
---------------
The functionality of `Top` lives in two submodules from the `uart` package:
`UartReceiver` is responsible for receiving data over UART,
and `UartSender` is responsible for sending it.

.. note::

    Virdant's `coding conventions </coding-conventions>`_ say that when you abbreviate something,
    you should do so with the first letter capitalized.
    So "UART Receiver" becomes `UartReceiver` when written as the name of a module.

Both connect to just the clock, their respective UART pin:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 15-23
    :dedent:

Of course, for `UartReceiver`, we are using the synchronized version of `rx`, `rx_sync.out`.

On the very last line, we see where they get their data from:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 25
    :dedent:

`:=:` is called a bidirectional driver.
`sender.inp` and `receiver.out` are two `Pipe` sockets.
`receiver.out` is a `forward socket` and `sender.inp` is a `reverse socket`.
This means that they were meant for each other!

`:=:` connects each of their corresponding ports together,
each in the proper direction.
It is just as if we had written (note the flip in direction):

.. code-block:: virdant

    sender.inp.data    :=: receiver.out.data
    receiver.out.ready :=: sender.inp.ready

Bidirectional drivers are super handy.
They save a bunch of typing.
And for larger sockets, such as AXI, TileLink, or Wishbone interfaces,
it can easily save you dozens of lines of typing.

The UART Protocol
-----------------
The UART protocol is pretty simple.
Let's take a look at how it works.

UART is asynchronous.
This means it doesn't send a clock over the wires.
Instead, both sides must keep time separately.
For this, our UART uses a standard baud rate of 115200 bits per second.

This means, separate from our clock, we must do timekeeping.
Our clock runs at a nominal 1 MHz.
So each bit we send (whether its a data bit or control bit) is sent every 1/115200 s.
If we do the math, this means we send or receive bits at roughly 1 bit per 9 clock cycles.

.. note::

    The exact number of cycles is closer to 8.68.
    This is captured in `baud_limit` (which is `8`, plus a clock cycle delay is 9 clock cycles).
    Our design accounts for this by using a regiser called `baud_timer`
    and it essentially lets us re-sync every time a byte is sent or received.
    This avoids our clocks ever getting too far out of sync.

Let's consider the sender.

When idle, we keep `tx` high.
In order to start a transmission, we pull `tx` low for one baud period (9 clock cycles).
We call this the **start bit**.

For the next 8 baud periods, we send each bit,
starting with the :tooltip:`LSB (least significant bit)`.

Finally, we end the transmission by bringing the `tx` line high again for one more baud period.
We call this the **stop bit**.
So in total, it takes us 10 baud periods (90 clock cycles).

.. note::

    Compared to `debug` which prints your data after *just 1* cycle,
    you can see how UART is rather slow.

Next, let's consider the receiver.
The sender's `tx` is treated as the receiver's `rx`.
So just imagine what the sender does, but observed on the `rx` signal.

The `rx` line idles high.
As soon as it is pulled low, we understand this as receiving the start bit.
We expect the `rx` line to be low for one baud period.

For the next 8 baud periods, we must sample the `rx` line to learn what data is being sent.
Since each bit is held for ~9 clock cycles (this might not be exact!),
it's best that we sample close to the middle.
(For this reason, `UartReceiver` has a wire called `half_period`).
For each bit we receive, we shift it into a shift register (called `buffer`).

Once all the bits are sampled, the sender will pull the line high.
This is the stop bit, and it's held high for one baud period.
Unless we are looking for errors, this isn't interesting to us.

.. note::

    This describes the most common configuration of UART: 8N1.
    That is, we sent 8 bits at a time,
    we do *not* include a parity bit, and
    we include 1 stop bit.

    The `Virt` platform defaults to 115200, which is the most second most common baud rate.
    You can change it with `vir run --baud 9600` if you want to use the most common value of 9600.


The UART State
--------------
The `uart` package defines three items: `Uartstate` `UartSender` and `UartReceiver`.
Let's look at each in turn.

.. literalinclude:: src/uart.vir
    :caption: uart.vir
    :language: virdant
    :lines: 1-6

The UART protocol requires that both our sender and receiver go through four states.

.. list-table::
    :header-rows: 1

    * - State
      - Meaning
    * - `@Idle()`
      - No data is being sent / received
    * - `@Start()`
      - | The start bit is being sent / received.
        | The line should be low.
    * - `@Data(i)`
      - | Bit number `i` is being sent / received.
        | `i` starts with `0` and goes up to `7`.
    * - `@Stop()`
      - | The stop bit is being sent / received.
        | The line should be high.


The Full `uart` Package
-----------------------
The full example of UART doesn't have any new features in Virdant.
It's best to simply study the code to understand how it all works.

.. literalinclude:: src/uart.vir
    :caption: uart.vir
    :language: virdant
    :lines: 1-169
