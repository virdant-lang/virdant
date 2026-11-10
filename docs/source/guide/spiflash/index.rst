SPI flash
=========
So far, our designs have only *produced* output.
Now let's do the opposite and *read* from the outside world:
every FPGA board ships with an SPI flash chip holding its bitstream,
and we can talk to it while the design is running.

Our project issues the industry-standard `0x03` READ command at address `0`
over the SPI bus, then streams the bytes it reads back out of the UART.
To give it something to read, we preload the simulated flash with a file
using `--flash`:

.. code-block:: console

  $ vir run --flash far_over_the_misty_mountains_cold.txt
  Far over the misty mountains cold
  To dungeons deep and caverns old
  ...

The project is a `Top` module on the `virt` board which wires together
two submodules: a `SpiFlash` controller and the `UartSender` we built in
the previous chapter.

.. literalinclude:: src/top.vir
    :caption: top.vir
    :language: virdant


The Top module
--------------
`Top` declares the UART's `tx` line and the four SPI flash pins:
`spi_ck` (serial clock), `spi_cs` (chip select, active low),
`spi_mosi` (controller-out data), and `spi_miso` (controller-in data).

The two submodules are connected almost trivially:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 48-57

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 59-63

The flash controller's byte stream feeds the UART sender directly,
pipe to pipe.
`Top` never touches the bytes at all:
whatever the flash returns goes straight out the serial line.

The only bytes `Top` looks at are for termination.
A wire watches the stream for the `0` terminator:

.. literalinclude:: src/top.vir
    :language: virdant
    :dedent:
    :lines: 65-72

When the NUL shows up and the sender is ready to accept it,
`exit` becomes `@Valid(0)` and the simulation ends.


The Spi socket
--------------
The SPI bus itself is described by a socket, `Spi`:

.. literalinclude:: src/spiflash.vir
    :language: virdant
    :dedent:
    :lines: 1-7

Like `Pipe`, this is a socket with two sides.
The `cosi` and `soci` prefixes say which way each wire flows:
`cosi` stands for *controller-out, slave-in*,
and `soci` for *slave-out, controller-in*.
The `SpiFlash` module is the controller on this bus,
so the clock, chip select, and `mosi` lines are `cosi`,
while `miso` flows the other way.


The flash controller
--------------------
`SpiFlash` is the interesting part.
It speaks the flash chip's protocol over its `spi` client socket
and offers each byte it reads on an `out` pipe:

.. literalinclude:: src/spiflash.vir
    :language: virdant
    :dedent:
    :lines: 9-24

The whole controller runs on a single 32-bit shift register,
which does double duty.
It starts out holding the command and address,
`0x03_000000`: the READ command followed by a 24-bit address of zero.
Each bit is shifted out `mosi`-first, MSB first, one bit every two cycles.

The `running` and `is_offbeat` registers generate the timing:
while `running`, `is_offbeat` toggles every cycle,
so a bit is transferred on every other cycle,
and `spi.clock` toggles at half the main clock rate.

A counter `tx_remaining` starts at `32` and counts the command bits down.
Once it reaches zero, the controller flips around and starts *receiving*:
`miso` is shifted into the bottom of the same register,
counted by `rx_bits`.
When a full byte has arrived, `running` drops,
and the byte sitting in the low bits of `shift` becomes `@Valid` on `out`.

The caller (here, `Top`'s `sender`) accepts the byte,
and `running` comes back up for the next one.
Notice that `tx_remaining` stays at zero:
after the very first `0x03` command, the controller never sends anything again.
That is not an accident:
a flash chip, once told to READ, keeps streaming sequential bytes
with its internal address auto-incrementing on its own,
so the controller only has to keep clocking.


Waveform
--------
Here is the beginning of the transaction,
reading the first few characters of the message:

.. vcd:: trace.vcd

    label SPI bus
    signal top.spi_cs
    signal top.spi_ck
    signal top.spi_mosi
    signal top.spi_miso

    label Flash controller
    signal top.flash.running
    signal top.flash.shift:hex

    label UART output
    signal top.sender.state:uart::UartState
    signal top.sender.buffer:dec
    signal top.tx

Watch `top.spi_mosi`:
it carries the `0x03` command and the 24-bit address, MSB first,
while `cs` is held low.
Then the direction flips:
`miso` carries the bytes of the poem in,
and `top.tx` carries them back out to the host.

.. note::

    You can hover over the waveform diagram with your mouse.
    The yellow line shows you what the value of every signal is
    at that exact moment in time.

There is something quietly satisfying about this design:
the poem that names "hammers" and "harps of gold" is dug out
of a simulated flash chip, one bit at a time,
by a shift register fifty lines long.
Reading a flash chip is the classic first step
toward designs that load their own programs --
and now we have the pieces to do it.
