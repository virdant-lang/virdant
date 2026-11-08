vir run
=======

Run your design in the Virdant simulator.

Usage
-----
.. code-block:: console

  $ vir run

``vir run`` simulates your ``Top`` module directly from the Virdant sources — no Verilog involved.
A new project runs as-is:

.. code-block:: console

  $ vir run
  Hello, world!

The simulator feeds your design's inputs and watches its outputs.
For a design running on a virtual board, input comes from standard input and output goes to standard output,
so the design behaves like an ordinary command-line program.

Options
-------
``--legacy``
    Force the legacy tree-walking interpreter simulator instead of the
    VM simulator (the default).
    Incompatible with ``--bytecode``.

``--bytecode``
    Dump the compiled VM bytecode (one disassembled program per driven
    component) and exit, without simulating.
    Requires the VM simulator: combining it with ``--legacy`` is an error.

``--baud <BAUD>``
    UART baud rate for the virt board (default 115200, overridable via
    ``[virt]`` in ``virdant.toml``).
    The board decodes the design's ``tx`` and drives ``rx`` at exactly
    ``clock_hz / baud`` cycles per bit: a design must clock its UART
    bits at that divisor (within the usual UART tolerance of half a
    bit over a frame) in both directions.
    The baud must leave at least two clock cycles per bit.

``--cycles <N>``
    Put an upper bound on the number of cycles simulated.
    When the limit is reached, the simulation stops.

``--trace [<FILE>]``
    Record a VCD waveform while the design runs.
    With a filename (e.g. ``--trace trace.vcd``) the trace is written there;
    bare ``--trace`` defaults to ``trace.vcd``.
    The waveform can then be opened in a viewer such as Surfer or GTKWave.

``-v``, ``--verbose``
    Print extra detail while the simulation runs.

``--fps``
    Pace the simulation to real time, based on the platform's declared clock frequency.
    Useful for designs that interact with you as they run.

``--fps-no-cap``
    Like ``--fps``, but without the frame-rate cap: the simulation runs unthrottled.

``--flash <FILE>``
    Preload the simulated SPI flash device with the contents of a file.

See also
--------
* :doc:`/tools/build`: Compile the design to Verilog instead of simulating it.
