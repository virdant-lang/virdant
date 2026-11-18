vir run
=======

Run your design in the Virdant simulator.

Usage
-----
.. code-block:: console

  $ vir run

``vir run`` simulates your `Top` module if it implements the `virt::Virt` platform
(see :doc:`/reference/platforms`).

The UART is controlled by standard input.

To terminate the simulation early, you can use `Ctrl+C`.

Options
-------
``--baud <BAUD>``
    UART baud rate (default 115200)

``--cycles <N>``
    Put an upper bound on the number of cycles simulated.
    When the limit is reached, the simulation stops.

``--trace [<FILE>]``
    Record a VCD waveform while the design runs.
    The waveform can then be opened in a viewer such as Surfer or GTKWave.
    Be careful when simulating large or long-running designs,
    since the VCD file can be huge!

``--flash <FILE>``
    Preload the simulated SPI flash device with the contents of a file.

See also
--------
* :doc:`/reference/platforms`: Platforms in Virdant.
* :doc:`/tools/build`: Compile the design to Verilog instead of simulating it.
* `Surfer <https://surfer-project.org/>`_: A modern, open-source waveform viewer.
* `GTKWave <https://gtkwave.sourceforge.net/>`_: A classic, long-standing waveform viewer.
