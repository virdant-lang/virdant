vir upload
==========

Upload your design to your FPGA.

Usage
-----
.. code-block:: console

  $ vir upload

``vir upload`` will attempt to build and upload your design to your FPGA.

In order, it will:

- compile your design to Verilog, placing it in the `build/` directory
- synthesize the Verilog with Yosys.
- place-and-route the synthesized design with NextPNR
- generate a bitstream
- flash the bitstream to your device

The programmer used is selected automatically from the platform.

.. list-table::
    :header-rows: 1

    * - Platform
      - Programmer
    * - ``ice40::IceSugar``
      - ``icesprog``
    * - ``ice40::IceStick``
      - ``iceprog``
    * - ``ice40::Fomu``
      - ``dfu-util``
    * - ``xilinx::ArtyA7``
      - ``openFPGALoader``
    * - ``xilinx::ArtyA7_100T``
      - ``openFPGALoader``

.. warning::

    For ``vir upload`` to work, all tools must be installed on your ``$PATH``.

.. warning::

    To upload your design, you must have the proper udev permissions to access the USB device.

Options
-------
``--tries <N>``
    Run place-and-route across several seeds (default: 1),
    and keep the best result.

``--seed <N>``
    Use a specific seed for the place-and-route algorithm.

``--allow-timing-failure``
    Do not fail when the design does not meet its timing constraints.

See also
--------
* :doc:`/tools/run`: Before uploading, try your design in the simulator.
