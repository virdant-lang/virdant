vir build
=========

Compile your design to Verilog.

Usage
-----
.. code-block:: console

  $ vir build

``vir build`` elaborates your Virdant design and translates it into Verilog,
written to your project's ``build`` directory.
The generated Verilog is self-contained:
one module per Virdant module, with the same names and ports,
so it can be dropped into any standard Verilog flow.

The generated Verilog is a faithful translation of your design —
it does not attempt to optimize or restructure it.
It is simply what your Virdant modules look like, expressed in Verilog.

Options
-------
``-C``, ``--dir <DIR>``
    Operate on the project in ``DIR`` instead of the current directory.

Next steps
----------
* :doc:`/tools/synth`: Synthesize the generated Verilog for an FPGA.
* :doc:`/tools/check`: Re-check your project after making changes.
