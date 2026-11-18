vir build
=========

Compile your design to Verilog.

Usage
-----
.. code-block:: console

  $ vir build

``vir build`` elaborates your Virdant design and translates it into Verilog.
The results are put in the ``build`` directory.

You can simulate your design using Icarus Verilog or Verilator.
You can also synthesizing it with the Yosys toolchain and get it running on your FPGA!

See Also
--------
* :doc:`/tools/check`: Re-check your project after making changes.
* `Icarus Verilog <https://steveicarus.github.io/iverilog/>`_: An open-source Verilog simulator.
* `Verilator <https://www.verilator.org/>`_: A fast, open-source Verilog simulator.
* `Yosys <https://yosyshq.net/yosys/>`_: An open-source synthesis suite for Verilog.
