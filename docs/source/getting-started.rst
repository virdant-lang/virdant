Getting Started
===============

Installation
------------
For now, Virdant only works on Linux.
You can install it by cloning the repository and running `make install`:

.. code-block:: console

  $ git clone https://github.com/virdant-lang/virdant
  $ cd virdant
  $ make install

This will build Virdant from source and then copies the binaries to `$HOME/.local/virdant/bin`.
Make sure that this directory is on your `$PATH`.


Blinky
------
Tradition dictates that the first program that is written in any programming language should be Hello, World!
In that same spirit, here is a small design which blinks an LED on and off:

.. literalinclude:: /examples/blink.vir
   :caption: blink.vir
   :language: virdant
   :linenos:


Compiling to Verilog
--------------------
We can compile this design to Verilog with the following:

.. code-block:: console

  $ vir compile blink.vir

The result will be a new file called `build/blink.v`.

To simulate the design, you need a Verilog testbench.
Here is one which will run the design for 100 cycles:

.. literalinclude:: /examples/testbench.v
   :caption: testbench.v
   :language: verilog
   :linenos:

If you have `Icarus Verilog`_ installed, you can compile a simulator and run it with this with these commands:

.. _Icarus Verilog: https://steveicarus.github.io/iverilog/

.. code-block:: console

  $ iverilog testbench.v build/blink.v -o build/blink
  $ ./build/blink
  VCD info: dumpfile build/out.vcd opened for output.

Finally, using a waveform viewer such as `GTK Wave`_, you can view the waveform:

.. _GTK Wave: https://github.com/gtkwave/gtkwave

.. image:: /_static/waveform.png
