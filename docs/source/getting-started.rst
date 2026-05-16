Getting Started
===============

Installation
------------
For now, Virdant only works on Linux.
You can install it by cloning the repository and running `make install`:

.. code-block:: console
  :caption: console

  $ git clone https://github.com/virdant-lang/virdant
  $ cd virdant
  $ make install

This will build Virdant from source and then copy the binaries to `$HOME/.local/virdant/bin`.
Make sure that this directory is on your `$PATH`.


Blinky
------
Tradition dictates that the first program that is written in any programming language should be Hello, World!
In hardware projects, the equivalent of Hello, World! is to get an LED to blink on and off.

Create a new Virdant project by running:

.. code-block:: console
  :caption: console

  $ vir new blink
  $ cd blink

This will create a new project in a subdirectory named `blink`.
If we look at the `src/top.vir` file, we will see our blinking LED design:

.. literalinclude:: /examples/blink.vir
   :caption: top.vir
   :language: virdant
   :linenos:


Compiling to Verilog
--------------------
We can compile this design to Verilog with the following:

.. code-block:: console
  :caption: console

  $ vir build

The result will be a new file called `build/top.sv`.

To simulate the design, you need a Verilog testbench.
Here is one which will run the design for 100 cycles:

.. literalinclude:: /examples/testbench.sv
   :caption: testbench.sv
   :language: verilog
   :linenos:

If you have `Icarus Verilog`_ installed, you can compile a simulator and run it with these commands:

.. _Icarus Verilog: https://steveicarus.github.io/iverilog/

.. code-block:: console
  :caption: console

  $ iverilog testbench.sv build/top.sv -o build/blink
  $ ./build/blink
  VCD info: dumpfile build/out.vcd opened for output.

Finally, using a waveform viewer such as `GTK Wave`_, you can view the waveform:

.. _GTK Wave: https://github.com/gtkwave/gtkwave

.. image:: /_static/waveform.png
