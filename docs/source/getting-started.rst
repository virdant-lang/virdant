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

This will build Virdant from source (in release mode) and then copy everything to `$HOME/.local/virdant`.


.. warning::

    You will need to add `$HOME/.local/virdant/bin` to your `$PATH`!
    If you forget, you'll see `vir: command not found`.


Running Hello World
-------------------
Create a new Virdant project by running:

.. code-block:: console
  :caption: console

  $ vir new hello
  $ cd hello

Next, we can run our project in the Virdant simulator.

.. code-block:: console
  :caption: console

  $ vir run
  Hello, world!

Oh! Why hello, Virdant!


What Virdant Looks Like
-----------------------
Next, let's just glance at the source code.
Open the file `src/top.vir` in the `hello` project you just created.
You will see our hello world design.

.. note::

    In software, you may be used to talking about your "program".
    But in hardware, we call them "designs".

    And rather than a function called `main`, as you might be used to in programming,
    the entry point to a hardware design is traditionally a module called `Top`

We won't worry about how any of this works for now.
The tutorial will teach you from scratch.
Just get a feel for what Virdant looks like:

.. literalinclude:: /examples/hello/src/top.vir
   :caption: src/top.vir
   :language: virdant
   :class: small-code


Compiling to Verilog
--------------------
You can compile Virdant to Verilog with the following:

.. code-block:: console
  :caption: console

  $ vir build

We won't be using this feature for now.
But I'll mention it for those who are experienced with hardware design.
