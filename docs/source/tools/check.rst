vir check
=========

Check a Virdant project for problems.

Usage
-----
.. code-block:: console

  $ vir check

``vir check`` type-checks your design without producing any output artifacts.
It elaborates your design, infers every expression's type, and reports any problems it finds:

.. code-block:: console

  $ vir check
  Check OK

If the project has problems, they are printed as diagnostics and ``vir check`` exits with a non-zero status:

.. code-block:: console

  $ vir check
  src/top.vir:4:9: Type error: Expected QuadU8, found Bit
  Check failed

Because ``vir check`` does not emit Verilog or run a toolchain, it is the fastest way to get feedback while editing.
It is also a natural fit for editor integration and CI pipelines:
exit code 0 means the design is valid, exit code 1 means it is not.

See also
--------
* :doc:`/tools/build`: Compile the design to Verilog once it checks.
* :doc:`/reference/index`: How the type system decides what is a problem.
