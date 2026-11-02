vir bitstream
=============

Generate a bitstream for your FPGA.

Usage
-----
.. code-block:: console

  $ vir bitstream

``vir bitstream`` runs the full FPGA toolchain — synthesis, place-and-route,
and bitstream generation — for your design,
writing the resulting bitstream to your project's ``build`` directory.

Like ``vir synth`` and ``vir pnr``, the target FPGA platform is determined
by the ``for`` clause of your top module.

Options
-------
``-C``, ``--dir <DIR>``
    Operate on the project in ``DIR`` instead of the current directory.

``--platform <NAME>``
    Build for a specific FPGA platform, rather than the one inferred
    from the top module's ``for`` clause.

Next steps
----------
* :doc:`/tools/upload`: Upload the bitstream to your FPGA.
