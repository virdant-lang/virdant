vir synth
=========

Synthesize your design.

Usage
-----
.. code-block:: console

  $ vir synth

``vir synth`` runs synthesis (with `yosys <https://yosyshq.net/yosys/>`_)
on the Verilog that Virdant generates for your design,
leaving the synthesis netlist at ``build/<project>.json``.

The target FPGA platform is determined by the ``for`` clause of your top module —
you don't need to configure it separately in ``virdant.toml``.

Options
-------
``-C``, ``--dir <DIR>``
    Operate on the project in ``DIR`` instead of the current directory.

``--platform <NAME>``
    Synthesize for a specific FPGA platform, rather than the one inferred
    from the top module's ``for`` clause.

Next steps
----------
* :doc:`/tools/pnr`: Place-and-route the synthesized netlist.
* :doc:`/tools/bitstream`: Go all the way to a bitstream.
