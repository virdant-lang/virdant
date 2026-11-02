vir pnr
=======

Place-and-route your design.

Usage
-----
.. code-block:: console

  $ vir pnr

``vir pnr`` places and routes an already-synthesized design,
reusing the netlist that :doc:`/tools/synth` left at ``build/<project>.json``.
It re-runs only the place-and-route (and pack) steps,
so it's the counterpart of ``vir synth`` for when you want to
re-run those steps without re-synthesizing.

The source-level checks and constraint emission still run,
so the constraints stay in sync with the sources —
but the netlist is not re-synthesized,
so source edits since the last ``vir synth`` are not reflected in it.

Options
-------
``--tries <N>``
    Run place-and-route across several seeds (default: 1),
    and keep the best result.

``--seed <N>``
    Use a specific seed for the place-and-route algorithm.

``--allow-timing-failure``
    Do not fail when the design does not meet its timing constraints.

Next steps
----------
* :doc:`/tools/bitstream`: Generate a bitstream from the placed-and-routed design.
