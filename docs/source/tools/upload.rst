vir upload
==========

Upload your design to your FPGA.

Usage
-----
.. code-block:: console

  $ vir upload

``vir upload`` does everything :doc:`/tools/bitstream` does —
synthesis, place-and-route, and bitstream generation —
and then uploads the resulting bitstream to your FPGA board.

The programmer used is selected automatically from the platform,
which in turn comes from your top module's ``for`` clause:

* ``IceSugar`` — ``icesprog``
* ``IceStick`` — ``iceprog``
* ``Fomu`` — ``dfu-util``
* ``ArtyA7`` and ``ArtyA7_100T`` — ``openFPGALoader``

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
* :doc:`/tools/run`: Before uploading, try your design in the simulator.
