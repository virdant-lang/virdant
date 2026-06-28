Platforms
=========
A *platform* is a Virdant source file that describes a physical FPGA board:
its FPGA family, its part number, and the mapping between logical port
names and physical pins.
A platform is declared with the ``platform`` keyword.

.. code-block:: virdant

    @fpga("ice40")
    @part("up5k-sg48")
    platform Icesugar {
        @period_ns("83.33")
        @pin(35)
        incoming clock : Clock

        @pin(39) outgoing led_red : Bit
    }

A platform item looks like a module that contains only ``incoming`` and
``outgoing`` ports.
Each port represents a physical pin on the board and is annotated with
``@pin`` (and, for clock ports, ``@period_ns``).
The platform itself is annotated with ``@fpga`` and ``@part``.


Platform Annotations
--------------------
The following annotations are consumed by platform analysis
(see also :doc:`annotations`):

* ``@fpga("family")`` --- the FPGA family.
  One of ``ice40``, ``ecp5``, ``nexus``, ``gowin``, ``artix7``.
  Required for bitstream generation.

* ``@part("part-number")`` --- the specific part number,
  passed to the place-and-route tool.

* ``@constraints("path")`` --- if present, the file at ``path`` is emitted
  verbatim as the constraint file instead of generating one from the
  ``@pin`` annotations.

* ``@pin(n)`` or ``@pin("name")`` --- the physical pin a port is bound to.
  A numeric value is suitable for the iCE40 flow (which uses pin numbers);
  a string value is suitable for the Artix-7 / ECP5 / Gowin flows
  (which use pin names like ``"E3"``).
  Exactly one ``@pin`` is required on every platform port.

* ``@period_ns(n)`` or ``@period_ns("n")`` --- the clock period in
  nanoseconds.
  Required on every port whose type is ``Clock``.

* ``@cell("CELL")`` --- annotates an ``ext mod`` declaration with the
  underlying FPGA cell name (e.g. ``SB_RAM40_4K``).
  Parsed but not yet consumed by the toolchain.


Shipped Platforms
-----------------
The Virdant distribution ships two platform files in ``platform/``:

* ``icesugar`` --- the iCESugar board (Lattice iCE40-UP5K, SG48).
* ``nexys4`` --- the Digilent Nexys 4 (Xilinx Artix-7).


Selecting a Platform
--------------------
A project selects its platform by setting ``platform`` under ``[prog]``
in ``Virdant.toml``:

.. code-block:: toml

    [project]
    name = "blink"

    [prog]
    platform = "icesugar"

When ``vir check``, ``vir build``, or ``vir bitstream`` is invoked on a
project with a platform set, the platform file is loaded as an extra
package and ``import <platform>`` resolves against it.


Top-Module Port Binding
-----------------------
The top module of a project is the single ``export mod`` declaration.
Each port of the top module is matched by name against the platform's
ports, and the matching ``@pin`` is emitted into the constraint file.

If a top-module port has no matching platform pin, ``vir bitstream``
prints a warning and leaves that port unconstrained.


Custom Platforms
----------------
To author a custom platform, drop a ``myboard.vir`` file into a
project-local ``platform/`` directory.
Project-local platforms take precedence over the shipped ones.

The file must declare exactly one ``platform`` item whose name matches
the file stem (e.g. ``platform Myboard`` in ``myboard.vir``).
Then set ``platform = "myboard"`` in ``Virdant.toml``.
