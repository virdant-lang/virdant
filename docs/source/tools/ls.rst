vir ls
======

Virdant Language Server.

Usage
-----
.. code-block:: console

  $ vir ls

``vir ls`` is the Virdant Language Server.
It provides language intelligence for Virdant projects:
diagnostics, hover information, go-to-definition, find-references, and more.
It is meant to be started by your editor, not run interactively.

Your editor should be configured to launch ``vir ls serve``
via the Language Server Protocol (LSP).

Beyond the LSP itself, ``vir ls`` has a few subcommands that are useful
for inspecting a project from the command line:

* ``vir ls serve`` — Run the language server (this is what your editor launches).
* ``vir ls hover <location>`` — Hover information at a source location
  (e.g. ``vir ls hover src/top.vir:10:5``).
* ``vir ls definition <target>`` — The definition of the item at a location,
  or of a name (e.g. ``vir ls definition Foo::Bar``).
* ``vir ls references <target>`` — Locations that reference the target.
