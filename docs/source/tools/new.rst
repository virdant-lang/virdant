vir new
=======

Create a new Virdant project.

Usage
-----
.. code-block:: console

  $ vir new <PROJECT>

This creates a new directory named ``<PROJECT>`` with a fresh Virdant project inside it,
ready to check, simulate, and build:

.. code-block:: console

  $ vir new hello
  $ cd hello

A new project comes with:

* ``virdant.toml`` — the project manifest,
* ``src/top.vir`` — a hello-world design with a ``Top`` module,
* and a Git repository, unless you asked not to.

Options
-------
``--no-git``
    Do not initialize a Git repository for the new project.

    By default, ``vir new`` runs ``git init`` for you.
    If the new project already lives inside a Git repository (or Git is unavailable),
    Virdant skips initialization either way.

Next steps
----------
* :doc:`/tools/check`: Verify the project compiles.
* :doc:`/tools/run`: Run it in the Virdant simulator.
