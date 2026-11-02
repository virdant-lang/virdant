vir completion
==============

Generate shell completion scripts.

Usage
-----
.. code-block:: console

  $ vir completion <SHELL>

``vir completion`` prints a completion script for ``vir`` to standard output,
so you can tab-complete commands and arguments in your shell.

Supported shells: ``bash``, ``zsh``, ``fish``, ``powershell``, ``elvish``.

Example
-------
For Bash, add the completion script to your profile:

.. code-block:: console

  $ vir completion bash >> ~/.bashrc

Then restart your shell (or ``source ~/.bashrc``) to pick up the completions.
