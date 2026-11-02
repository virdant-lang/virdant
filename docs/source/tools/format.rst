vir format
==========

Format your Virdant source files.

Usage
-----
.. code-block:: console

  $ vir format

``vir format`` formats Virdant source files in your project.
It takes care of consistent indentation and line breaks,
so you don't have to spend your time on it.

With no file arguments, ``vir format`` formats every ``src/*.vir`` file in the project.
Alternatively, you can pass specific files to format:

.. code-block:: console

  $ vir format src/top.vir src/helper.vir

What gets formatted
-------------------
* Indentation is normalized to 4 spaces.
* Vertical alignment of consecutive driver operators (``:=``, ``<=``, ``:=:``)
  within the same block is preserved.
* Vertical alignment of type-annotation colons (``:``) within the same block
  is also preserved.

This means that if you line up a group of assignments,
``vir format`` won't undo your work.

See also
--------
* :doc:`/tools/check`: Check your project for problems after formatting.
