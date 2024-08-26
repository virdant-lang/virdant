The Virdant Hardware Language
=============================

.. toctree::
  :hidden:

  getting-started
  modules/index
  types/index
  expressions/index
  principles
  coding-conventions
  Repository <https://github.com/virdant-lang/virdant>

Virdant is a modern hardware description language.

It was created in response to a simple ask:
If my design typechecks, it should do what I expect.

The industry standard Verilog fails this by having very loose rules for when designs are malformed.
Some notable painpoints include:

* automatic truncation of data
* flop inference from incomplete case coverage
* tool-dependent rules for invalid wire connections

The other industry standard VHDL suffers from the fact the author of Virdant doesn't know it and doesn't want to know it.

Repository: `https://github.com/virdant-lang/virdant <https://github.com/virdant-lang/virdant>`_
