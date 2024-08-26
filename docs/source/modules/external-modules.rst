External Modules
================
An external module is a module which is defined in the host language, but which is made accessible to Virdant.

External modules are declared using the `ext` modifier before the `mod` keyword:

.. literalinclude:: /examples/external.vir
   :language: virdant

An external module may only declare ports.

The Virdant compiler will create a stub to read in code written in the host language.
Today, this will look up a Verilog file in a special directory whose name matches the module definition:

.. literalinclude:: /examples/Memory.v
   :caption: Memory.v
   :language: verilog
   :linenos:

External modules are useful for getting access to features the host language provides, but which Virdant does not.
For example, you can use them to call system tasks, like `$display`, add SystemVerilog assertions to your design,
define behavior memories, define clock generators, or instantiate a module your synthesis tool provides.
