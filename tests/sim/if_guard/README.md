# sim/when_guard

Regression test for the "lost when-arm guard" compiler bug: the design
uses when-arm guards that must survive codegen, and the testbench asserts
the guarded behavior through the generated Verilog.
