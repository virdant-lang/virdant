# sim/drivers

Regression test for driver forms (`:=`, `<=`, and it-blocks) around an
enum-typed state machine: the testbench asserts on every clock edge that
each driver form lowers to the same values in the generated Verilog.
