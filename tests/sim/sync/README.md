# sim/sync

Behavioral test of the builtin `Sync` component: an asynchronous input is
synchronized into the clock domain (with a cycle counter alongside), and
the testbench toggles the async input and asserts the synchronized output
through the generated Verilog.
