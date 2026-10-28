# sim/fifo

FIFO behavioral test. `src/top.vir` implements a FIFO; `fifo_tb.sv` runs a
randomized push/pop stress test (pushes and pops hundreds of items with
random data) and asserts full/empty behavior and data integrity through the
generated Verilog.
