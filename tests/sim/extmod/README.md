# sim/extmod

Behavioral test of `ext mod`: the design declares an external module whose
implementation is hand-written SystemVerilog in `ext/Rom.sv` (a ROM backed
by `ext/rom.hex`). The testbench drives the design through the external
module and asserts the values read back.
