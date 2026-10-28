# sim/i2c_open_drain

Behavioral test of open-drain bus semantics: the design implements an
I2C-style open-drain line where any device may pull it low and none drives
it high, and the testbench asserts the pull-low / release behavior of the
generated Verilog.
