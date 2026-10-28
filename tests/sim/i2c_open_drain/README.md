# sim/i2c_open_drain

Behavioral test of open-drain bus semantics. The source (`src/top.vir`,
docstring: "I2C-style open-drain SDA line") implements an I2C-style
open-drain line where any device may pull the line low and no device drives
it high. The testbench asserts the open-drain pull-low / release behavior
of the generated Verilog. This was `tests/pass/i2c_open_drain_sim/` before
the restructure (the directory was renamed to drop the `_sim` suffix).
