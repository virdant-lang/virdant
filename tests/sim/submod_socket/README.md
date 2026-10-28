# sim/submod_socket

Regression test for the riscv-SoC socket-wiring bug: two submodules
connected with `:=:` inside an instantiation block. The source is a minimal
repro (`m.busif :=: it.busif` between `Master` and `Slave` instances); the
testbench asserts the end-to-end value (`PASS: out=8`).

Note: before the restructure this project never compiled — `m.bus` used the
reserved keyword `bus` as an identifier, which produced a cascade of parse
and resolution errors. Fixing the single token made the whole project build
and pass.
