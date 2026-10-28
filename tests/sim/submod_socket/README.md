# sim/submod_socket

Regression test for socket wiring: two submodules connected with `:=:`
inside an instantiation block (`m.busif :=: it.busif`). The testbench
asserts the end-to-end value through the generated Verilog.
