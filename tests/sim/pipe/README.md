# sim/pipe

Behavioral test of the builtin generic socket `Pipe[T]`: the top module
instantiates `Pipe` at two different type arguments, connects them with
`:=:` on mono sockets, and the testbench asserts data arrives end-to-end
through the generated Verilog.
