# sim/pipe

Behavioral test of the builtin generic socket `Pipe[T]` (from
`lib/builtin.vir`). The top module instantiates `Pipe` at two different
type arguments in the same module, connects them with `:=:` on mono
sockets, and drives data end-to-end through the generated Verilog. The
testbench asserts the value arrives.
