# sim/generics_mod

End-to-end test of generics through generated Verilog. The source defines
generic socket defs and generic mod defs (a generic `Bus[T : Type]` socket
plus generic register/enum modules), instantiates them at concrete type
arguments, and wires them with `:=:` connections. The testbench asserts
that the combinational generic socket pair passes data through the
connection and that the generic register/enum modules latch and unwrap
their argument's payload width.
