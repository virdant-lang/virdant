# sim/generics_mod

End-to-end test of generics: generic socket defs and generic mod defs are
instantiated at concrete type arguments and wired with `:=:` connections.
The testbench asserts that the connections pass data through and that the
generic modules latch and unwrap their argument's payload width.
