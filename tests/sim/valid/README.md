# sim/valid

Behavioral test of `Valid` matching: the top module produces a wire from a
`match @Valid(n)` on a `Valid[Word[8]]`, unwrapping `@Valid(n)` and mapping
`@Invalid()` to 0, and the testbench asserts that wire through the
generated Verilog.
