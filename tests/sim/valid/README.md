# sim/valid

Behavioral test of `Valid` matching. The top module produces a wire `foo`
from `match @Valid(3) : Valid[Word[8]]`, unwrapping `@Valid(n)` and mapping
`@Invalid()` to 0. The testbench asserts `dut.foo` — this wire is the
test's one distinguishing feature, and its assertion was missing before the
restructure.
