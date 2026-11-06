# sim/else_match

Regression test for the bug where a statement-level `else match` arm in a
reg driver block was silently dropped from the generated design: the
register only ever took the assignments from the `when` arms before it.

`state_else_match` and `state_flat` describe the same two-state FSM.  The
testbench asserts that both reach `#Running` after reset deasserts.
