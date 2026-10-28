# golden/generics

Verifies that a generic mod instantiated at a concrete type argument
generates the same Verilog as a hand-expanded non-generic copy of it.
The Makefile runs `vir check` + `vir build` on both sources, greps the
generated files for the escaped FQN, and diffs the two Verilog bodies
(dropping each file's header line). This was `tests/pass/generics_golden/`
before the restructure.
