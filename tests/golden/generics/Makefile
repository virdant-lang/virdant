# The generic design must produce Verilog identical to the
# hand-expanded non-generic one (generics_golden_expanded.vir),
# up to the top module's name: every module is emitted under its
# escaped fully-qualified name, and the two files live in different
# packages (generics_golden_generic vs generics_golden_expanded).
# (Generic *mod defs* cannot compare byte-for-byte — their mono
# modules get hash-mangled names by design — so their equivalence is
# proven behaviorally by the iverilog project tests/pass/generics_mod.)
BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) check
	$(VIR) build
	grep -q 'module \\generics_golden_generic::Top (' build/generics_golden_generic.sv
	grep -q 'module \\generics_golden_expanded::Top (' build/generics_golden_expanded.sv
	# Compare past the module declaration line (the only line which may
	# differ: the escaped FQN of the top module).
	tail -n +2 build/generics_golden_generic.sv > build/generic.body
	tail -n +2 build/generics_golden_expanded.sv > build/expanded.body
	diff build/generic.body build/expanded.body
