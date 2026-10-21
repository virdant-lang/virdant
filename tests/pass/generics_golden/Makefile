# The generic design must produce byte-identical Verilog to the
# hand-expanded non-generic one (generics_golden_expanded.vir).
# (Generic *mod defs* cannot compare byte-for-byte — their mono
# modules get hash-mangled names by design — so their equivalence is
# proven behaviorally by the iverilog project tests/pass/generics_mod.)
BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) check
	$(VIR) build
	diff build/generics_golden_generic.sv build/generics_golden_expanded.sv
