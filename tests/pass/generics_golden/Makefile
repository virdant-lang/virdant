# The generic design must produce byte-identical Verilog to the
# hand-expanded non-generic one (generics_golden_expanded.vir).
BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) check
	$(VIR) build
	diff build/generics_golden_generic.sv build/generics_golden_expanded.sv
