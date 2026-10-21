# The generated Verilog of the monomorphized generic design must
# simulate correctly: proves that per-instantiation substitution is
# semantics-preserving (PLANv2 §12.4) where the types-only golden test
# proves textual equivalence.
BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	iverilog build/*.sv tb.sv -o build/run
	./build/run
