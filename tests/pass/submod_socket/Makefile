BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	iverilog build/*.sv tb.sv -o build/run
	./build/run
