BASEDIR = $(abspath ../../../)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	iverilog build/*.sv -g2012 tb.sv -o build/run
	./build/run
