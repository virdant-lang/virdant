BASEDIR = $(abspath ../../../)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	iverilog build/*.sv build/ext/Rom.sv tb.sv -o build/run
	./build/run > /dev/null