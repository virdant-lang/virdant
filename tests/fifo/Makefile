BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

test: src/top.vir fifo_tb.sv
	$(VIR) build
	iverilog build/*.sv -g2012 fifo_tb.sv -o build/run
	./build/run

clean:
	rm -rf build
