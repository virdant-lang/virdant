all: build/top.v

sim: build/testbench
	build/blink

build/testbench: build/top.v tb/testbench.v
	iverilog testbench.v build/top.v -o build/testbench

build/top.v: src/top.vir
	vir compile src/top.vir

clean:
	rm -rf build/

.PHONY: all clean sim
