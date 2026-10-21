# The builtin generic socket Pipe[T] must behave end-to-end after
# monomorphization: the iverilog testbench drives the generated
# Verilog, checking payload flow (client to server) and readiness flow
# (server to client) through the `:=:` mono-socket connections.
BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	iverilog build/*.sv tb.sv -o build/run
	./build/run