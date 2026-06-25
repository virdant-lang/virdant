BASEDIR = $(abspath ../../../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	verilator --lint-only -Wno-MULTITOP build/*.sv > /dev/null
