BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) build
	verilator --lint-only build/*.sv > /dev/null
