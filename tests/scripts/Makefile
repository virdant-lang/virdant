BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

RHAI_SCRIPTS := $(wildcard *.rhai)
LUA_SCRIPTS := $(wildcard *.lua)

all:
	for script in $(RHAI_SCRIPTS); do \
		echo "--------------------------------------------------------------------------------"; \
		echo "TEST: tests/scripts/$$script"; \
		echo "--------------------------------------------------------------------------------"; \
		$(VIR) run $$script || exit 1; \
	done
	for script in $(LUA_SCRIPTS); do \
		echo "--------------------------------------------------------------------------------"; \
		echo "TEST: tests/scripts/$$script"; \
		echo "--------------------------------------------------------------------------------"; \
		$(VIR) run $$script || exit 1; \
	done
