BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

LUA_SCRIPTS := $(wildcard *.lua)

all:
	for script in $(LUA_SCRIPTS); do \
		echo "--------------------------------------------------------------------------------"; \
		echo "TEST: tests/scripts/$$script"; \
		echo "--------------------------------------------------------------------------------"; \
		$(VIR) run $$script || exit 1; \
	done
