VIR_FILES := $(wildcard *.vir)

# Project-style tests (directory + Virdant.toml + own Makefile),
# listed explicitly: other project directories carry simulator tests
# that need external tools (iverilog, verilator) and are not wired in.
VIR_PROJS := platform_icesugar

BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir
FILECHECK ?= $(BASEDIR)/target/release/filecheck

test:
	mkdir -p build
	for f in $(VIR_FILES); do \
		name=$$(basename $$f .vir); \
		$(VIR) -F $$f check > build/$$name.out 2>&1; \
		rc=$$?; \
		if grep -q 'WARNING' build/$$name.out; then \
			echo "ERROR: $$f emitted unexpected warnings"; \
			exit 1; \
		fi; \
		cat build/$$name.out | $(FILECHECK) $$(realpath $$f) || exit 1; \
		if [ $$rc -ne 0 ]; then \
			echo "ERROR: $$f check failed"; \
			exit 1; \
		fi; \
	done
	for d in $(VIR_PROJS); do \
		echo "TEST: tests/pass/$$d"; \
		$(MAKE) -C $$d VIR=$(VIR) FILECHECK=$(FILECHECK) || exit 1; \
	done
