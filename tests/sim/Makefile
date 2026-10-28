# Behavioral tests: `vir build` + iverilog / verilator simulation.
#
# Tool policy: the suite runs every project when the simulators are
# installed. If a simulator is missing, the suite fails loudly unless
# VIRDANT_ALLOW_MISSING_SIM=1 is set, in which case every project is
# skipped with a notice instead.
SUBDIRS := $(filter-out build/,$(wildcard */))

BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	@missing=""; \
	command -v iverilog >/dev/null 2>&1 || missing="$$missing iverilog"; \
	command -v verilator >/dev/null 2>&1 || missing="$$missing verilator"; \
	if [ -n "$$missing" ]; then \
		echo "MISSING SIMULATOR TOOLS:$$missing"; \
		if [ "$(VIRDANT_ALLOW_MISSING_SIM)" = "1" ]; then \
			for d in $(SUBDIRS); do echo "SKIP: sim/$$d (simulator tools missing)"; done; \
			exit 0; \
		else \
			echo "ERROR: install iverilog/verilator, or set VIRDANT_ALLOW_MISSING_SIM=1 to skip sim tests"; \
			exit 1; \
		fi; \
	fi
	@for d in $(SUBDIRS); do \
		echo "TEST: tests/sim/$$d"; \
		$(MAKE) -C $$d VIR=$(VIR) || exit 1; \
	done
