# Every subdirectory is a project test (Virdant.toml + own Makefile).
# Recursion is automatic: no project can be forgotten.
SUBDIRS := $(filter-out build/,$(wildcard */))

BASEDIR = $(abspath ../../..)
VIR ?= $(BASEDIR)/target/release/vir
FILECHECK ?= $(BASEDIR)/target/release/filecheck

test:
	for d in $(SUBDIRS); do \
		echo "TEST: tests/check/projects/$$d"; \
		$(MAKE) -C $$d VIR=$(VIR) FILECHECK=$(FILECHECK) || exit 1; \
	done
