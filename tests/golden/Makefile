SUBDIRS := $(filter-out build/,$(wildcard */))

BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	for d in $(SUBDIRS); do \
		echo "TEST: tests/golden/$$d"; \
		$(MAKE) -C $$d VIR=$(VIR) || exit 1; \
	done
