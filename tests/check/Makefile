VIR_FILES := $(wildcard *.vir)
SUBDIRS := $(filter-out build/,$(wildcard */))

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
	for d in $(SUBDIRS); do \
		echo "TEST: tests/check/$$d"; \
		$(MAKE) -C $$d VIR=$(VIR) FILECHECK=$(FILECHECK) || exit 1; \
	done
