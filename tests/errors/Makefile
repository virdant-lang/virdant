VIR_FILES := $(wildcard *.vir)

BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir
FILECHECK ?= $(BASEDIR)/target/release/filecheck

test:
	mkdir -p build
	for f in $(VIR_FILES); do \
		name=$$(basename $$f .vir); \
		$(VIR) -F $$f check > build/$$name.out 2>&1; \
		rc=$$?; \
		cat build/$$name.out | $(FILECHECK) $$(realpath $$f) || exit 1; \
		if [ $$rc -eq 0 ]; then echo "ERROR: $$f reported an unexpected 0 errors"; exit 1; fi \
	done

