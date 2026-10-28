BASEDIR = $(abspath ../../../..)
VIR ?= $(BASEDIR)/target/release/vir
FILECHECK ?= $(BASEDIR)/target/release/filecheck

test:
	mkdir -p build
	$(VIR) check > build/check.out 2>&1; rc=$$?; \
	cat build/check.out | $(FILECHECK) $$(realpath src/top.vir) || exit 1; \
	if [ $$rc -eq 0 ]; then echo "ERROR: expected vir check to fail"; exit 1; fi
