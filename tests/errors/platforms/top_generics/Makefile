BASEDIR = $(abspath ../../../..)
VIR ?= $(BASEDIR)/target/release/vir
FILECHECK ?= $(BASEDIR)/target/release/filecheck

test:
	mkdir -p build
	$(VIR) bitstream > build/bitstream.out 2>&1; rc=$$?; \
	cat build/bitstream.out | $(FILECHECK) $$(realpath src/top.vir) || exit 1; \
	if [ $$rc -eq 0 ]; then echo "ERROR: expected vir bitstream to fail"; exit 1; fi
