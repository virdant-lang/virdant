VIR_FILES := $(wildcard *.vir)

BASEDIR = $(abspath ../..)
VIR := $(BASEDIR)/target/release/vir
FILECHECK := $(BASEDIR)/target/release/filecheck

test: $(VIR) $(FILECHECK)
	mkdir -p build
	for f in $(VIR_FILES); do \
		name=$$(basename $$f .vir); \
		$(VIR) -F $$f check > build/$$name.out 2>&1; \
		rc=$$?; \
		cat build/$$name.out | $(FILECHECK) $$(realpath $$f) || exit 1; \
		if [ $$rc -ne 0 ]; then echo "ERROR: $$f reported an unexpected errors"; exit 1; fi \
	done

target/release/vir:
	cargo build --release

target/release/filecheck:
	cargo build --release
