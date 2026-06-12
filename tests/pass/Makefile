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

target/release/vir:
	cargo build --release

target/release/filecheck:
	cargo build --release