VIR_FILES := $(wildcard *.vir)

test:
	mkdir -p build
	for f in $(VIR_FILES); do \
		name=$$(basename $$f .vir); \
		cargo run -- -F $$f check > build/$$name.out 2>&1 || true; \
		cat build/$$name.out | cargo run --bin filecheck -- $$(realpath $$f) || exit 1; \
	done
