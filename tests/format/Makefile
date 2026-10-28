BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

# vir format needs vir-format in PATH
export PATH := $(BASEDIR)/target/release:$(PATH)

# Only match input files (not *.golden.vir which are expected outputs).
VIRFILES := $(filter-out %.golden.vir,$(wildcard *.vir))
GOLDEN := $(VIRFILES:.vir=.golden.vir)

# Format each input file, diff against golden, and verify parse succeeds.
# The parse gate greps for 'Syntax Error' in the parse output instead of
# trusting the exit code: `vir parse` exits 0 even on syntax errors.
.PHONY: test
test: $(VIRFILES)
	@for f in $^; do \
		golden="$${f%.vir}.golden.vir"; \
		tmpdir=$$(mktemp -d /tmp/virfmt.XXXXXX); \
		cp "$$f" "$$tmpdir/input.vir"; \
		$(VIR) format "$$tmpdir/input.vir" > /dev/null 2>&1; \
		if ! diff -q "$$tmpdir/input.vir" "$$golden" > /dev/null 2>&1; then \
			echo "FAIL: $$f differs from golden"; \
			diff "$$tmpdir/input.vir" "$$golden" || true; \
			rm -rf "$$tmpdir"; \
			exit 1; \
		fi; \
		$(VIR) parse "$$tmpdir/input.vir" > "$$tmpdir/parse.out" 2>&1 || true; \
		if grep -q 'Syntax Error' "$$tmpdir/parse.out"; then \
			echo "FAIL: $$f (formatted) does not parse"; \
			grep 'Syntax Error' "$$tmpdir/parse.out" || true; \
			rm -rf "$$tmpdir"; \
			exit 1; \
		fi; \
		rm -rf "$$tmpdir"; \
		echo "PASS: $$f"; \
	done