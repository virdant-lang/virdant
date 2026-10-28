BASEDIR = $(abspath ../..)
VIR ?= $(BASEDIR)/target/release/vir

test:
	$(VIR) doc
	# Verify that the HTML output exists
	test -f build/doc/index.html
	test -f build/doc/top/index.html
	test -f build/doc/top/Top.html
	# Verify package-level docstring (//!) is present in the package index
	grep -q "package-level docstring" build/doc/top/index.html
	# Verify item-level docstring (//>) is present in the module page
	grep -q "Top module" build/doc/top/Top.html
	grep -q "counter and blinking LED" build/doc/top/Top.html
	# Verify that at least one builtin item page documents its item.
	grep -q "is an n-bit value" build/doc/builtin/Word.html
	# TODO: assert that platform doc pages are generated. The doc
	# renderer currently drops platform items (see TESTING_REPORT/work/
	# infra_findings.md section 2.4: the renderer's `_ => continue`
	# skips platforms), so no platform pages exist to assert on.
	# Once fixed, grep build/doc for a platform page here.

open:
	$(VIR) doc --open
