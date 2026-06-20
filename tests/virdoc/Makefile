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

open:
	$(VIR) doc --open
