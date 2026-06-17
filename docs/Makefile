# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= uv run sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
BASEDIR       = $(abspath ..)
VIR          ?= $(BASEDIR)/target/release/vir
EXAMPLES      = $(wildcard source/examples/*.vir)
TESTS         = $(wildcard source/examples/*.lua)

auto: Makefile $(EXAMPLES) # rustdocs
	$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

watch:
	bash watch.sh

serve: auto
	bash serve.sh

.PHONY: auto clean push rustdocs watch

clean:
	rm -rf $(BUILDDIR)

push: auto
	rsync -cazvhP $(BUILDDIR)/html/* digitalocean:/root/projects/virdant/docs/build/html

rustdocs:
	cargo doc --manifest-path ../virdant/Cargo.toml
	cp -r ../virdant/target/doc/ $(BUILDDIR)/html/.

test: $(EXAMPLES) $(TESTS)
	for example in $(EXAMPLES); do $(VIR) -F $$example check  || exit 1; done
	for script in $(TESTS); do \
		echo "--------------------------------------------------------------------------------"; \
		echo "TEST: docs/$$script"; \
		echo "--------------------------------------------------------------------------------"; \
		$(VIR) run $$script || exit 1; \
	done
