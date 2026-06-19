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
	retry=0; until $(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O); do \
	  if [ $$retry -ge 5 ]; then exit 1; fi; \
	  if grep -q "FileNotFoundError.*searchindexfn" /tmp/sphinx-err-*.log 2>/dev/null; then \
	    echo "Sphinx race condition detected, retrying in 1s..."; \
	    sleep 1; \
	    retry=$$((retry + 1)); \
	  else \
	    exit 1; \
	  fi; \
	done

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
