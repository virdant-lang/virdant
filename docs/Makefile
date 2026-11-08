# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= uv run sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
BASEDIR       = $(abspath ..)

# The commit the docs were built from, with -dirty if the tree has changes.
COMMIT        = $(shell git -C $(BASEDIR) rev-parse --short HEAD 2>/dev/null)
DIRTY         = $(shell test -n "$$(git -C $(BASEDIR) status --porcelain 2>/dev/null)" && echo "-dirty")
export VIRDANT_COMMIT = $(COMMIT)$(DIRTY)

auto: Makefile # rustdocs
	# dirhtml writes each document as <docname>/index.html, so internal
	# links read .../guide/halfadder/ instead of .../guide/halfadder/index.html.
	# (Invoked with -b rather than -M so output lands in build/html,
	# where serve.sh, push, and rustdocs expect it.)
	retry=0; until $(SPHINXBUILD) -b dirhtml "$(SOURCEDIR)" "$(BUILDDIR)/html" $(SPHINXOPTS) $(O); do \
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

.PHONY: auto clean push rustdocs watch test

clean:
	rm -rf $(BUILDDIR)

push: auto
	rsync -cazvhP $(BUILDDIR)/html/* digitalocean:/root/projects/virdant/docs/build/html

rustdocs:
	cargo doc --manifest-path ../virdant/Cargo.toml
	cp -r ../virdant/target/doc/ $(BUILDDIR)/html/.

# The examples under source/examples/ are full Virdant projects, each with
# a test.toml, so the vir-virtue runner (see the root Makefile) checks them.
# This target is kept as a no-op for compatibility.
test:
	@echo "Examples are checked by the vir-virtue runner (see source/examples/README.md)."