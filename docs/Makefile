# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
EXAMPLES      = $(wildcard sources/examples/*.vir)


auto: Makefile $(EXAMPLES) # rustdocs
	$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: auto clean push rustdocs

clean:
	rm -rf $(BUILDDIR)

%.pdf: %.tex
	mkdir -p ./build/html

push: auto
	rsync -cazvhP $(BUILDDIR)/html/* digitalocean:/root/projects/virdant/docs/build/html

rustdocs:
	cargo doc --manifest-path ../virdant/Cargo.toml
	cp -r ../virdant/target/doc/ $(BUILDDIR)/html/.
