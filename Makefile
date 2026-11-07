.PHONY: build test clean install grammar docs release

VIR    ?= $(CURDIR)/target/release/vir
RUNNER ?= $(CURDIR)/target/release/vir-virtue

GIT_COMMIT := $(shell git rev-parse --short=7 HEAD)
GIT_DIRTY  := $(shell [ -n "$$(git status --porcelain)" ] && echo -dirty)
BUNDLE_DIR := build/virdant

build: virdant/target/lib assets
	cargo build --release -p vir
	mkdir -p $(BUNDLE_DIR)/bin
	mkdir -p $(BUNDLE_DIR)/lib
	mkdir -p $(BUNDLE_DIR)/assets
	find ./target/release -maxdepth 1 -type f -executable -name 'vir*' -exec cp {} $(BUNDLE_DIR)/bin/ \;
	cp -r ./lib/* $(BUNDLE_DIR)/lib/
	cp -r ./assets/* $(BUNDLE_DIR)/assets/
	@echo
	@echo "########################################"
	@echo "# BUILT: $(BUNDLE_DIR)"
	@echo "########################################"

# Cargo's build script places generated artifacts in OUT_DIR, which has the
# concrete form: target/<profile>/build/<crate>-<hash>/out/foo.grammar
# (for example: target/debug/build/virdant-5ca77787fdbb9ef7/out/syntax/virdant.grammar).
grammar:
	cargo build -p virdant
	cp $$(ls -t target/debug/build/virdant-*/out/syntax/virdant.grammar | head -n1) GRAMMAR.txt

virdant/target/lib:
	mkdir -p target/lib
	cp -r ./lib/* target/lib/

test_runner: virdant/target/lib build
	$(RUNNER)
	@echo
	@echo "########################################"
	@echo "# RUNNER TESTS PASS"
	@echo "########################################"

# The standard entry point: Rust unit tests + the parallel test runner.
# The runner discovers every test.toml in the tree, including the docs
# example projects under docs/source/examples/.
test: virdant/target/lib build
	@echo "########################################"
	@echo "RUNNING CARGO TESTS"
	@echo "########################################"
	@echo
	cargo test
	@echo
	@echo
	@echo "########################################"
	@echo "# RUNNING TESTS IN tests/ls/"
	@echo "########################################"
	@echo
	bash tests/ls/run.sh
	@echo
	@echo "########################################"
	@echo "# RUNNING TESTS IN tests/"
	@echo "########################################"
	@echo
	$(RUNNER)
	@echo
	@echo "########################################"
	@echo "# ALL TESTS PASS"
	@echo "########################################"

test_cli: install
	make -C tests/cli/ test

clean:
	cargo clean

# Installs the built bundle as a versioned snapshot at
# $HOME/.local/virdant/virdant-<7-digit-commit>[-dirty], then symlinks each
# executable into $HOME/.local/virdant/bin/. Symlink targets are relative, so
# the whole tree can be moved/synced without breaking.
#
# bin/vir* is replaced ONLY if it is a symlink (or missing); a real file there
# is never clobbered.
install: build
	mkdir -p $(HOME)/.local/virdant/bin
	rm -rf $(HOME)/.local/virdant/virdant-$(GIT_COMMIT)$(GIT_DIRTY)
	cp -r $(BUNDLE_DIR)/ $(HOME)/.local/virdant/virdant-$(GIT_COMMIT)$(GIT_DIRTY)/
	for exe in $(HOME)/.local/virdant/virdant-$(GIT_COMMIT)$(GIT_DIRTY)/bin/vir*; do \
		name=$$(basename $$exe); \
		if [ -L $(HOME)/.local/virdant/bin/$$name ] || [ ! -e $(HOME)/.local/virdant/bin/$$name ]; then \
			ln -sfn ../virdant-$(GIT_COMMIT)$(GIT_DIRTY)/bin/$$name $(HOME)/.local/virdant/bin/$$name; \
		else \
			echo "WARNING: $(HOME)/.local/virdant/bin/$$name exists and is not a symlink; leaving it alone." >&2; \
		fi; \
	done
	@echo
	@echo "########################################"
	@echo "# INSTALLED: $(HOME)/.local/virdant/virdant-$(GIT_COMMIT)$(GIT_DIRTY)"
	@echo "########################################"

docs:
	$(MAKE) -C docs

release: build
	mkdir -p build/deb/virdant
	mkdir -p build/deb/virdant/DEBIAN
	mkdir -p build/deb/virdant/usr/local/bin
	mkdir -p build/deb/virdant/usr/lib/virdant
	cp -r ./target/release/vir     build/deb/virdant/usr/local/bin/
	cp -r ./target/release/vir-ls build/deb/virdant/usr/local/bin/
	cp -r ./lib                    build/deb/virdant/usr/lib/virdant
	cp -r ./assets                 build/deb/virdant/usr/lib/virdant/
	VERSION=$$(git branch --show-current); \
	echo "Package: virdant"                                   > build/deb/virdant/DEBIAN/control; \
	echo "Version: $${VERSION#v}"                             >> build/deb/virdant/DEBIAN/control; \
	echo "Section: devel"                                     >> build/deb/virdant/DEBIAN/control; \
	echo "Priority: optional"                                 >> build/deb/virdant/DEBIAN/control; \
	echo "Architecture: amd64"                                >> build/deb/virdant/DEBIAN/control; \
	echo "Maintainer: Vir Codificat <vircodificat@gmail.com>" >> build/deb/virdant/DEBIAN/control; \
	echo "Description: The Virdant Hardware Language"         >> build/deb/virdant/DEBIAN/control; \
	cd build/deb && dpkg-deb --root-owner-group --build virdant
