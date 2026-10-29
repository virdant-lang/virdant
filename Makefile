.PHONY: build test clean install grammar docs release

VIR    ?= $(CURDIR)/target/release/vir
RUNNER ?= $(CURDIR)/target/release/vir-virtue

build: virdant/target/lib
	cargo build --release -p vir

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
	@echo "########################################"
	@echo "# RUNNING TESTS IN tests/"
	@echo "########################################"
	@echo
	$(RUNNER)
	@echo
	@echo "########################################"
	@echo "# RUNNER TESTS PASS"
	@echo "########################################"

test_cli: install
	make -C tests/cli/ test

clean:
	cargo clean

install: build
	mkdir -p ${HOME}/.local/virdant/bin
	mkdir -p ${HOME}/.local/virdant/lib
	cp ./target/release/vir ${HOME}/.local/virdant/bin/
	cp ./target/release/vir-* ${HOME}/.local/virdant/bin/
	cp -r ./lib/* ${HOME}/.local/virdant/lib/

docs:
	$(MAKE) -C docs

release: build
	mkdir -p build/deb/virdant
	mkdir -p build/deb/virdant/DEBIAN
	mkdir -p build/deb/virdant/usr/local/bin
	mkdir -p build/deb/virdant/usr/lib/virdant
	cp -r ./target/release/vir     build/deb/virdant/usr/local/bin/
	cp -r ./target/release/vir-lsp build/deb/virdant/usr/local/bin/
	cp -r ./lib                    build/deb/virdant/usr/lib/virdant
	VERSION=$$(git branch --show-current); \
	echo "Package: virdant"                                   > build/deb/virdant/DEBIAN/control; \
	echo "Version: $${VERSION#v}"                             >> build/deb/virdant/DEBIAN/control; \
	echo "Section: devel"                                     >> build/deb/virdant/DEBIAN/control; \
	echo "Priority: optional"                                 >> build/deb/virdant/DEBIAN/control; \
	echo "Architecture: amd64"                                >> build/deb/virdant/DEBIAN/control; \
	echo "Maintainer: Vir Codificat <vircodificat@gmail.com>" >> build/deb/virdant/DEBIAN/control; \
	echo "Description: The Virdant Hardware Language"         >> build/deb/virdant/DEBIAN/control; \
	cd build/deb && dpkg-deb --root-owner-group --build virdant
