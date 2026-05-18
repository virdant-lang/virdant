.PHONY: build build-wasm test clean install grammar docs release

build: virdant/target/lib
	cargo build --release --features "vir-bin,filecheck-bin,vir-lsp-bin,lua,rhai"

# Cargo's build script places generated artifacts in OUT_DIR, which has the
# concrete form: target/<profile>/build/<crate>-<hash>/out/foo.grammar
# (for example: target/debug/build/virdant-5ca77787fdbb9ef7/out/syntax/grammar.grammar).
grammar:
	cargo build -p virdant
	cp $$(ls -t target/debug/build/virdant-*/out/syntax/grammar.grammar | head -n1) GRAMMAR.txt

virdant/target/lib:
	mkdir -p target/lib
	cp -r ./lib/* target/lib/

build-wasm:
	cargo build -p virdant --lib --target wasm32-unknown-unknown --no-default-features --features "wasm,rhai"

test: virdant/target/lib build
	cargo test
	$(MAKE) -C tests test_all
	@echo
	@printf '%*s\n' "$$(tput cols)" '' | tr ' ' '#'
	@echo "  All Tests Pass  "
	@printf '%*s\n' "$$(tput cols)" '' | tr ' ' '#'

clean:
	cargo clean

install:
	cargo build --release --all-features
	mkdir -p ${HOME}/.local/virdant/bin
	cp ./target/release/vir ${HOME}/.local/virdant/bin/
	cp ./target/release/vir-* ${HOME}/.local/virdant/bin/
	cp -r ./lib ${HOME}/.local/virdant/lib
	cp ./target/release/filecheck ${HOME}/.local/virdant/bin/

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
