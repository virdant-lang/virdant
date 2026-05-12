.PHONY: build test clean install grammar docs

build: virdant/target/lib
	cargo build --release --all-features

# Cargo's build script places generated artifacts in OUT_DIR, which has the
# concrete form: target/<profile>/build/<crate>-<hash>/out/foo.grammar
# (for example: target/debug/build/virdant-5ca77787fdbb9ef7/out/syntax/grammar.grammar).
grammar:
	cargo build -p virdant
	cp $$(ls -t target/debug/build/virdant-*/out/syntax/grammar.grammar | head -n1) GRAMMAR.txt

virdant/target/lib:
	mkdir -p target/lib
	cp -r ./lib/* target/lib/

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
