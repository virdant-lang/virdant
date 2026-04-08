.PHONY: build test clean install

build: virdant/target/lib
	cargo build --release --all-features

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
