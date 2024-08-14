.PHONY: build test clean install

build:
	cargo build --release

test:
	cargo test

clean:
	cargo clean

install:
	cargo build --release

	mkdir -p ${HOME}/.local/virdant/bin
	cp ./target/release/vir ${HOME}/.local/virdant/bin/
	cp ./target/release/vir-* ${HOME}/.local/virdant/bin/
