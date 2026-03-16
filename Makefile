.PHONY: build test clean install

build:
	cargo build --release --all-features

test:
	cargo test

clean:
	cargo clean

install:
	cargo build --release --all-features

	mkdir -p ${HOME}/.local/virdant/bin
	cp ./target/release/vir ${HOME}/.local/virdant/bin/
	cp ./target/release/vir-* ${HOME}/.local/virdant/bin/
	cp ./bin/vir-* ${HOME}/.local/virdant/bin/
