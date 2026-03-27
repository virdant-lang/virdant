.PHONY: build test clean install

build:
	cargo build --release --all-features
	cp -r ./lib target/

test:
	cargo test
	cd tests && make

clean:
	cargo clean

install:
	cargo build --release --all-features

	mkdir -p ${HOME}/.local/virdant/bin
	cp ./target/release/vir ${HOME}/.local/virdant/bin/
	cp ./target/release/vir-* ${HOME}/.local/virdant/bin/
	cp -r ./lib ${HOME}/.local/virdant/lib
