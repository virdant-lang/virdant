test_all:
	cargo run --bin vir -- -C basics build
	verilator --lint-only basics/build/*.sv

	cargo run --bin vir -- -C uart build
	verilator --lint-only uart/build/*.sv

	cargo run --bin vir -- -C typedefs build
	verilator --lint-only typedefs/build/*.sv

.PHONY: sim
