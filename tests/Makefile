test_all:
	cargo run -- -C basics build
	verilator --lint-only basics/build/*.sv
	cargo run -- -C uart build
	verilator --lint-only uart/build/*.sv

.PHONY: sim
