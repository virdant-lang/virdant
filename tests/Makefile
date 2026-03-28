test_all:
	cargo run -- -C basics build
	verilator --lint-only basics/build/*.sv

	cargo run -- -C uart build
	verilator --lint-only uart/build/*.sv

#	cargo run -- -C typedefs build
#	verilator --lint-only typedefs/build/*.sv

.PHONY: sim
