RHAI_SCRIPTS := $(wildcard *.rhai)

all:
	for script in $(RHAI_SCRIPTS); do cargo run --bin vir -- run $$script || exit 1; done
