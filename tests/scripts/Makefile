RHAI_SCRIPTS := $(wildcard *.rhai)
LUA_SCRIPTS := $(wildcard *.lua)

all:
	for script in $(RHAI_SCRIPTS); do cargo run --bin vir --features rhai -- run $$script || exit 1; done
	for script in $(LUA_SCRIPTS); do cargo run --bin vir --features lua -- run $$script || exit 1; done
