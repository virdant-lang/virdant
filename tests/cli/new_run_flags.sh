#!/bin/bash

set -e

TMPDIR_WORK="$(mktemp -d "$1"/new_run_flags.XXXXXX)"
cd "$TMPDIR_WORK"

vir new hello
cd hello

vir run --fps-no-cap --fps --verbose --cycles 100000
vir run --fps-no-cap --trace
test -f trace.vcd
vir run --fps-no-cap --trace custom.vcd
test -f custom.vcd
