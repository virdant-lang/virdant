#!/bin/bash

set -e

TMPDIR_WORK="$(mktemp -d "$1"/new_compile.XXXXXX)"
cd "$TMPDIR_WORK"

vir new hello
cd hello

vir compile src
test -f build/src/top.sv
