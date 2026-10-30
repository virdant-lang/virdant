#!/bin/bash

set -e

TMPDIR_WORK="$(mktemp -d "$1"/new_check.XXXXXX)"
cd "$TMPDIR_WORK"

vir new hello
cd hello
vir check
vir run
