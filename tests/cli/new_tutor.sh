#!/bin/bash

set -e

TMPDIR_WORK="$(mktemp -d "$1"/new_tutor.XXXXXX)"
cd "$TMPDIR_WORK"

# `vir tutor` creates a workspace with only the first lesson unlocked.
vir-tutor
test -d tutorial/000_passthrough
test ! -d tutorial/001_buffer

# Re-running `vir tutor` fails because the workspace already exists.
if vir-tutor > /dev/null 2>&1; then
    echo "Expected 'vir tutor' to fail when tutorial/ already exists" >&2
    exit 1
fi

# `vir tutor check` outside of a lesson directory fails.
cd tutorial
if vir-tutor check > /dev/null 2>&1; then
    echo "Expected 'vir tutor check' to fail outside a lesson directory" >&2
    exit 1
fi
cd 000_passthrough

# The first lesson ships complete, so checking it passes and unlocks the next lesson.
OUT="$(vir-tutor check)"
echo "$OUT" | grep -q "PASSED!"
echo "$OUT" | grep -q "You've unlocked the next lesson: 001_buffer"
test -d ../001_buffer

# Re-checking an already-passed lesson passes again, without re-unlocking.
OUT="$(vir-tutor check)"
echo "$OUT" | grep -q "PASSED!"
if echo "$OUT" | grep -q "unlocked"; then
    echo "Expected no unlock message when re-checking lesson 000" >&2
    exit 1
fi

# A broken lesson fails the check.
cd ../001_buffer
echo "mod Broken {" > src/bad.vir
if vir-tutor check > check.out 2>&1; then
    echo "Expected 'vir tutor check' to fail on broken source" >&2
    exit 1
fi
grep -q "Failed..." check.out
rm src/bad.vir

# Once fixed, the lesson passes and unlocks the next one.
OUT="$(vir-tutor check)"
echo "$OUT" | grep -q "PASSED!"
echo "$OUT" | grep -q "You've unlocked the next lesson: 002_mux"
