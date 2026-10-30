#!/bin/bash
# Differential-trace check: recapture a trace for every golden case and
# diff it against the committed golden in `sim-bench/golden/`.  Prints
# PASS/FAIL per case and exits nonzero on any diff.
#
# Determinism proof: run it twice in a row — both runs must PASS
# (each run's recapture must reproduce its own golden exactly).
# Engine-equivalence proof: after a simulator rewrite, any FAIL is a
# behavioral difference against the current engine (see README.md).
set -uo pipefail
cd "$(dirname "$0")/.."

STAGES="1 8 32 128 512"
SEED=42
CYCLES=16
GOLDEN=sim-bench/golden
ACTUAL=build/simtrace-check
BIN="cargo run -q --release -p virdant --example simtrace --"

fail=0

check() {
    if [ ! -f "$GOLDEN/$1.trace" ]; then
        echo "FAIL $1 (no golden trace; run sim-bench/capture.sh first)"
        fail=1
    elif diff -u "$GOLDEN/$1.trace" "$ACTUAL/$1.trace"; then
        echo "PASS $1"
    else
        echo "FAIL $1 (diff sim-bench/golden/$1.trace $ACTUAL/$1.trace)"
        fail=1
    fi
}

rm -rf "$ACTUAL"
mkdir -p "$ACTUAL" "$GOLDEN"
for n in $STAGES; do
    dir="$ACTUAL/gen-stages-$n"
    python3 sim-bench/gen.py --stages "$n" --out "$dir"
    echo "capture: bench-stages-$n"
    $BIN --project "$dir" --cycles "$CYCLES" --seed "$SEED" \
        --out "$ACTUAL/bench-stages-$n.trace" \
        || { echo "FAIL bench-stages-$n (simtrace exited nonzero)"; fail=1; continue; }
    check "bench-stages-$n"
done
echo "capture: new-hello"
$BIN --project assets/new --cycles "$CYCLES" --seed "$SEED" \
    --out "$ACTUAL/new-hello.trace" \
    || { echo "FAIL new-hello (simtrace exited nonzero)"; fail=1; }
[ "$fail" -eq 0 ] && check "new-hello"
echo "capture: gcd"
$BIN --project tests/sim/gcd --cycles "$CYCLES" --seed "$SEED" \
    --out "$ACTUAL/gcd.trace" \
    || { echo "FAIL gcd (simtrace exited nonzero)"; fail=1; }
[ "$fail" -eq 0 ] && check "gcd"

if [ "$fail" -eq 0 ]; then
    echo "ALL PASS"
else
    echo "FAILED"
fi
exit "$fail"
