#!/bin/bash
# Recapture the golden differential traces in `sim-bench/golden/`.
#
# Run this only when a trace-format or deliberate behavior change makes
# the old golden traces obsolete: the golden files are the frozen
# record of the current simulator engine's behavior, and `check.sh`
# diffs against them.
#
# NOTE: this repository's `.gitignore` ignores `*.sh` globally, so the
# scripts themselves are force-added (`git add -f`).
#
# Cases (see README.md):
#   - generated bench designs at stages 1, 8, 32, 128, 512
#     (generated into build/simtrace-bench/, never committed)
#   - the `assets/new` hello project
#   - the `tests/sim/gcd` design
set -euo pipefail
cd "$(dirname "$0")/.."

STAGES="1 8 32 128 512"
SEED=42
CYCLES=16
GOLDEN=sim-bench/golden
GEN=build/simtrace-bench
BIN="cargo run -q --release -p virdant --example simtrace --"

mkdir -p "$GOLDEN" "$GEN"
for n in $STAGES; do
    dir="$GEN/stages-$n"
    python3 sim-bench/gen.py --stages "$n" --out "$dir"
    echo "capture: bench-stages-$n"
    $BIN --project "$dir" --cycles "$CYCLES" --seed "$SEED" \
        --out "$GOLDEN/bench-stages-$n.trace"
done
echo "capture: new-hello"
$BIN --project assets/new --cycles "$CYCLES" --seed "$SEED" \
    --out "$GOLDEN/new-hello.trace"
echo "capture: gcd"
$BIN --project tests/sim/gcd --cycles "$CYCLES" --seed "$SEED" \
    --out "$GOLDEN/gcd.trace"
echo "Golden traces written to $GOLDEN"
