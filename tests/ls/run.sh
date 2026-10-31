#!/bin/bash
# Golden tests for the `vir ls` query CLI (see VIR_LS.md, "Testing").
#
# Runs every query subcommand against the fixture projects
# tests/ls/fixture/ (valid) and tests/ls/fixture_bad/ (invalid) in the
# modes the contract specifies -- human text by default, --json as the
# canonical form -- and diffs the output against the checked-in goldens
# in tests/ls/goldens/.
#
# All queries must exit 0: a not-found answer and diagnostics on a
# project with errors are answers, not tool failures (exit 2 is reserved
# for the tool failing to run).

set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
VIR="$REPO_ROOT/target/release/vir"
FIXTURE="$REPO_ROOT/tests/ls/fixture"
FIXTURE_BAD="$REPO_ROOT/tests/ls/fixture_bad"
GOLDENS="$REPO_ROOT/tests/ls/goldens"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

if [ ! -x "$VIR" ]; then
  echo "tests/ls: no $VIR built — run make build first" >&2
  exit 2
fi

mkdir -p "$GOLDENS"
FAILED=0
CHECKED=0

# `vir ls` delegates to the `vir-ls` companion binary found on $PATH, so
# the freshly built target/release one must win over any stale
# `vir-ls` installed in ~/.local/virdant/bin.
export PATH="$REPO_ROOT/target/release:$PATH"

# run_query <golden-name> [fixture] <args...>
# Runs `vir ls "$@"` in the given fixture project, saves stdout to the
# golden, and asserts the exit-0 contract.
run_query() {
  local name="$1"; shift
  local dir="$FIXTURE"
  if [ "$1" = "--bad" ]; then
    dir="$FIXTURE_BAD"
    shift
  fi
  local out="$TMP/$name.out"
  ( cd "$dir" && "$VIR" ls "$@" ) > "$out" 2> "$TMP/$name.err"
  local code=$?
  CHECKED=$((CHECKED + 1))
  if [ $code -ne 0 ]; then
    echo "FAIL $name: exit $code ($(cat "$TMP/$name.err"))" >&2
    FAILED=$((FAILED + 1))
    return
  fi
  local golden="$GOLDENS/$name"
  if [ -f "$golden" ]; then
    if ! diff -u "$golden" "$out" > "$TMP/$name.diff" 2>&1; then
      echo "FAIL $name: output differs from golden" >&2
      cat "$TMP/$name.diff" >&2
      FAILED=$((FAILED + 1))
    fi
  else
    cp "$out" "$golden"
  fi
}

# ------------------------------------------------------------------ hover
run_query hover-json            hover src/top.vir:13:20 --json
run_query hover-human           hover src/top.vir:13:20
run_query hover-notfound-json   hover src/top.vir:100:1 --json

# ------------------------------------------------------------- definition
run_query definition-json          definition types::Vector --json
run_query definition-human         definition types::Vector
run_query definition-notfound-json definition NoSuch::Thing --json

# The fixture defines `Base` in both the top and types packages, so the
# bare name `Base` is ambiguous; the ambiguity JSON shapes are pinned.
run_query definition-ambiguous-json   definition Base --json

# ------------------------------------------------------------- references
run_query references-json             references types::Base --json
run_query references-component-json   references types::Base::data --json
run_query references-human            references types::Base
run_query references-notfound-json    references NoSuch::Thing --json

# Ambiguous bare name: an ambiguity has no single reference set, so the
# JSON pins the list of the candidate names instead.
run_query references-ambiguous-json   references Base --json

# ------------------------------------------------------------ diagnostics
run_query diagnostics-json        diagnostics --json
run_query diagnostics-human       diagnostics
run_query diagnostics-bad-json    --bad diagnostics --json
run_query diagnostics-bad-human   --bad diagnostics

# ---------------------------------------------------------------- symbols
run_query symbols-json           symbols types --json
run_query symbols-human          symbols types
run_query symbols-notfound-json  symbols nosuchpkg --json

# ------------------------------------------------------------- components
run_query components-json          components types::Base --json
run_query components-top-json      components top::Top --json
run_query components-human         components types::Base
run_query components-notfound-json components NoSuch::Module --json

# ---------------------------------------------------------------- type-of
run_query typeof-json           type-of src/top.vir:15:20 --json
run_query typeof-human          type-of src/top.vir:15:20
# A location that is not an expression is an answer, not a failure.
run_query typeof-notexpr-json   type-of src/top.vir:13:20 --json
run_query typeof-notfound-json  type-of src/top.vir:100:1 --json

echo "tests/ls: $CHECKED checks, $FAILED failures"
exit $(( FAILED > 0 ? 1 : 0 ))
