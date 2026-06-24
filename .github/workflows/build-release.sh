#!/bin/sh
# Build the release artifacts for one platform family.
#
# Usage: build-release.sh <version> <linux>
#
# Produces, under release-assets/:
#   virdant-<ver>-<host>.tar.gz         the toolchain
#   virdant-<ver>-<host>.tar.gz.sha256
#
# See RELEASES.md for the archive layouts and rationale.

set -eu

VER="$1"
FAMILY="$2"
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$HERE/../.." && pwd)"
cd "$REPO_ROOT"

case "$FAMILY" in
  linux)
    targets="x86_64-unknown-linux-gnu"
    ;;
  *)
    echo "error: unknown family '$FAMILY' (expected linux)" >&2
    exit 1
    ;;
esac

# The feature set matches `make build`.
TOOLCHAIN_FEATURES="vir-bin,filecheck-bin,vir-lsp-bin,lua,rhai"

# Pick a sha256 command that works on Linux (sha256sum).
sha256_sum() { sha256sum "$1"; }

mkdir -p release-assets

build_one() {
  host="$1"
  echo "==> building for $host"

  CARGO="cargo"

  # --- toolchain ---
  # One build produces vir, vir-lsp, and vir-format binaries.
  $CARGO build --release --target "$host" --features "$TOOLCHAIN_FEATURES"
  tc_stage="$(mktemp -d)"
  tc_dir="virdant-$VER-$host"
  mkdir -p "$tc_stage/$tc_dir/bin" "$tc_stage/$tc_dir/lib"
  cp "target/$host/release/vir"        "$tc_stage/$tc_dir/bin/"
  cp "target/$host/release/vir-lsp"    "$tc_stage/$tc_dir/bin/"
  cp "target/$host/release/vir-format" "$tc_stage/$tc_dir/bin/"
  cp -r lib/*                           "$tc_stage/$tc_dir/lib/" 2>/dev/null || true
  tar -C "$tc_stage" -czf "release-assets/$tc_dir.tar.gz" "$tc_dir"
  rm -rf "$tc_stage"
}

for host in $targets; do
  build_one "$host"
done

# Generate .sha256 sidecars for everything this family produced.
cd release-assets
for f in *.tar.gz; do
  sha256_sum "$f" > "$f.sha256"
done
