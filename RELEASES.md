# Release Procedure

This document describes how to cut a Virdant release: what a release is,
which files must be kept in sync, what artifacts get produced, and the
exact steps to perform one.

A release is:

1. A **version bump commit** on `master` that sets the version in every
   place it lives (see [Version sources](#version-sources)).
2. A **git tag** `vX.Y.Z` pushed to `origin`.
3. A **GitHub release** containing the toolchain archive(s), produced
   automatically by `.github/workflows/release.yaml` when the tag lands.
4. Optionally, a **rebuilt and redeployed** documentation site
   (virdant.org).

---

## Version sources

The `VERSION` file in the repository root is the **canonical** version.
`vir/build.rs` reads it at build time and bakes it into every binary as
`vir --version` output (e.g. `v0.4.0 (8451d12 2026-06-24)`; in a release
tarball, outside a git checkout, it is just `v0.4.0`).

The version string must appear in **all** of the following places, and
they must agree:

| File                                | What it is                       |
| ----------------------------------- | -------------------------------- |
| `VERSION`                           | Canonical version                |
| `virdant/Cargo.toml`                | Compiler crate version           |
| `vir/Cargo.toml`                    | CLI / tools crate version        |
| `docs/pyproject.toml`               | `virdant-docs` Sphinx project    |
| `docs/uv.lock`                      | Lockfile embeds `virdant-docs`' version — update it too |
| `Cargo.lock`                        | Lockfile embeds both crate versions — refresh after bumping |
| `virdant/Cargo.toml` `rust-version` | MSRV — only changed deliberately, not per release |

The simplest way to keep the lockfiles in sync is to rebuild after
editing the manifests: `cargo build -p virdant` refreshes `Cargo.lock`,
and `make -C docs` (or `uv lock --project docs`) refreshes `docs/uv.lock`.

While a release is *in development*, `VERSION` already holds the
upcoming version number (e.g. the tree carries `0.4.0` while
`v0.4.0` has not been tagged yet). The tag is cut when the code is
actually ready.

## Version numbering

Versions are `X.Y.Z` semver-style. During the pre-1.0 phase, breaking
language changes are acceptable in minor (`0.Y`) bumps.

Historically, pre-release stabilization has happened on a
`v0.Y.0-dev` branch (see `v0.2.0-dev`, `v0.3.0-dev`, `v0.4.0-dev`).
This is optional: if the tree is already stable, cutting straight from
`master` is fine.

## Release artifacts

### Toolchain archive (the primary artifact)

Built by `.github/workflows/build-release.sh`, triggered by the
release workflow. For the `linux` family (currently the only one), it
produces, under `release-assets/`:

    virdant-<version>-<host>.tar.gz          the toolchain
    virdant-<version>-<host>.tar.gz.sha256   checksum sidecar

with `host` = `x86_64-unknown-linux-gnu`, e.g.:

    virdant-v0.4.0-x86_64-unknown-linux-gnu.tar.gz
    virdant-v0.4.0-x86_64-unknown-linux-gnu.tar.gz.sha256

Archive layout (mirrors `make install`):

    virdant-<version>-<host>/
      bin/    the `vir` dispatcher plus every `vir-*` companion binary
      lib/    contents of the repository's `lib/` (builtin, ice40, virt, xilinx)

Notes on the layout:

- Only `vir` and `vir-*` executables are shipped; internal test-only
  binaries (e.g. the runner harness) are excluded because the copy is
  `target/$host/release/vir` and `vir-*` — any binary whose name does
  not start with `vir` never lands in the archive.
- `lib/` is included so that `builtin` and platform definitions are
  available out of the box. `assets/` (logos, tutorial material) is
  **not** included in the archive; it is only installed by `make
  install` from a source checkout.
- A `.sha256` sidecar is generated for each archive so users can
  verify downloads.

### Debian package (local, optional)

`make release` builds a `.deb` locally under `build/deb/`. It is a
separate, manual path — not part of the GitHub release — and currently
has known issues (see [Known issues](#known-issues)). Treat it as
experimental until fixed.

### Documentation

The Sphinx docs (virdant.org) are built with `make -C docs` and
deployed manually with `make -C docs push` (rsync to the web host).
The docs embed the version via `docs/pyproject.toml`, so cut them
after the version bump.

---

## How the release workflow works

`.github/workflows/release.yaml` triggers on any pushed tag matching
`v*`:

1. The `linux` job checks out the tagged commit, installs Rust 1.88
   and SDL2, and runs `build-release.sh "$VER" linux`, where `$VER` is
   the tag name (`GITHUB_REF_NAME`, e.g. `v0.4.0`).
2. Artifacts are uploaded as a workflow artifact named `release-linux`.
3. The `publish` job downloads all artifacts and runs
   `gh release create "$VER"` with `--verify-tag`, attaching every
   `*.tar.gz` and `*.sha256`.

The workflow does **not** run the test suite — testing is the
releaser's responsibility before pushing the tag (see step 3 below).

---

## Step-by-step procedure

The following assumes the release is `0.4.0` (tag `v0.4.0`); substitute
the real version throughout.

### 1. Decide the release is ready

- All `FEATURE`/`CHANGE`/`REMOVED` work intended for the release has
  landed on `master`.
- If stabilizing on a `v0.4.0-dev` branch, merge it back into `master`
  first. The tag must point at a commit on `master`.
- Review the log since the last tag (`git log v0.3.0..master --oneline`)
  and confirm nothing unintended sneaks in.

### 2. Update the version everywhere

Edit these files, setting the version to `0.4.0`:

- `VERSION`
- `virdant/Cargo.toml`
- `vir/Cargo.toml`
- `docs/pyproject.toml`

Then refresh the lockfiles:

    cargo build -p virdant     # refreshes Cargo.lock
    make -C docs               # refreshes docs/uv.lock (may be skipped if deps unchanged)

Verify nothing is stale:

    grep -rn "0\.3\.0" VERSION virdant/Cargo.toml vir/Cargo.toml \
        docs/pyproject.toml docs/uv.lock Cargo.lock

(Expect to match only unrelated dependency versions; the Virdant
version should be `0.4.0` everywhere it appears.)

### 3. Final verification

Run the full test suite **on a clean build** with the external
simulators installed (Verilator, Icarus Verilog, SDL2):

    make clean
    make test        # Rust unit tests + tests/ls + the parallel test runner
    make test_cli    # CLI end-to-end tests (installs into ~/.local/virdant first)

Also confirm generated artifacts are current:

    make grammar     # GRAMMAR.txt matches virdant.lalrpop
    git diff --exit-code GRAMMAR.txt

And build the docs once so the release ships with a site that actually
renders:

    make -C docs

Fix anything that fails; only proceed when everything passes.

### 4. Commit the version bump

One commit containing all version files and lockfiles, using the
`RELEASE:` prefix (see the commit skill):

    RELEASE: Bump crate version to 0.4.0.

The commit must be the *tip* of `master` before tagging, because the
tagged commit is what CI builds and what `vir --version`'s commit hash
refers to.

### 5. Tag the release

    git tag v0.4.0

Historically these tags are lightweight; keep that convention unless
there is a reason to annotate.

### 6. Push

    git push origin master
    git push origin v0.4.0

Pushing the tag is what triggers the release workflow.

### 7. Watch CI

- On GitHub: Actions → "Release" run for the tag.
- The `linux` job builds the archive; the `publish` job creates the
  GitHub release and attaches the artifacts.

If the workflow fails, fix the problem, then re-cut: delete the tag
(`git push origin :refs/tags/v0.4.0`), redo steps 3–6 with a new commit
if needed, and re-push. Deleting and re-creating a GitHub release is
safe as long as no one has consumed the artifacts yet — prefer re-cutting
over force-patching an existing release's assets.

### 8. Verify the release

- The GitHub release exists at
  `https://github.com/virdant-lang/virdant/releases/tag/v0.4.0` with
  both the `.tar.gz` and its `.sha256` attached.
- Download the archive, verify the checksum, and smoke-test it:

      sha256sum -c virdant-v0.4.0-x86_64-unknown-linux-gnu.tar.gz.sha256
      tar -xzf virdant-v0.4.0-x86_64-unknown-linux-gnu.tar.gz
      virdant-v0.4.0-x86_64-unknown-linux-gnu/bin/vir --version
      # expect: v0.4.0

- Confirm `vir --version` built from the tagged checkout also reports
  `v0.4.0` (not `-dirty`).

### 9. Deploy the docs

    make -C docs push

This rebuilds the docs (including rustdocs) and rsyncs them to
virdant.org. Do this after the version bump so the site reflects the
new release.

### 10. Start the next cycle

- If it does not already exist, create `v0.5.0-dev` (or
  `v0.4.1-dev` for a patch line) for the next round of stabilization.
- `VERSION` stays at the released number until the next bump commit;
  the next release repeats this procedure with the new number.

---

## Checklist (condensed)

- [ ] Intended work merged to `master`
- [ ] `VERSION`, `virdant/Cargo.toml`, `vir/Cargo.toml`,
      `docs/pyproject.toml` all bumped to the same version
- [ ] `Cargo.lock` and `docs/uv.lock` refreshed
- [ ] `make test` and `make test_cli` pass
- [ ] `make grammar` leaves `GRAMMAR.txt` unchanged
- [ ] `make -C docs` builds cleanly
- [ ] Version-bump commit (`RELEASE:` prefix) is the tip of `master`
- [ ] Tag `v0.4.0` created and pushed
- [ ] Release workflow succeeded; tarball + sha256 attached to the
      GitHub release
- [ ] Archive smoke-tested (`vir --version` shows the release version)
- [ ] Docs deployed (`make -C docs push`)
- [ ] Next dev branch created

---

## Known issues

Things that are currently rough, worth fixing before they bite:

- **`make release` (.deb) derives the version from the git branch
  name** (`git branch --show-current`), which yields nonsense on
  `master`. It should read `VERSION` instead.
- **The `.deb` ships only `vir`, `vir-run`, and `vir-ls`**, while
  `make install` and the tarball ship every `vir-*` binary. The `.deb`
  should copy all `vir*` executables for parity.
- **The release workflow does not run the test suite.** Release
  verification is entirely manual (step 3). Consider adding a
  `test` job gated on the tag before `publish`.
- **Only one platform family** (`linux`, `x86_64-unknown-linux-gnu`)
  is built. `build-release.sh` is structured to accept more families
  but only `linux` is implemented; a macOS/arm64 release would need a
  `macos` family and a matching CI job.
- **`docs/uv.lock` embeds the docs version** and is easy to forget
  when bumping; `grep`ing for the old version (step 2) catches it.
- **`virdant/Cargo.toml` depends on a git fork of `lalrpop`**
  (`virdant-lang/lalrpop`), not a crates.io release. Anything that
  changes that dependency is a release risk beyond the usual version
  bump.
