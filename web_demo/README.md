# Virdant Web Demo

A client-side-only web demo of Virdant: a multi-file IDE-style editor
with live error checking and the `virt::Virt` board simulator
(`vir run`'s simulator) compiled to WebAssembly.  No server logic
anywhere — the built output under `dist/` is fully static.

## Layout

- `wasm/` — the `virdant-web` Rust crate.  Wraps the `virdant`
  compiler library (check path, diagnostics as JSON) and ports the
  `virt::Virt` board from `vir/src/vir/virt.rs` to wasm: the Unix
  shell (stdin thread, termios, SIGINT, `thread::sleep` pacing) is
  replaced by shared byte buffers drained/filled from JS and a rAF
  loop that paces the simulated clock against wall time.
- `src/wasm/` — wasm-bindgen output (generated; do not edit).
- `src/main.js`, `index.html`, `src/style.css` — the SPA: CodeMirror 6
  editor on the left (file sidebar with add/remove, Default or Vim
  keybindings), an Errors/Console tabbed panel on the right, and a
  Run/Stop/Settings bar along the bottom.
- `src/vir-language.js` — a small Virdant syntax-highlighting mode.
- `src/default_top.vir` — copy of `assets/new/src/top.vir`, the
  starting file.

## Building and serving

    make serve            # build wasm + frontend, then serve on port 8000
    make serve PORT=8001  # ... or another port

Requirements: `cargo` with the `wasm32-unknown-unknown` target,
`wasm-bindgen-cli` (version-pinned to wasm-bindgen 0.2.104 in
`wasm/Cargo.toml`; `cargo binstall wasm-bindgen-cli@0.2.104` installs
it), and `npm`.

## Using the demo

- **Files**: add/remove files in the left sidebar.  Everything
  (contents, open file, keymap, settings) persists in `localStorage`;
  Settings has a "Reset files to default" button.
- **Errors tab**: live diagnostics (debounced re-check on every edit)
  with click-to-jump.  The Run button is greyed out while there are
  errors.
- **Run** compiles all files linked against the `lib/` packages
  (embedded into the wasm binary at build time) and, if the design
  checks clean, starts the VM.  UART TX output appears in the Console
  tab; the console input line feeds the design's UART RX; the design's
  `exit : Valid[Word[8]]` port ends the run with its code.
- **Stop** stops a run early.  The Run/Stop button swaps while the
  simulator is running and swaps back when it exits.
- **Settings**: top module (default `top::Top`), disable real-time
  pacing (`--fps-no-cap`), and display the average clock frequency.

## Not yet supported

- SPI flash image preload (the native `--flash`); the flash device
  model itself is implemented, only the image upload is missing.
- VCD traces.
