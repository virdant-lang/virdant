import { defineConfig } from 'vite';

// Build for the docs-site "try dock" (see PLAN.md and trydock/):
// `trydock.js` is the dock UI that docs pages load lazily from
// /try/trydock/trydock.js; `trydock-worker.js` is the SharedWorker
// hosting the wasm checker, the session state and the nested
// simulator.  Both land in `dist/trydock/` next to the SPA's dist
// output, so the existing `push_web` rsync deploys them unchanged
// (same origin as the docs HTML, which is what SharedWorker needs).
//
// Entry filenames are pinned (no hash) because the docs pages and
// the boot script reference the paths directly.

export default defineConfig({
  base: './',
  build: {
    target: 'es2022',
    outDir: 'dist/trydock',
    emptyOutDir: true,
    rollupOptions: {
      input: {
        trydock: 'trydock/trydock.js',
        'trydock-worker': 'trydock/trydock-worker.js',
      },
      output: {
        entryFileNames: '[name].js',
        chunkFileNames: 'assets/trydock-[hash].js',
        assetFileNames: 'assets/trydock-[hash][extname]',
      },
    },
    // src/sim-worker.js is instantiated as an ES-module nested worker
    // (same as the SPA does for its dedicated sim worker).
    // Known wart: the checker and the nested sim-worker each emit
    // their own (identical) copy of the wasm under assets/, so a sim
    // run downloads it twice.  Vite's worker build does not honor a
    // shared assetFileNames; collapsing them would need post-build
    // symlinking — not worth it for now.
    worker: {
      format: 'es',
    },
  },
  // The wasm-bindgen glue imports the .wasm via `new URL(...,
  // import.meta.url)`; make sure vite treats the asset as a URL.
  assetsInclude: ['**/*.wasm'],
});
