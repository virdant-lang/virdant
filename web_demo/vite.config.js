import { defineConfig } from 'vite';

// The web demo is a client-side-only SPA: no server code anywhere.
// `vite build` emits a fully static `dist/`; `vite preview` serves it.
export default defineConfig({
  // The demo is deployed under the docs site at /web (see the root
  // Makefile's push_web target), but must also work when served
  // locally at the domain root.  A relative base gives `./assets/...`
  // URLs, which are correct in both layouts because `dist/index.html`
  // sits directly next to `assets/` in both.
  base: './',
  build: {
    target: 'es2022',
  },
  // The wasm-bindgen output is imported directly from src/wasm; make
  // sure vite treats the .wasm asset as a static asset URL (it does,
  // via `new URL(..., import.meta.url)` in the generated JS).
  assetsInclude: ['**/*.wasm'],
});
