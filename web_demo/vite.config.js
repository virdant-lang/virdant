import { defineConfig } from 'vite';

// The web demo is a client-side-only SPA: no server code anywhere.
// `vite build` emits a fully static `dist/`; `vite preview` serves it.
export default defineConfig({
  // The demo is deployed under the docs site at /web (see the root
  // Makefile's push_web target), so assets must be addressed relative
  // to that base rather than the domain root.
  base: '/web/',
  build: {
    target: 'es2022',
  },
  // The wasm-bindgen output is imported directly from src/wasm; make
  // sure vite treats the .wasm asset as a static asset URL (it does,
  // via `new URL(..., import.meta.url)` in the generated JS).
  assetsInclude: ['**/*.wasm'],
});
