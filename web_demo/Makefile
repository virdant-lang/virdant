PORT ?= 8000

# Serve the built demo (static, client-side-only) on $(PORT).
# Example: `make serve` or `make serve PORT=8001`.
.PHONY: serve build wasm node_modules all clean

all: build

node_modules: package.json
	npm install

# Build the Rust wasm backend (virdant-web) and generate the
# wasm-bindgen JS glue into src/wasm/.
wasm:
	cd wasm && cargo build --target wasm32-unknown-unknown --release
	wasm-bindgen --target web \
		--out-dir src/wasm \
		wasm/target/wasm32-unknown-unknown/release/virdant_web.wasm

build: node_modules wasm
	npm run build

serve: build
	npm run preview -- --port $(PORT) --strictPort

clean:
	rm -rf node_modules dist src/wasm wasm/target
