PORT ?= 8000

# Serve the built demo (static, client-side-only) on $(PORT).
# Example: `make serve` or `make serve PORT=8001`.
.PHONY: serve build trydock wasm node_modules all clean push_web

all: build

node_modules: package.json
	npm install

# Build the Rust wasm backend (virdant-try) and generate the
# wasm-bindgen JS glue into src/wasm/.
wasm:
	cd wasm && cargo build --target wasm32-unknown-unknown --release
	wasm-bindgen --target web \
		--out-dir src/wasm \
		wasm/target/wasm32-unknown-unknown/release/virdant_try.wasm

# Build the docs-site "try dock" (SharedWorker + dock UI; see PLAN.md
# and trydock/).  Emits dist/trydock/{trydock,trydock-worker}.js; the
# docs pages load /try/trydock/trydock.js lazily, so both the SPA dist
# and this must be deployed together (the existing push_web rsync
# covers both).
trydock: node_modules
	npm run build:trydock

build: node_modules wasm
	npm run build
	npm run build:trydock
	@# The preset flash binaries are served next to the built demo;
	@# `flash/manifest.json` lists them for the settings dropdown.
	mkdir -p dist/flash
	cp flash/*.bin dist/flash/
	node -e 'const fs=require("fs");const names=fs.readdirSync("flash").filter(f=>f.endsWith(".bin")).sort();fs.writeFileSync("dist/flash/manifest.json",JSON.stringify(names));'

serve: build
	npm run preview -- --port $(PORT) --strictPort

push_web: build
	rsync -avz --delete dist/ digitalocean:/root/projects/virdant/docs/build/html/try

clean:
	rm -rf node_modules dist src/wasm wasm/target
