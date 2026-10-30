// Virdant web demo — simulator worker.
//
// Runs the `virt::Virt` board (the wasm VM) off the main thread so
// that editing, rendering and input never block on the simulator.
// The worker owns the whole step loop, including the real-time pacing
// (`--fps-no-cap`) and adaptive chunk sizing: each `step()` call is
// sized to hit a time target, so a heavy design yields between chunks
// and Stop/input messages stay responsive instead of arriving late.
//
// Protocol (postMessage):
//   main → worker:
//     {type:'create', payload, top, fpsNoCap, flash?:ArrayBuffer}
//     {type:'input', bytes:Uint8Array}
//     {type:'config', fpsNoCap}
//     {type:'stop'}
//   worker → main:
//     {type:'created', clockHz}
//     {type:'output', buf:ArrayBuffer}          (UART tx bytes)
//     {type:'progress', cycles}                 (throttled)
//     {type:'finished', exitCode, cycles}
//     {type:'stopped', cycles}
//     {type:'create-error', message}
//     {type:'error', message}                   (runtime failure)

import init, { VirtVm } from './wasm/virdant_web.js';

const ready = init({
  module_or_path: new URL('./wasm/virdant_web_bg.wasm', import.meta.url),
});

let vm = null;
let running = false;
let fpsNoCap = false;
let startWall = 0;
let lastProgressWall = 0;

// Adaptive chunk sizing.  `step()` cannot be interrupted, so a fixed
// chunk size freezes the loop for its whole duration on a heavy
// design.  Instead: measure how long each step() took, keep an EMA of
// the rate (cycles/ms), and size the next chunk to hit the target —
// so a chunk always costs about STEP_TARGET_MS regardless of design
// weight, and the loop regains control between chunks.
const STEP_TARGET_MS = 5;
const FRAME_BUDGET_MS = 12;
const MIN_CHUNK = 1_000;    // cycles (plain numbers: adapt() clamps in
const MAX_CHUNK = 4_000_000; // Math-land; BigInt math stays in tick())
let chunkCycles = 20_000n;
let cyclesPerMs = 0;

function adapt(nCycles, ms) {
  if (ms <= 0) ms = 0.05;
  const measured = Number(nCycles) / ms;
  cyclesPerMs = cyclesPerMs ? 0.5 * cyclesPerMs + 0.5 * measured : measured;
  let target = Math.round(cyclesPerMs * STEP_TARGET_MS);
  // Ramp up at most ×2 per measured step: a single noisy "fast"
  // measurement must not commit us to a huge step() that would then
  // freeze the loop for seconds on a slow design.  Shrinking is
  // always immediate.
  const current = Number(chunkCycles);
  if (target > current * 2) target = current * 2;
  chunkCycles = BigInt(Math.min(MAX_CHUNK, Math.max(MIN_CHUNK, target)));
}

function postOutput() {
  const bytes = vm.take_output();
  if (bytes.length) postMessage({ type: 'output', buf: bytes.buffer }, [bytes.buffer]);
}

// Progress is only needed for the fps readout; throttle so the paced
// loop's 1 ms naps don't flood the main thread with messages.
function postProgress() {
  const now = performance.now();
  if (now - lastProgressWall < 50) return;
  lastProgressWall = now;
  postMessage({ type: 'progress', cycles: Number(vm.cycles()) });
}

function tick() {
  if (!running || !vm) return;
  try {
    const deadline = performance.now() + FRAME_BUDGET_MS;
    let stepped = false;
    while (performance.now() < deadline) {
      let n;
      if (fpsNoCap) {
        n = chunkCycles;
      } else {
        const periodNs = 1e9 / Number(vm.clock_hz());
        const target = BigInt(Math.floor((performance.now() - startWall) * 1e6 / periodNs));
        const deficit = target - vm.cycles();
        if (deficit <= 0n) break; // ahead of wall time; nap below
        n = deficit < chunkCycles ? deficit : chunkCycles;
      }
      const t0 = performance.now();
      vm.step(n);
      // Only adapt from near-full chunks: a deficit-truncated step
      // would otherwise understate the chunk cost.
      if (n >= chunkCycles / 2n) adapt(n, performance.now() - t0);
      stepped = true;
      if (vm.is_finished()) break;
    }
    if (stepped) postOutput();
    postProgress();
    if (vm.is_finished()) {
      running = false;
      const exitCode = vm.exit_code();
      const cycles = Number(vm.cycles());
      postOutput();
      postMessage({ type: 'finished', exitCode, cycles });
      vm = null;
      return;
    }
    // Yield so pending messages (stop/input) are processed; nap when
    // the paced sim is ahead of wall time instead of busy-spinning.
    setTimeout(tick, stepped ? 0 : 1);
  } catch (e) {
    running = false;
    vm = null;
    postMessage({ type: 'error', message: String(e) });
  }
}

async function handle(m) {
  switch (m.type) {
    case 'create': {
      if (vm) {
        postMessage({ type: 'create-error', message: 'a simulation is already running' });
        return;
      }
      fpsNoCap = !!m.fpsNoCap;
      try {
        vm = m.flash
          ? VirtVm.create_with_flash(m.payload, m.top, new Uint8Array(m.flash))
          : VirtVm.create(m.payload, m.top);
      } catch (e) {
        vm = null;
        postMessage({ type: 'create-error', message: String(e) });
        return;
      }
      postMessage({ type: 'created', clockHz: Number(vm.clock_hz()) });
      running = true;
      startWall = performance.now();
      lastProgressWall = 0;
      setTimeout(tick, 0);
      break;
    }
    case 'input':
      if (vm) vm.send_input(new Uint8Array(m.bytes));
      break;
    case 'config':
      fpsNoCap = !!m.fpsNoCap;
      break;
    case 'stop': {
      if (!running || !vm) return;
      running = false;
      const cycles = Number(vm.cycles());
      postOutput();
      postMessage({ type: 'stopped', cycles });
      vm = null;
      break;
    }
  }
}

self.onmessage = (e) => {
  ready.then(() => handle(e.data));
};
