// Virdant docs-site "try dock" — shared backend.
//
// A SharedWorker (served from /try/trydock/trydock-worker.js) that hosts:
//   - the wasm compiler (`check`) for live diagnostics,
//   - the session state (projects, files, diagnostics, console log),
//     so the dock survives moving between docs pages, and
//   - the simulator (the virt::Virt VM with the SPA's adaptive
//     real-time pacing, inlined below).
//
// Note: the simulator cannot be a nested dedicated worker here —
// `Worker` is not exposed on SharedWorkerGlobalScope (Chromium et al).
// It runs directly in this worker instead; the compiler and the VM
// share one wasm instance.  Live re-checks are deferred while a
// simulation runs, so the two never compete (same rule as the SPA).
//
// Protocol (postMessage):
//   dock → worker:
//     {type:'hello'}                          handshake; reply is `sync`
//     {type:'edit', file, text}               update one file, re-check
//     {type:'selectProject', index}
//     {type:'run', top?, fpsNoCap?}           check, then start the sim
//     {type:'config', fpsNoCap}               mid-run settings update
//     {type:'input', bytes:Uint8Array}        UART rx bytes
//     {type:'stop'}
//     {type:'resetProject'}                   restore seed files
//   worker → dock(s):
//     {type:'sync', ...session snapshot}      on hello (that port only)
//     {type:'diagnostics', diagnostics, ok, checked}
//     {type:'console', buf:ArrayBuffer}       UART tx / worker notices
//     {type:'created', clockHz}
//     {type:'progress', cycles}
//     {type:'finished', exitCode, cycles}
//     {type:'stopped', cycles}
//     {type:'sim-error', message}
//     {type:'consoleReset'}                   clear terminal + log
//
// Console output is appended to an in-memory log (capped) and
// replayed in `sync`, so a freshly navigated page picks up the
// scrollback and the running state mid-stream.  Persistence beyond
// the worker's lifetime is deliberately out of scope (see PLAN.md).

import init, { check as wasmCheck, VirtVm } from '../src/wasm/virdant_try.js';

// ── Seed projects (same rules as the SPA's main.js) ─────────

const projectTomls = import.meta.glob('../projects/*/virdant.toml', {
  query: '?raw',
  import: 'default',
  eager: true,
});
const projectVirs = import.meta.glob('../projects/*/src/*.vir', {
  query: '?raw',
  import: 'default',
  eager: true,
});

import projectOrderSpec from '../projects/projects.json';
const projectOrder =
  Array.isArray(projectOrderSpec?.order) && projectOrderSpec.order.every((n) => typeof n === 'string')
    ? projectOrderSpec.order
    : [];
function orderKey(name) {
  const i = projectOrder.indexOf(name);
  return i === -1 ? projectOrder.length : i;
}

function tomlString(text, key) {
  const m = text.match(new RegExp(`^\\s*${key}\\s*=\\s*"([^"]*)"`, 'm'));
  return m?.[1] ?? null;
}

const seedSpecs = Object.entries(projectTomls)
  .map(([tomlPath, tomlText]) => {
    const dir = tomlPath.split('/')[2];
    const files = Object.entries(projectVirs)
      .filter(([path]) => path.split('/')[2] === dir)
      .map(([path, text]) => ({ name: path.split('/').pop(), text }))
      .sort((a, b) => a.name.localeCompare(b.name));
    return {
      name: dir,
      files,
      top: tomlString(tomlText, 'top') ?? 'top::Top',
    };
  })
  .filter((spec) => spec.files.length > 0)
  .sort((a, b) =>
    (orderKey(a.name) - orderKey(b.name)) || a.name.localeCompare(b.name));

// ── Session state (in-memory; no persistence by design) ─────

const session = {
  projects: seedSpecs.map((spec) => ({
    name: spec.name,
    top: spec.top,
    files: spec.files.map((f) => ({ name: f.name, text: f.text })),
    activeIdx: 0,
  })),
  currentIdx: 0,
  diagnostics: [],
  ok: false,
  checked: false,
  running: false,   // a simulation is stepping
  starting: false,  // a create request is in flight
  // False until the user edits something (or a dock restores a saved
  // session).  A pristine worker replays seed files; if a dock has a
  // session saved from a previous worker's lifetime, it can push it
  // back in via `restore` (see below) — the backstop for browsers
  // that reap the SharedWorker between page navigations.
  dirty: false,
};

// Console scrollback: capped ring of byte chunks, replayed in `sync`.
const LOG_CAP = 512 * 1024;
let logChunks = [];
let logBytes = 0;
const encoder = new TextEncoder();

function appendLog(bytes) {
  logChunks.push(bytes);
  logBytes += bytes.length;
  while (logBytes > LOG_CAP && logChunks.length > 1) {
    logBytes -= logChunks[0].length;
    logChunks.shift();
  }
}

function logLine(text) {
  const bytes = encoder.encode(`${text}\r\n`);
  appendLog(bytes);
  broadcast({ type: 'console', buf: bytes.buffer });
}

function clearLog() {
  logChunks = [];
  logBytes = 0;
}

// ── Ports (one per connected dock) ──────────────────────────

const ports = new Set();

function broadcast(msg) {
  for (const port of ports) {
    try {
      port.postMessage(msg);
    } catch {
      // Closed ports have no close event (MessagePort), so prune
      // them lazily: posting to a closed port throws.
      ports.delete(port);
    }
  }
}

function project() {
  return session.projects[session.currentIdx];
}

function syncSnapshot() {
  let total = 0;
  for (const c of logChunks) total += c.length;
  const log = new Uint8Array(total);
  let at = 0;
  for (const c of logChunks) { log.set(c, at); at += c.length; }
  return {
    type: 'sync',
    projects: session.projects,
    currentIdx: session.currentIdx,
    diagnostics: session.diagnostics,
    ok: session.ok,
    checked: session.checked,
    running: session.running,
    starting: session.starting,
    pristine: !session.dirty,
    log: log.buffer,
  };
}

// ── wasm compiler (lazy init; ~2.6 MB loads on first dock open) ─

let wasmReady = null;
function ensureWasm() {
  if (!wasmReady) {
    // No explicit module_or_path (unlike the SPA's main.js): the
    // glue's own `new URL('virdant_try_bg.wasm', import.meta.url)` is
    // rewritten by vite to the emitted asset — one wasm instance for
    // checker + simulator.
    wasmReady = init();
  }
  return wasmReady;
}

function buildPayload() {
  return JSON.stringify({
    files: project().files.map((f) => ({ name: f.name, text: f.text })),
  });
}

let checkTimer = 0;
function scheduleCheck(delay = 400) {
  clearTimeout(checkTimer);
  checkTimer = setTimeout(runCheck, delay);
}

async function runCheck() {
  await ensureWasm();
  let res;
  try {
    res = JSON.parse(wasmCheck(buildPayload()));
  } catch (e) {
    logLine(`\u26a0 wasm check failed: ${e}`);
    return null;
  }
  session.diagnostics = res.diagnostics;
  session.ok = res.ok;
  session.checked = true;
  broadcast({ type: 'diagnostics', diagnostics: res.diagnostics, ok: res.ok });
  return res;
}

// ── Simulator (adapted from the SPA's src/sim-worker.js; the step
//    loop, adaptive chunk sizing and real-time pacing are unchanged) ─

let vm = null;
let fpsNoCap = false;
let startWall = 0;
let lastProgressWall = 0;

const STEP_TARGET_MS = 5;
const FRAME_BUDGET_MS = 12;
const MIN_CHUNK = 1_000;
const MAX_CHUNK = 4_000_000;
let chunkCycles = 20_000n;
let cyclesPerMs = 0;

function adapt(nCycles, ms) {
  if (ms <= 0) ms = 0.05;
  const measured = Number(nCycles) / ms;
  cyclesPerMs = cyclesPerMs ? 0.5 * cyclesPerMs + 0.5 * measured : measured;
  let target = Math.round(cyclesPerMs * STEP_TARGET_MS);
  const current = Number(chunkCycles);
  if (target > current * 2) target = current * 2;
  chunkCycles = BigInt(Math.min(MAX_CHUNK, Math.max(MIN_CHUNK, target)));
}

function postOutput() {
  const bytes = vm.take_output();
  if (bytes.length) {
    appendLog(bytes);
    broadcast({ type: 'console', buf: bytes.buffer });
  }
}

function postProgress() {
  const now = performance.now();
  if (now - lastProgressWall < 50) return;
  lastProgressWall = now;
  broadcast({ type: 'progress', cycles: Number(vm.cycles()) });
}

function tick() {
  if (!session.running || !vm) return;
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
      if (n >= chunkCycles / 2n) adapt(n, performance.now() - t0);
      stepped = true;
      if (vm.is_finished()) break;
    }
    if (stepped) postOutput();
    postProgress();
    if (vm.is_finished()) {
      session.running = false;
      const exitCode = vm.exit_code();
      const cycles = Number(vm.cycles());
      postOutput();
      logLine(`\u2500\u2500 exited with code ${exitCode} after ${cycles} cycles \u2500\u2500`);
      broadcast({ type: 'finished', exitCode, cycles });
      vm = null;
      return;
    }
    // Yield so pending messages (stop/input) are processed; nap when
    // the paced sim is ahead of wall time instead of busy-spinning.
    setTimeout(tick, stepped ? 0 : 1);
  } catch (e) {
    session.running = false;
    vm = null;
    logLine(`\u26a0 Simulation failed: ${e}`);
    broadcast({ type: 'sim-error', message: String(e) });
  }
}

async function startRun(m) {
  if (session.running || session.starting) {
    logLine('\u26a0 A simulation is already running');
    return;
  }
  const res = await runCheck();
  if (!res || !res.ok) {
    broadcast({ type: 'runBlocked' });
    return;
  }
  const top = m.top || project().top;
  fpsNoCap = !!m.fpsNoCap;
  // A new run starts from a clean console: clear the log and tell
  // every dock to reset its terminal (message order preserves the
  // header below).
  clearLog();
  broadcast({ type: 'consoleReset' });
  try {
    vm = VirtVm.create(buildPayload(), top);
  } catch (e) {
    vm = null;
    logLine(`\u26a0 Could not start the simulation: ${e}`);
    broadcast({ type: 'sim-error', message: String(e) });
    return;
  }
  session.starting = false;
  session.running = true;
  startWall = performance.now();
  lastProgressWall = 0;
  const clockHz = Number(vm.clock_hz());
  logLine(`\u2500\u2500 running ${top} on virt::Virt (${clockHz / 1e6} MHz) \u2500\u2500`);
  broadcast({ type: 'created', clockHz });
  setTimeout(tick, 0);
}

// ── Message handling ────────────────────────────────────────

self.onconnect = (e) => {
  const port = e.ports[0];
  ports.add(port);
  port.onmessage = (ev) => {
    const m = ev.data;
    switch (m.type) {
      case 'hello':
        port.postMessage(syncSnapshot());
        // First dock of the session triggers the initial check.
        if (!session.checked) scheduleCheck(0);
        break;
      case 'selectProject': {
        const idx = Math.max(0, Math.min(m.index, session.projects.length - 1));
        if (idx !== session.currentIdx) {
          if (session.running || session.starting) {
            // Switching projects mid-run stops the sim first.
            stopRun();
          }
          session.currentIdx = idx;
        }
        break;
      }
      case 'edit': {
        const f = project().files.find((x) => x.name === m.file);
        if (f && f.text !== m.text) {
          f.text = m.text;
          session.dirty = true;
          // Live re-checks are deferred while a simulation runs (the
          // wasm check would compete with the simulator); one check
          // catches up when the run ends.
          if (!session.running && !session.starting) scheduleCheck();
        }
        break;
      }
      case 'restore': {
        // The dock found a session saved under a previous worker's
        // lifetime (this worker is pristine).  Adopt it, replay the
        // console scrollback and re-check.
        if (session.dirty || !Array.isArray(m.projects) || m.projects.length === 0) break;
        session.projects = m.projects.map((p) => ({
          name: String(p.name),
          top: String(p.top ?? 'top::Top'),
          files: (Array.isArray(p.files) ? p.files : []).map((f) => ({
            name: String(f.name),
            text: String(f.text ?? ''),
          })),
          activeIdx: Math.max(0, p.activeIdx | 0),
        })).filter((p) => p.files.length > 0);
        if (session.projects.length === 0) break;
        session.currentIdx = Math.max(0, Math.min(m.currentIdx | 0, session.projects.length - 1));
        session.dirty = true;
        if (m.log) {
          const bytes = new Uint8Array(m.log);
          logChunks = [bytes];
          logBytes = bytes.length;
        }
        session.checked = false;
        session.ok = false;
        session.diagnostics = [];
        broadcast(syncSnapshot());
        scheduleCheck(0);
        break;
      }
      case 'run':
        startRun(m).catch((e) => {
          session.starting = false;
          session.running = false;
          logLine(`\u26a0 Run failed: ${e}`);
          broadcast({ type: 'sim-error', message: String(e) });
        });
        break;
      case 'config':
        fpsNoCap = !!m.fpsNoCap;
        break;
      case 'input':
        if (vm) vm.send_input(new Uint8Array(m.bytes));
        break;
      case 'stop':
        stopRun();
        break;
      case 'resetProject': {
        // Reset the current project's files to the example sources.
        // Stops a running simulation first; then re-syncs every dock
        // and re-checks.
        if (session.running) stopRun();
        const spec = seedSpecs.find((s) => s.name === project().name);
        if (!spec) break;
        project().files = spec.files.map((f) => ({ name: f.name, text: f.text }));
        project().activeIdx = 0;
        // A reset also starts from a clean console (like Start does):
        // clear the log and tell every dock to reset its terminal.
        clearLog();
        broadcast({ type: 'consoleReset' });
        broadcast(syncSnapshot());
        scheduleCheck(0);
        break;
      }
    }
  };
  port.postMessage(syncSnapshot());
};

function stopRun() {
  if (!session.running || !vm) return;
  session.running = false;
  const cycles = Number(vm.cycles());
  postOutput();
  logLine(`\u2500\u2500 stopped after ${cycles} cycles \u2500\u2500`);
  broadcast({ type: 'stopped', cycles });
  vm = null;
}
