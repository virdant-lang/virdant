// Virdant web demo — the whole SPA.
//
// Client-side only: the `virdant-web` wasm module (built from
// `wasm/`, a port of the `virt::Virt` board plus the compiler's
// check path) does all the work; this file wires it to a
// CodeMirror 6 editor (left), an Errors/Console tabbed panel
// (right), and a Run/Stop/Settings bar (bottom).
//
// State (files, keymap, settings) persists in localStorage.

import init, { check as wasmCheck, VirtVm } from './wasm/virdant_web.js';
import defaultTop from './default_top.vir?raw';
import { virdantLanguage } from './vir-language.js';

import { EditorState, Compartment, StateEffect } from '@codemirror/state';
import {
  EditorView, keymap as cmKeymap, lineNumbers, highlightActiveLine,
  highlightActiveLineGutter, drawSelection, dropCursor,
} from '@codemirror/view';
import { defaultKeymap, history, historyKeymap, indentMore, indentLess } from '@codemirror/commands';
import { bracketMatching, indentOnInput, foldGutter, indentUnit } from '@codemirror/language';
import { linter } from '@codemirror/lint';
import { oneDark } from '@codemirror/theme-one-dark';
import { vim } from '@replit/codemirror-vim';

// ── DOM handles ──────────────────────────────────────────────

const $ = (id) => document.getElementById(id);
const filelistEl = $('filelist');
const editorHost = $('editorhost');
const errorPanel = $('errorpanel');
const consolePanelEl = $('consolepanel');
const errorPanelEl = $('errorpanel');
const consoleOut = $('consoleout');
const consoleIn = $('consolein');
const runBtn = $('run');
const stopBtn = $('stop');
const keymapSelect = $('keymap');
const fpsReadout = $('fpsreadout');
const errCountEl = $('errcount');

// ── Persistent state ─────────────────────────────────────────

const LS_FILES = 'virdant-web-demo:files';
const LS_ACTIVE = 'virdant-web-demo:active';
const LS_KEYMAP = 'virdant-web-demo:keymap';
const LS_SETTINGS = 'virdant-web-demo:settings';

const defaultSettings = { top: 'top::Top', fpsNoCap: false, showFps: false };

function defaultFiles() {
  return [{ name: 'top.vir', text: defaultTop }];
}

function loadState() {
  let files = defaultFiles();
  let active = 0;
  try {
    const stored = JSON.parse(localStorage.getItem(LS_FILES));
    if (Array.isArray(stored) && stored.length > 0 &&
        stored.every((f) => typeof f?.name === 'string' && typeof f?.text === 'string')) {
      files = stored;
    }
  } catch { /* fall back to defaults */ }
  const storedActive = parseInt(localStorage.getItem(LS_ACTIVE), 10);
  if (Number.isInteger(storedActive) && storedActive >= 0 && storedActive < files.length) {
    active = storedActive;
  }
  const keymap = localStorage.getItem(LS_KEYMAP) === 'vim' ? 'vim' : 'default';
  let settings = { ...defaultSettings };
  try {
    const stored = JSON.parse(localStorage.getItem(LS_SETTINGS));
    if (stored && typeof stored === 'object') settings = { ...settings, ...stored };
  } catch { /* fall back to defaults */ }
  return { files, activeIdx: active, keymap, settings };
}

function saveFiles() {
  localStorage.setItem(LS_FILES, JSON.stringify(
    files.map((f) => ({ name: f.name, text: f.text }))));
  localStorage.setItem(LS_ACTIVE, String(activeIdx));
}
function saveKeymap() { localStorage.setItem(LS_KEYMAP, keymap); }
function saveSettings() { localStorage.setItem(LS_SETTINGS, JSON.stringify(settings)); }

// ── Mutable app state ────────────────────────────────────────

let { files, activeIdx, keymap, settings } = loadState();
// Compartment generation: bumped when the keymap toggles; a stored
// per-file state older than the current generation is rebuilt (its
// doc and selection are kept; its undo history is not).
let compartmentGen = 0;
// Latest diagnostics, keyed by package name (file stem): DiagJson[].
let diagnosticsByFile = new Map();
let hasErrors = true;

// VM state (null when not running).
let vm = null;
let runStartWall = 0;
let rafId = 0;
let decoder = new TextDecoder('utf8');

const fileStem = (name) => name.replace(/\.vir$/, '');

// ── Editor setup ─────────────────────────────────────────────

const vimCompartment = new Compartment();

// Tab inserts 4 spaces; with a selection it indents the selected
// lines instead.  `indentUnit` keeps auto-indent (and the vim port,
// which reads this facet) using spaces too.
function tabToIndent({ state, dispatch }) {
  if (state.selection.ranges.some((r) => !r.empty)) {
    return indentMore({ state, dispatch });
  }
  dispatch(state.update(state.replaceSelection('    '), {
    scrollIntoView: true,
    userEvent: 'input.type',
  }));
  return true;
}

// Backspace deletes a whole indent step when the cursor sits after
// only spaces on its line (to the previous multiple of 4); otherwise
// it falls through to the default single-character delete.
function backspaceToIndent({ state, dispatch }) {
  const ranges = state.selection.ranges;
  if (ranges.length !== 1 || !ranges[0].empty) return false;
  const head = ranges[0].head;
  const line = state.doc.lineAt(head);
  if (head === line.from) return false;
  const before = line.text.slice(0, head - line.from);
  if (!/^[ ]+$/.test(before)) return false;
  const col = before.length; // 0-based column
  const deleteCount = Math.min(col % 4 === 0 ? 4 : col % 4, col);
  dispatch(state.update({ changes: { from: head - deleteCount, to: head } }));
  return true;
}

function keymapExtensions() {
  const tabKeys = cmKeymap.of([
    { key: 'Tab', run: tabToIndent },
    { key: 'Shift-Tab', run: indentLess },
    { key: 'Backspace', run: backspaceToIndent },
  ]);
  if (keymap === 'vim') return [vim(), tabKeys];
  return [tabKeys, cmKeymap.of([...defaultKeymap, ...historyKeymap])];
}

// Lint plumbing: the lint source reads from `diagnosticsByFile` (the
// wasm checker is the real linter); `refreshLint` forces a re-lint
// after each check without a document change.
const refreshLint = StateEffect.define();
const virdantLinter = linter((view) => {
  const stem = fileStem(files[activeIdx].name);
  const diags = diagnosticsByFile.get(stem) || [];
  return diags.map((d) => {
    const doc = view.state.doc;
    const lineAt = Math.max(1, Math.min(d.line, doc.lines));
    const lineEndAt = Math.max(1, Math.min(d.end_line, doc.lines));
    const line = doc.line(lineAt);
    const lineEnd = doc.line(lineEndAt);
    const from = Math.min(line.from + Math.max(0, d.col - 1), line.to);
    const to = Math.min(lineEnd.from + Math.max(0, d.end_col - 1), lineEnd.to);
    return {
      from,
      to: to > from ? to : Math.min(from + 1, doc.length),
      severity: d.level === 'error' ? 'error' : d.level === 'warning' ? 'warning' : 'info',
      message: d.message,
    };
  });
}, {
  needsRefresh: (update) => update.transactions.some((tr) =>
    tr.effects.some((e) => e.is(refreshLint))),
});

function baseExtensions() {
  return [
    lineNumbers(),
    highlightActiveLineGutter(),
    foldGutter(),
    highlightActiveLine(),
    drawSelection(),
    dropCursor(),
    history(),
    indentOnInput(),
    indentUnit.of('    '),
    bracketMatching(),
    oneDark,
    virdantLanguage,
    virdantLinter,
    vimCompartment.of(keymapExtensions()),
    EditorView.updateListener.of(onEditorUpdate),
  ];
}

function ensureState(f) {
  if (!f.state) {
    f.state = EditorState.create({ doc: f.text, extensions: baseExtensions() });
    f.gen = compartmentGen;
  }
  return f.state;
}

const view = new EditorView({
  parent: editorHost,
  state: ensureState(files[activeIdx]),
});

function onEditorUpdate(update) {
  if (update.docChanged) {
    files[activeIdx].text = update.state.doc.toString();
    saveFiles();
    scheduleCheck();
  }
}

function switchToFile(idx) {
  if (idx === activeIdx) return;
  files[activeIdx].state = view.state;
  activeIdx = idx;
  const f = files[activeIdx];
  if (f.gen !== compartmentGen) {
    // Rebuild with the current compartment values (keymap changed
    // since this file's state was stored).
    const old = f.state;
    f.state = EditorState.create({
      doc: old.doc,
      selection: old.selection,
      extensions: baseExtensions(),
    });
    f.gen = compartmentGen;
  }
  view.setState(ensureState(f));
  renderFileList();
  saveFiles();
  view.dispatch({ effects: refreshLint.of(null) });
  view.focus();
}

// ── File list UI ─────────────────────────────────────────────

function renderFileList() {
  filelistEl.replaceChildren();
  files.forEach((f, i) => {
    const li = document.createElement('li');
    if (i === activeIdx) li.classList.add('active');
    const name = document.createElement('span');
    name.className = 'filename';
    name.textContent = f.name;
    const remove = document.createElement('button');
    remove.className = 'removefile';
    remove.textContent = '✕';
    remove.title = `Remove ${f.name}`;
    remove.disabled = files.length <= 1;
    remove.addEventListener('click', (e) => {
      e.stopPropagation();
      removeFile(i);
    });
    li.append(name, remove);
    li.addEventListener('click', () => switchToFile(i));
    filelistEl.append(li);
  });
}

function addFile() {
  const name = prompt('New file name (e.g. utils.vir):');
  if (name === null) return;
  const trimmed = name.trim();
  if (!trimmed) return;
  const finalName = trimmed.endsWith('.vir') ? trimmed : `${trimmed}.vir`;
  if (files.some((f) => f.name === finalName)) {
    alert(`A file named ${finalName} already exists.`);
    return;
  }
  files.push({ name: finalName, text: '', state: null, gen: compartmentGen });
  switchToFile(files.length - 1);
  runCheckSoon();
}

function removeFile(idx) {
  if (files.length <= 1) return;
  const wasActive = idx === activeIdx;
  files.splice(idx, 1);
  if (idx < activeIdx) activeIdx -= 1;
  else if (wasActive) activeIdx = Math.min(idx, files.length - 1);
  const f = files[activeIdx];
  if (f.gen !== compartmentGen) {
    f.state = null;
  }
  view.setState(ensureState(f));
  renderFileList();
  saveFiles();
  runCheckSoon();
}

$('addfile').addEventListener('click', addFile);

// ── Checking (live) ──────────────────────────────────────────

let checkTimer = 0;
function scheduleCheck() {
  clearTimeout(checkTimer);
  checkTimer = setTimeout(runCheck, 400);
}
function runCheckSoon() {
  clearTimeout(checkTimer);
  checkTimer = setTimeout(runCheck, 0);
}

function buildPayload() {
  return JSON.stringify({
    files: files.map((f) => ({
      name: f.name,
      text: f === files[activeIdx] ? view.state.doc.toString() : (f.state ? f.state.doc.toString() : f.text),
    })),
  });
}

function runCheck() {
  let res;
  try {
    res = JSON.parse(wasmCheck(buildPayload()));
  } catch (e) {
    showFatal(`wasm check failed: ${e}`);
    return null;
  }
  diagnosticsByFile = new Map();
  for (const d of res.diagnostics) {
    if (!diagnosticsByFile.has(d.file)) diagnosticsByFile.set(d.file, []);
    diagnosticsByFile.get(d.file).push(d);
  }
  hasErrors = !res.ok;
  renderDiagnostics(res);
  view.dispatch({ effects: refreshLint.of(null) });
  updateRunButton();
  return res;
}

function renderDiagnostics(res) {
  const errors = res.diagnostics.filter((d) => d.level === 'error').length;
  const warnings = res.diagnostics.filter((d) => d.level === 'warning').length;
  errCountEl.textContent = errors > 0 ? `(${errors})` : warnings > 0 ? `(${warnings}⚠)` : '';
  errorPanel.replaceChildren();
  const summary = document.createElement('div');
  summary.id = 'diag-summary';
  if (res.diagnostics.length === 0) {
    summary.textContent = 'Check OK';
    errorPanel.append(summary);
    return;
  }
  summary.textContent = `${errors} error${errors === 1 ? '' : 's'}, ${warnings} warning${warnings === 1 ? '' : 's'}`;
  errorPanel.append(summary);
  for (const d of res.diagnostics) {
    const row = document.createElement('div');
    row.className = `diag ${d.level}`;
    const badge = document.createElement('span');
    badge.className = 'badge';
    badge.textContent = d.level.toUpperCase();
    const loc = document.createElement('span');
    loc.className = 'loc';
    loc.textContent = `${d.file}.vir:${d.line}:${d.col}`;
    const msg = document.createElement('span');
    msg.className = 'msg';
    msg.textContent = d.message;
    row.append(badge, loc, msg);
    row.addEventListener('click', () => jumpToDiagnostic(d));
    errorPanel.append(row);
  }
}

function jumpToDiagnostic(d) {
  const idx = files.findIndex((f) => fileStem(f.name) === d.file);
  if (idx >= 0 && idx !== activeIdx) switchToFile(idx);
  const doc = view.state.doc;
  if (doc.lines === 0) return;
  const line = doc.line(Math.max(1, Math.min(d.line, doc.lines)));
  const pos = Math.min(line.from + Math.max(0, d.col - 1), line.to);
  view.dispatch({
    selection: { anchor: pos },
    scrollIntoView: true,
    effects: EditorView.scrollIntoView(pos, { y: 'center' }),
  });
  view.focus();
}

function updateRunButton() {
  runBtn.disabled = hasErrors || vm !== null;
}

function showFatal(message) {
  console.error(message);
  appendConsoleLine(`⚠ ${message}`);
  switchTab('console');
}

// ── Tabs ─────────────────────────────────────────────────────

function switchTab(tab) {
  const isErrors = tab === 'errors';
  $('tab-errors').classList.toggle('active', isErrors);
  $('tab-console').classList.toggle('active', !isErrors);
  errorPanelEl.hidden = !isErrors;
  consolePanelEl.hidden = isErrors;
  if (!isErrors) consoleOut.scrollTop = consoleOut.scrollHeight;
}

$('tab-errors').addEventListener('click', () => switchTab('errors'));
$('tab-console').addEventListener('click', () => switchTab('console'));

// ── Console ──────────────────────────────────────────────────

function appendConsoleText(text) {
  consoleOut.append(document.createTextNode(text));
  consoleOut.scrollTop = consoleOut.scrollHeight;
}

function appendConsoleLine(text) {
  appendConsoleText(text + '\n');
}

$('consolein-form').addEventListener('submit', (e) => {
  e.preventDefault();
  if (!vm || vm.is_finished()) return;
  const text = consoleIn.value + '\n';
  consoleIn.value = '';
  vm.send_input(new TextEncoder().encode(text));
});

// ── Run / Stop ───────────────────────────────────────────────

function setRunning(running) {
  runBtn.hidden = running;
  stopBtn.hidden = !running;
  consoleIn.disabled = !running;
  updateRunButton();
}

function clearConsole() {
  consoleOut.replaceChildren();
  decoder = new TextDecoder('utf8');
}

function pumpOutput() {
  const bytes = vm.take_output();
  if (bytes.length) appendConsoleText(decoder.decode(bytes, { stream: true }));
}

function startRun() {
  if (vm) return;
  const res = runCheck();
  if (!res || !res.ok) {
    switchTab('errors');
    return;
  }
  clearConsole();
  switchTab('console');
  try {
    vm = VirtVm.create(buildPayload(), settings.top);
  } catch (e) {
    vm = null;
    appendConsoleLine(`⚠ Could not start the simulation: ${String(e)}`);
    setRunning(false);
    return;
  }
  appendConsoleLine(`── running ${settings.top} on virt::Virt (${Number(vm.clock_hz()) / 1e6} MHz) ──`);
  runStartWall = performance.now();
  setRunning(true);
  rafId = requestAnimationFrame(runFrame);
}

const FPS_NOCAP_CHUNK = 100_000n;
const FRAME_BUDGET_MS = 12;
// When pacing to wall time, forgive debt: a design heavier than
// real-time runs flat out instead of chasing an ever-growing deficit
// (matching `vir run`, which simply stops sleeping when it can't
// keep up).
const MAX_PACED_CHUNK = 250_000n;

function runFrame(now) {
  if (!vm) return;
  const clockHz = Number(vm.clock_hz());
  const periodNs = 1e9 / clockHz;
  if (settings.fpsNoCap) {
    // Spend most of the frame's budget simulating in chunks, so a
    // heavy design runs at full wasm speed while the UI stays alive.
    const deadline = performance.now() + FRAME_BUDGET_MS;
    do {
      vm.step(FPS_NOCAP_CHUNK);
      if (vm.is_finished()) break;
    } while (performance.now() < deadline);
  } else {
    const elapsedNs = (now - runStartWall) * 1e6;
    const target = BigInt(Math.floor(elapsedNs / periodNs));
    const deficit = target - vm.cycles();
    if (deficit > 0n) vm.step(deficit < MAX_PACED_CHUNK ? deficit : MAX_PACED_CHUNK);
  }
  pumpOutput();
  if (settings.showFps) updateFpsReadout(now);
  if (vm.is_finished()) {
    finishRun();
    return;
  }
  rafId = requestAnimationFrame(runFrame);
}

function updateFpsReadout(now) {
  const cycles = Number(vm.cycles());
  const secs = (now - runStartWall) / 1000;
  const mhz = secs > 0 ? cycles / secs / 1e6 : 0;
  fpsReadout.textContent = `${formatMhz(mhz)} MHz (avg)`;
}

function formatMhz(mhz) {
  return mhz.toFixed(4).replace(/\.?0+$/, '') || '0';
}

function finishRun() {
  pumpOutput();
  const cycles = Number(vm.cycles());
  appendConsoleLine(`\n── exited with code ${vm.exit_code()} after ${cycles} cycles ──`);
  if (settings.showFps) {
    const secs = (performance.now() - runStartWall) / 1000;
    if (cycles > 0 && secs > 0) {
      appendConsoleLine(`Average clock frequency: ${formatMhz(cycles / secs / 1e6)} MHz`);
    }
  }
  vm = null;
  fpsReadout.textContent = '';
  setRunning(false);
}

function stopRun() {
  if (!vm) return;
  cancelAnimationFrame(rafId);
  pumpOutput();
  appendConsoleLine(`\n── stopped after ${Number(vm.cycles())} cycles ──`);
  vm = null;
  fpsReadout.textContent = '';
  setRunning(false);
}

runBtn.addEventListener('click', startRun);
stopBtn.addEventListener('click', stopRun);

// ── Keymap ───────────────────────────────────────────────────

keymapSelect.value = keymap;
keymapSelect.addEventListener('change', () => {
  keymap = keymapSelect.value;
  saveKeymap();
  compartmentGen += 1;
  // The active view reconfigures in place (history preserved); other
  // files' stored states are rebuilt lazily on switch.
  view.dispatch({ effects: vimCompartment.reconfigure(keymapExtensions()) });
  for (const f of files) {
    if (f.state && f.gen !== compartmentGen) {
      f.state = EditorState.create({
        doc: f.state.doc,
        selection: f.state.selection,
        extensions: baseExtensions(),
      });
      f.gen = compartmentGen;
    }
  }
  view.focus();
});

// ── Settings ─────────────────────────────────────────────────

const settingsModal = $('settingsmodal');

function openSettings() {
  $('set-top').value = settings.top;
  $('set-fpsnocap').checked = settings.fpsNoCap;
  $('set-showfps').checked = settings.showFps;
  settingsModal.hidden = false;
}

function closeSettings() {
  settings.top = $('set-top').value.trim() || defaultSettings.top;
  settings.fpsNoCap = $('set-fpsnocap').checked;
  settings.showFps = $('set-showfps').checked;
  saveSettings();
  settingsModal.hidden = true;
}

$('settings').addEventListener('click', openSettings);
$('set-close').addEventListener('click', closeSettings);
settingsModal.addEventListener('click', (e) => {
  if (e.target === settingsModal) closeSettings();
});
window.addEventListener('keydown', (e) => {
  if (e.key === 'Escape' && !settingsModal.hidden) closeSettings();
});

$('set-reset').addEventListener('click', () => {
  if (!confirm('Reset all files to the default project?')) return;
  files = defaultFiles();
  activeIdx = 0;
  for (const f of files) { f.state = null; f.gen = compartmentGen; }
  view.setState(ensureState(files[0]));
  renderFileList();
  saveFiles();
  runCheckSoon();
  settingsModal.hidden = true;
});

// ── Splitter (drag to resize the IDE / console split) ──────

const LS_SPLIT = 'virdant-web-demo:split';
const splitter = $('splitter');
const appEl = $('app');

// Restore the last width (a percentage like "56").
const savedSplit = parseFloat(localStorage.getItem(LS_SPLIT));
if (Number.isFinite(savedSplit)) {
  appEl.style.setProperty('--left-width', `${savedSplit}%`);
}

function setSplitPct(pct) {
  const clamped = Math.min(75, Math.max(20, pct));
  appEl.style.setProperty('--left-width', `${clamped}%`);
  localStorage.setItem(LS_SPLIT, String(clamped));
}

splitter.addEventListener('mousedown', (e) => {
  e.preventDefault();
  splitter.classList.add('dragging');
  document.body.style.cursor = 'col-resize';
  document.body.style.userSelect = 'none';
  const onMove = (ev) => {
    const rect = appEl.getBoundingClientRect();
    // The splitter's center follows the mouse: the 4px gutter sits
    // between the panes, so left width = mouse x − half the gutter.
    setSplitPct(((ev.clientX - rect.left - 2) / rect.width) * 100);
  };
  const onUp = () => {
    splitter.classList.remove('dragging');
    document.body.style.cursor = '';
    document.body.style.userSelect = '';
    window.removeEventListener('mousemove', onMove);
    window.removeEventListener('mouseup', onUp);
  };
  window.addEventListener('mousemove', onMove);
  window.addEventListener('mouseup', onUp);
});

// ── Boot ─────────────────────────────────────────────────────

await init({
  // Pass the wasm bytes explicitly: vite serves the asset, but the
  // generated glue's URL resolution is environment-dependent.
  module_or_path: new URL('./wasm/virdant_web_bg.wasm', import.meta.url),
});

renderFileList();
runCheck();
