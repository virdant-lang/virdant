// Virdant web demo — the whole SPA.
//
// Client-side only: the `virdant-web` wasm module (built from
// `wasm/`, a port of the `virt::Virt` board plus the compiler's
// check path) does all the work; this file wires it to a
// CodeMirror 6 editor (left), an Errors/Console tabbed panel
// (right), and a Run/Stop/Settings bar (bottom).
//
// State (files, keymap, settings) persists in localStorage.

import init, { check as wasmCheck } from './wasm/virdant_web.js';

// SPI flash: 16 MiB device (see `wasm/src/virt.rs`); larger images are
// truncated, matching the native `--flash`.
const FLASH_SIZE = 16 * 1024 * 1024;
import defaultTop from './default_top.vir?raw';
import echoVir from './projects/echo.vir?raw';
// Every `.vir` file under `src/projects/riscv/` becomes a file of the
// seeded `riscv` project; add files there and they show up automatically.
const riscvSources = import.meta.glob('./projects/riscv/*.vir', {
  query: '?raw',
  import: 'default',
  eager: true,
});
const riscvVirFiles = Object.entries(riscvSources)
  .map(([path, text]) => ({ name: path.split('/').pop(), text }))
  .sort((a, b) => a.name.localeCompare(b.name));
import { virdantLanguage } from './vir-language.js';

import { Terminal } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';
import '@xterm/xterm/css/xterm.css';

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
const terminalEl = $('terminal');
const runBtn = $('run');
const stopBtn = $('stop');
const keymapSelect = $('keymap');
const fpsReadout = $('fpsreadout');
const errCountEl = $('errcount');

// ── Persistent state ─────────────────────────────────────────

const LS_PROJECTS = 'virdant-web-demo:projects';
const LS_CURRENT = 'virdant-web-demo:current';
const LS_FILES = 'virdant-web-demo:files';
const LS_ACTIVE = 'virdant-web-demo:active';
const LS_KEYMAP = 'virdant-web-demo:keymap';
const LS_SETTINGS = 'virdant-web-demo:settings';

const defaultSettings = { fpsNoCap: false, showFps: false };

// Seed sources per well-known project name.
function seedFiles(projectName) {
  if (projectName === 'echo') {
    return [{ name: 'echo.vir', text: echoVir }];
  }
  if (projectName === 'riscv') {
    return riscvVirFiles;
  }
  return [{ name: 'top.vir', text: defaultTop }];
}

function seedTop(projectName) {
  if (projectName === 'echo') return 'echo::Top';
  if (projectName === 'riscv') return 'top::Top';
  return 'top::Top';
}

function seedProjects() {
  return ['hello', 'echo', 'riscv'].map((name) => ({
    name,
    files: seedFiles(name),
    activeIdx: 0,
    top: seedTop(name),
  }));
}

function validProject(p) {
  return p && typeof p.name === 'string' && Array.isArray(p.files) &&
    p.files.length > 0 &&
    p.files.every((f) => typeof f?.name === 'string' && typeof f?.text === 'string') &&
    typeof p.top === 'string';
}

function loadProjects() {
  try {
    const stored = JSON.parse(localStorage.getItem(LS_PROJECTS));
    if (Array.isArray(stored) && stored.length > 0 && stored.every(validProject)) {
      // Drop any stale serialized state fields defensively.
      for (const p of stored) {
        p.files = p.files.map((f) => ({ name: f.name, text: f.text }));
        p.activeIdx = Math.min(Math.max(0, p.activeIdx || 0), p.files.length - 1);
      }
      // Add seed projects that didn't exist when this browser's state
      // was saved (e.g. `riscv`), leaving user-modified projects alone.
      for (const seed of seedProjects()) {
        if (!stored.some((p) => p.name === seed.name)) {
          stored.push(seed);
        }
      }
      return stored;
    }
  } catch { /* fall through to migration */ }

  // Migrate the pre-projects single-project layout (LS_FILES/LS_ACTIVE
  // plus the old settings' top field) into a project named "hello".
  const projects = seedProjects();
  try {
    const stored = JSON.parse(localStorage.getItem(LS_FILES));
    if (Array.isArray(stored) && stored.length > 0 &&
        stored.every((f) => typeof f?.name === 'string' && typeof f?.text === 'string')) {
      projects[0].files = stored;
      const storedActive = parseInt(localStorage.getItem(LS_ACTIVE), 10);
      if (Number.isInteger(storedActive) && storedActive >= 0 && storedActive < stored.length) {
        projects[0].activeIdx = storedActive;
      }
      try {
        const oldSettings = JSON.parse(localStorage.getItem(LS_SETTINGS));
        if (typeof oldSettings?.top === 'string' && oldSettings.top) {
          projects[0].top = oldSettings.top;
        }
      } catch { /* ignore */ }
    }
  } catch { /* ignore */ }
  return projects;
}

function saveProjects() {
  // Serialize only the durable fields: file records also carry
  // live EditorState objects, which must never reach storage.
  localStorage.setItem(LS_PROJECTS, JSON.stringify(
    projects.map((p) => ({
      name: p.name,
      top: p.top,
      activeIdx: p.activeIdx,
      files: p.files.map((f) => ({ name: f.name, text: f.text })),
    }))));
  localStorage.setItem(LS_CURRENT, String(currentIdx));
}
function saveKeymap() { localStorage.setItem(LS_KEYMAP, keymap); }
function saveSettings() { localStorage.setItem(LS_SETTINGS, JSON.stringify(settings)); }

// ── Mutable app state ────────────────────────────────────────

const projects = loadProjects();
let currentIdx = Math.min(
  Math.max(0, parseInt(localStorage.getItem(LS_CURRENT), 10) || 0),
  projects.length - 1);
// `files`/`activeIdx` mirror the current project (rebound on switch).
let files = projects[currentIdx].files;
let activeIdx = Math.min(projects[currentIdx].activeIdx, files.length - 1);
let keymap = localStorage.getItem(LS_KEYMAP) === 'vim' ? 'vim' : 'default';
let settings = { ...defaultSettings };
try {
  const stored = JSON.parse(localStorage.getItem(LS_SETTINGS));
  if (stored && typeof stored === 'object') settings = { ...settings, ...stored };
} catch { /* fall back to defaults */ }
// Compartment generation: bumped when the keymap toggles; a stored
// per-file state older than the current generation is rebuilt (its
// doc and selection are kept; its undo history is not).
let compartmentGen = 0;
// Latest diagnostics, keyed by package name (file stem): DiagJson[].
let diagnosticsByFile = new Map();
let hasErrors = true;

// The simulator runs in a Web Worker (`sim-worker.js`): the wasm VM,
// the step loop and the real-time pacing all live there, so the main
// thread stays free for editing/rendering while a design runs.  The
// worker reports back via postMessage (see the protocol documented
// there); UART input and Stop go the other way the same way.
const simWorker = new Worker(new URL('./sim-worker.js', import.meta.url), { type: 'module' });
let running = false;   // a simulation is stepping in the worker
let starting = false;  // a create request is in flight
let runStartWall = 0;
let runCycles = 0;
let decoder = new TextDecoder('utf8');

// Flash image to preload at the next Run: `{ name, bytes: Uint8Array }`
// or null.  Session-only (not persisted to localStorage — flash images
// can be up to 16 MiB).
let flashImage = null;

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
    saveProjects();
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
  projects[currentIdx].activeIdx = idx;
  renderFileList();
  saveProjects();
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
  saveProjects();
  runCheckSoon();
}

$('addfile').addEventListener('click', addFile);

// ── Projects ─────────────────────────────────────────────────

const projectSelect = $('project');

function renderProjectSelect() {
  projectSelect.replaceChildren();
  projects.forEach((p, i) => {
    const opt = document.createElement('option');
    opt.value = String(i);
    opt.textContent = p.name;
    if (i === currentIdx) opt.selected = true;
    projectSelect.append(opt);
  });
  $('removeproject').disabled = projects.length <= 1;
}

function switchProject(idx) {
  if (idx === currentIdx) return;
  stopRun();
  projects[currentIdx].activeIdx = activeIdx;
  currentIdx = idx;
  files = projects[currentIdx].files;
  activeIdx = Math.min(projects[currentIdx].activeIdx, files.length - 1);
  const f = files[activeIdx];
  if (f.gen !== compartmentGen) {
    f.state = null;
  }
  view.setState(ensureState(f));
  renderProjectSelect();
  renderFileList();
  saveProjects();
  view.dispatch({ effects: refreshLint.of(null) });
  runCheckSoon();
  view.focus();
}

function addProject() {
  const name = prompt('New project name:');
  if (name === null) return;
  const trimmed = name.trim();
  if (!trimmed) return;
  if (projects.some((p) => p.name === trimmed)) {
    alert(`A project named ${trimmed} already exists.`);
    return;
  }
  projects.push({ name: trimmed, files: seedFiles('hello'), activeIdx: 0, top: seedTop('hello') });
  switchProject(projects.length - 1);
}

function removeProject() {
  if (projects.length <= 1) return;
  const name = projects[currentIdx].name;
  if (!confirm(`Delete project "${name}" and all its files?`)) return;
  stopRun();
  projects.splice(currentIdx, 1);
  currentIdx = Math.min(currentIdx, projects.length - 1);
  files = projects[currentIdx].files;
  activeIdx = Math.min(projects[currentIdx].activeIdx, files.length - 1);
  const f = files[activeIdx];
  if (f.gen !== compartmentGen) {
    f.state = null;
  }
  view.setState(ensureState(f));
  renderProjectSelect();
  renderFileList();
  saveProjects();
  runCheckSoon();
  view.focus();
}

projectSelect.addEventListener('change', () => switchProject(parseInt(projectSelect.value, 10)));
$('addproject').addEventListener('click', addProject);
$('removeproject').addEventListener('click', removeProject);

// ── Checking (live) ──────────────────────────────────────────

let checkTimer = 0;
// While a simulation is running, live re-checks are deferred (the
// synchronous wasm check would compete with the simulator); once the
// run ends, one check catches up with all pending edits.
let checkPending = false;
function deferCheckWhileRunning() {
  if (!running && !starting) return false;
  checkPending = true;
  return true;
}
function scheduleCheck() {
  if (deferCheckWhileRunning()) return;
  clearTimeout(checkTimer);
  checkTimer = setTimeout(runCheck, 400);
}
function runCheckSoon() {
  if (deferCheckWhileRunning()) return;
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
  runBtn.disabled = hasErrors || running || starting;
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
  if (!isErrors) fitTerminal();
}

$('tab-errors').addEventListener('click', () => switchTab('errors'));
$('tab-console').addEventListener('click', () => switchTab('console'));

// ── Console (a real TTY, via xterm.js) ──────────────────────

// The console is a terminal emulator attached to the design's UART:
// the design's TX output is written straight in, and keystrokes go
// straight out — like a serial console.  Two input modes:
//
//   - raw (default): every keystroke byte is forwarded as-is, and
//     echo/editing is the design's job (the `echo` project does its
//     own echo, so characters appear via the round trip).
//   - local echo (Settings): the demo acts as the line discipline —
//     it echoes and edits locally (backspace, Ctrl+U, history), and
//     the design receives clean lines terminated by `\r` — for
//     designs that do not echo their input.

const term = new Terminal({
  convertEol: true,
  cursorBlink: true,
  fontSize: 12,
  fontFamily: '"JetBrains Mono", "Fira Mono", "SF Mono", Menlo, Consolas, monospace',
  scrollback: 5000,
  theme: {
    background: '#191b20',
    foreground: '#d7dae0',
    cursor: '#4fb583',
    cursorAccent: '#191b20',
    selectionBackground: '#3a4150',
  },
});
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);
term.open(terminalEl);

function fitTerminal() {
  if (!consolePanelEl.hidden && terminalEl.clientWidth > 0) {
    try { fitAddon.fit(); } catch { /* zero-size pane */ }
  }
}
new ResizeObserver(fitTerminal).observe(terminalEl);

const termEncoder = new TextEncoder();
let pendingLine = '';
let inputHistory = [];
let historyIdx = null;

function termSend(text) {
  if (running) simWorker.postMessage({ type: 'input', bytes: termEncoder.encode(text) });
}

// If the design's last output left the cursor mid-row, start input
// (or a re-render) on a fresh row so it cannot clobber the output.
function ensureFreshLine() {
  if (term.buffer.active.cursorX !== 0) term.write('\r\n');
}

function appendConsoleText(text) {
  term.write(text);
}

function appendConsoleLine(text) {
  term.write(`\x1b[2m${text}\x1b[0m\r\n`);
}

// ── Raw mode: the design owns echo and line handling ────────

function rawInput(data) {
  for (const ch of data) {
    if (ch === '\x03') {
      // Ctrl+C: like `vir run`'s SIGINT, stop the simulation.
      ensureFreshLine();
      term.write('^C');
      stopRun();
      continue;
    }
    if (ch === '\r') {
      // The design receives the authentic TTY Enter byte; give the
      // local view a fresh row too (if the design echoes the `\r`,
      // it is a harmless column reset on the now-empty row).
      termSend(ch);
      term.write('\r\n');
      continue;
    }
    if (ch === '\x7f') {
      // Backspace: forward the byte and erase locally.  (A design
      // that echoes the DEL byte back is no-op'd by the terminal.)
      termSend(ch);
      term.write('\b \b');
      continue;
    }
    termSend(ch);
  }
}

// ── Line-discipline mode: the demo owns echo and editing ────

function lineDiscipline(data) {
  // Arrow keys arrive as complete escape sequences.
  if (data.startsWith('\x1b')) {
    if (data === '\x1b[A') historyNav(-1);
    else if (data === '\x1b[B') historyNav(1);
    return;
  }
  for (const ch of data) {
    if (ch === '\r' || ch === '\n') {
      submitLine();
      continue;
    }
    if (ch === '\x7f') {
      eraseChar();
      continue;
    }
    if (ch === '\x15') {
      // Ctrl+U: clear the line.
      ensureFreshLine();
      term.write('\r\x1b[2K');
      pendingLine = '';
      continue;
    }
    if (ch === '\x03') {
      ensureFreshLine();
      term.write('^C\r\n');
      pendingLine = '';
      stopRun();
      continue;
    }
    if (ch < ' ') {
      continue; // other control bytes are not line-discipline material
    }
    if (pendingLine.length === 0) ensureFreshLine();
    pendingLine += ch;
    term.write(ch);
  }
}

function eraseChar() {
  if (!pendingLine) return;
  pendingLine = pendingLine.slice(0, -1);
  term.write('\b \b');
}

function submitLine() {
  term.write('\r\n');
  const line = pendingLine;
  pendingLine = '';
  historyIdx = null;
  if (line) {
    inputHistory.push(line);
    if (inputHistory.length > 100) inputHistory.shift();
  }
  termSend(line + '\r');
}

function historyNav(dir) {
  if (!inputHistory.length) return;
  if (historyIdx === null) {
    if (dir > 0) return;
    historyIdx = inputHistory.length - 1;
  } else {
    historyIdx += dir;
    if (historyIdx >= inputHistory.length) historyIdx = null;
    else if (historyIdx < 0) historyIdx = 0;
  }
  pendingLine = historyIdx === null ? '' : inputHistory[historyIdx];
  ensureFreshLine();
  term.write(`\r\x1b[2K${pendingLine}`);
}

term.onData((data) => {
  if (!running) return;
  if (settings.localEcho) lineDiscipline(data);
  else rawInput(data);
});

// ── Run / Stop ───────────────────────────────────────────────

function setRunning(nowRunning) {
  runBtn.hidden = nowRunning;
  stopBtn.hidden = !nowRunning;
  updateRunButton();
  if (nowRunning) {
    appendConsoleLine('(type to feed the design\'s UART rx — Ctrl+C stops)');
    term.write('\r\n');
    switchTab('console');
    fitTerminal();
    term.focus();
  } else {
    fpsReadout.textContent = '';
    // Catch up with edits made while the run deferred live re-checks.
    if (checkPending) {
      checkPending = false;
      runCheckSoon();
    }
  }
}

function clearConsole() {
  term.reset();
  decoder = new TextDecoder('utf8');
}

function startRun() {
  if (running || starting) return;
  const res = runCheck();
  if (!res || !res.ok) {
    switchTab('errors');
    return;
  }
  clearConsole();
  switchTab('console');
  starting = true;
  updateRunButton();
  const msg = {
    type: 'create',
    payload: buildPayload(),
    top: projects[currentIdx].top,
    fpsNoCap: settings.fpsNoCap,
  };
  if (flashImage) {
    // Copy before transferring: the loaded image must survive a stop.
    msg.flash = flashImage.bytes.slice().buffer;
    simWorker.postMessage(msg, [msg.flash]);
  } else {
    simWorker.postMessage(msg);
  }
}

// ── Worker messages (the sim lives in sim-worker.js) ─────────

simWorker.onmessage = (e) => {
  const m = e.data;
  switch (m.type) {
    case 'created':
      starting = false;
      runStartWall = performance.now();
      runCycles = 0;
      appendConsoleLine(`── running ${projects[currentIdx].top} on virt::Virt (${m.clockHz / 1e6} MHz) ──`);
      setRunning(true);
      break;
    case 'output':
      appendConsoleText(decoder.decode(new Uint8Array(m.buf), { stream: true }));
      break;
    case 'progress':
      runCycles = m.cycles;
      if (settings.showFps) updateFpsReadout();
      break;
    case 'finished':
      runCycles = m.cycles;
      appendConsoleLine(`\n── exited with code ${m.exitCode} after ${m.cycles} cycles ──`);
      if (settings.showFps) {
        const secs = (performance.now() - runStartWall) / 1000;
        if (m.cycles > 0 && secs > 0) {
          appendConsoleLine(`Average clock frequency: ${formatMhz(m.cycles / secs / 1e6)} MHz`);
        }
      }
      starting = false;
      setRunning(false);
      break;
    case 'stopped':
      runCycles = m.cycles;
      appendConsoleLine(`\n── stopped after ${m.cycles} cycles ──`);
      starting = false;
      setRunning(false);
      break;
    case 'create-error':
      starting = false;
      setRunning(false);
      appendConsoleLine(`⚠ Could not start the simulation: ${m.message}`);
      break;
    case 'error':
      starting = false;
      setRunning(false);
      appendConsoleLine(`⚠ Simulation failed: ${m.message}`);
      break;
  }
};

simWorker.onerror = (e) => {
  // Worker-level failure (e.g. the wasm failed to load).
  starting = false;
  setRunning(false);
  appendConsoleLine(`⚠ Simulator worker crashed: ${e.message || e}`);
};

function updateFpsReadout() {
  const secs = (performance.now() - runStartWall) / 1000;
  const mhz = secs > 0 ? runCycles / secs / 1e6 : 0;
  fpsReadout.textContent = `${formatMhz(mhz)} MHz (avg)`;
}

function formatMhz(mhz) {
  return mhz.toFixed(4).replace(/\.?0+$/, '') || '0';
}

function stopRun() {
  if (!running && !starting) return;
  simWorker.postMessage({ type: 'stop' });
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
  $('set-top').value = projects[currentIdx].top;
  $('set-fpsnocap').checked = settings.fpsNoCap;
  $('set-showfps').checked = settings.showFps;
  $('set-localecho').checked = settings.localEcho;
  ensureFlashPresets();
  renderFlash();
  settingsModal.hidden = false;
}

function closeSettings() {
  projects[currentIdx].top = $('set-top').value.trim() || seedTop(projects[currentIdx].name);
  settings.fpsNoCap = $('set-fpsnocap').checked;
  settings.showFps = $('set-showfps').checked;
  settings.localEcho = $('set-localecho').checked;
  if (running && simWorker) simWorker.postMessage({ type: 'config', fpsNoCap: settings.fpsNoCap });
  saveProjects();
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
  if (!confirm(`Reset all files of "${projects[currentIdx].name}" to the default?`)) return;
  files = seedFiles(projects[currentIdx].name);
  projects[currentIdx].files = files;
  activeIdx = 0;
  projects[currentIdx].activeIdx = 0;
  for (const f of files) { f.state = null; f.gen = compartmentGen; }
  view.setState(ensureState(files[0]));
  renderFileList();
  saveProjects();
  runCheckSoon();
  settingsModal.hidden = true;
});

// ── Settings: SPI flash image ────────────────────────────────
// The loaded image applies at the next Run (`create_with_flash`);
// it is session-only (not persisted — images can be 16 MiB).

const flashDrop = $('set-flash-drop');
const flashEmpty = $('set-flash-empty');
const flashInfo = $('set-flash-info');
const flashUnload = $('set-flash-unload');
const flashPreset = $('set-flash-preset');
const flashFileInput = $('set-flash-file');

function formatKiB(bytes) {
  const kib = bytes / 1024;
  return `${kib >= 100 ? Math.round(kib).toLocaleString('en-US') : kib.toFixed(1)} KiB`;
}

// Sync the dropzone/info display, the Unload button and the preset
// select with the current `flashImage` (null = nothing loaded).
function renderFlash() {
  flashDrop.classList.remove('dragover');
  flashInfo.classList.remove('error');
  if (flashImage) {
    flashEmpty.hidden = true;
    flashInfo.hidden = false;
    flashInfo.textContent = flashImage.truncated
      ? `${flashImage.name} — ${formatKiB(flashImage.bytes.length)} (truncated to fit the 16 MiB flash)`
      : `${flashImage.name} — ${formatKiB(flashImage.bytes.length)}`;
    flashUnload.hidden = false;
  } else {
    flashEmpty.hidden = false;
    flashInfo.hidden = true;
    flashUnload.hidden = true;
  }
  flashPreset.value = (flashImage && flashImage.preset) || '';
}

function setFlashImage(name, bytes, preset) {
  flashImage = { name, bytes, preset };
  if (bytes.length > FLASH_SIZE) {
    flashImage.truncated = true;
    flashImage.bytes = bytes.slice(0, FLASH_SIZE);
  }
  renderFlash();
}

async function loadFlashFile(file) {
  try {
    setFlashImage(file.name, new Uint8Array(await file.arrayBuffer()), null);
  } catch (e) {
    showFlashError(`Could not read ${file.name}: ${e}`);
  }
}

// Inline error in place of the info line; keeps the current image.
function showFlashError(message) {
  flashInfo.hidden = false;
  flashEmpty.hidden = true;
  flashInfo.classList.add('error');
  flashInfo.textContent = `⚠ ${message}`;
  flashPreset.value = (flashImage && flashImage.preset) || '';
}

// Click the dropzone to browse; a picked file replaces whatever was
// loaded before (preset or upload alike).
flashDrop.addEventListener('click', () => flashFileInput.click());
flashFileInput.addEventListener('change', () => {
  const file = flashFileInput.files?.[0];
  if (file) loadFlashFile(file);
  flashFileInput.value = '';
});
flashDrop.addEventListener('dragover', (e) => {
  e.preventDefault();
  flashDrop.classList.add('dragover');
});
flashDrop.addEventListener('dragleave', () => flashDrop.classList.remove('dragover'));
flashDrop.addEventListener('drop', (e) => {
  e.preventDefault();
  const file = e.dataTransfer?.files?.[0];
  if (file) loadFlashFile(file);
  else renderFlash(); // clear the highlight, keep the current image
});

flashUnload.addEventListener('click', () => {
  flashImage = null;
  renderFlash();
});

// Preset flash binaries, served next to the built demo from
// `web_demo/flash/` (copied into `dist/flash/` by the Makefile, along
// with the generated `manifest.json`).  Selecting one loads it as if
// the user had uploaded it, discarding the previous image.
let flashPresets = null; // cached list of filenames, null until fetched
async function ensureFlashPresets() {
  if (flashPresets) return;
  try {
    const res = await fetch('flash/manifest.json');
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const names = await res.json();
    flashPresets = Array.isArray(names) ? names.filter((n) => typeof n === 'string' && n.endsWith('.bin')) : [];
    for (const name of flashPresets) {
      const opt = document.createElement('option');
      opt.value = name;
      opt.textContent = name;
      flashPreset.appendChild(opt);
    }
    if (flashPresets.length === 0) {
      flashPreset.options[0].textContent = 'No preset flash images';
      flashPreset.disabled = true;
    }
    renderFlash();
  } catch (e) {
    flashPresets = [];
    flashPreset.options[0].textContent = 'Presets unavailable (not built in)';
    flashPreset.disabled = true;
  }
}

flashPreset.addEventListener('change', async () => {
  const name = flashPreset.value;
  if (!name) return;
  try {
    const res = await fetch(`flash/${encodeURIComponent(name)}`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    setFlashImage(name, new Uint8Array(await res.arrayBuffer()), name);
  } catch (e) {
    showFlashError(`Could not load preset ${name}: ${e}`);
  }
});

// Dropping a file anywhere else would let the browser navigate away;
// neutralize that (the dropzone above re-enables drops locally).
window.addEventListener('dragover', (e) => e.preventDefault());
window.addEventListener('drop', (e) => e.preventDefault());

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
    // The splitter's center follows the mouse: the 8px gutter sits
    // between the panes, so left width = mouse x − half the gutter.
    setSplitPct(((ev.clientX - rect.left - 4) / rect.width) * 100);
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

renderProjectSelect();
renderFileList();
runCheck();
