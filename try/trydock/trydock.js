// Virdant docs-site "try dock" — the UI half (design B: bottom dock).
//
// Docs pages load this module from /try/trydock/trydock.js (injected
// lazily by _static/trydock-boot.js the first time the pill is
// clicked).  It renders the dock chrome (toolbar, CodeMirror editor,
// Errors/Console panes) as native DOM on the docs page and talks to
// the SharedWorker (`trydock-worker.js`) for checking, running and
// session state.  All UI state is a mirror of the worker's session:
// the worker is the source of truth (see PLAN.md).
//
// The dock is always dark (the playground's IDE chrome) regardless of
// the docs theme, so the tool reads as a distinct instrument against
// the page — as in the design-B sketch.

import cssText from './ui.css?inline';
import xtermCss from '@xterm/xterm/css/xterm.css?inline';

import { EditorState, Compartment, StateEffect } from '@codemirror/state';
import {
  EditorView, keymap as cmKeymap, lineNumbers, highlightActiveLine,
  highlightActiveLineGutter, drawSelection, dropCursor,
} from '@codemirror/view';
import { defaultKeymap, history, historyKeymap, indentMore, indentLess } from '@codemirror/commands';
import { vim } from '@replit/codemirror-vim';
import {
  bracketMatching, indentOnInput, foldGutter, indentUnit,
  HighlightStyle, syntaxHighlighting,
} from '@codemirror/language';
import { tags as t } from '@lezer/highlight';
import { linter } from '@codemirror/lint';
import { Terminal } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';

import { virdantLanguage } from '../src/vir-language.js';

// ── Styles: one <style> with the dock chrome + xterm ────────

const styleEl = document.createElement('style');
styleEl.textContent = `${cssText}\n${xtermCss}`;
document.head.appendChild(styleEl);

// ── DOM skeleton ────────────────────────────────────────────

const root = document.createElement('div');
root.className = 'trydock-root';
root.innerHTML = `
  <div id="trydock" hidden>
    <div class="td-resize" id="td-resize" title="Drag to resize"></div>
    <div class="td-toolbar">
      <button class="td-btn primary" id="td-run" title="Run (vir run)">
        <span class="td-runlabel" data-state="run">Run</span>
        <span class="td-runlabel" data-state="checking">Checking</span>
        <span class="td-runlabel" data-state="error">Error</span>
      </button>
      <button class="td-btn danger" id="td-stop" title="Stop the simulation" hidden>Stop</button>
      <span class="td-led" id="td-led"></span>
      <span class="td-status" id="td-status">idle</span>
      <span class="td-spacer"></span>
      <span class="td-fps" id="td-fps"></span>
      <button class="td-btn" id="td-reset" title="Reset to the example: stops the simulation (if running) and reloads all files with the original example sources">Reset</button>
      <button class="td-iconbtn" id="td-settings" title="Settings">&#9881;</button>
      <a class="td-iconbtn" id="td-popout" href="/try/" target="_blank"
         title="Open the full playground in a new tab">
        <svg class="td-svg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"
             width="17" height="17" aria-hidden="true">
          <path d="M15 3h6v6"/>
          <path d="M10 14 21 3"/>
          <path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"/>
        </svg>
      </a>
      <button class="td-iconbtn" id="td-close" title="Close the dock">&#10005;</button>
    </div>
    <div class="td-body">
      <div class="td-editor-col">
        <div class="td-filetabs" id="td-filetabs"></div>
        <div class="td-editorhost" id="td-editorhost"></div>
      </div>
      <div class="td-vsplit" id="td-vsplit" title="Drag to resize"></div>
      <div class="td-right">
        <div class="td-tabs">
          <button class="td-tab active" id="td-tab-console">Console</button>
          <button class="td-tab" id="td-tab-diagnostics">Diagnostics <span id="td-errcount"></span></button>
        </div>
        <div class="td-panel" id="trydock-consolepanel">
          <div id="trydock-terminal"></div>
        </div>
        <div class="td-panel" id="td-diagpanel" hidden></div>
      </div>
    </div>
  </div>

  <div id="td-confirmmodal" hidden>
    <div class="td-confirmbox">
      <h2>Reset to the example?</h2>
      <p id="td-confirm-msg"></p>
      <div class="td-confirmbuttons">
        <button class="td-btn" id="td-confirm-cancel">Cancel</button>
        <button class="td-btn danger" id="td-confirm-ok">Reset</button>
      </div>
    </div>
  </div>

  <div id="td-settingsmodal" hidden>
    <div class="td-settingsbox">
      <h2>Settings</h2>
      <label>
        <span>Top module</span>
        <input id="td-set-top" type="text" spellcheck="false">
      </label>
      <label>
        <span>Keybindings</span>
        <select id="td-set-keymap">
          <option value="default">Default</option>
          <option value="vim">Vim</option>
        </select>
      </label>
      <label class="td-check">
        <input id="td-set-fpsnocap" type="checkbox">
        <span>Disable real-time pacing (<code>--fps-no-cap</code>)</span>
      </label>
      <label class="td-check">
        <input id="td-set-showfps" type="checkbox">
        <span>Display average clock frequency (fps)</span>
      </label>
      <label class="td-check">
        <input id="td-set-localecho" type="checkbox">
        <span>Local echo (for designs that don't echo input)</span>
      </label>
      <div class="td-settingsbuttons">
        <button class="td-btn" id="td-set-close">Close</button>
      </div>
    </div>
  </div>
`;
document.body.appendChild(root);

// Keep dock keystrokes away from the theme's global key handlers:
// Sphinx's doctools.js hijacks '/' (search focus) and arrow keys
// (page navigation) for any non-INPUT target — including a focused
// CodeMirror editor (contenteditable div).  Bubble-phase on the dock
// root: the editor and terminal see every key first, and the theme's
// document-level handlers never do.  (The dock's own Escape handler
// is window-capture and unaffected.)
root.addEventListener('keydown', (e) => e.stopPropagation());

const $ = (id) => root.querySelector(`#${id}`);
const dock = $('trydock');
const pill = document.getElementById('trydock-pill')
  ?? (() => {
    // The boot script normally creates the pill; fall back to a
    // minimal one if this bundle was loaded directly.
    const p = document.createElement('button');
    p.id = 'trydock-pill';
    p.textContent = 'Try Virdant';
    document.body.appendChild(p);
    return p;
  })();
const fileTabs = $('td-filetabs');
const editorHost = $('td-editorhost');
const errorPanel = $('td-diagpanel');
const consolePanel = $('trydock-consolepanel');
const terminalEl = $('trydock-terminal');
const runBtn = $('td-run');
const stopBtn = $('td-stop');
const settingsBtn = $('td-settings');
const settingsModal = $('td-settingsmodal');
const confirmModal = $('td-confirmmodal');
const led = $('td-led');
const statusEl = $('td-status');
const fpsEl = $('td-fps');
const errCountEl = $('td-errcount');

// ── Worker (the session's source of truth) ──────────────────

const port = new SharedWorker(
  new URL('trydock-worker.js', import.meta.url),
  { type: 'module' },
).port;
port.start();

// ── Keymap (shared localStorage key with the SPA) ────────────

const KEYMAP_KEY = 'virdant-try:keymap';
let keymap = localStorage.getItem(KEYMAP_KEY) === 'vim' ? 'vim' : 'default';
const vimCompartment = new Compartment();

function keymapExtensions() {
  const tabKeys = cmKeymap.of([
    { key: 'Tab', run: tabToIndent },
    { key: 'Shift-Tab', run: indentLess },
    { key: 'Backspace', run: backspaceToIndent },
  ]);
  if (keymap === 'vim') return [vim(), tabKeys];
  return [tabKeys, cmKeymap.of([...defaultKeymap, ...historyKeymap])];
}

// ── Editor ──────────────────────────────────────────────────

// Syntax colors from the virdant.nvim palette (same as the SPA).
const virdantHighlight = HighlightStyle.define([
  { tag: t.keyword, color: '#556B2F' },
  { tag: t.comment, color: '#8f95a1', fontStyle: 'italic' },
  { tag: t.operator, color: '#8B6E47' },
  { tag: t.punctuation, color: '#8B6E47' },
  { tag: t.bracket, color: '#8B6E47' },
  { tag: t.number, color: '#C65D3B' },
  { tag: t.bool, color: '#C65D3B' },
  { tag: t.string, color: '#AE604F' },
  { tag: t.variableName, color: '#AE604F' },
  { tag: t.typeName, color: '#B83A34' },
  { tag: t.self, color: '#AE604F', fontWeight: 'bold' },
]);

const virdantTheme = EditorView.theme({
  '&': { color: '#d7dae0', backgroundColor: '#191b20' },
  '.cm-content': { caretColor: '#4fb583' },
  '&.cm-focused .cm-cursor': { borderLeftColor: '#4fb583' },
  '&.cm-focused .cm-selectionBackground, ::selection': { backgroundColor: '#3a4150' },
  '.cm-selectionBackground, ::selection': { backgroundColor: '#31353f' },
  '.cm-gutters': { backgroundColor: '#191b20', color: '#8f95a1', border: 'none' },
  '.cm-activeLine': { backgroundColor: '#21242b' },
  '.cm-activeLineGutter': { backgroundColor: '#21242b' },
}, { dark: true });

// Tab inserts 4 spaces; with a selection it indents the selected
// lines instead (same as the SPA).
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

function backspaceToIndent({ state, dispatch }) {
  const ranges = state.selection.ranges;
  if (ranges.length !== 1 || !ranges[0].empty) return false;
  const head = ranges[0].head;
  const line = state.doc.lineAt(head);
  if (head === line.from) return false;
  const before = line.text.slice(0, head - line.from);
  if (!/^[ ]+$/.test(before)) return false;
  const col = before.length;
  const deleteCount = Math.min(col % 4 === 0 ? 4 : col % 4, col);
  dispatch(state.update({ changes: { from: head - deleteCount, to: head } }));
  return true;
}

const refreshLint = StateEffect.define();

const diagnosticsByFile = new Map(); // file stem -> DiagJson[]
let hasErrors = true;

const fileStem = (name) => name.replace(/\.vir$/, '');

const virdantLinter = linter((view) => {
  const f = activeFile();
  if (!f) return [];
  const diags = diagnosticsByFile.get(fileStem(f.name)) || [];
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
    virdantTheme,
    syntaxHighlighting(virdantHighlight),
    virdantLanguage,
    virdantLinter,
    vimCompartment.of(keymapExtensions()),
    EditorView.updateListener.of(onEditorUpdate),
  ];
}

function applyKeymap() {
  // The active view reconfigures in place (history preserved); other
  // files' stored states are rebuilt (same rule as the SPA).
  view.dispatch({ effects: vimCompartment.reconfigure(keymapExtensions()) });
  for (const p of projects) {
    for (const f of p.files) {
      if (f.state) {
        f.state = EditorState.create({
          doc: f.state.doc,
          selection: f.state.selection,
          extensions: baseExtensions(),
        });
      }
    }
  }
}

function ensureState(f) {
  if (!f.state) {
    f.state = EditorState.create({ doc: f.text, extensions: baseExtensions() });
  }
  return f.state;
}

const view = new EditorView({ parent: editorHost });

function onEditorUpdate(update) {
  if (!update.docChanged) return;
  // A user-driven change (typing, paste, undo, …) means a re-check is
  // coming: show "checking..." immediately, before the debounced
  // dispatch even happens.  Programmatic changes (applySync's
  // setState) carry no userEvent and don't count.
  if (update.transactions.some((tr) =>
    tr.isUserEvent('input') || tr.isUserEvent('delete') ||
    tr.isUserEvent('undo') || tr.isUserEvent('redo'))) {
    checking = true;
    updateStatus();
    updateRunButton();
  }
  const f = activeFile();
  if (!f) return;
  f.text = update.state.doc.toString();
  scheduleEdit(f.name, f.text);
  saveSessionSoon();
}

// Debounced edit forwarding: the worker stores the text and re-checks.
// lastSentText tracks what was last dispatched per file name: when the
// debounced dispatch turns out to be a no-op (the user typed and
// undid within one debounce window), the worker dedups the edit and
// no check will run — so the "checking..." state is revoked instead
// of hanging until a diagnostics that never comes.
let editTimer = 0;
const lastSentText = new Map();
function scheduleEdit(file, text) {
  clearTimeout(editTimer);
  editTimer = setTimeout(() => {
    if (lastSentText.get(file) !== text) {
      lastSentText.set(file, text);
      port.postMessage({ type: 'edit', file, text });
    } else {
      checking = false;
      updateStatus();
      updateRunButton();
    }
  }, 150);
}

// ── Session mirror ──────────────────────────────────────────

let projects = [];      // [{name, top, files: [{name, text, state?}], activeIdx}]
let currentIdx = 0;
let running = false;
let starting = false;
let checked = false;
let checking = false;   // a re-check is in flight (yellow "checking...")

const project = () => projects[currentIdx];
const files = () => project()?.files ?? [];
const activeFile = () => files()[project()?.activeIdx ?? 0];

// ── localStorage backstop ───────────────────────────────────────
//
// Browsers may reap the SharedWorker between page navigations (a
// fresh worker then replays the seed files).  The dock therefore
// saves the session to localStorage and pushes it back into a
// pristine worker via `restore` — see PLAN.md.  (Workers have no
// localStorage of their own; this must live page-side.)

const LS_KEY = 'virdant-trydock:session';
const LOCAL_LOG_CAP = 256 * 1024;
let localLogChunks = [];  // mirror of the console scrollback for saving

function bytesToB64(bytes) {
  let s = '';
  for (let i = 0; i < bytes.length; i += 0x8000) {
    s += String.fromCharCode.apply(null, bytes.subarray(i, i + 0x8000));
  }
  return btoa(s);
}

function concatLocalLog() {
  let total = 0;
  for (const c of localLogChunks) total += c.length;
  const out = new Uint8Array(total);
  let at = 0;
  for (const c of localLogChunks) { out.set(c, at); at += c.length; }
  if (total > LOCAL_LOG_CAP) return out.subarray(total - LOCAL_LOG_CAP);
  return out;
}

function saveSession() {
  if (!projects.length || !opened) return;
  try {
    localStorage.setItem(LS_KEY, JSON.stringify({
      projects: projects.map((p) => ({
        name: p.name,
        top: p.top,
        activeIdx: p.activeIdx,
        files: p.files.map((f) => ({ name: f.name, text: f.text })),
      })),
      currentIdx,
      logB64: bytesToB64(concatLocalLog()),
    }));
  } catch { /* storage unavailable or quota */ }
}

function loadSession() {
  try {
    const raw = localStorage.getItem(LS_KEY);
    if (!raw) return null;
    const saved = JSON.parse(raw);
    if (!Array.isArray(saved.projects) || saved.projects.length === 0) return null;
    return saved;
  } catch { return null; }
}

function b64ToBytes(b64) {
  const s = atob(b64);
  const bytes = new Uint8Array(s.length);
  for (let i = 0; i < s.length; i++) bytes[i] = s.charCodeAt(i);
  return bytes;
}

function saveSessionSoon() {
  clearTimeout(saveSession._t);
  saveSession._t = setTimeout(saveSession, 500);
}

window.addEventListener('pagehide', saveSession);
document.addEventListener('visibilitychange', () => {
  if (document.visibilityState === 'hidden') saveSession();
});

function applySync(s) {
  // Fresh page: rebuild everything from the worker's snapshot.
  // Editor undo history is not preserved across navigations (states
  // are rebuilt from the worker's texts).
  projects = s.projects.map((p) => ({
    ...p,
    files: p.files.map((f) => ({ name: f.name, text: f.text, state: null })),
  }));
  currentIdx = s.currentIdx;
  // The worker's texts are the baseline now: equal texts dispatched
  // later are no-ops (see scheduleEdit).
  lastSentText.clear();
  for (const p of s.projects) {
    for (const f of p.files) lastSentText.set(f.name, f.text);
  }
  running = s.running;
  starting = s.starting;
  checked = s.checked;

  diagnosticsByFile.clear();
  for (const d of s.diagnostics) {
    if (!diagnosticsByFile.has(d.file)) diagnosticsByFile.set(d.file, []);
    diagnosticsByFile.get(d.file).push(d);
  }
  lastDiagnostics = s.diagnostics;
  lastOk = s.ok;
  hasErrors = !s.ok;

  localLogChunks = [new Uint8Array(s.log)];
  term.reset();
  term.write(new Uint8Array(s.log));

  renderFileTabs();
  view.setState(ensureState(activeFile()));
  renderDiagnostics({ diagnostics: s.diagnostics, ok: s.ok });
  updateRunButton();
  updateStatus();
  view.dispatch({ effects: refreshLint.of(null) });

  // Console is the default surface; Diagnostics only when problems
  // show up (see the `diagnostics` handler).
  switchTab('console');

  // Pristine worker + a session saved under a previous worker's
  // lifetime: push the saved session back in (the worker replies
  // with a fresh sync + diagnostics).
  if (s.pristine) {
    const saved = loadSession();
    if (saved) {
      let log = null;
      try {
        if (saved.logB64) log = b64ToBytes(saved.logB64).buffer;
      } catch { /* malformed; ignore the log */ }
      port.postMessage({
        type: 'restore',
        projects: saved.projects,
        currentIdx: saved.currentIdx | 0,
        log,
      });
      return; // pendingLoad (if any) waits for the restore's sync
    }
  }
  saveSession();

  // A virtry.load() request parked before the mirror existed: now the
  // project list is known, switch and reset.
  if (pendingLoad != null) {
    const name = pendingLoad;
    pendingLoad = null;
    performLoad(name);
  }
}

// ── File tabs ───────────────────────────────────────────────

function renderFileTabs() {
  fileTabs.replaceChildren();
  files().forEach((f, i) => {
    const b = document.createElement('button');
    b.className = 'td-filetab' + (i === project().activeIdx ? ' active' : '');
    b.textContent = f.name;
    b.addEventListener('click', () => switchToFile(i));
    fileTabs.append(b);
  });
}

function switchToFile(idx) {
  if (idx === project().activeIdx) return;
  files()[project().activeIdx].state = view.state;
  project().activeIdx = idx;
  view.setState(ensureState(files()[idx]));
  renderFileTabs();
  view.dispatch({ effects: refreshLint.of(null) });
  view.focus();
}

// ── File tabs ─────────────────────────────────────────────

// ── Diagnostics panel ───────────────────────────────────────

function renderDiagnostics(res) {
  const errors = res.diagnostics.filter((d) => d.level === 'error').length;
  const warnings = res.diagnostics.filter((d) => d.level === 'warning').length;
  errCountEl.textContent = errors > 0 ? `(${errors})` : warnings > 0 ? `(${warnings}\u26a0)` : '';

  errorPanel.replaceChildren();
  const summary = document.createElement('div');
  summary.className = 'td-diag-summary' + (res.ok ? ' ok' : '');
  summary.textContent = res.diagnostics.length === 0
    ? 'Check OK'
    : `${errors} error${errors === 1 ? '' : 's'}, ${warnings} warning${warnings === 1 ? '' : 's'}`;
  errorPanel.append(summary);

  for (const d of res.diagnostics) {
    const row = document.createElement('div');
    row.className = `td-diag ${d.level}`;
    const badge = document.createElement('span');
    badge.className = 'td-badge';
    badge.textContent = d.level.toUpperCase();
    const loc = document.createElement('span');
    loc.className = 'td-loc';
    loc.textContent = `${d.file}.vir:${d.line}:${d.col}`;
    const msg = document.createElement('span');
    msg.className = 'td-msg';
    msg.textContent = d.message;
    row.append(badge, loc, msg);
    row.addEventListener('click', () => jumpToDiagnostic(d));
    errorPanel.append(row);
  }
}

function jumpToDiagnostic(d) {
  const idx = files().findIndex((f) => fileStem(f.name) === d.file);
  if (idx >= 0 && idx !== project().activeIdx) switchToFile(idx);
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

// ── Tabs (Console first; Diagnostics on demand) ──────────────

function switchTab(tab) {
  const isDiag = tab === 'diagnostics';
  $('td-tab-console').classList.toggle('active', !isDiag);
  $('td-tab-diagnostics').classList.toggle('active', isDiag);
  consolePanel.hidden = isDiag;
  errorPanel.hidden = !isDiag;
  if (!isDiag) fitTerminal();
}

// Whether we switched to Diagnostics automatically (an error showed
// up); only auto-restore to Console on a clean result if so — a
// manually chosen Diagnostics tab stays put.
let diagAuto = false;

$('td-tab-console').addEventListener('click', () => { diagAuto = false; switchTab('console'); });
$('td-tab-diagnostics').addEventListener('click', () => { diagAuto = false; switchTab('diagnostics'); });

// ── Console (a real TTY on the design's UART) ───────────────

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
  if (!consolePanel.hidden && terminalEl.clientWidth > 0) {
    try { fitAddon.fit(); } catch { /* zero-size pane */ }
  }
}
new ResizeObserver(fitTerminal).observe(terminalEl);

const termEncoder = new TextEncoder();

// Raw input mode (the SPA's default): the design owns echo and line
// handling; Ctrl+C stops like `vir run`'s SIGINT.
// Raw input mode (the SPA's default): the design owns echo and line
// handling; Ctrl+C stops like `vir run`'s SIGINT.
function rawInput(data) {
  for (const ch of data) {
    if (ch === '\x03') {
      term.write('^C');
      stopRun();
      continue;
    }
    if (ch === '\r') {
      port.postMessage({ type: 'input', bytes: termEncoder.encode(ch) });
      term.write('\r\n');
      continue;
    }
    if (ch === '\x7f') {
      port.postMessage({ type: 'input', bytes: termEncoder.encode(ch) });
      term.write('\b \b');
      continue;
    }
    port.postMessage({ type: 'input', bytes: termEncoder.encode(ch) });
  }
}

// Line-discipline mode (Settings \u2192 local echo): the dock acts as
// the line discipline \u2014 it echoes and edits locally (backspace,
// Ctrl+U, history) and the design receives clean lines terminated by
// `\r`.  Ported from the SPA.
let pendingLine = '';
const inputHistory = [];
let historyIdx = null;

function ensureFreshLine() {
  if (term.buffer.active.cursorX !== 0) term.write('\r\n');
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
  port.postMessage({ type: 'input', bytes: termEncoder.encode(line + '\r') });
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

function lineDiscipline(data) {
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
    if (ch < ' ') continue;
    if (pendingLine.length === 0) ensureFreshLine();
    pendingLine += ch;
    term.write(ch);
  }
}

term.onData((data) => {
  if (!running) return;
  if (settings.localEcho) lineDiscipline(data);
  else rawInput(data);
});

// ── Run / Stop ──────────────────────────────────────────────

function updateRunButton() {
  runBtn.disabled = (hasErrors && !checking) || running || starting;
  runBtn.hidden = running;
  stopBtn.hidden = !running;
  // Tri-state label: green Run / yellow Checking / red Error — with
  // the SAME priority as the status label: an in-flight check wins
  // over stale error state, so the button and the label always read
  // the same state (editing a file that already has errors must show
  // yellow "checking...", not the stale red "Error").  All three
  // spans are stacked in one grid cell, so the button keeps the
  // widest label's width and never reflows the toolbar.
  const state = checking ? 'checking' : (hasErrors ? 'error' : 'run');
  for (const el of runBtn.querySelectorAll('.td-runlabel')) {
    el.classList.toggle('active', el.dataset.state === state);
  }
  runBtn.classList.toggle('checking', checking);
  runBtn.classList.toggle('error', hasErrors && !checking);
}

function updateStatus() {
  led.className = 'td-led';
  statusEl.className = 'td-status';
  if (starting) {
    led.classList.add('busy');
    statusEl.textContent = 'starting\u2026';
  } else if (running) {
    // Green, flashing very quickly (signals a live simulation).
    led.classList.add('run');
    statusEl.textContent = 'running';
  } else if (checking) {
    // A re-check is in flight (e.g. the user just edited): yellow,
    // flashing at a steady rate, until the result arrives.
    led.classList.add('checking');
    statusEl.classList.add('checking');
    statusEl.textContent = 'checking...';
  } else if (!checked) {
    statusEl.textContent = 'idle';
  } else {
    const errors = (sessionDiagnostics() ?? []).filter((d) => d.level === 'error').length;
    const warnings = (sessionDiagnostics() ?? []).filter((d) => d.level === 'warning').length;
    if (errors > 0) {
      led.classList.add('error');
      statusEl.classList.add('error');
      // Cross between the red circle and the count; all three red.
      statusEl.innerHTML =
        `<span class="td-cross">\u2715</span> ${errors} error${errors === 1 ? '' : 's'}`;
    } else if (warnings > 0) {
      led.classList.add('warn');
      statusEl.textContent = `${warnings} warning${warnings === 1 ? '' : 's'}`;
    } else {
      led.classList.add('ok');
      statusEl.classList.add('ok');
      statusEl.textContent = '\u2713 compiles';
    }
  }
}

// Latest diagnostics straight from the worker messages (kept outside
// the per-file map, which is keyed by stem for the linter).
let lastDiagnostics = [];
let lastOk = false;
function sessionDiagnostics() {
  return lastDiagnostics;
}

function startRun() {
  if (running || starting) return;
  port.postMessage({
    type: 'run',
    top: project()?.top,
    fpsNoCap: settings.fpsNoCap,
  });
}

function stopRun() {
  port.postMessage({ type: 'stop' });
}

// ── Settings ────────────────────────────────────────────────

const settings = { fpsNoCap: false, showFps: false, localEcho: false };

function openSettings() {
  $('td-set-top').value = project()?.top ?? '';
  $('td-set-keymap').value = keymap;
  $('td-set-fpsnocap').checked = settings.fpsNoCap;
  $('td-set-showfps').checked = settings.showFps;
  $('td-set-localecho').checked = settings.localEcho;
  settingsModal.hidden = false;
}

function closeSettings() {
  const top = $('td-set-top').value.trim();
  if (top && project()) {
    project().top = top;
    saveSessionSoon();
  }
  const newKeymap = $('td-set-keymap').value;
  if (newKeymap !== keymap) {
    keymap = newKeymap;
    try { localStorage.setItem(KEYMAP_KEY, keymap); } catch { /* storage unavailable */ }
    applyKeymap();
  }
  settings.fpsNoCap = $('td-set-fpsnocap').checked;
  settings.showFps = $('td-set-showfps').checked;
  if (!settings.showFps) fpsEl.textContent = '';
  settings.localEcho = $('td-set-localecho').checked;
  if (running) port.postMessage({ type: 'config', fpsNoCap: settings.fpsNoCap });
  settingsModal.hidden = true;
  view.focus();
}

settingsBtn.addEventListener('click', openSettings);
$('td-set-close').addEventListener('click', closeSettings);
settingsModal.addEventListener('click', (e) => {
  if (e.target === settingsModal) closeSettings();
});

// Reset: acts like clicking Stop (stops a running simulation) and
// reloads all the open files with the original example sources —
// for when the user has broken the example while trying things out.
// The worker re-syncs every dock and re-checks afterwards.
let confirmAction = null;

function openConfirm(msg, action) {
  $('td-confirm-msg').textContent = msg;
  confirmAction = action;
  confirmModal.hidden = false;
}

function closeConfirm() {
  confirmModal.hidden = true;
  confirmAction = null;
}

$('td-confirm-cancel').addEventListener('click', closeConfirm);
$('td-confirm-ok').addEventListener('click', () => {
  const action = confirmAction;
  closeConfirm();
  if (action) action();
});
confirmModal.addEventListener('click', (e) => {
  if (e.target === confirmModal) closeConfirm();
});

// Reset the current project unconditionally: stops a running
// simulation and has the worker restore the example sources, clear
// the console and re-check.  The Reset button wraps this in a
// confirmation; programmatic callers (virtry.load) go through
// directly.
function resetProjectNow() {
  stopRun();
  port.postMessage({ type: 'resetProject' });
}

$('td-reset').addEventListener('click', () => {
  const name = project()?.name;
  if (!name) return;
  openConfirm(
    `Reset "${name}" to the example sources? Unsaved edits will be lost.`,
    resetProjectNow,
  );
});

// ── virtry.load(project): programmatic open + load + reset ───
//
// Docs pages call `virtry.load('counter')` (global defined in
// trydock-boot.js) to open the dock on a specific seeded project and
// reset it unconditionally.  The dock has no project switcher of its
// own; the SharedWorker hosts all seeded projects, so loading is a
// `selectProject` (which makes the worker broadcast a fresh sync)
// followed by an unconditional reset.  The request may arrive before
// the session mirror exists (bundle just loaded), so it parks in
// `pendingLoad` and is consumed once a sync provides the project
// list.  If that sync leads to a localStorage `restore` (pristine
// worker), consumption waits for the restore's follow-up sync so the
// switch cannot be overridden by the restored session.

let pendingLoad = null; // project name, or null

function performLoad(name) {
  // Single atomic request: the worker finds-or-appends the seed
  // project by name (the session mirror here may be a restored
  // session that dropped projects — or not have arrived yet), selects
  // it and resets it.  Unknown names are logged by the worker into
  // the dock console.
  port.postMessage({ type: 'loadProject', name, reset: true });
}

function loadProject(name) {
  if (!opened || projects.length === 0) {
    pendingLoad = String(name ?? '');
    openDock(); // also posts `hello`; the sync triggers consumption
    return;
  }
  openDock(); // reopens a manually closed dock (no-op if already open)
  performLoad(name);
}

// virtry.list(): the seeded project names.  Always asked from the
// worker (the seeds live there): the live session mirror may be a
// restored session that has dropped projects, while "all projects"
// means all seeds.  Replies arrive per port in request order, so a
// FIFO resolver queue pairs them up.
const listResolvers = [];

function listProjects() {
  return new Promise((resolve) => {
    listResolvers.push(resolve);
    port.postMessage({ type: 'listProjects' });
  });
}

runBtn.addEventListener('click', startRun);
stopBtn.addEventListener('click', stopRun);

// fps readout (average MHz since `created`) \u2014 shown only when
// enabled in Settings (off by default, like the SPA).
let runStartWall = 0;
function updateFps(cycles) {
  if (!settings.showFps) {
    fpsEl.textContent = '';
    return;
  }
  const secs = (performance.now() - runStartWall) / 1000;
  if (cycles > 0 && secs > 0) {
    const mhz = cycles / secs / 1e6;
    fpsEl.textContent = `${mhz.toFixed(4).replace(/\.?0+$/, '')} MHz (avg)`;
  }
}

// ── Worker messages ─────────────────────────────────────────

port.onmessage = (e) => {
  const m = e.data;
  try {
  switch (m.type) {
    case 'sync':
      checking = false; // worker state is authoritative; a new check (if
                        // any) announces itself with its own `checking`
      applySync(m);
      if (running) {
        runStartWall = performance.now();
        fitTerminal();
        term.focus();
      }
      updateStatus();
      break;
    case 'checking':
      checking = true;
      updateStatus();
      updateRunButton();
      break;
    case 'checkAborted':
      // The check could not run; keep showing the previous result.
      checking = false;
      updateStatus();
      updateRunButton();
      break;
    case 'diagnostics':
      checking = false;
      lastDiagnostics = m.diagnostics;
      lastOk = m.ok;
      checked = true;
      hasErrors = !m.ok;
      diagnosticsByFile.clear();
      for (const d of m.diagnostics) {
        if (!diagnosticsByFile.has(d.file)) diagnosticsByFile.set(d.file, []);
        diagnosticsByFile.get(d.file).push(d);
      }
      renderDiagnostics(m);
      updateRunButton();
      updateStatus();
      view.dispatch({ effects: refreshLint.of(null) });
      // Diagnostics changed: surface problems (falling back to the
      // console once clean, if we auto-switched).  While a run is in
      // flight the console stays up (it just cleared the screen).
      if (running || starting) {
        switchTab('console');
      } else if (m.diagnostics.some((d) => d.level === 'error')) {
        diagAuto = true;
        switchTab('diagnostics');
      } else if (diagAuto) {
        diagAuto = false;
        switchTab('console');
      }
      break;
    case 'consoleReset':
      // A new run cleared the console (Start clears the console).
      term.reset();
      localLogChunks = [];
      break;
    case 'console':
      term.write(new Uint8Array(m.buf));
      localLogChunks.push(new Uint8Array(m.buf));
      saveSessionSoon();
      break;
    case 'created':
      starting = false;
      running = true;
      runStartWall = performance.now();
      fpsEl.textContent = '';
      switchTab('console');
      updateRunButton();
      updateStatus();
      fitTerminal();
      term.focus();
      break;
    case 'progress':
      updateFps(m.cycles);
      break;
    case 'finished':
      running = false;
      updateRunButton();
      updateStatus();
      break;
    case 'stopped':
      running = false;
      updateRunButton();
      updateStatus();
      break;
    case 'sim-error':
      starting = false;
      running = false;
      updateRunButton();
      updateStatus();
      switchTab('console');
      break;
    case 'projectsList': {
      const resolve = listResolvers.shift();
      if (resolve) resolve(Array.isArray(m.names) ? m.names.map(String) : []);
      break;
    }
    case 'runBlocked':
      starting = false;
      running = false;
      updateRunButton();
      updateStatus();
      switchTab('diagnostics');
      break;
  }
  } catch (err) {
    // One bad message must not wedge the dock's message pump (an
    // exception here would otherwise skip the rest of this message
    // and look like a stuck session).
    console.error('trydock: message handler failed for', m.type, err);
  }
};

// ── Open / close ────────────────────────────────────────────

let opened = false;

const OPEN_KEY = 'virdant-trydock:open'; // sessionStorage: stays open
                                        // across in-site navigation

function openDock() {
  dock.hidden = false;
  pill.hidden = true;
  if (!opened) {
    opened = true;
    port.postMessage({ type: 'hello' });
  }
  try { sessionStorage.setItem(OPEN_KEY, '1'); } catch { /* storage unavailable */ }
  syncPageScroller();
  fitTerminal();
  view.focus();
}

function closeDock() {
  dock.hidden = true;
  pill.hidden = false;
  try { sessionStorage.removeItem(OPEN_KEY); } catch { /* storage unavailable */ }
  syncPageScroller();
  saveSession();
}

// The dock is position:fixed, so it would otherwise just cover the
// bottom of the page — including the main scrollbar, which cannot be
// shortened while the document is the scroller.  While the dock is
// open, hand scrolling over to the theme's .wy-grid-for-nav, sized to
// end exactly at the dock's top edge: the main scrollbar then stops
// above the dock and the page's last lines remain reachable (and the
// handover follows the drag handle).  vcd.js listens for scroll events
// in the document's capture phase, so its waveform repositioning keeps
// working with the new scroller.
//
// Handover defeats the browser's own scroll restoration on
// reload/back-forward (the root becomes unscrollable before the
// restore applies), so the position is parked in sessionStorage on
// pagehide and restored here for those navigation types only — fresh
// navigations still start at the top.

const SCROLL_KEY = 'virdant-trydock:scrolly';

function parkPageScroll() {
  const grid = document.querySelector('.wy-grid-for-nav');
  const y = (grid && !dock.hidden) ? grid.scrollTop : window.scrollY;
  try { sessionStorage.setItem(SCROLL_KEY, String(Math.round(y))); } catch { /* storage unavailable */ }
}
window.addEventListener('pagehide', parkPageScroll);

function restoreScrollY() {
  let y = window.scrollY;
  try {
    const nav = performance.getEntriesByType('navigation')[0]?.type;
    const saved = sessionStorage.getItem(SCROLL_KEY);
    const n = saved === null ? NaN : parseInt(saved, 10);
    if (Number.isFinite(n) && n >= 0 && (nav === 'reload' || nav === 'back_forward')) y = n;
  } catch { /* storage unavailable */ }
  return y;
}

let pageScrollerOwned = false;

function syncPageScroller() {
  const grid = document.querySelector('.wy-grid-for-nav');
  if (!grid) return; // unknown layout: the dock overlays the page as before
  const side = document.querySelector('.wy-side-scroll'); // ToC scroller
  const h = dock.hidden ? 0 : dock.getBoundingClientRect().height;
  const hpx = `${Math.round(h)}px`;
  if (h > 0) {
    const taking = !pageScrollerOwned;
    pageScrollerOwned = true;
    grid.style.height = `calc(100% - ${hpx})`;
    grid.style.overflowY = 'auto';
    grid.style.overflowX = 'hidden'; // matches the theme's root overflow-x
    if (taking) grid.scrollTop = restoreScrollY();
    // The ToC sidebar is position:fixed (outside the grid), so give its
    // own scroller the same treatment: it then ends above the dock too.
    // The theme also pads .wy-nav-side by 2em below the scroller (space
    // for the RTD version badge, absent here); drop it while open so the
    // ToC runs right down to the dock's top edge instead of leaving a
    // dead band that reads as a clipped bottom margin.
    if (side) {
      side.style.height = `calc(100% - ${hpx})`;
      side.parentElement.style.paddingBottom = '0';
    }
  } else {
    if (!pageScrollerOwned) return; // never owned: don't touch the page
    pageScrollerOwned = false;
    const y = grid.scrollTop;
    grid.style.height = '';
    grid.style.overflowY = '';
    grid.style.overflowX = '';
    if (side) {
      side.style.height = '';
      side.parentElement.style.paddingBottom = '';
    }
    window.scrollTo(0, y);
  }
}

function toggleDock() {
  if (dock.hidden) openDock();
  else closeDock();
}

pill.addEventListener('click', toggleDock);
$('td-close').addEventListener('click', closeDock);

// The pill may carry the boot script's "loading" state (kept through
// bundle load so the click isn't double-processed); clear it now that
// the bundle owns the pill.
pill.classList.remove('trydock-loading');

// A click that loaded this bundle already meant "open".  A parked
// virtry.load() request additionally names the project to load.
if (window.__trydockBootOpen) {
  delete window.__trydockBootOpen;
  const bootProject = window.__trydockBootProject;
  delete window.__trydockBootProject;
  openDock();
  if (bootProject) loadProject(bootProject);
}

// Escape closes the Settings modal but deliberately does not close
// the dock itself (closing is the ✕ button's job; Esc inside the
// editor/terminal belongs to the tool).
window.addEventListener('keydown', (e) => {
  if (e.key !== 'Escape') return;
  if (!confirmModal.hidden) {
    closeConfirm();
    return;
  }
  if (!settingsModal.hidden) closeSettings();
}, true);

// ── Dock height: drag the top edge to resize; remembered ────

const DOCK_HEIGHT_KEY = 'virdant-trydock:height';
const DOCK_MIN = 260;

function clampDockHeight(px) {
  return Math.round(Math.min(window.innerHeight * 0.85, Math.max(DOCK_MIN, px)));
}

function applyDockHeight(px) {
  dock.style.height = `${clampDockHeight(px)}px`;
  syncPageScroller();
}

// Restore the remembered height (applies whether or not the dock is
// open — the style survives the hidden toggling).
try {
  const savedHeight = parseInt(localStorage.getItem(DOCK_HEIGHT_KEY), 10);
  if (Number.isFinite(savedHeight) && savedHeight > 0) applyDockHeight(savedHeight);
} catch { /* storage unavailable */ }

const resizeHandle = $('td-resize');
resizeHandle.addEventListener('mousedown', (e) => {
  e.preventDefault();
  const startY = e.clientY;
  const startHeight = dock.getBoundingClientRect().height;
  const onMove = (ev) => {
    applyDockHeight(startHeight + (startY - ev.clientY));
    fitTerminal();
  };
  const onUp = () => {
    try { localStorage.setItem(DOCK_HEIGHT_KEY, String(clampDockHeight(dock.getBoundingClientRect().height))); } catch { /* storage unavailable */ }
    window.removeEventListener('mousemove', onMove);
    window.removeEventListener('mouseup', onUp);
  };
  window.addEventListener('mousemove', onMove);
  window.addEventListener('mouseup', onUp);
});

// Keep the scroller sized to the dock if the viewport changes size
// while open (the dock's height is a fixed px once restored/dragged,
// but keep the two consistent anyway).
window.addEventListener('resize', syncPageScroller);

// ── Editor | output split: drag the vertical divider ────────

const SPLIT_KEY = 'virdant-trydock:split';
const SPLIT_MIN_PX = 320; // matches the editor column's CSS minimum
const SPLIT_MAX_FRAC = 0.75; // keep some output pane even on wide docks

const editorCol = root.querySelector('.td-editor-col');

function applySplit(px) {
  const w = dock.clientWidth;
  if (w <= 0) return; // dock hidden or stacked: nothing to measure against
  const clamped = Math.round(Math.min(w * SPLIT_MAX_FRAC, Math.max(SPLIT_MIN_PX, px)));
  dock.style.setProperty('--td-editor-w', `${clamped}px`);
}

// Restore the remembered split (the style survives hidden toggling).
try {
  const savedSplit = parseInt(localStorage.getItem(SPLIT_KEY), 10);
  if (Number.isFinite(savedSplit) && savedSplit > 0) applySplit(savedSplit);
} catch { /* storage unavailable */ }

// Re-clamp if the viewport changes size while a split is remembered.
window.addEventListener('resize', () => {
  try {
    const savedSplit = parseInt(localStorage.getItem(SPLIT_KEY), 10);
    if (Number.isFinite(savedSplit) && savedSplit > 0) applySplit(savedSplit);
  } catch { /* storage unavailable */ }
});

const vsplitHandle = $('td-vsplit');
vsplitHandle.addEventListener('mousedown', (e) => {
  e.preventDefault();
  const startX = e.clientX;
  const startW = editorCol.getBoundingClientRect().width;
  const onMove = (ev) => {
    applySplit(startW + (ev.clientX - startX));
    fitTerminal();
  };
  const onUp = () => {
    try { localStorage.setItem(SPLIT_KEY, String(Math.round(editorCol.getBoundingClientRect().width))); } catch { /* storage unavailable */ }
    window.removeEventListener('mousemove', onMove);
    window.removeEventListener('mouseup', onUp);
  };
  window.addEventListener('mousemove', onMove);
  window.addEventListener('mouseup', onUp);
});

// Expose a tiny handle for debugging / future integrations.
// (The public entry point for pages is `virtry.load(project)` /
// `virtry.list()`, defined in trydock-boot.js; they delegate here.)
window.__trydock = { open: openDock, close: closeDock, toggle: toggleDock, loadProject, listProjects };

// virtry.list() calls made before this bundle loaded: hand their
// resolvers over now (the worker connection exists, so the reply
// works even with the dock closed).
if (Array.isArray(window.__trydockBootListWaiters)) {
  const waiters = window.__trydockBootListWaiters;
  delete window.__trydockBootListWaiters;
  for (const resolve of waiters) listProjects().then(resolve);
}
