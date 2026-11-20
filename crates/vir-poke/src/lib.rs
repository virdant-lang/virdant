//! The `vir-poke` library: the peek/poke session and scripting
//! engine.  `vir poke` execs the `vir-poke` binary via the `vir-*`
//! companion-binary mechanism; see `vir/src/vir/cli.rs`.
//!
//! This library owns the session (the live `Sim` plus the `cd`-style
//! current-module location), `commands` executes parsed commands,
//! `eval` evaluates expressions, and `display` renders values
//! round-trippably; `driver` runs scripts and interactive sessions.
//!
//! Everything here is WASM-clean for `wasm32-wasip1`/`wasip2`
//! (wasmtime): the only host-only component is the readline REPL,
//! gated behind `#[cfg(not(target_arch = "wasm32"))]` — on WASI the
//! interactive loop degrades to the plain line loop, exactly like
//! the documented non-tty behavior.  Scripts, assertions, `peek`/
//! `poke`/`tick`/`vcd`, and value rendering all work under WASI
//! (WASI provides `std::fs`, so VCD traces can still be written).

pub mod ast;
pub mod commands;
pub mod display;
pub mod eval;
pub mod lexer;

// Generated from `grammar.lalrpop` by the build script (into OUT_DIR).
lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/grammar.rs"
);

pub mod driver;

// ── Output routing ──────────────────────────────────────────
//
// Session output (`peek`, `show`, `ls`, watches, …) goes through
// [`emit`] instead of `println!` so a host can capture it: WASM
// embedders install a sink with [`set_output_sink`] and read it back
// (wasm32-unknown-unknown's stdout discards writes); native runs keep
// the default of writing to real stdout.

thread_local! {
    static SINK: RefCell<Option<Rc<RefCell<String>>>> = const { RefCell::new(None) };
}

/// Route session output into `sink` instead of stdout, until cleared
/// with `None`.
pub fn set_output_sink(sink: Option<Rc<RefCell<String>>>) {
    SINK.with_borrow_mut(|slot| *slot = sink);
}

/// Take the installed sink (if any), leaving stdout as the output.
pub fn take_output_sink() -> Option<Rc<RefCell<String>>> {
    SINK.with_borrow_mut(|slot| slot.take())
}

/// Emit one piece of session output: into the installed sink when a
/// host has one, or to real stdout otherwise.
pub(crate) fn emit(text: &str) {
    SINK.with(|slot| {
        if let Some(sink) = slot.borrow().as_ref() {
            sink.borrow_mut().push_str(text);
            return;
        }
        let stdout = std::io::stdout();
        let mut lock = stdout.lock();
        let _ = lock.write_all(text.as_bytes());
    });
}

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::rc::Rc;

use bstr::ByteSlice;

use virdant::analysis::elaboration::SignalId;
use virdant::analysis::symbols::{SymbolId, SymbolKind};
use virdant::common::{ChannelDir, ComponentKind, SocketRole};
use virdant::db::Db;
use virdant::syntax::payload::AstNodePayload;
use virdant::sim::{InitStrategy, Sim, Value};
use virdant::types::Type;

use bstr::BString;

use ast::SPath;


/// The live peek/poke session: one elaborated [`Sim`] for one top
/// module, the current module (`cd` location), the `it` binding, and
/// the breakpoint flag shared with `break` watchers.
pub struct Session {
    pub db: Db,
    pub sim: Sim,
    /// The top module's symbol, kept for `restart`.
    pub top: SymbolId,
    /// The current module as segments under `top` (empty = the top).
    pub cwd: Vec<String>,
    /// The previously peeked value (`it`).
    pub it: Option<Value>,
    /// Set by `break` watchers when their signal changes; the value is
    /// the watched signal's full path.  Checked after every settling
    /// command (see `commands::check_break`).
    pub break_flag: Rc<Cell<Option<String>>>,
    /// Whether `show` runs automatically after every `poke` and
    /// `tick` (`autoshow`).
    pub autoshow: bool,
    /// Per-signal display radices (`format`), keyed by full
    /// elaborated path (`top.foo.bar`).  The value is the stored
    /// state as an `Option<Fmt>`: a signal absent from the map was
    /// never formatted (it just *renders* as `bin`); an entry holds
    /// whatever `format` set, `bin` included, as `Some(fmt)`.
    /// Shared with watch callbacks so they pick up changes live
    /// (like `vcd_signals`).  Cleared by `top` (a new design); kept
    /// across `restart` (the same design's signals).
    pub formats: Rc<RefCell<HashMap<String, Option<ast::Fmt>>>>,
    /// `--format-json`: the signals named by the scanned script's
    /// `show` commands, in script order — exactly the entries the
    /// JSON summary prints.  Filled by [`Session::scan_format_json`]
    /// before [`Session::format_json`] reads it.
    pub format_json_signals: RefCell<Vec<SignalId>>,
    /// The file a VCD trace (`vcd`) is being written to, if any.
    /// Cleared by `restart`: the trace lives and dies with the `Sim`.
    pub vcd: Option<String>,
    /// Which signals the VCD trace carries: `None` is every signal
    /// (the starting state); `Some(set)` is only the signals whose
    /// full elaborated paths are in the set — built up by
    /// `show <SIGNAL>` (the first call replaces "every signal" with
    /// just that one, each further call adds one more).  Shared with
    /// the sim's VCD watchers, so a trace attached earlier picks up
    /// signals added to the list later.  Reset by `restart`/`top`.
    pub vcd_signals: Rc<RefCell<Option<HashSet<BString>>>>,
    /// The running script's path, when a script is running (used for
    /// the default `vcd` file name: `<script>.vcd`).
    pub script_name: Option<String>,
}

impl Session {
    /// The current module's elaborated path prefix: `"top"` or
    /// `"top.foo.bar"`.
    pub fn prefix(&self) -> String {
        if self.cwd.is_empty() {
            "top".to_string()
        } else {
            format!("top.{}", self.cwd.join("."))
        }
    }

    /// The full elaborated path for a script path, relative to the
    /// current module (or absolute for `top.`-prefixed paths).
    pub fn full_path(&self, spath: &SPath) -> Result<String, String> {
        let tail = spath
            .segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(".");
        match &spath.start {
            ast::PathStart::Top => Ok(if tail.is_empty() {
                "top".to_string()
            } else {
                format!("top.{tail}")
            }),
            ast::PathStart::Ident(head) => {
                Ok(if tail.is_empty() {
                    format!("{}.{}", self.prefix(), head)
                } else {
                    format!("{}.{}.{}", self.prefix(), head, tail)
                })
            }
            ast::PathStart::It => Err("`it` is a value, not a path (field access on `it` is not supported yet)".to_string()),
        }
    }

    /// Resolve a script path to a signal.
    pub fn resolve_signal(&self, spath: &SPath) -> Result<SignalId, String> {
        let full = self.full_path(spath)?;
        self.sim.try_resolve(full.as_bytes()).ok_or_else(|| {
            let hint = self.path_hint(&full);
            match hint {
                Some(hint) => format!("no signal named `{full}` ({hint})"),
                None => format!("no signal named `{full}`"),
            }
        })
    }

    /// The current value of the signal at a script path.
    pub fn get_value(&self, spath: &SPath) -> Result<Value, String> {
        let signal_id = self.resolve_signal(spath)?;
        Ok(self.sim.get(signal_id))
    }

    /// A maybe-helpful note for a failed path resolution: near-miss
    /// component names in the current module, and its submodules.
    fn path_hint(&self, full: &str) -> Option<String> {
        let prefix = self.prefix();
        let components: Vec<String> = self
            .sim
            .children_of_path(prefix.as_bytes())
            .iter()
            .map(|(_, name)| name.to_str_lossy().into_owned())
            .collect();
        let modules: Vec<String> = self
            .sim
            .module_children(prefix.as_bytes())
            .iter()
            .map(|name| name.to_str_lossy().into_owned())
            .collect();
        if components.is_empty() && modules.is_empty() {
            return None;
        }
        let mut hint = String::new();
        if !components.is_empty() {
            hint.push_str(&format!(
                "signals here: {}",
                components.join(", ")
            ));
        }
        if !modules.is_empty() {
            if !hint.is_empty() {
                hint.push_str("; ");
            }
            hint.push_str(&format!("submodules: {}", modules.join(", ")));
        }
        let _ = full;
        Some(hint)
    }

    /// `format <path> <fmt>`: set the display radix of the signal at
    /// a script path — any signal takes one.  Whatever the command
    /// gave (`bin` included) is stored as `Some(fmt)`; a signal
    /// never formatted has no entry (see `formats`).
    pub fn format(&mut self, spath: &SPath, fmt: ast::Fmt) -> Result<(), String> {
        let full = self.set_format(spath, fmt)?;
        emit(&format!("format: {full} = {fmt}\n"));
        Ok(())
    }

    /// The quiet core of [`Session::format`]: resolve the path,
    /// store the radix, and return the signal's full path.  (The
    /// `--format-json` scan uses this so its stdout stays
    /// machine-readable.)
    fn set_format(&mut self, spath: &SPath, fmt: ast::Fmt) -> Result<String, String> {
        let signal_id = self.resolve_signal(spath)?;
        let full = self.sim.full_name(signal_id).to_str_lossy().into_owned();
        self.formats.borrow_mut().insert(full.clone(), Some(fmt));
        Ok(full)
    }

    /// `--format-json`: parse and scan a script *without running
    /// it*, recording what the JSON summary needs.  Each `show
    /// <path>` names a signal for the summary, in script order;
    /// each `format` sets that signal's display radix.  `cd`
    /// commands are followed so relative paths resolve exactly as
    /// they would when the script runs; every other command is
    /// ignored.  A parse error or an unresolvable `show`/`format`
    /// path is an error.
    pub fn scan_format_json(&mut self, lines: &[(usize, String)]) -> Result<(), String> {
        for (number, text) in lines {
            let trimmed = text.trim();
            if trimmed.is_empty() || trimmed.starts_with("//") {
                continue;
            }
            let command = driver::parse_line(trimmed)
                .map_err(|e| format!("line {number}: {e}"))?;
            match command {
                Some(ast::Command::Cd(segments)) => self.cd(&segments)?,
                Some(ast::Command::CdAbs(segments)) => self.cd_abs(&segments)?,
                Some(ast::Command::CdTop) => self.cwd.clear(),
                Some(ast::Command::CdUp) => self.cd_up()?,
                Some(ast::Command::Show(Some(path))) => {
                    let signal_id = self.resolve_signal(&path)?;
                    self.format_json_signals.borrow_mut().push(signal_id);
                }
                Some(ast::Command::Format { path, fmt }) => {
                    self.set_format(&path, fmt)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// The display radix in effect for a signal (from its full
    /// elaborated path), or the default (`bin`).
    pub(crate) fn fmt_of(&self, signal_id: SignalId) -> Option<ast::Fmt> {
        let full = self.sim.full_name(signal_id).to_str_lossy().into_owned();
        self.fmt_of_path(&full)
    }

    /// [`Session::fmt_of`] by full path (for the places that hold a
    /// path string, not a `SignalId` — watch/break output).  See
    /// `formats` for the `Option<Fmt>` semantics.
    pub(crate) fn fmt_of_path(&self, full: &str) -> Option<ast::Fmt> {
        self.formats.borrow().get(full).copied().flatten()
    }

    /// `--format-json`: the signal summary as a JSON array — one
    /// object per signal (`signal`, `type`, `format`), one per
    /// `show <path>` command in the scanned script, in script order
    /// (see [`Session::scan_format_json`]).  Types render as
    /// source-like strings with every type name spelled as its
    /// fully-qualified name
    /// ([`virdant::types::render::render_type_fqn`]); `format` is the
    /// display radix when one was set with `format`, else null.
    pub fn format_json(&self) -> String {
        let entries: Vec<String> = self
            .format_json_signals
            .borrow()
            .iter()
            .map(|signal_id| self.format_json_entry(*signal_id))
            .collect();
        format!("[\n{}\n]\n", entries.join(",\n"))
    }

    /// [`Session::format_json`] object.  `format` is the stored
    /// radix when `format` set one (bin included); null when none
    /// was ever set for the signal.
    fn format_json_entry(&self, signal_id: SignalId) -> String {
        let full = self.sim.full_name(signal_id).to_str_lossy().into_owned();
        let typ = self.sim.component_type(signal_id);
        let typ_str = virdant::types::render::render_type_fqn(&self.db, &typ);
        let stored = self.formats.borrow().get(&full).copied().flatten();
        // `Fmt`'s `Display` renders exactly the JSON spelling
        // ("bin"/"hex"/"dec"); quote it with `serde_json`.
        let fmt = match stored {
            Some(fmt) => serde_json::to_string(&fmt.to_string()).unwrap(),
            None => "null".to_string(),
        };
        format!(
            "  {{\"signal\": {}, \"type\": {}, \"format\": {}}}",
            serde_json::to_string(&full).unwrap(),
            serde_json::to_string(&typ_str).unwrap(),
            fmt
        )
    }

    /// `show <path>`: select the signal for the VCD trace.  The trace
    /// starts off carrying every signal; the first `show` replaces
    /// that with just this signal, and each further `show` adds one
    /// more.  Bare `show` (no argument) never changes the list.
    pub fn show_signal(&mut self, spath: &SPath) -> Result<(), String> {
        let signal_id = self.resolve_signal(spath)?;
        let full = self.sim.full_name(signal_id).to_str_lossy().into_owned();
        let mut list = self.vcd_signals.borrow_mut();
        match &mut *list {
            None => {
                *list = Some(HashSet::from([BString::from(full.clone())]));
                emit(&format!("show: tracing {full} (only)\n"));
            }
            Some(set) => {
                if set.contains(full.as_bytes().as_bstr()) {
                    emit(&format!("show: {full} already traced\n"));
                } else {
                    set.insert(BString::from(full.clone()));
                    emit(&format!(
                        "show: tracing {full} ({} traced)\n",
                        set.len()
                    ));
                }
            }
        }
        Ok(())
    }

    /// The default VCD file name for a bare `vcd`: the running
    /// script's name with a `.vcd` extension (`counter.poke` →
    /// `counter.vcd`), or `trace.vcd` when no script is running.
    fn default_vcd_name(&self) -> String {
        match &self.script_name {
            Some(script) => std::path::Path::new(script)
                .with_extension("vcd")
                .to_string_lossy()
                .into_owned(),
            None => "trace.vcd".to_string(),
        }
    }

    /// `cd <path>`: drill into a submodule, one or more levels down.
    pub fn cd(&mut self, segments: &[String]) -> Result<(), String> {
        for segment in segments {
            let candidate = format!("{}.{}", self.prefix(), segment);
            if self.sim.has_module_prefix(candidate.as_bytes()) {
                self.cwd.push(segment.clone());
            } else {
                let modules: Vec<String> = self
                    .sim
                    .module_children(self.prefix().as_bytes())
                    .iter()
                    .map(|name| name.to_str_lossy().into_owned())
                    .collect();
                if modules.is_empty() {
                    return Err(format!(
                        "no submodule named `{segment}` under `{}` (the module has no submodules)",
                        self.prefix()
                    ));
                }
                return Err(format!(
                    "no submodule named `{segment}` under `{}` (submodules: {})",
                    self.prefix(),
                    modules.join(", ")
                ));
            }
        }
        Ok(())
    }

    /// `cd top.<path>`: jump to an absolute module path from the top.
    pub fn cd_abs(&mut self, segments: &[String]) -> Result<(), String> {
        let candidate = format!("top.{}", segments.join("."));
        if self.sim.has_module_prefix(candidate.as_bytes()) {
            self.cwd = segments.to_vec();
            Ok(())
        } else {
            Err(format!("no submodule path `{candidate}`"))
        }
    }

    /// `cd ..`: up one level (a no-op at the top).
    pub fn cd_up(&mut self) -> Result<(), String> {
        self.cwd.pop();
        Ok(())
    }

    /// `ls`: list the current module's components (kind + type) and
    /// submodule/socket instances (their `of` definition).  Rows are
    /// sorted by name so the listing is stable across runs.
    pub fn ls(&self) {
        let prefix = self.prefix();
        let symboltable = self.db.get_symboltable();

        let mut rows: Vec<(String, String, String)> = Vec::new();
        for (signal_id, name) in self.sim.children_of_path(prefix.as_bytes()) {
            let kind = kind_name(self.sim.component_kind(signal_id));
            let typ = self.sim.component_type(signal_id);
            let typ_str = virdant::types::render::render_type(&self.db, &typ);
            rows.push((
                name.to_str_lossy().into_owned(),
                kind.to_string(),
                format!(": {typ_str}"),
            ));
        }
        for name in self.sim.module_children(prefix.as_bytes()) {
            let full = format!("{}.{}", prefix, name.to_str_lossy());
            let of = self
                .sim
                .module_symbol(full.as_bytes())
                .map(|sym_id| symboltable.symbol(sym_id).fqn)
                .unwrap_or_default();
            rows.push((
                name.to_str_lossy().into_owned(),
                "mod".to_string(),
                format!("of {}", of.to_str_lossy()),
            ));
        }
        for (name, socketdef) in self.sim.socket_children(prefix.as_bytes()) {
            let of = symboltable.symbol(socketdef).fqn;
            rows.push((
                name.to_str_lossy().into_owned(),
                "socket".to_string(),
                format!("of {}", of.to_str_lossy()),
            ));
        }

        if rows.is_empty() {
            emit("(no components or submodule instances)\n");
            return;
        }
        rows.sort_by(|a, b| a.0.cmp(&b.0));
        let width = rows.iter().map(|(name, _, _)| name.len()).max().unwrap_or(0);
        for (name, kind, tail) in rows {
            emit(&format!("{name:<width$} {kind} {tail}\n"));
        }
    }

    /// `show`: print the value of every signal in the design, grouped
    /// by module (elaboration order) and then by socket instance, with
    /// columns aligned across the whole listing.
    pub fn show(&self) {
        let symboltable = self.db.get_symboltable();

        // (header, rows) where a row is (name, kind, type, value).
        let mut groups: Vec<(String, Vec<ShowRow>)> = Vec::new();
        for (moddef, prefix) in self.sim.modules() {
            let moddef_fqn = symboltable.symbol(moddef).fqn.to_str_lossy().into_owned();
            let rows: Vec<ShowRow> = self
                .sim
                .children_of_path(prefix.as_bytes())
                .into_iter()
                .map(|(signal_id, leaf)| self.show_row(signal_id, leaf.as_bstr()))
                .collect();
            groups.push((
                format!("{} ({})", prefix.to_str_lossy(), moddef_fqn),
                rows,
            ));
        }
        for (socketdef, prefix) in self.sim.sockets() {
            let socketdef_fqn = symboltable
                .symbol(socketdef)
                .fqn
                .to_str_lossy()
                .into_owned();
            let rows: Vec<ShowRow> = self
                .sim
                .children_of_path(prefix.as_bytes())
                .into_iter()
                .map(|(signal_id, leaf)| self.show_row(signal_id, leaf.as_bstr()))
                .collect();
            groups.push((
                format!("{} ({}, socket)", prefix.to_str_lossy(), socketdef_fqn),
                rows,
            ));
        }

        // Column widths, aligned across the whole listing.
        let name_width = groups
            .iter()
            .flat_map(|(_, rows)| rows.iter().map(|row| row.name.len()))
            .max()
            .unwrap_or(0);
        let kind_width = groups
            .iter()
            .flat_map(|(_, rows)| rows.iter().map(|row| row.kind.len()))
            .max()
            .unwrap_or(0);
        let typ_width = groups
            .iter()
            .flat_map(|(_, rows)| rows.iter().map(|row| row.typ.len()))
            .max()
            .unwrap_or(0);

        let mut first = true;
        for (header, rows) in groups {
            if rows.is_empty() {
                continue;
            }
            if !first {
                emit("\n");
            }
            first = false;
            emit(&format!("{}:\n", header));
            for row in rows {
                // A Clock signal is opaque: its row shows no value.
                if row.value.is_empty() {
                    emit(&format!(
                        "  {:<nw$} {:<kw$} : {:<tw$}\n",
                        row.name,
                        row.kind,
                        row.typ,
                        nw = name_width,
                        kw = kind_width,
                        tw = typ_width
                    ));
                } else {
                    emit(&format!(
                        "  {:<nw$} {:<kw$} : {:<tw$} = {}\n",
                        row.name,
                        row.kind,
                        row.typ,
                        row.value,
                        nw = name_width,
                        kw = kind_width,
                        tw = typ_width
                    ));
                }
            }
        }
    }

    fn show_row(&self, signal_id: SignalId, leaf: &bstr::BStr) -> ShowRow {
        let typ = self.sim.component_type(signal_id);
        let fmt = self.fmt_of(signal_id);
        // A memory's scalar value is a change-detection version
        // counter; `show` its words instead (first few, then an
        // ellipsis) so a 1024-word ROM stays printable.
        let value_str = if let Some(words) = self.sim.memory_words(signal_id) {
            const SHOW_WORDS: usize = 8;
            let shown: Vec<String> = words
                .iter()
                .take(SHOW_WORDS)
                .enumerate()
                .map(|(index, word)| format!("[{index}] {}", display::render_fmt(&self.sim, word, fmt)))
                .collect();
            if words.len() > SHOW_WORDS {
                format!(
                    "{} words: {} ...",
                    words.len(),
                    shown.join(", ")
                )
            } else {
                format!("{} words: {}", words.len(), shown.join(", "))
            }
        } else {
            let value = self.sim.get(signal_id);
            // Clocks are opaque: the `show` row carries no value.
            if matches!(typ, Type::Clock) {
                String::new()
            } else {
                display::render_fmt(&self.sim, &value, fmt)
            }
        };
        ShowRow {
            name: leaf.to_str_lossy().into_owned(),
            kind: kind_name(self.sim.component_kind(signal_id)).to_string(),
            typ: virdant::types::render::render_type(&self.db, &typ),
            value: value_str,
        }
    }

    /// TAB-completion candidates for `line_so_far` (the text up to the
    /// cursor).  Two modes: completing the command word itself (only
    /// whitespace precedes it), and completing a path argument.
    /// TAB-completion candidates for the readline REPL (host-only;
    /// see `driver.rs`).
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn completions(&self, line_so_far: &str) -> Vec<String> {
        let words: Vec<&str> = line_so_far.split_whitespace().collect();
        let word_start = line_so_far
            .rfind(char::is_whitespace)
            .map(|i| i + 1)
            .unwrap_or(0);
        let word = &line_so_far[word_start..];

        if word_start == 0 {
            // Completing the command name itself.
            return COMMANDS
                .iter()
                .filter(|command| command.starts_with(word))
                .map(|command| command.to_string())
                .collect();
        }

        // Index of the word being completed (0 = the command).  An
        // empty `word` means the line ends in whitespace and the next
        // argument is starting.
        let arg_index = if word.is_empty() {
            words.len()
        } else {
            words.len() - 1
        };
        let Some(command) = words.first() else {
            return vec![];
        };

        match *command {
            "cd" => {
                if arg_index == 0 {
                    return vec![];
                }
                self.complete_path(word, true)
            }
            "peek" | "watch" | "break" | "show" => {
                if arg_index == 1 {
                    self.complete_path(word, false)
                } else {
                    vec![]
                }
            }
            "format" => {
                if arg_index == 1 {
                    self.complete_path(word, false)
                } else if arg_index == 2 {
                    ["hex", "bin", "dec"]
                        .iter()
                        .filter(|s| s.starts_with(word))
                        .map(|s| s.to_string())
                        .collect()
                } else {
                    vec![]
                }
            }
            "poke" => {
                if arg_index == 1 || (arg_index >= 2 && looks_like_path(word)) {
                    self.complete_path(word, false)
                } else {
                    vec![]
                }
            }
            "tick" => {
                // `tick [n] [<clock>]`: the clock is the second
                // argument, or the first when it doesn't look like a
                // count.
                let first_is_count = words
                    .get(1)
                    .map(|w| w.chars().all(|c| c.is_ascii_digit()))
                    .unwrap_or(false);
                let clock_index = if first_is_count { 2 } else { 1 };
                if arg_index == clock_index && looks_like_path(word) {
                    self.complete_path(word, false)
                } else {
                    vec![]
                }
            }
            "assert" | "print" => {
                if arg_index >= 1 && looks_like_path(word) {
                    self.complete_path(word, false)
                } else {
                    vec![]
                }
            }
            "autoshow" => {
                if arg_index == 1 {
                    ["on", "off"]
                        .iter()
                        .filter(|s| s.starts_with(word))
                        .map(|s| s.to_string())
                        .collect()
                } else {
                    vec![]
                }
            }
            "top" => {
                // Complete against the project's module definitions,
                // by fully-qualified name.
                if arg_index == 1 {
                    let symboltable = self.db.get_symboltable();
                    let mut names: Vec<String> = symboltable
                        .items()
                        .into_iter()
                        .filter(|sym| sym.kind == SymbolKind::ModDef)
                        .map(|sym| sym.fqn.to_str_lossy().into_owned())
                        .filter(|name| name.starts_with(word))
                        .collect();
                    names.sort();
                    names.dedup();
                    names
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    /// Complete a dotted path `partial` against the design's signal
    /// tree.  `module_only` restricts candidates to the steps `cd`
    /// accepts (submodule names, `..`, `top`).
    #[cfg(not(target_arch = "wasm32"))]
    fn complete_path(&self, partial: &str, module_only: bool) -> Vec<String> {
        let (head, leaf) = match partial.rfind('.') {
            Some(i) => (&partial[..i], &partial[i + 1..]),
            None => ("", partial),
        };

        // Resolve `head` (the already-typed prefix) to a full
        // elaborated path prefix.
        let base: String = if head.is_empty() {
            self.prefix()
        } else if head == "top" {
            "top".to_string()
        } else if let Some(rest) = head.strip_prefix("top.") {
            let candidate = format!("top.{}", rest);
            if self.is_path_prefix(&candidate) {
                candidate
            } else {
                return vec![];
            }
        } else {
            let candidate = format!("{}.{}", self.prefix(), head);
            if self.is_path_prefix(&candidate) {
                candidate
            } else {
                return vec![];
            }
        };

        let head_dot = if head.is_empty() {
            String::new()
        } else {
            format!("{}.", head)
        };
        let mut out: Vec<String> = Vec::new();

        // Path steps: submodule instances (always) and socket
        // instances (whose channels hang beneath them; not `cd`
        // targets).
        for name in self.sim.module_children(base.as_bytes()) {
            let name = name.to_str_lossy();
            if name.starts_with(leaf) {
                out.push(format!("{}{}", head_dot, name));
            }
        }
        if !module_only {
            for (name, _) in self.sim.socket_children(base.as_bytes()) {
                let name = name.to_str_lossy();
                if name.starts_with(leaf) {
                    out.push(format!("{}{}", head_dot, name));
                }
            }
            for (_, leaf_name) in self.sim.children_of_path(base.as_bytes()) {
                let name = leaf_name.to_str_lossy();
                if name.starts_with(leaf) {
                    out.push(format!("{}{}", head_dot, name));
                }
            }
        }
        // `top` (an absolute root step) and, for `cd`, `..`.
        if head.is_empty() && "top".starts_with(leaf) {
            out.push("top".to_string());
        }
        if module_only && head.is_empty() && "..".starts_with(leaf) {
            out.push("..".to_string());
        }

        out.sort();
        out.dedup();
        out
    }

    /// Whether `prefix` names a path step: the root (`"top"`), a
    /// module instance, or a socket instance (whose channels are
    /// components beneath it).
    #[cfg(not(target_arch = "wasm32"))]
    fn is_path_prefix(&self, prefix: &str) -> bool {
        if prefix == "top" {
            return true;
        }
        if self.sim.has_module_prefix(prefix.as_bytes()) {
            return true;
        }
        self.sim
            .sockets()
            .iter()
            .any(|(_, socket_prefix)| socket_prefix.as_bytes() == prefix.as_bytes())
    }

    /// The current module's default clock: its only Clock-typed
    /// component.  Zero or several Clock-typed components is an error;
    /// a clock path must then be given explicitly.
    pub fn default_clock(&self) -> Result<SignalId, String> {
        let prefix = self.prefix();
        let clocks: Vec<(SignalId, String)> = self
            .sim
            .children_of_path(prefix.as_bytes())
            .into_iter()
            .filter(|(signal_id, _)| {
                matches!(self.sim.component_type(*signal_id), Type::Clock)
            })
            .map(|(signal_id, name)| {
                (signal_id, name.to_str_lossy().into_owned())
            })
            .collect();
        match clocks.len() {
            0 => Err(format!(
                "no clock in `{prefix}`: give the clock path explicitly (tick <n> <path>)"
            )),
            1 => Ok(clocks[0].0),
            _ => Err(format!(
                "`{prefix}` has more than one clock ({}): give the clock path explicitly (tick <n> <path>)",
                clocks
                    .iter()
                    .map(|(_, name)| name.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }

    /// Register a watch on `signal_id`.  `watch` prints on every
    /// change; `break` silently sets the session's break flag (the
    /// pause is reported once, by `commands::check_break`).
    pub fn register_watch(
        &mut self,
        signal_id: SignalId,
        path: String,
        is_break: bool,
    ) {
        // Read `formats` live at change time (shared `Rc`) so a
        // later `format` affects existing watches without
        // re-registering.
        let formats = self.formats.clone();
        let break_flag = self.break_flag.clone();
        self.sim.on_change(
            signal_id,
            Box::new(move |sim| {
                if is_break {
                    break_flag.set(Some(path.clone()));
                } else {
                    let value = sim.get(signal_id);
                    // Clocks are opaque: a watch on a clock reports the
                    // change without a value.
                    if matches!(sim.component_type(signal_id), Type::Clock) {
                        emit(&format!("watch: {}\n", path));
                    } else {
                        let fmt = formats.borrow().get(path.as_str()).copied().flatten();
                        emit(&format!(
                            "watch: {} = {}\n",
                            path,
                            display::render_fmt(sim, &value, fmt)
                        ));
                    }
                }
            }),
        );
    }

    /// Clear and return a pending breakpoint hit.
    pub fn take_break(&self) -> Option<String> {
        self.break_flag.take()
    }

    /// `restart`: a fresh `Sim` (fresh `InitStrategy`, `initial`
    /// statements re-run).  Registered watches/breaks do not survive a
    /// restart.
    pub fn restart(&mut self) {
        self.sim = Sim::new(&self.db, self.top, InitStrategy::Random);
        self.break_flag.set(None);
        self.it = None;
        // A VCD trace was attached to the old `Sim` (its watchers died
        // with it); forget it so a fresh trace can be started.
        self.vcd = None;
        // Back to tracing every signal.
        *self.vcd_signals.borrow_mut() = None;
    }

    /// `top <module>`: reload the simulator with a new top module.
    /// Resolution mirrors the startup logic: an exact
    /// `package::ModDef` match first, then a simple-name match
    /// (ambiguous → error).  Like `restart`, everything tied to the
    /// old `Sim` resets: the `cd` location, `it`, watches/breaks, and
    /// any VCD trace.
    pub fn set_top(&mut self, name: &str) -> Result<(), String> {
        let symboltable = self.db.get_symboltable();
        let is_moddef = |sym: &virdant::analysis::symbols::Symbol| {
            sym.kind == SymbolKind::ModDef
        };
        let new_top = match symboltable
            .items()
            .into_iter()
            .find(|sym| is_moddef(sym) && sym.fqn.as_bytes() == name.as_bytes())
        {
            Some(sym) => sym,
            None => {
                // Fall back to a simple-name match (the module named
                // without its package qualifier).  Ambiguity is an
                // error.
                let matches: Vec<_> = symboltable
                    .items()
                    .into_iter()
                    .filter(|sym| is_moddef(sym) && sym.name.as_bytes() == name.as_bytes())
                    .collect();
                match matches.len() {
                    0 => return Err(format!("top module '{name}' not found")),
                    1 => matches.into_iter().next().unwrap(),
                    _ => {
                        let names: Vec<String> = matches
                            .iter()
                            .map(|sym| sym.fqn.to_str_lossy().into_owned())
                            .collect();
                        return Err(format!(
                            "top module '{name}' is ambiguous: {}",
                            names.join(", ")
                        ));
                    }
                }
            }
        };
        self.top = new_top.id;
        self.sim = Sim::new(&self.db, new_top.id, InitStrategy::Random);
        self.cwd.clear();
        self.it = None;
        self.break_flag.set(None);
        self.vcd = None;
        // Back to tracing every signal (the new design may not even
        // have the old list's paths).
        *self.vcd_signals.borrow_mut() = None;
        // A new design: the old formats' paths mean nothing here.
        self.formats.borrow_mut().clear();
        emit(&format!("top: {}\n", new_top.fqn.to_str_lossy()));
        Ok(())
    }
}

/// Whether `signal_id` can be poked: top-level incoming ports (paths
/// of the form `top.<port>`), registers at any depth, and top-level
/// socket channels on the side the testbench drives.  Anything else
/// is immediately overridden by the next combinational settle, so the
/// tool rejects it rather than silently doing something misleading.
pub fn poke_legal(session: &Session, signal_id: SignalId) -> Result<(), String> {
    // Clocks are never poked: time is driven with `tick`, and the
    // clock path itself only makes sense as a `tick` argument.
    if matches!(session.sim.component_type(signal_id), Type::Clock) {
        let path = session.sim.full_name(signal_id).to_str_lossy().into_owned();
        return Err(format!(
            "cannot poke `{path}`: Clock signals cannot be poked; use `tick [n] [<clock>]` to apply clock edges"
        ));
    }
    let kind = session.sim.component_kind(signal_id);
    if kind == ComponentKind::Reg || kind == ComponentKind::OutgoingReg {
        return Ok(());
    }
    if kind == ComponentKind::Incoming {
        let path = session.sim.full_name(signal_id);
        let dots = path.iter().filter(|b| **b == b'.').count();
        if dots + 1 == 2 {
            return Ok(());
        }
    }
    // A channel of a socket instance declared directly in the top
    // module (`top.<inst>.<channel>`) is poked exactly like a
    // top-level incoming port when it sits on the socket's incoming
    // side: the testbench plays the peer role there, so nothing in the
    // design overrides the written value.  (The module drives the
    // other side itself, so pokes there would not stick.)
    if kind == ComponentKind::Net {
        if let Some((role, dir)) = top_socket_channel(session, signal_id) {
            let incoming = match role {
                SocketRole::Server => dir == ChannelDir::Cosi,
                SocketRole::Client => dir == ChannelDir::Soci,
            };
            if incoming {
                return Ok(());
            }
        }
    }
    let path = session.sim.full_name(signal_id).to_str_lossy().into_owned();
    Err(format!(
        "cannot poke `{path}`: only top-level incoming ports, registers, and top-level socket input channels can be poked (anything else is overridden by the next settle)"
    ))
}

/// If `signal_id` is a channel of a socket instance declared directly
/// in the top module — a path of the form `top.<inst>.<channel>` —
/// return the instance's role and the channel's declared direction
/// (`cosi`/`soci`).  `None` for anything deeper, anything not under
/// `top`, and any name that is not a socket channel.
fn top_socket_channel(session: &Session, signal_id: SignalId) -> Option<(SocketRole, ChannelDir)> {
    let path = session.sim.full_name(signal_id).to_str_lossy().into_owned();
    let mut segments = path.split('.');
    let top = segments.next()?;
    let instance = segments.next()?;
    let chan = segments.next()?;
    if segments.next().is_some() || top != "top" {
        return None;
    }
    let socket_prefix = format!("top.{instance}");
    let socketdef = session
        .sim
        .sockets()
        .into_iter()
        .find(|(_, prefix)| prefix.as_slice() == socket_prefix.as_bytes())
        .map(|(socketdef, _)| socketdef)?;
    let role = session.sim.socket_role(socket_prefix.as_bytes())?;
    let symboltable = session.db.get_symboltable();
    let socketdef_symbol = symboltable.symbol(socketdef);
    let parsing = session.db.get_parsing(socketdef_symbol.location().package());
    let socketdef_ast = parsing.ast_node(session.db.get_symbol_ast(socketdef));
    for stmt in socketdef_ast.children() {
        if let AstNodePayload::Channel(channel) = stmt.payload() {
            if parsing.string(channel.name).as_bytes() == chan.as_bytes() {
                return Some((role, channel.dir));
            }
        }
    }
    None
}

/// The REPL's command names, for TAB completion (and `help`'s list).
// Host-only: consumed by the readline REPL and its completions.
#[cfg(not(target_arch = "wasm32"))]
const COMMANDS: [&str; 18] = [
    "cd", "peek", "poke", "watch", "break", "advance", "tick", "assert", "print", "ls", "show", "autoshow", "vcd", "format", "top", "restart", "exit", "help",
];

/// One `show` row: a signal's leaf name, kind, type, and rendered value.
struct ShowRow {
    name: String,
    kind: String,
    typ: String,
    value: String,
}

/// Whether `word` looks like a (possibly partial) path: it starts with
/// an identifier character and continues with identifier, digit, or dot
/// characters.  Used to decide whether to offer path completions inside
/// expressions (`print ctr + <TAB>`).
// Host-only: used by the readline path-completion helper.
#[cfg(not(target_arch = "wasm32"))]
fn looks_like_path(word: &str) -> bool {
    let mut chars = word.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
}

/// The user-facing name of a [`ComponentKind`], as printed by `ls`.
fn kind_name(kind: ComponentKind) -> &'static str {
    match kind {
        ComponentKind::Incoming => "incoming",
        ComponentKind::Outgoing => "outgoing",
        ComponentKind::OutgoingWire => "wire",
        ComponentKind::OutgoingReg => "reg",
        ComponentKind::Reg => "reg",
        ComponentKind::Memory => "memory",
        ComponentKind::Wire => "wire",
        ComponentKind::Bus => "bus",
        ComponentKind::Net => "net",
    }
}

