//! The `vir-poke` binary: a REPL-driven peek/poke tester built on the
//! `sim` library, with a small custom scripting language whose
//! expressions use Virdant syntax.  Scripts double as assertion tests.
//!
//! `vir poke` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.
//!
//! This module owns the session (the live `Sim` plus the `cd`-style
//! current-module location); `repl` drives the line loop, `commands`
//! executes parsed commands, `eval` evaluates expressions, and
//! `display` renders values round-trippably.

mod ast;
mod commands;
mod display;
mod eval;
mod lexer;
mod repl;

use clap::value_parser;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::ProjectLocation;

/// Assemble the `vir-poke` command line interface.
fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-poke")
        .about("Interactive peek/poke tester")
        .arg(clap::Arg::new("top").long("top").num_args(1)
            .value_parser(value_parser!(String))
            .help("Top module (may be non-pub); defaults to [prog] top"))
        .arg(clap::Arg::new("file").value_name("SCRIPT").num_args(0..=1)
            .value_parser(value_parser!(String))
            .help("Script to preload into the REPL"))
        .arg(clap::Arg::new("script").long("script").value_name("SCRIPT").num_args(0..=1)
            .default_missing_value("")
            .value_parser(value_parser!(String))
            .help("Run a script standalone as an assertion test"));
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);
    let top = matches.get_one::<String>("top").cloned();
    let file = matches.get_one::<String>("file").cloned();
    let script = matches.get_one::<String>("script").cloned();

    // Three forms:
    //   vir poke                  — REPL
    //   vir poke test.poke        — REPL, script preloaded
    //   vir poke test.poke --script / vir poke --script test.poke
    //                             — standalone script run
    let (preload, standalone) = match (file, script.as_deref()) {
        // `vir poke test.poke --script`: bare --script flag.
        (Some(file), Some("")) => (None, Some(file)),
        // `vir poke a.poke --script b.poke`: conflicting.
        (Some(_), Some(_)) => {
            eprintln!("error: cannot give both a script and --script <path>");
            std::process::exit(2);
        }
        (Some(file), None) => (Some(file), None),
        (None, Some("")) => {
            eprintln!("error: --script needs a script file");
            std::process::exit(2);
        }
        (None, Some(path)) => (None, Some(path.to_string())),
        (None, None) => (None, None),
    };

    let code = run(&location, top, preload, standalone);
    std::process::exit(code);
}

// Generated from `grammar.lalrpop` by the build script (into OUT_DIR).

use lalrpop_util::lalrpop_mod;

// Generated from `grammar.lalrpop` by the build script (into OUT_DIR).
lalrpop_mod!(grammar, "/poke/grammar.rs");

use std::cell::Cell;
use std::rc::Rc;

use bstr::ByteSlice;

use virdant::analysis::elaboration::SignalId;
use virdant::analysis::symbols::{SymbolId, SymbolKind};
use virdant::common::ComponentKind;
use virdant::db::Db;
use virdant::sim::{InitStrategy, Sim, Value};
use virdant::types::Type;

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
    /// The file a VCD trace (`vcd`) is being written to, if any.
    /// Cleared by `restart`: the trace lives and dies with the `Sim`.
    pub vcd: Option<String>,
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
            println!("(no components or submodule instances)");
            return;
        }
        rows.sort_by(|a, b| a.0.cmp(&b.0));
        let width = rows.iter().map(|(name, _, _)| name.len()).max().unwrap_or(0);
        for (name, kind, tail) in rows {
            println!("{name:<width$} {kind} {tail}");
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
                println!();
            }
            first = false;
            println!("{}:", header);
            for row in rows {
                // A Clock signal is opaque: its row shows no value.
                if row.value.is_empty() {
                    println!(
                        "  {:<nw$} {:<kw$} : {:<tw$}",
                        row.name,
                        row.kind,
                        row.typ,
                        nw = name_width,
                        kw = kind_width,
                        tw = typ_width
                    );
                } else {
                    println!(
                        "  {:<nw$} {:<kw$} : {:<tw$} = {}",
                        row.name,
                        row.kind,
                        row.typ,
                        row.value,
                        nw = name_width,
                        kw = kind_width,
                        tw = typ_width
                    );
                }
            }
        }
    }

    fn show_row(&self, signal_id: SignalId, leaf: &bstr::BStr) -> ShowRow {
        let value = self.sim.get(signal_id);
        let typ = self.sim.component_type(signal_id);
        // Clocks are opaque: the `show` row carries no value.
        let value_str = if matches!(typ, Type::Clock) {
            String::new()
        } else {
            display::render(&self.sim, &value)
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
            "peek" | "watch" | "break" => {
                if arg_index == 1 {
                    self.complete_path(word, false)
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
            _ => vec![],
        }
    }

    /// Complete a dotted path `partial` against the design's signal
    /// tree.  `module_only` restricts candidates to the steps `cd`
    /// accepts (submodule names, `..`, `top`).
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
                        println!("watch: {}", path);
                    } else {
                        println!("watch: {} = {}", path, display::render(sim, &value));
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
    }
}

/// Whether `signal_id` can be poked: only top-level incoming ports
/// (paths of the form `top.<port>`) and registers at any depth.
/// Anything else is immediately overridden by the next combinational
/// settle, so the tool rejects it rather than silently doing something
/// misleading.
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
    let path = session.sim.full_name(signal_id).to_str_lossy().into_owned();
    Err(format!(
        "cannot poke `{path}`: only top-level incoming ports and registers can be poked (anything else is overridden by the next settle)"
    ))
}

/// The REPL's command names, for TAB completion (and `help`'s list).
const COMMANDS: [&str; 17] = [
    "cd", "peek", "poke", "watch", "break", "advance", "tick", "assert", "print", "ls", "show", "autoshow", "vcd", "prop", "restart", "exit", "help",
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
        ComponentKind::Wire => "wire",
        ComponentKind::Bus => "bus",
        ComponentKind::Net => "net",
    }
}

/// Run `vir poke`.
///
/// `top_arg` is the explicit `--top` (may be non-pub); when absent, the
/// top comes from `[prog] top` in `virdant.toml`.  `preload` is a
/// script preloaded into the REPL; `standalone` a script run as an
/// assertion test.  Returns the process exit code.
pub fn run(
    location: &vircommon::ProjectLocation,
    top_arg: Option<String>,
    preload: Option<String>,
    standalone: Option<String>,
) -> i32 {
    if preload.is_some() && standalone.is_some() {
        eprintln!("error: cannot both preload and run a script standalone");
        return 2;
    }

    let cwd = location.resolve_cwd();
    let db = location.project_db();

    // Resolve the top module: explicit `--top` first, else `[prog] top`.
    let allow_non_pub = top_arg.is_some();
    let top_name = match top_arg {
        Some(name) => name,
        None => match common::read_prog_key(&cwd, "top") {
            Some(name) => name,
            None => {
                eprintln!("error: no top module: pass --top or set [prog] top in virdant.toml");
                return 2;
            }
        },
    };

    let symboltable = db.get_symboltable();
    let exact = symboltable
        .items()
        .into_iter()
        .find(|sym| sym.kind == SymbolKind::ModDef && sym.fqn.as_bytes() == top_name.as_bytes());
    let top_symbol = match exact {
        Some(sym) => sym,
        None => {
            // Fall back to a simple-name match (the top named without
            // its package qualifier).  Ambiguity is an error.
            let matches: Vec<_> = symboltable
                .items()
                .into_iter()
                .filter(|sym| sym.kind == SymbolKind::ModDef && sym.name().as_bytes() == top_name.as_bytes())
                .collect();
            match matches.len() {
                0 => {
                    eprintln!("error: top module '{top_name}' not found");
                    return 2;
                }
                1 => matches[0].clone(),
                _ => {
                    let names: Vec<String> = matches
                        .iter()
                        .map(|sym| sym.fqn().to_str_lossy().into_owned())
                        .collect();
                    eprintln!(
                        "error: top module '{top_name}' is ambiguous: {}",
                        names.join(", ")
                    );
                    return 2;
                }
            }
        }
    };

    // Check the project.  An explicit `--top` may name a non-pub
    // module: the tester bypasses the pub-check by dropping the
    // `TopNotPub` diagnostics (a non-pub mod with a `for` clause) —
    // they gate top selection, which is exactly what `--top` overrides.
    let top_fqn = top_symbol.fqn().to_str_lossy().into_owned();
    let diagnostics = match virdant::util::check_db(&db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let diagnostics: Vec<_> = diagnostics
        .iter()
        .filter(|diag| {
            if allow_non_pub
                && matches!(&diag.payload, virdant::diagnostics::DiagnosticPayload::TopNotPub(_))
            {
                return false;
            }
            true
        })
        .collect();
    let mut failed = false;
    for diagnostic in &diagnostics {
        common::dump_diagnostic(&db, diagnostic);
        if diagnostic.level() == virdant::diagnostics::DiagnosticLevel::Error {
            failed = true;
        }
    }
    if failed {
        eprintln!("Check failed");
        return 1;
    }

    let sim = Sim::new(&db, top_symbol.id(), InitStrategy::Random);
    let session = Session {
        db,
        sim,
        top: top_symbol.id(),
        cwd: Vec::new(),
        it: None,
        break_flag: Rc::new(Cell::new(None)),
        autoshow: false,
        vcd: None,
    };

    let script = if let Some(path) = standalone {
        match read_script(&path) {
            Ok(lines) => Some((lines, true)),
            Err(code) => return code,
        }
    } else if let Some(path) = preload {
        match read_script(&path) {
            Ok(lines) => Some((lines, false)),
            Err(code) => return code,
        }
    } else {
        None
    };

    if script.as_ref().map(|(_, standalone)| !*standalone).unwrap_or(true) {
        println!("vir poke: top {top_fqn} (type `exit` to leave, `help` for commands)");
    }
    repl::run(session, script)
}

/// Read a `.poke` script file.
fn read_script(path: &str) -> Result<Vec<(usize, String)>, i32> {
    match std::fs::read_to_string(path) {
        Ok(text) => Ok(text
            .lines()
            .enumerate()
            .map(|(i, line)| (i + 1, line.to_string()))
            .collect()),
        Err(e) => {
            eprintln!("error: cannot read script {path}: {e}");
            Err(2)
        }
    }
}
