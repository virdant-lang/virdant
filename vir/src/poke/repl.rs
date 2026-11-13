//! The `vir poke` line loop: one shared driver for the interactive
//! REPL, a preloaded script (which pauses on the first failed `assert`
//! and drops into the interactive REPL), and a standalone script run
//! (which exits nonzero on the first failure).
//!
//! The interactive prompt is readline-style (line editing, history,
//! TAB completion) via `rustyline`; when stdin is not a terminal
//! (tests, piped sessions) it degrades to a plain line loop.

use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;

use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::history::FileHistory;
use rustyline::validate::Validator;
use rustyline::{CompletionType, Config, Context, Editor, Helper};

use super::ast::Command;
use super::commands::{exec, Flow, SourceLine};
use super::{grammar, lexer, Session};

/// How the session was started.
enum Mode {
    /// Direct REPL: errors print and the session continues.
    Interactive,
    /// A script preloaded into the REPL: a failed `assert` pauses the
    /// script and returns control to the interactive prompt.
    Preloaded,
    /// A script run as an assertion test: any failure stops the run
    /// with a nonzero exit code.
    Standalone,
}

/// Run the session.  `script` is `(lines with their numbers,
/// standalone?)`.  Returns the process exit code.
pub fn run(session: Session, script: Option<(Vec<(usize, String)>, bool)>) -> i32 {
    // Shared with the readline helper so TAB completion can inspect
    // the live session.
    let session = Rc::new(RefCell::new(session));
    let (lines, standalone) = match script {
        Some((lines, standalone)) => (Some(lines), standalone),
        None => (None, false),
    };
    let mode = if standalone {
        Mode::Standalone
    } else if lines.is_some() {
        Mode::Preloaded
    } else {
        Mode::Interactive
    };

    if let Some(lines) = lines {
        match run_script(&mut session.borrow_mut(), &lines, &mode) {
            ScriptStop::Finished | ScriptStop::Exit => return 0,
            ScriptStop::Failed(code) => return code,
            ScriptStop::Paused => {
                // A preloaded script hit a failed assert or a break:
                // fall through to the interactive prompt.
            }
        }
    }

    if matches!(mode, Mode::Standalone) {
        return 0;
    }

    interactive(session)
}

enum ScriptStop {
    /// The script ran to the end (pass).
    Finished,
    /// The script ended with `exit`.
    Exit,
    /// The script failed: exit with the given code.
    Failed(i32),
    /// A preloaded script paused (failed assert or break): the
    /// interactive prompt takes over.
    Paused,
}

fn run_script(session: &mut Session, lines: &[(usize, String)], mode: &Mode) -> ScriptStop {
    for (number, text) in lines {
        let trimmed = text.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        match handle_line(session, text, Some((*number, text.clone()))) {
            LineResult::Continue => {}
            LineResult::Exit => return ScriptStop::Exit,
            LineResult::Error(message) => {
                eprintln!("error (line {number}): {message}");
                return match mode {
                    Mode::Standalone => ScriptStop::Failed(1),
                    // A preloaded script with an error drops into the
                    // interactive prompt, like a failed assert.
                    _ => ScriptStop::Paused,
                };
            }
            LineResult::AssertFailed(message) | LineResult::BreakHit(message) => {
                println!("{message}");
                return match mode {
                    Mode::Standalone => ScriptStop::Failed(1),
                    _ => ScriptStop::Paused,
                };
            }
        }
    }
    ScriptStop::Finished
}

/// The interactive prompt.  With a terminal, a readline-style loop
/// (editing, history, TAB completion); with piped stdin (tests,
/// scripted sessions), a plain line loop.
fn interactive(session: Rc<RefCell<Session>>) -> i32 {
    if unsafe { libc::isatty(libc::STDIN_FILENO) } == 1 {
        interactive_tty(session)
    } else {
        interactive_basic(&mut session.borrow_mut())
    }
}

/// The plain loop: used when stdin is not a terminal or when the
/// readline editor cannot start.  Reads one line at a time with no
/// editing support.
fn interactive_basic(session: &mut Session) -> i32 {
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    loop {
        print!("{}> ", session.prefix());
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        match stdin.read_line(&mut line) {
            Ok(0) | Err(_) => {
                // EOF (Ctrl+D): leave.
                println!();
                return 0;
            }
            Ok(_) => {}
        }
        let trimmed = line.trim();
        if trimmed == "help" {
            print_help();
            continue;
        }
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        match handle_line(session, &line, None) {
            LineResult::Continue => {}
            LineResult::Exit => return 0,
            LineResult::Error(message) => eprintln!("error: {message}"),
            // In the interactive prompt, a failed assert or a break
            // prints and the session continues.
            LineResult::AssertFailed(message) | LineResult::BreakHit(message) => {
                println!("{message}");
            }
        }
    }
}

/// The readline-style loop: line editing, history, and TAB completion
/// (command names and signal paths, via [`Session::completions`]).
fn interactive_tty(session: Rc<RefCell<Session>>) -> i32 {
    let config = Config::builder()
        .completion_type(CompletionType::List)
        .build();
    let mut editor: Editor<PokeHelper, FileHistory> = match Editor::with_config(config) {
        Ok(editor) => editor,
        Err(error) => {
            // The plain loop is better than failing outright.
            eprintln!("warning: readline unavailable ({error})");
            return interactive_basic(&mut session.borrow_mut());
        }
    };
    editor.set_helper(Some(PokeHelper { session: session.clone() }));

    loop {
        let prompt = session.borrow().prefix();
        match editor.readline(&format!("{prompt}> ")) {
            Ok(line) => {
                let trimmed = line.trim();
                if !trimmed.is_empty() {
                    let _ = editor.add_history_entry(trimmed);
                }
                if trimmed == "help" {
                    print_help();
                } else if !trimmed.is_empty() && !trimmed.starts_with("//") {
                    let mut borrowed = session.borrow_mut();
                    match handle_line(&mut borrowed, &line, None) {
                        LineResult::Continue => {}
                        LineResult::Exit => return 0,
                        LineResult::Error(message) => eprintln!("error: {message}"),
                        // In the interactive prompt, a failed assert
                        // or a break prints and the session continues.
                        LineResult::AssertFailed(message) | LineResult::BreakHit(message) => {
                            println!("{message}");
                        }
                    }
                }
            }
            // Ctrl+D (EOF) and Ctrl+C both leave the session.
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => {
                println!();
                return 0;
            }
            Err(error) => {
                eprintln!("readline error: {error}");
                return 0;
            }
        }
    }
}

/// The rustyline helper: TAB completion over command names and signal
/// paths (see [`Session::completions`]).
struct PokeHelper {
    session: Rc<RefCell<Session>>,
}

impl Helper for PokeHelper {}

impl Completer for PokeHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        // The word being completed starts after the last whitespace
        // before the cursor.  The script language is ASCII-only, so
        // byte offsets are fine.
        let word_start = line[..pos]
            .rfind(char::is_whitespace)
            .map_or(0, |index| index + 1);
        let candidates = {
            let session = self.session.borrow();
            session.completions(&line[..pos])
        };
        Ok((word_start, candidates))
    }
}

impl Hinter for PokeHelper {
    type Hint = String;
}

impl Highlighter for PokeHelper {}

impl Validator for PokeHelper {}

enum LineResult {
    Continue,
    Exit,
    Error(String),
    AssertFailed(String),
    BreakHit(String),
}

/// Parse and execute one line, mapping the command outcome to a
/// [`LineResult`].
fn handle_line(
    session: &mut Session,
    line: &str,
    source: Option<(usize, String)>,
) -> LineResult {
    let mut command = match parse_line(line) {
        Ok(Some(command)) => command,
        Ok(None) => return LineResult::Continue,
        Err(error) => return LineResult::Error(error),
    };
    // Attach the raw source text to `assert` commands so failure
    // diagnostics can quote the line.
    if let Command::Assert { line, .. } = &mut command {
        *line = line.trim().to_string();
    }
    let source_line = source.map(|(number, text)| SourceLine { number, text });
    match exec(session, &command, source_line.as_ref()) {
        Ok(Flow::Continue) => LineResult::Continue,
        Ok(Flow::Exit) => LineResult::Exit,
        Ok(Flow::AssertFailed(message)) => LineResult::AssertFailed(message),
        Ok(Flow::BreakHit(message)) => LineResult::BreakHit(message),
        Err(error) => LineResult::Error(error),
    }
}

/// Parse one line into a [`Command`].  Lines that are empty or only a
/// comment produce no tokens; the caller treats `None` as a no-op.
fn parse_line(line: &str) -> Result<Option<Command>, String> {
    let tokens = lexer::lex(line).map_err(|e| format!("{e}"))?;
    if tokens.is_empty() {
        return Ok(None);
    }
    grammar::LineParser::new()
        .parse(tokens)
        .map(|command| Some(command))
        .map_err(|e| format!("syntax error: {e}"))
}

fn print_help() {
    println!(
        "commands:
  cd <path>          drill into a submodule (cd .. up, cd top back to the top)
  ls                 list components (kind + type) and submodule/socket instances
  show               print the value of every signal in the design, by module
  autoshow [on|off]  show the design after every poke/tick (no arg toggles)
  vcd <file>         write a VCD trace to the file from now on
  peek <path>        print a signal's value (sets `it`)
  poke <path> <expr> drive a top-level incoming port or a register
  watch <path>       print a signal whenever it changes
  break <path>       stop after the next change of a signal
  advance [n]        move time forward n timesteps, no clock needed (default 1)
  tick [n] [<clock>] apply n clock edges (default 1)
  assert <expr>      fail the script if the expression is false
  print <expr>       evaluate and print an expression
  restart            re-initialize the simulation
  top <module>       reload the simulation with a new top module
  exit               leave the session

TAB completes command and signal-path names; up/down walk the history.

paths are relative to the current module; `top.` prefixes are absolute;
expressions use Virdant syntax (literals, + - & | ^, comparisons,
&& || ^^, ~ ! -, x[i], x[hi..lo], @Ctor, ${{Struct}})."
    );
}
