//! `vir-parse`: a standalone Virdant parser built around the *ideal*
//! grammar (`src/syntax/ideal.lalrpop`), with its own redesigned AST
//! node structure (see `syntax/payload.rs` and GRAMMAR_REPORT.md).
//!
//! Usage:
//!
//! ```text
//! vir-parse [--tree] <file.vir>
//! ```
//!
//! On success prints a one-line summary (or the whole AST tree with
//! `--tree`) and exits 0.  On a parse error prints the offending
//! location, the unexpected token, and the set of expected terminals,
//! and exits 1.

use std::process::ExitCode;

use bstr::BStr;

mod syntax;

use syntax::parsing::{self, ParseError, Parsing};

fn main() -> ExitCode {
    let mut tree = false;
    let mut path: Option<String> = None;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--tree" => tree = true,
            "--help" | "-h" => {
                println!("usage: vir-parse [--tree] [--compare] <file.vir>");
                return ExitCode::SUCCESS;
            }
            other => path = Some(other.to_owned()),
        }
    }
    let Some(path) = path else {
        eprintln!("usage: vir-parse [--tree] [--compare] <file.vir>");
        return ExitCode::FAILURE;
    };

    let text = match std::fs::read(&path) {
        Ok(text) => text,
        Err(error) => {
            eprintln!("vir-parse: cannot read {path}: {error}");
            return ExitCode::FAILURE;
        }
    };

    match parsing::parse(&text) {
        Ok(parsing) => {
            if tree {
                print!("{}", parse_tree(&parsing));
            } else {
                println!("OK {path}: {} nodes", parsing.num_nodes());
            }
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("{}", render_error(&path, &text, &error));
            ExitCode::FAILURE
        }
    }
}

fn parse_tree(parsing: &Parsing) -> String {
    parsing.dump()
}

fn render_error(path: &str, text: &[u8], error: &ParseError) -> String {
    let source = BStr::new(text);
    let token_text = |(start, _token, end): (virdant::common::source::SourceOffset, _, _)| {
        String::from_utf8_lossy(&source[usize::from(start)..usize::from(end)]).into_owned()
    };
    let join_expected = |expected: &Vec<String>| expected.join(" ");
    match error {
        ParseError::UnrecognizedToken { token, expected } => {
            let (line, col) = linecol(text, usize::from(token.0));
            format!(
                "{path}:{line}:{col}: unexpected token `{}`. Expected one of: {}",
                token_text(*token),
                join_expected(expected),
            )
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let (line, col) = linecol(text, usize::from(*location));
            format!(
                "{path}:{line}:{col}: unexpected end of file. Expected one of: {}",
                join_expected(expected),
            )
        }
        ParseError::InvalidToken { location } => {
            let (line, col) = linecol(text, usize::from(*location));
            format!("{path}:{line}:{col}: invalid token")
        }
        ParseError::ExtraToken { token } => {
            let (line, col) = linecol(text, usize::from(token.0));
            format!(
                "{path}:{line}:{col}: unexpected extra token `{}` after the end of the file",
                token_text(*token),
            )
        }
        ParseError::User { error: _ } => format!("{path}: lexical error"),
    }
}

/// The 1-based (line, column) of a byte offset.
fn linecol(text: &[u8], offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut line_start = 0;
    for (i, byte) in text.iter().enumerate() {
        if i >= offset {
            break;
        }
        if *byte == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    (line, offset - line_start + 1)
}
