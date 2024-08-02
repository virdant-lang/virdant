//! Utilities for parsing a Virdant source file.
//!
//! [`parse_package()`](parse_package) is used to parse a package.
//! This results in a [`Ast`] object (or a [`ParseError`] on failure).

use pest::error::Error;
use pest::error::LineColLocation;

use pest_derive::Parser;

use crate::ast::Ast;
use crate::location::{Pos, Span};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Parser;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualIdent(Option<String>, String);

impl QualIdent {
    pub fn new(qualident: &str) -> QualIdent {
        if let Some(idx) = qualident.find("::") {
            let package = qualident[..idx].to_string();
            let name = qualident[idx+2..].to_string();
            QualIdent(Some(package), name)
        } else {
            QualIdent(None, qualident.to_string())
        }
    }

    pub fn package(&self) -> Option<&String> {
        self.0.as_ref()
    }

    pub fn name(&self) -> &str {
        &self.1
    }

    pub fn in_package(&self, in_package: &str) -> QualIdent {
        if self.0.is_some() {
            self.clone()
        } else {
            QualIdent(Some(in_package.to_string()), self.name().to_string())
        }
    }
}

impl std::fmt::Display for QualIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name();
        if let Some(package) = self.package() {
            write!(f, "{package}::{name}")
        } else {
            write!(f, "{name}")
        }
    }
}

/// Parse a Virdant package
pub fn parse_package<'a>(text: &'a str) -> Result<Ast<'a>, ParseError> {
    use pest::Parser as PestParser;
    Parser::parse(Rule::package, text)
        .map(|mut pairs| Ast::new(pairs.next().unwrap()))
        .map_err(|err| ParseError(err))
}

/// An error encountered during parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(Error<Rule>);

impl ParseError {
    /// Where in the source file did the span occur.
    pub fn span(&self) -> Span {
        match self.err().line_col {
            LineColLocation::Pos((line, col)) => {
                let start = Pos::new(line, col);
                let end = Pos::new(line, col);
                Span::new(start, end)
            },
            LineColLocation::Span(start, end) => {
                let (start_line, start_col) = start;
                let (end_line, end_col) = end;
                Span::new(Pos::new(start_line, start_col), Pos::new(end_line, end_col))
            },
        }

    }

    pub fn message(&self) -> String {
        "Syntax Error".to_owned()
    }

    fn err(&self) -> &Error<Rule> {
        &self.0
    }
}
