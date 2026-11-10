//! The `Parsing` structure owning the parsed AST (payloads, spans,
//! child counts, parents) plus the interned string table, the `parse`
//! entry point running the lalrpop `PackageParser`, and a tree dump
//! for inspection.
//!
//! This is a deliberately minimal counterpart of the production
//! `virdant::syntax::parsing::Parsing`: it carries exactly what the
//! ideal grammar's actions need and nothing more.  (The production
//! parser also folds several textual lint checks — tabs, indentation,
//! trailing spaces, doccomment spacing — into `parse`; those are lint
//! passes, not grammar, so they have no place here.)

use bstr::BStr;

use virdant::common::source::SourceOffset;
use virdant::syntax::token::{Token, TokenError, tokenize};

use crate::syntax::payload::{NodeId, Payload};

pub type ParseError = lalrpop_util::ParseError<SourceOffset, Token, TokenError>;

lalrpop_util::lalrpop_mod!(grammar, "/syntax/ideal.rs");

#[derive(Debug)]
pub struct Parsing {
    text: Vec<u8>,
    line_starts: Vec<u32>,
    strings: Vec<Vec<u8>>,
    payloads: Vec<Payload>,
    spans: Vec<(u32, u32)>,
    num_children: Vec<u16>,
    parents: Vec<NodeId>,
}

/// Parse a Virdant source file with the ideal grammar.
pub fn parse(text: &[u8]) -> Result<Parsing, ParseError> {
    let mut parsing = Parsing::new(text);
    let parser = grammar::PackageParser::new();
    parser.parse(&mut parsing, tokenize(BStr::new(text)))?;
    parsing.fix_parents();
    Ok(parsing)
}

impl Parsing {
    pub fn new(text: &[u8]) -> Parsing {
        let mut line_starts = vec![0u32];
        for (i, byte) in text.iter().enumerate() {
            if *byte == b'\n' {
                line_starts.push(i as u32 + 1);
            }
        }
        Parsing {
            text: text.to_vec(),
            line_starts,
            strings: vec![],
            payloads: vec![],
            spans: vec![],
            num_children: vec![],
            parents: vec![],
        }
    }

    /// Add a node with an explicit child list (in source order).
    pub fn node(&mut self, payload: Payload, ll: SourceOffset, rr: SourceOffset, children: Vec<NodeId>) -> NodeId {
        let num_children = u16::try_from(children.len()).expect("more than 65535 children");
        self.add_node(payload, ll, rr, num_children)
    }

    fn add_node(&mut self, payload: Payload, ll: SourceOffset, rr: SourceOffset, num_children: u16) -> NodeId {
        let node_id = self.payloads.len();
        self.payloads.push(payload);
        self.spans.push((usize::from(ll) as u32, usize::from(rr) as u32));
        self.num_children.push(num_children);
        node_id
    }

    /// Intern the source text of a span.
    pub fn intern(&mut self, ll: SourceOffset, rr: SourceOffset) -> usize {
        let text = self.span_text(ll, rr).to_vec();
        if let Some(id) = self.strings.iter().position(|string| *string == text) {
            id
        } else {
            let id = self.strings.len();
            self.strings.push(text);
            id
        }
    }

    pub fn sym_text(&self, sym: usize) -> &[u8] {
        &self.strings[sym]
    }

    /// Parse a `Nat` literal (ignoring digit separators) into a `u16`,
    /// saturating on overflow instead of panicking.
    pub fn nat_u16(&self, ll: SourceOffset, rr: SourceOffset) -> u16 {
        let text = self.span_text(ll, rr);
        let cleaned: String = text.iter().filter(|c| **c != b'_').map(|c| *c as char).collect();
        cleaned.parse::<u16>().unwrap_or(u16::MAX)
    }

    fn span_text(&self, ll: SourceOffset, rr: SourceOffset) -> &[u8] {
        let start = usize::from(ll);
        let end = usize::from(rr);
        &self.text[start..end]
    }

    /// The 1-based (line, column) of a source offset.
    pub fn linecol(&self, offset: SourceOffset) -> (u32, u32) {
        let offset = usize::from(offset) as u32;
        let line = match self.line_starts.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let col = offset - self.line_starts[line];
        (line as u32 + 1, col + 1)
    }

    pub fn span_linecol(&self, node_id: NodeId) -> ((u32, u32), (u32, u32)) {
        let (ll, rr) = self.spans[node_id];
        (self.linecol(SourceOffset(ll)), self.linecol(SourceOffset(rr)))
    }

    /// Reconstruct parent links from the child counts: a node's
    /// children are the most recently created nodes at the time the
    /// node itself is created.
    fn fix_parents(&mut self) {
        self.parents = (0..self.payloads.len()).collect();
        let mut stack: Vec<NodeId> = Vec::with_capacity(self.payloads.len());
        for i in 0..self.payloads.len() {
            for _ in 0..self.num_children[i] {
                let child = stack.pop().expect("child underflow");
                self.parents[child] = i;
            }
            stack.push(i);
        }
    }

    /// Render the whole tree, one node per line, indented by depth.
    pub fn dump(&self) -> String {
        let mut child_lists: Vec<Vec<NodeId>> = vec![Vec::new(); self.payloads.len()];
        for (child, parent) in self.parents.iter().enumerate() {
            if child != *parent {
                child_lists[*parent].push(child);
            }
        }
        let root = self.payloads.len() - 1;
        let mut out = String::new();
        self.dump_level(root, 0, &child_lists, &mut out);
        out
    }

    fn dump_level(&self, node_id: NodeId, depth: usize, child_lists: &[Vec<NodeId>], out: &mut String) {
        let payload = &self.payloads[node_id];
        let (line, col) = self.span_linecol(node_id).0;
        for _ in 0..depth {
            out.push_str("  ");
        }
        out.push_str(payload.kind());
        let detail = payload.detail(self);
        if !detail.is_empty() {
            out.push(' ');
            out.push_str(&detail);
        }
        out.push_str(&format!(" [{}:{}]\n", line, col));
        for child in child_lists[node_id].clone() {
            self.dump_level(child, depth + 1, child_lists, out);
        }
    }

    pub fn num_nodes(&self) -> usize {
        self.payloads.len()
    }
}
