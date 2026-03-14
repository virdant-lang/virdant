use bstr::{BStr, BString};

use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Region, Source, SourceOffset, Span};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::grammar::PackageParser;
use crate::syntax::token::tokenize;

lalrpop_util::lalrpop_mod!(grammar, "/syntax/grammar.rs");

#[derive(Debug)]
pub struct Parsing {
    pub(super) source: Source,
    pub(super) strings: Vec<BString>,
    pub(super) payloads: Vec<AstNodePayload>,
    pub(super) spans: Vec<Span>,
    pub(super) parents: Vec<AstNodeId>,
    pub(super) num_children: Vec<u16>,
    pub(super) errors: Vec<AstNodeId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternedString(usize);

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString #{}", self.0)
    }
}

#[cfg(test)]
#[test]
fn test_parse() {
    let text = b"mod Top {
    incoming inp : Bit;
    outgoing out : Bit;

    out := inp;
}
";
    let source = Source::new(crate::fqn::PackageFqn::new("top".into()), text.into());
    let parsing = parse(&source);
}

pub fn parse(source: &Source) -> Parsing {
    let mut parsing = Parsing::new(source);

    let parser = PackageParser::new();
    let text = source.text();
    let tokens = tokenize(BStr::new(&text));

    parser.parse(&mut parsing, tokens).unwrap();

    // Fix parents
    {
        parsing.parents = vec![AstNodeId(u16::MAX); parsing.payloads.len()];
        let mut stack: Vec<AstNodeId> = Vec::with_capacity(parsing.payloads.len());
        for i in 0..parsing.payloads.len() {
            let ast_node_id = AstNodeId(i.try_into().unwrap());

            let num_children = parsing.num_children[i];
            for _ in 0..num_children {
                let child_ast_node_id = stack.pop().unwrap();
                parsing.parents[child_ast_node_id.index()] = ast_node_id.clone();
            }

            stack.push(ast_node_id);
        }
    }

    parsing
}

impl Parsing {
    pub fn new(source: &Source) -> Parsing {
        Parsing {
            source: source.clone(),
            strings: vec![],
            payloads: vec![],
            spans: vec![],
            parents: vec![],
            num_children: vec![],
            errors: vec![],
        }
    }

    pub fn package(&self) -> PackageFqn {
        self.source.package()
    }

    pub fn add_node(&mut self, payload: AstNodePayload, span: Span, num_children: u16) -> AstNodeId {
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.spans.push(span);
        self.num_children.push(num_children);
        ast_node_id
    }

    pub fn add_error_node(&mut self, span: Span) -> AstNodeId {
        let payload = AstNodePayload::Error;
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.spans.push(span);
        self.num_children.push(0);
        self.errors.push(ast_node_id.clone());
        ast_node_id
    }

    pub fn span(&mut self, ll: SourceOffset, rr: SourceOffset) -> Span {
        let start = self.source.to_linecol(ll);
        let end = self.source.to_linecol(rr);
        Span::new(start, end)
    }

    pub fn intern(&mut self, span: Span) -> InternedString {
        let string_id = self.strings.len();
        self.strings.push(self.text(span).to_owned());
        InternedString(string_id)
    }

    pub fn text(&self, span: Span) -> &BStr {
        let text = &self.source[span];
        BStr::new(text)
    }

    pub fn string(&self, s: InternedString) -> &BStr {
        BStr::new(&self.strings[s.0])
    }

    pub fn root(&self) -> AstNode<'_> {
        let root_node_id = AstNodeId((self.payloads.len() - 1) as u16);
        self.ast_node(root_node_id)
    }

    pub fn ast_node(&self, ast_node_id: AstNodeId) -> AstNode<'_> {
        let payload = self.payloads[ast_node_id.index()].clone();
        let span = self.spans[ast_node_id.index()].clone();

        let parent = self.parents.get(ast_node_id.index()).cloned();

        AstNode {
            id: ast_node_id,
            payload,
            parent,
            parsing: self,
        }
    }

    pub fn errors(&self) -> Vec<AstNode<'_>> {
        let mut errors = vec![];

        for error_id in &self.errors {
            errors.push(self.ast_node(error_id.clone()));
        }
        errors
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        for error in self.errors() {
            let region = Region::new(self.package(), error.span());
            let diagnostic = diagnostics::ParseError {
                region,
            };
            diagnostics.push(diagnostic.into());
        }
        diagnostics
    }

    pub fn at(&self, linecol: LineCol) -> Option<AstNodeId> {
        let mut result: Option<AstNodeId> = None;

        for (i, span) in self.spans.iter().enumerate() {
            if span.contains(linecol) {
                let ast_node_id = AstNodeId(i.try_into().unwrap());
                if let Some(current) = &result {
                    if self.spans[current.index()].contains_span(*span) {
                        result = Some(ast_node_id);
                    }
                } else {
                    result = Some(ast_node_id);
                }
            }
        }

        result
    }

    pub fn dump(&self) {
        let root_node_id = AstNodeId((self.payloads.len() - 1) as u16);
        self.dump_level(root_node_id, 0);
    }

    fn dump_level(&self, ast_node_id: AstNodeId, level: usize) {
        let node = self.ast_node(ast_node_id);
        node.dump();
    }

    pub fn summary(&self) -> String {
        format!("[Parsing: \"{}\" with {} nodes and {} errors]", self.source.package(), self.payloads.len(), self.errors.len())
    }
}
