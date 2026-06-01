use bstr::{BStr, BString};

use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::common::source::{LineCol, Region, Source, SourceOffset, Span};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::grammar::PackageParser;
use crate::syntax::token::{Token, TokenError, tokenize};

lalrpop_util::lalrpop_mod!(grammar, "/syntax/virdant.rs");

pub type ParseError = lalrpop_util::ErrorRecovery<SourceOffset, Token, TokenError>;

#[derive(Debug)]
pub struct Parsing {
    pub(super) source: Source, // TODO Do I need the source?
    pub(super) strings: Vec<BString>, // TODO don't be pub(super)
    pub(super) payloads: Vec<AstNodePayload>,
    pub(super) spans: Vec<Span>,
    pub(super) parents: Vec<AstNodeId>,
    pub(super) num_children: Vec<u16>,
    pub(super) errors: Vec<AstNodeId>,
    pub(super) error_data: Vec<ParseError>,
    pub(super) docstring_diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString {
    package: PackageFqn,
    id: usize,
}

impl InternedString {
    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString {}#{}", self.package, self.id)
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
    let _ = parse(&source);
}

pub fn parse(source: &Source) -> Parsing {
    let mut parsing = Parsing::new(source);

    let parser = PackageParser::new();
    let text = source.text();
    let tokens = tokenize(BStr::new(&text));

    parser.parse(&mut parsing, tokens).unwrap();

    // Fix parents
    {
        let _root_id = AstNodeId(u16::try_from(parsing.payloads.len()).unwrap() - 1);
        parsing.parents = (0..parsing.payloads.len())
            .map(|i| AstNodeId(i.try_into().unwrap()))
            .collect();
        let mut stack: Vec<AstNodeId> = Vec::with_capacity(parsing.payloads.len());
        for i in 0..parsing.payloads.len() {
            let ast_node_id = AstNodeId(i.try_into().unwrap());

            let num_children = parsing.num_children[i];
            for _ in 0..num_children {
                let child_ast_node_id = stack.pop().unwrap();
                parsing.parents[child_ast_node_id.index()] = ast_node_id;
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
            error_data: vec![],
            docstring_diagnostics: vec![],
        }
    }

    pub fn package(&self) -> PackageFqn {
        self.source.package()
    }

    pub(super) fn add_node(&mut self, payload: AstNodePayload, span: Span, num_children: u16) -> AstNodeId {
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.spans.push(span);
        self.num_children.push(num_children);
        ast_node_id
    }

    pub(super) fn add_error_node(&mut self, span: Span, error: ParseError) -> AstNodeId {
        let payload = AstNodePayload::Error;
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.spans.push(span);
        self.num_children.push(0);
        self.errors.push(ast_node_id);
        self.error_data.push(error);
        ast_node_id
    }

    pub fn span(&mut self, ll: SourceOffset, rr: SourceOffset) -> Span {
        let start = self.source.to_linecol(ll);
        let end = self.source.to_linecol(rr);
        Span::new(start, end)
    }

    pub(super) fn intern(&mut self, span: Span) -> InternedString {
        let text = self.text(span).to_owned();
        let package = self.package();
        if let Some(string_id) = self.strings.iter().position(|string| string == &text) {
            InternedString {
                package,
                id: string_id,
            }
        } else {
            let string_id = self.strings.len();
            self.strings.push(text);
            InternedString {
                package,
                id: string_id,
            }
        }
    }

    pub fn text(&self, span: Span) -> &BStr {
        let text = &self.source[span];
        BStr::new(text)
    }

    // TODO check to make sure string table is deduplicated
    pub fn string(&self, s: InternedString) -> &BStr {
        assert_eq!(self.package(), s.package);
        BStr::new(&self.strings[s.id])
    }

    pub fn root(&self) -> AstNode<'_> {
        let root_node_id = AstNodeId((self.payloads.len() - 1) as u16);
        self.ast_node(root_node_id)
    }

    pub fn ast_node(&self, ast_node_id: AstNodeId) -> AstNode<'_> {
        let payload = self.payloads[ast_node_id.index()].clone();
        let _span = self.spans[ast_node_id.index()];

        let parent_id = self.parents[ast_node_id.index()];
        let parent = if parent_id == ast_node_id {
            None
        } else {
            Some(parent_id)
        };

        AstNode {
            id: ast_node_id,
            payload,
            parent,
            parsing: self,
        }
    }

    pub fn errors(&self) -> Vec<(AstNode<'_>, ParseError)> {
        let mut errors = vec![];

        for (error_id, error_data) in self.errors.iter().zip(self.error_data.iter().cloned()) {
            errors.push((self.ast_node(*error_id), error_data));
        }
        errors
    }

    pub(super) fn add_docstring_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.docstring_diagnostics.push(diagnostic);
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        diagnostics.extend(self.docstring_diagnostics.clone());
        for (error, error_data) in self.errors() {
            let (message, region) = match error_data.error {
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                    let (start, _token, end) = token;
                    let region = self.source.to_region(start, end);
                    let expected_tokens = expected.into_iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");
                    let token_str = BStr::new(&self.source[region.span()]);
                    let message = format!("Unexpected token: \"{token_str}\". Expected: {expected_tokens}").into();
                    (message, region)
                }
                lalrpop_util::ParseError::InvalidToken { location:_ } => {
                    let region = Region::new(self.package(), error.span());
                    let message = "Invalid token".into();
                    (message, region)
                }
                lalrpop_util::ParseError::UnrecognizedEof { location:_, expected: _ } => {
                    let region = Region::new(self.package(), error.span());
                    let message = "Unexpected end of file".into();
                    (message, region)
                }
                lalrpop_util::ParseError::ExtraToken { token: _ } => {
                    let region = Region::new(self.package(), error.span());
                    let message = "Expected end of file".into();
                    (message, region)
                }
                lalrpop_util::ParseError::User { error: _ } => unreachable!(),
            };
            let diagnostic = diagnostics::SyntaxError {
                region,
                message,
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

    fn dump_level(&self, ast_node_id: AstNodeId, _level: usize) {
        let node = self.ast_node(ast_node_id);
        node.dump();
    }

    pub fn summary(&self) -> String {
        format!("[Parsing: \"{}\" with {} nodes and {} errors]", self.source.package(), self.payloads.len(), self.errors.len())
    }
}
