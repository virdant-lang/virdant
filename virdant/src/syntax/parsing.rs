use bstr::{BStr, BString};

use crate::source::{Region, Source, SourceOffset, Span};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::grammar::PackageParser;
use crate::syntax::token::tokenize;

lalrpop_util::lalrpop_mod!(grammar);

#[derive(Debug)]
pub struct Parsing {
    pub(crate) source: Source,
    strings: Vec<BString>,

    payloads: Vec<AstNodePayload>,
    pub(crate) spans: Vec<Span>,
    pub(crate) parents: Vec<AstNodeId>,
    pub(crate) num_children: Vec<u16>,
    errors: Vec<AstNodeId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternedString(usize);

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString #{}", self.0)
    }
}

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

    dbg!(&parsing);
    parsing.dump();

    parsing
}

impl Parsing {
    pub(crate) fn new(source: &Source) -> Parsing {
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

    pub(crate) fn add_node(&mut self, payload: AstNodePayload, span: Span, num_children: u16) -> AstNodeId {
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.spans.push(span);
        self.num_children.push(num_children);
        ast_node_id
    }

    pub(crate) fn add_error_node(&mut self, span: Span) -> AstNodeId {
        todo!()
    }

    pub(crate) fn span(&mut self, ll: SourceOffset, rr: SourceOffset) -> Span {
        let start = self.source.to_linecol(ll);
        let end = self.source.to_linecol(rr);
        Span::new(start, end)
    }

    pub(crate) fn intern(&mut self, span: Span) -> InternedString {
        let string_id = self.strings.len();
        self.strings.push(self.text(span).to_owned());
        InternedString(string_id)
    }

    pub(crate) fn text(&self, span: Span) -> &BStr {
        let text = &self.source[span];
        BStr::new(text)
    }

    pub fn ast_node(&self, ast_node_id: AstNodeId) -> AstNode {
        let payload = self.payloads[ast_node_id.index()].clone();
        let span = self.spans[ast_node_id.index()].clone();

        let parent = self.parents.get(ast_node_id.index()).cloned();
        let region = Region::new(self.source.package(), span);

        AstNode {
            id: ast_node_id,
            payload,
            region,
            parent,
            parsing: self,
        }
    }

    pub fn dump(&self) {
        let root_node_id = AstNodeId((self.payloads.len() - 1) as u16);
        self.dump_level(root_node_id, 0);
    }

    fn dump_level(&self, ast_node_id: AstNodeId, level: usize) {
        let node = self.ast_node(ast_node_id);
        node.dump();
    }
}
