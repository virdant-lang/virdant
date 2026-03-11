use bstr::BStr;

use crate::source::{Region, Source, SourceOffset, Span};
use crate::syntax::ast::{AstNode, AstNodeId, AstNodePayload};

lalrpop_util::lalrpop_mod!(grammar);

pub struct Parsing {
    source: Source,
    strings: Vec<String>,

    payloads: Vec<AstNodePayload>,
    spans: Vec<Span>,
    parents: Vec<AstNodeId>,
    num_children: Vec<u16>,
    errors: Vec<AstNodeId>,
}

pub type InternedString = usize;

pub fn parse(source: &Source) -> Parsing {
    Parsing::new(source)
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
        todo!()
    }

    pub(crate) fn add_error_node(&mut self, span: Span) -> AstNodeId {
        todo!()
    }

    pub(crate) fn span(&mut self, ll: SourceOffset, rr: SourceOffset) -> Span {
        todo!()
    }

    pub(crate) fn intern(&mut self, span: Span) -> InternedString {
        todo!()
    }

    pub(crate) fn text(&self, span: Span) -> &BStr {
        let text = &self.source[span];
        BStr::new(text)
    }

    pub fn ast_node(&self, ast_node_id: AstNodeId) -> AstNode {
        let payload = self.payloads[ast_node_id.index()].clone();
        let span = self.spans[ast_node_id.index()].clone();
        let parent = self.parents[ast_node_id.index()].clone();
        let region = Region::new(self.source.package(), span);

        AstNode {
            id: ast_node_id,
            payload,
            region,
            parent,
            parser: self,
        }
    }
}

/*
pub fn parse(ast_data: &mut AstData, stringtable: &StringTable, source: &Source) {
    let parser = grammar::PackageParser::new();
    let text = source.text();
    let tokens = tokenize(BStr::new(text.as_ref()));

    let _ = parser.parse(ast_data, stringtable, source, tokens);
    let package = ast_data.package();

    ast_data.parents = vec![AstNodeId(package.clone(), u16::MAX); ast_data.payloads.len()];
    let mut stack: Vec<AstNodeId> = Vec::with_capacity(ast_data.payloads.len());
    for i in 0..ast_data.payloads.len() {
        let ast_node_id = AstNodeId(package.clone(), i.try_into().unwrap());

        let num_children = ast_data.num_children[i];
        for _ in 0..num_children {
            let child_ast_node_id = stack.pop().unwrap();
            ast_data.parents[child_ast_node_id.index()] = ast_node_id.clone();
        }

        stack.push(ast_node_id);
    }
}
*/
