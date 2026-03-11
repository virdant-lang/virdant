use bstr::{BStr, ByteSlice};

//pub mod parser_lalrpop;
//pub use parser_lalrpop as parser;

use crate::source::{Region, Span};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::Parsing;
//use crate::stringtable::{InternedString, StringTable};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNodeId(pub(crate) u16);

#[derive(Clone)]
pub struct AstNode<'a> {
    pub(crate) parsing: &'a Parsing,
    pub(crate) id: AstNodeId,
    pub(crate) payload: AstNodePayload,
    pub(crate) region: Region,
    pub(crate) parent: Option<AstNodeId>,
}

/*
    pub fn errors(&self) -> Vec<AstNode> {
        let mut errors = vec![];
        for ast_node_id in &self.errors {
            errors.push(self.ast_node(ast_node_id.clone()));
        }
        errors
    }

    pub fn stringtable(&self) -> StringTable {
        self.stringtable.clone()
    }
*/

impl<'p> std::fmt::Debug for AstNode<'p> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.id, &self.payload)
    }
}

impl<'p> AstNode<'p> {
    pub fn id(&self) -> AstNodeId {
        self.id.clone()
    }

    pub fn spelling(&self) -> &BStr {
        self.parsing.source[self.region.span()].as_bstr()
    }

    pub fn parent(&self) -> Option<AstNode> {
        if let Some(parent) = &self.parent {
            Some(self.parsing.ast_node(parent.clone()))
        } else {
            None
        }
    }

    pub fn child(&self, mut n: u16) -> AstNode {
        let mut ast_node_id = 0;
        loop {
            if self.parsing.parents[ast_node_id] == self.id {
                if n == 0 {
                    let ast_node_id = AstNodeId(ast_node_id.try_into().unwrap());
                    return self
                        .parsing
                        .ast_node(ast_node_id);
                }
                n -= 1;
            }
            ast_node_id += 1;
        }
    }

    pub fn children(&self) -> Vec<AstNode> {
        let mut result = vec![];
        let num_children = self.parsing.num_children[self.id.index()];
        for i in 0..num_children {
            result.push(self.child(i.try_into().unwrap()));
        }
        result
    }

/*
    pub fn walk(&self) -> Vec<AstNode> {
        let mut result = vec![];
        let mut queue = vec![self.clone()];
        while let Some(node) = queue.pop() {
            result.push(node.clone());
            for child in node.children() {
                queue.push(child);
            }
        }
        result
    }
*/

    pub fn payload(&self) -> AstNodePayload {
        self.payload.clone()
    }

    pub fn span(&self) -> Span {
        self.parsing.spans[self.id.index()].clone()
    }

    #[allow(dead_code)]
    pub(crate) fn dump(&self) {
        self.dump_level(0);
    }

    #[allow(dead_code)]
    pub(crate) fn dump_level(&self, level: usize) {
        use bstr::io::BufReadExt;

        let padding = " ".repeat(4 * level);
        let spelling = self.spelling();
        if spelling.byte_lines().collect::<Vec<_>>().len() == 1 {
            eprintln!(
                "{padding}{:?} {:?} @ {} {spelling:?}",
                self.payload(),
                self.id(),
                self.span()
            );
        } else {
            eprintln!(
                "{padding}{:?} {:?} @ {}",
                self.payload(),
                self.id(),
                self.span()
            );
        }
        for child in self.children() {
            child.dump_level(level + 1);
        }
    }
}

impl AstNodeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}
