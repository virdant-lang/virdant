use super::payload;

use std::sync::Arc;

use bstr::{BStr, ByteSlice};

//pub mod parser_lalrpop;
//pub use parser_lalrpop as parser;

use crate::fqn::PackageFqn;
use crate::source::{Region, Source, SourceOffset, Span};
use crate::syntax::parsing::Parsing;
//use crate::stringtable::{InternedString, StringTable};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNodeId(PackageFqn, u16);

#[derive(Debug, Clone)]
pub enum AstNodePayload {
    Error,

    Package,

    Import(payload::Import),
    ModDef(payload::ModDef),
    StructDef(payload::StructDef),
    UnionDef(payload::UnionDef),
    EnumDef(payload::EnumDef),
    BuiltinDef(payload::BuiltinDef),
    FnDef(payload::FnDef),
    SocketDef(payload::SocketDef),

    Component(payload::Component),
    Driver(payload::Driver),
    BidirectionalDriver,
    Module(payload::Module),
    Socket(payload::Socket),

    Field(payload::Field),
    Ctor(payload::Ctor),
    Enumerant(payload::Enumerant),
    Channel(payload::Channel),

    Param(payload::Param),

    Kind(payload::Kind),
    Type(payload::Type),

    ExprReference,
    ExprParen,
    ExprIf,
    ExprMatch,
    ExprBitLit(payload::ExprBitLit),
    ExprWordLit(payload::ExprWordLit),
    ExprBinOp(payload::ExprBinOp),
    ExprUnOp(payload::ExprUnOp),
    ExprMethod(payload::ExprMethod),
    ExprFn,
    ExprCtor(payload::ExprCtor),
    ExprEnumerant(payload::ExprEnumerant),
    ExprStruct,
    ExprIndex(payload::ExprIndex),
    ExprIndexRange(payload::ExprIndexRange),
    ExprWord,
    ExprZext,
    ExprSext,

    Assign(payload::Assign),

    PatIdent(payload::PatIdent),
    PatEnumerant(payload::PatEnumerant),
    PatElse,

    Ofness(payload::Ofness),
    Path,
}

#[derive(Clone)]
pub struct AstNode<'a> {
    pub(crate) parser: &'a Parsing,
    pub(crate) id: AstNodeId,
    pub(crate) payload: AstNodePayload,
    pub(crate) region: Region,
    pub(crate) parent: AstNodeId,
}

/*
    pub fn new(source: Source, stringtable: StringTable) -> Ast {
        const INIT_CAP: usize = 4096;
        const ERROR_CAP: usize = 16;

        let mut ast_data = AstData {
            source: source.clone(),
            stringtable: stringtable.clone(),
            payloads: Vec::with_capacity(INIT_CAP),
            regions: Vec::with_capacity(INIT_CAP),
            parents: Vec::with_capacity(INIT_CAP),
            num_children: Vec::with_capacity(INIT_CAP),
            errors: Vec::with_capacity(ERROR_CAP),
        };

        parser::parse(&mut ast_data, stringtable, source);

        Ast(Arc::new(ast_data))
    }

    pub fn root(&self) -> node::Package {
        let ast_node_id: u16 = (self.payloads.len() - 1).try_into().unwrap();
        node::Package(self.clone(), AstNodeId(self.package(), ast_node_id))
    }
*/


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

/*
impl AstData {
    pub fn package(&self) -> PackageFqn {
        self.source.package()
    }

    fn region(&self, source: &Source, ll: SourceOffset, rr: SourceOffset) -> Region {
        let start = source.to_linecol(ll);
        let end = source.to_linecol(rr);
        Region::new(self.package().clone(), Span::new(start, end))
    }

    fn add_node(
        &mut self,
        payload: AstNodePayload,
        region: Region,
        num_children: u16,
    ) -> AstNodeId {
        let ast_node_id = AstNodeId(self.package(), self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.regions.push(region);
        self.num_children.push(num_children);
        ast_node_id
    }

    fn add_error_node(&mut self, region: Region) -> AstNodeId {
        let payload = AstNodePayload::Error;
        let ast_node_id = AstNodeId(self.package(), self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.regions.push(region);
        self.num_children.push(0);
        self.errors.push(ast_node_id.clone());
        ast_node_id
    }
}
*/

impl<'p> std::fmt::Debug for AstNode<'p> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.id, &self.payload)
    }
}

/*
impl AstNode {
    pub fn ast(&self) -> Ast {
        self.ast.clone()
    }

    pub fn id(&self) -> AstNodeId {
        self.id.clone()
    }

    pub fn spelling(&self) -> &BStr {
        self.ast.source[self.region.span()].as_bstr()
    }

    pub fn parent(&self) -> AstNode {
        self.ast.ast_node(self.parent.clone())
    }

    pub fn child(&self, mut n: u16) -> AstNode {
        let mut ast_node_id = 0;
        loop {
            if self.ast.parents[ast_node_id] == self.id {
                if n == 0 {
                    return self
                        .ast
                        .ast_node(AstNodeId(self.id.package(), ast_node_id.try_into().unwrap()));
                }
                n -= 1;
            }
            ast_node_id += 1;
        }
    }

    pub fn children(&self) -> Vec<AstNode> {
        let mut result = vec![];
        let num_children = self.ast.num_children[self.id.index()];
        for i in 0..num_children {
            result.push(self.child(i.try_into().unwrap()));
        }
        result
    }

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

    pub fn payload(&self) -> AstNodePayload {
        self.payload.clone()
    }

    pub fn region(&self) -> Region {
        self.ast.regions[self.id.index()].clone()
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
                self.region()
            );
        } else {
            eprintln!(
                "{padding}{:?} {:?} @ {}",
                self.payload(),
                self.id(),
                self.region()
            );
        }
        for child in self.children() {
            child.dump_level(level + 1);
        }
    }
}
*/

impl AstNodeId {
    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn index(&self) -> usize {
        self.1 as usize
    }
}
