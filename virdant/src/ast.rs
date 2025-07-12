pub mod node;
pub mod payload;

use std::sync::Arc;

use bstr::{BStr, ByteSlice};

pub mod parser_lalrpop;
pub use parser_lalrpop as parser;

use crate::fqn::PackageFqn;
use crate::source::{Region, Source, SourceOffset, Span};
use crate::stringtable::{InternedString, StringTable};

#[derive(Clone)]
pub struct Ast(Arc<AstData>);

pub struct AstData {
    source: Source,
    stringtable: StringTable,
    payloads: Vec<AstNodePayload>,
    regions: Vec<Region>,
    parents: Vec<AstNodeId>,
    num_children: Vec<u16>,
    errors: Vec<AstNodeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstNodeId(pub u16);

#[derive(Debug, Clone)]
pub enum AstNodePayload {
    Error,

    Package,

    Import(payload::Import),
    ModDef(payload::ModDef),
    StructDef(payload::StructDef),
    UnionDef(payload::UnionDef),
    EnumDef(payload::EnumDef),
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

    Type(InternedString, Option<InternedString>),

    ExprReference,
    ExprParen,
    ExprIf,
    ExprMatch,
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

pub struct AstNode {
    ast: Ast,
    id: AstNodeId,
    payload: AstNodePayload,
    region: Region,
    parent: AstNodeId,
}

impl Ast {
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
        node::Package(self.clone(), AstNodeId(ast_node_id))
    }

    pub fn ast_node(&self, ast_node_id: AstNodeId) -> AstNode {
        let payload = self.payloads[usize::from(ast_node_id)].clone();
        let region = self.regions[usize::from(ast_node_id)].clone();
        let parent = self.parents[usize::from(ast_node_id)].clone();

        AstNode {
            ast: self.clone(),
            id: ast_node_id,
            payload,
            region,
            parent,
        }
    }

    pub fn errors(&self) -> Vec<AstNode> {
        let mut errors = vec![];
        for ast_node_id in &self.errors {
            errors.push(self.ast_node(*ast_node_id));
        }
        errors
    }

    pub fn stringtable(&self) -> StringTable {
        self.stringtable.clone()
    }
}

impl std::ops::Deref for Ast {
    type Target = AstData;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl AstData {
    pub fn package(&self) -> PackageFqn {
        self.source.package()
    }

    fn region(&self, source: &Source, ll: usize, rr: usize) -> Region {
        let start = source.to_linecol(SourceOffset(ll.try_into().unwrap()));
        let end = source.to_linecol(SourceOffset(rr.try_into().unwrap()));
        Region::new(self.package().clone(), Span::new(start, end))
    }

    fn add_node(
        &mut self,
        payload: AstNodePayload,
        region: Region,
        num_children: u16,
    ) -> AstNodeId {
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.regions.push(region);
        self.num_children.push(num_children);
        ast_node_id
    }

    fn add_error_node(&mut self, region: Region) -> AstNodeId {
        let payload = AstNodePayload::Error;
        let ast_node_id = AstNodeId(self.payloads.len().try_into().unwrap());
        self.payloads.push(payload);
        self.regions.push(region);
        self.num_children.push(0);
        self.errors.push(ast_node_id);
        ast_node_id
    }
}

impl From<AstNodeId> for usize {
    fn from(value: AstNodeId) -> Self {
        usize::from(value.0)
    }
}

impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.id, &self.payload)
    }
}

impl AstNode {
    pub fn ast(&self) -> Ast {
        self.ast.clone()
    }

    pub fn id(&self) -> AstNodeId {
        self.id
    }

    pub fn spelling(&self) -> &BStr {
        self.ast.source[self.region.span()].as_bstr()
    }

    pub fn parent(&self) -> AstNode {
        self.ast.ast_node(self.parent)
    }

    pub fn child(&self, mut n: u16) -> AstNode {
        let mut ast_node_id = 0;
        loop {
            if self.ast.parents[ast_node_id] == self.id {
                if n == 0 {
                    return self
                        .ast
                        .ast_node(AstNodeId(ast_node_id.try_into().unwrap()));
                }
                n -= 1;
            }
            ast_node_id += 1;
        }
    }

    pub fn children(&self) -> Vec<AstNode> {
        let mut result = vec![];
        let num_children = self.ast.num_children[usize::from(self.id)];
        for i in 0..num_children {
            result.push(self.child(i.try_into().unwrap()));
        }
        result
    }

    pub fn payload(&self) -> AstNodePayload {
        self.payload.clone()
    }

    pub fn region(&self) -> Region {
        self.ast.regions[usize::from(self.id)].clone()
    }
}
