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
pub struct AstNodeId(pub(crate) u16);

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

impl AstNodePayload {
    pub fn kind(&self) -> &str {
        match self {
            AstNodePayload::Error => "Error",
            AstNodePayload::Package => "Package",
            AstNodePayload::Import(import) => "Import",
            AstNodePayload::ModDef(mod_def) => "ModDef",
            AstNodePayload::StructDef(struct_def) => "StructDef",
            AstNodePayload::UnionDef(union_def) => "UnionDef",
            AstNodePayload::EnumDef(enum_def) => "EnumDef",
            AstNodePayload::BuiltinDef(builtin_def) => "BuiltinDef",
            AstNodePayload::FnDef(fn_def) => "FnDef",
            AstNodePayload::SocketDef(socket_def) => "SocketDef",
            AstNodePayload::Component(component) => "Component",
            AstNodePayload::Driver(driver) => "Driver",
            AstNodePayload::BidirectionalDriver => "BidirectionalDriver",
            AstNodePayload::Module(module) => "Module",
            AstNodePayload::Socket(socket) => "Socket",
            AstNodePayload::Field(field) => "Field",
            AstNodePayload::Ctor(ctor) => "Ctor",
            AstNodePayload::Enumerant(enumerant) => "Enumerant",
            AstNodePayload::Channel(channel) => "Channel",
            AstNodePayload::Param(param) => "Param",
            AstNodePayload::Kind(kind) => "Kind",
            AstNodePayload::Type(_) => "Type",
            AstNodePayload::ExprReference => "ExprReference",
            AstNodePayload::ExprParen => "ExprParen",
            AstNodePayload::ExprIf => "ExprIf",
            AstNodePayload::ExprMatch => "ExprMatch",
            AstNodePayload::ExprBitLit(expr_bit_lit) => "ExprBitLit",
            AstNodePayload::ExprWordLit(expr_word_lit) => "ExprWordLit",
            AstNodePayload::ExprBinOp(expr_bin_op) => "ExprBinOp",
            AstNodePayload::ExprUnOp(expr_un_op) => "ExprUnOp",
            AstNodePayload::ExprMethod(expr_method) => "ExprMethod",
            AstNodePayload::ExprFn => "ExprFn",
            AstNodePayload::ExprCtor(expr_ctor) => "ExprCtor",
            AstNodePayload::ExprEnumerant(expr_enumerant) => "ExprEnumerant",
            AstNodePayload::ExprStruct => "ExprStruct",
            AstNodePayload::ExprIndex(expr_index) => "ExprIndex",
            AstNodePayload::ExprIndexRange(expr_index_range) => "ExprIndexRange",
            AstNodePayload::ExprWord => "ExprWord",
            AstNodePayload::ExprZext => "ExprZext",
            AstNodePayload::ExprSext => "ExprSext",
            AstNodePayload::Assign(assign) => "Assign",
            AstNodePayload::PatIdent(pat_ident) => "PatIdent",
            AstNodePayload::PatEnumerant(pat_enumerant) => "PatEnumerant",
            AstNodePayload::PatElse => "PatElse",
            AstNodePayload::Ofness(ofness) => "Ofness",
            AstNodePayload::Path => "Path",
        }
    }
}
