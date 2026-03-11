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
    pub(crate) parent: Option<AstNodeId>,
}

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
        self.parsing.source[self.span()].as_bstr()
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

    pub fn region(&self) -> Region {
        Region::new(self.parsing.source.package(), self.span())
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
                "{padding}{:} @ {} {spelling:?}",
                self.summary(),
                self.span()
            );
        } else {
            eprintln!(
                "{padding}{:} @ {}",
                self.summary(),
                self.span()
            );
        }
        for child in self.children() {
            child.dump_level(level + 1);
        }
    }

    pub fn summary(&self) -> String {
        let parsing = self.parsing;
        match &self.payload {
            AstNodePayload::Error => format!("Error"),
            AstNodePayload::Package => format!("Package"),
            AstNodePayload::Import(import) => format!("Import {}", parsing.string(import.package)),
            AstNodePayload::ModDef(mod_def) => {
                let ext = if mod_def.is_ext {
                    "ext "
                } else {
                    ""
                };
                format!("ModDef {ext}{}", parsing.string(mod_def.name))
            }
            AstNodePayload::StructDef(struct_def) => format!("StructDef"),
            AstNodePayload::UnionDef(union_def) => format!("UnionDef"),
            AstNodePayload::EnumDef(enum_def) => format!("EnumDef"),
            AstNodePayload::BuiltinDef(builtin_def) => format!("BuiltinDef"),
            AstNodePayload::FnDef(fn_def) => format!("FnDef"),
            AstNodePayload::SocketDef(socket_def) => format!("SocketDef"),
            AstNodePayload::Component(component) => format!("Component {:?} {}", component.kind, parsing.string(component.name)),
            AstNodePayload::Driver(driver) => format!("Driver"),
            AstNodePayload::BidirectionalDriver => format!("BidirectionalDriver"),
            AstNodePayload::Module(module) => format!("Module"),
            AstNodePayload::ModDefStmtBlock(mod_def_stmt_block) => format!("ModDefStmtBlock"),
            AstNodePayload::ModDefStmtIf => format!("ModDefStmtIf"),
            AstNodePayload::ModDefStmtMatch => format!("ModDefStmtMatch"),
            AstNodePayload::Socket(socket) => format!("Socket"),
            AstNodePayload::Field(field) => format!("Field"),
            AstNodePayload::Ctor(ctor) => format!("Ctor"),
            AstNodePayload::Enumerant(enumerant) => format!("Enumerant"),
            AstNodePayload::Channel(channel) => format!("Channel"),
            AstNodePayload::Param(param) => format!("Param"),
            AstNodePayload::Kind(kind) => format!("Kind"),
            AstNodePayload::Type(_) => format!("Type"),
            AstNodePayload::ExprReference => format!("ExprReference"),
            AstNodePayload::ExprParen => format!("ExprParen"),
            AstNodePayload::ExprIf => format!("ExprIf"),
            AstNodePayload::ExprMatch => format!("ExprMatch"),
            AstNodePayload::ExprBitLit(expr_bit_lit) => format!("ExprBitLit"),
            AstNodePayload::ExprWordLit(expr_word_lit) => format!("ExprWordLit"),
            AstNodePayload::ExprBinOp(expr_bin_op) => format!("ExprBinOp"),
            AstNodePayload::ExprUnOp(expr_un_op) => format!("ExprUnOp"),
            AstNodePayload::ExprMethod(expr_method) => format!("ExprMethod"),
            AstNodePayload::ExprFn => format!("ExprFn"),
            AstNodePayload::ExprCtor(expr_ctor) => format!("ExprCtor"),
            AstNodePayload::ExprEnumerant(expr_enumerant) => format!("ExprEnumerant"),
            AstNodePayload::ExprStruct => format!("ExprStruct"),
            AstNodePayload::ExprIndex(expr_index) => format!("ExprIndex"),
            AstNodePayload::ExprIndexRange(expr_index_range) => format!("ExprIndexRange"),
            AstNodePayload::ExprWord => format!("ExprWord"),
            AstNodePayload::ExprZext => format!("ExprZext"),
            AstNodePayload::ExprSext => format!("ExprSext"),
            AstNodePayload::Assign(assign) => format!("Assign"),
            AstNodePayload::PatIdent(pat_ident) => format!("PatIdent"),
            AstNodePayload::PatEnumerant(pat_enumerant) => format!("PatEnumerant"),
            AstNodePayload::PatElse => format!("PatElse"),
            AstNodePayload::Ofness(ofness) => format!("Ofness"),
            AstNodePayload::It => format!("It"),
            AstNodePayload::Path => format!("Path"),
        }
    }
}

impl AstNodeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}
