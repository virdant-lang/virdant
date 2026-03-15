use bstr::{BStr, ByteSlice};

//pub mod parser_lalrpop;
//pub use parser_lalrpop as parser;

use crate::common::ComponentKind;
use crate::common::json::ToJson;
use crate::source::{Region, Span};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::{InternedString, Parsing};
//use crate::stringtable::{InternedString, StringTable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(pub u16);

#[derive(Clone)]
pub struct AstNode<'a> {
    pub parsing: &'a Parsing,
    pub id: AstNodeId,
    pub payload: AstNodePayload,
    pub parent: Option<AstNodeId>,
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

    pub fn contains_errors(&self) -> bool {
        let mut queue = vec![self.id()];
        while let Some(node_id) = queue.pop() {
            let node = self.parsing.ast_node(node_id);
            if let AstNodePayload::Error = node.payload() {
                return true;
            }
            for child in node.children() {
                queue.push(child.id());
            }
        }
        false
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
    pub fn dump(&self) {
        self.dump_level(0);
    }

    #[allow(dead_code)]
    pub fn dump_level(&self, level: usize) {
        use bstr::io::BufReadExt;

        let padding = " ".repeat(4 * level);
        let spelling = self.spelling();
        if spelling.byte_lines().collect::<Vec<_>>().len() == 1 {
            eprintln!(
                "{padding}{:}    {:?}    @{} {spelling:?}",
                self.summary(),
                self.id(),
                self.span()
            );
        } else {
            eprintln!(
                "{padding}{:}    {:?}    @{}",
                self.summary(),
                self.id(),
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
                let export = if mod_def.is_export {
                    "export "
                } else {
                    ""
                };
                format!("ModDef {ext}{export}{}", parsing.string(mod_def.name))
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
            AstNodePayload::Path(path) => format!("Path"),
        }
    }

    pub fn is_item(&self) -> bool {
        matches!(&self.payload,
            AstNodePayload::ModDef(_) |
            AstNodePayload::StructDef(_) |
            AstNodePayload::UnionDef(_) |
            AstNodePayload::EnumDef(_) |
            AstNodePayload::BuiltinDef(_) |
            AstNodePayload::FnDef(_) |
            AstNodePayload::SocketDef(_)
        )
    }

    pub fn name(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::ModDef(mod_def) => Some(mod_def.name),
            AstNodePayload::UnionDef(union_def) => Some(union_def.name),
            AstNodePayload::StructDef(struct_def) => Some(struct_def.name),
            AstNodePayload::FnDef(fn_def) => Some(fn_def.name),
            AstNodePayload::SocketDef(socket_def) => Some(socket_def.name),
            AstNodePayload::BuiltinDef(builtin_def) => Some(builtin_def.name),
            AstNodePayload::EnumDef(enum_def) => Some(enum_def.name),
            _ => None,
        }
    }

    pub fn typ(&self) -> Option<AstNode<'_>> {
        match &self.payload {
            AstNodePayload::Component(_component) => Some(self.child(0)),
            _ => None,
        }
    }

    pub fn path(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::ExprReference => self.child(0).path(),
            AstNodePayload::Path(path) => Some(path.path),
            _ => None,
        }
    }

    pub fn package(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::Import(import) => Some(import.package),
            _ => None
        }
    }

    pub fn driver(&self) -> Option<AstNode<'_>> {
        match &self.payload {
            AstNodePayload::Driver(_driver) => Some(self.child(1)),
            _ => None
        }
    }

    pub fn clock(&self) -> Option<AstNode<'_>> {
        match &self.payload {
            AstNodePayload::Component(component)
                if component.kind == ComponentKind::Reg => Some(self.child(1)),
            _ => None
        }
    }
}

impl AstNodeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl ToJson for AstNodeId {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
