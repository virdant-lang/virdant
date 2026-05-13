use bstr::{BStr, ByteSlice};

use crate::analysis::Location;
use crate::common::{ComponentKind, DriverType};
use crate::fqn::PackageFqn;
use crate::common::source::{Region, Span};
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::{InternedString, Parsing};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn parsing(&self) -> &Parsing {
        self.parsing
    }

    pub fn spelling(&self) -> &BStr {
        self.parsing.source[self.span()].as_bstr()
    }

    pub fn parent(&self) -> Option<AstNode<'_>> {
        if let Some(parent) = &self.parent {
            Some(self.parsing.ast_node(parent.clone()))
        } else {
            None
        }
    }

    pub fn child(&self, mut n: u16) -> AstNode<'_> {
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

    pub fn children(&self) -> Vec<AstNode<'_>> {
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

    pub fn location(&self) -> Location {
        Location::new(self.package(), self.id)
    }

    pub fn package(&self) -> PackageFqn {
        self.parsing.package()
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
            AstNodePayload::Import(import) => format!("Import {}", parsing.string(import.package.clone())),
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
                format!("ModDef {ext}{export}{}", parsing.string(mod_def.name.clone()))
            }
            AstNodePayload::StructDef(_struct_def) => format!("StructDef"),
            AstNodePayload::UnionDef(_union_def) => format!("UnionDef"),
            AstNodePayload::EnumDef(_enum_def) => format!("EnumDef"),
            AstNodePayload::BuiltinDef(_builtin_def) => format!("BuiltinDef"),
            AstNodePayload::FnDef(_fn_def) => format!("FnDef"),
            AstNodePayload::SocketDef(_socket_def) => format!("SocketDef"),
            AstNodePayload::Component(component) => format!("Component {:?} {}", component.kind, parsing.string(component.name.clone())),
            AstNodePayload::Driver(_driver) => format!("Driver"),
            AstNodePayload::BidirectionalDriver => format!("BidirectionalDriver"),
            AstNodePayload::Submodule(_module) => format!("Module"),
            AstNodePayload::ModDefStmtBlock => format!("ModDefStmtBlock"),
            AstNodePayload::ModDefStmtIf => format!("ModDefStmtIf"),
            AstNodePayload::ModDefStmtMatch => format!("ModDefStmtMatch"),
            AstNodePayload::ModDefStmtDrop => format!("ModDefStmtDrop"),
            AstNodePayload::Socket(_socket) => format!("Socket"),
            AstNodePayload::Field(_field) => format!("Field"),
            AstNodePayload::Ctor(_ctor) => format!("Ctor"),
            AstNodePayload::Enumerant(_enumerant) => format!("Enumerant"),
            AstNodePayload::Channel(_channel) => format!("Channel"),
            AstNodePayload::Param(_param) => format!("Param"),
            AstNodePayload::GenericsParams(params) => format!("GenericsParams {:?}", params.value),
            AstNodePayload::Generics => format!("Generics"),
            AstNodePayload::Kind(_kind) => format!("Kind"),
            AstNodePayload::Type(_) => format!("Type"),
            AstNodePayload::ExprReference => format!("ExprReference"),
            AstNodePayload::ExprParen => format!("ExprParen"),
            AstNodePayload::ExprIf => format!("ExprIf"),
            AstNodePayload::ExprMatch => format!("ExprMatch"),
            AstNodePayload::ExprBitLit(_expr_bit_lit) => format!("ExprBitLit"),
            AstNodePayload::ExprWordLit(_expr_word_lit) => format!("ExprWordLit"),
            AstNodePayload::ExprStrLit(_expr_str_lit) => format!("ExprStrLit"),
            AstNodePayload::ExprBinOp(_expr_bin_op) => format!("ExprBinOp"),
            AstNodePayload::ExprUnOp(_expr_un_op) => format!("ExprUnOp"),
            AstNodePayload::ExprField(_expr_field) => format!("ExprField"),
            AstNodePayload::ExprFn => format!("ExprFn"),
            AstNodePayload::ExprCtor(_expr_ctor) => format!("ExprCtor"),
            AstNodePayload::ExprEnumerant(_expr_enumerant) => format!("ExprEnumerant"),
            AstNodePayload::ExprStruct => format!("ExprStruct"),
            AstNodePayload::ExprIndex(_expr_index) => format!("ExprIndex"),
            AstNodePayload::ExprIndexRange(_expr_index_range) => format!("ExprIndexRange"),
            AstNodePayload::ExprWord => format!("ExprWord"),
            AstNodePayload::ExprZext => format!("ExprZext"),
            AstNodePayload::ExprSext => format!("ExprSext"),
            AstNodePayload::ExprAs => format!("ExprAs"),
            AstNodePayload::ExprHole => format!("ExprHole"),
            AstNodePayload::Assign(_assign) => format!("Assign"),
            AstNodePayload::PatCtor(_pat_ident) => format!("PatCtor"),
            AstNodePayload::PatEnumerant(_pat_enumerant) => format!("PatEnumerant"),
            AstNodePayload::Ofness(_ofness) => format!("Ofness"),
            AstNodePayload::It => format!("It"),
            AstNodePayload::Path(_path) => format!("Path"),
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
            AstNodePayload::ModDef(mod_def) => Some(mod_def.name.clone()),
            AstNodePayload::UnionDef(union_def) => Some(union_def.name.clone()),
            AstNodePayload::StructDef(struct_def) => Some(struct_def.name.clone()),
            AstNodePayload::FnDef(fn_def) => Some(fn_def.name.clone()),
            AstNodePayload::SocketDef(socket_def) => Some(socket_def.name.clone()),
            AstNodePayload::BuiltinDef(builtin_def) => Some(builtin_def.name.clone()),
            AstNodePayload::EnumDef(enum_def) => Some(enum_def.name.clone()),
            _ => None,
        }
    }

    pub fn typ(&self) -> Option<AstNode<'_>> {
        match &self.payload {
            AstNodePayload::Component(_component) => Some(self.child(0)),
            AstNodePayload::Channel(_channel) => Some(self.child(0)),
            _ => None,
        }
    }

    pub fn path(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::ExprReference => self.child(0).path(),
            AstNodePayload::Path(path) => Some(path.path.clone()),
            _ => None,
        }
    }

    pub fn import_package(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::Import(import) => Some(import.package.clone()),
            _ => None
        }
    }

    pub fn target(&self) -> Option<InternedString> {
        match &self.payload {
            AstNodePayload::Driver(_driver) => {
                let target_node = self.child(0);
                target_node.path()
            }
            _ => None
        }
    }

    pub fn driver_type(&self) -> Option<DriverType> {
        match &self.payload {
            AstNodePayload::Driver(driver) => Some(driver.driver_type),
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
            AstNodePayload::Component(component) => {
                if component.kind == ComponentKind::Reg {
                    for child in self.children().into_iter().skip(1) {
                        if !matches!(child.payload(), AstNodePayload::It) {
                            return Some(child);
                        }
                    }
                }
                None
            }
            _ => None
        }
    }

    pub fn is_expr(&self) -> bool {
        matches!(self.payload(),
            AstNodePayload::ExprReference |
            AstNodePayload::ExprParen |
            AstNodePayload::ExprIf |
            AstNodePayload::ExprMatch |
            AstNodePayload::ExprBitLit(_) |
            AstNodePayload::ExprWordLit(_) |
            AstNodePayload::ExprStrLit(_) |
            AstNodePayload::ExprBinOp(_) |
            AstNodePayload::ExprUnOp(_) |
            AstNodePayload::ExprFn |
            AstNodePayload::ExprCtor(_) |
            AstNodePayload::ExprEnumerant(_) |
            AstNodePayload::ExprStruct |
            AstNodePayload::ExprIndex(_) |
            AstNodePayload::ExprIndexRange(_) |
            AstNodePayload::ExprWord |
            AstNodePayload::ExprZext |
            AstNodePayload::ExprSext
        )
    }

    pub fn is_pat(&self) -> bool {
        matches!(
            self.payload(),
            AstNodePayload::PatCtor(_) |
            AstNodePayload::PatEnumerant(_)
        )
    }

    pub fn subject(&self) -> Option<AstNode<'_>> {
        match &self.payload {
            AstNodePayload::ExprParen => Some(self.child(0)),
            AstNodePayload::ExprIf => Some(self.child(0)),
            AstNodePayload::ExprMatch => Some(self.child(0)),
            AstNodePayload::ExprUnOp(_expr_un_op) => Some(self.child(0)),
            AstNodePayload::ExprField(_) => Some(self.child(0)),
            AstNodePayload::ExprFn => Some(self.child(1)), // skips over the Ofness
            AstNodePayload::ExprIndex(_expr_index) => Some(self.child(0)),
            AstNodePayload::ExprIndexRange(_expr_index_range) => Some(self.child(0)),
            AstNodePayload::ExprAs => Some(self.child(0)),
            _ => None,
        }
    }

    pub fn args(&self) -> Option<Vec<AstNode<'_>>> {
        match &self.payload {
            AstNodePayload::ExprFn => {
                let args = self.children().into_iter().skip(1).collect();
                Some(args)
            }
            _ => None,
        }
    }
}

impl AstNodeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Debug for AstNodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstNodeId({})", self.0)
    }
}
