use crate::common::{BinOp, ChannelDir, ComponentKind, DriverType, SocketRole, UnOp, Width};
use crate::syntax::parsing::InternedString;

#[derive(Debug, Clone)]
pub enum AstNodePayload {
    Error,

    Package(PackagePayload),

    Import(Import),
    ModDef(ModDef),
    StructDef(StructDef),
    UnionDef(UnionDef),
    EnumDef(EnumDef),
    BuiltinDef(BuiltinDef),
    FnDef(FnDef),
    SocketDef(SocketDef),

    Component(Component),
    Driver(Driver),
    BidirectionalDriver,
    Submodule(Submodule),
    ModDefStmtBlock,
    ModDefStmtIf,
    ModDefStmtMatch,
    ModDefStmtUnused,
    Socket(Socket),

    Field(Field),
    Ctor(Ctor),
    Enumerant(Enumerant),
    Channel(Channel),

    GenericsParams(GenericsParams),
    Generics,
    Param(Param),

    Kind(Kind),
    Type(Type),

    ExprReference,
    ExprParen,
    ExprIf,
    ExprMatch,
    ExprBitLit(ExprBitLit),
    ExprWordLit(ExprWordLit),
    ExprStrLit(ExprStrLit),
    ExprBinOp(ExprBinOp),
    ExprUnOp(ExprUnOp),
    ExprField(ExprField),
    ExprFn,
    ExprCtor(ExprCtor),
    ExprEnumerant(ExprEnumerant),
    ExprStruct,
    ExprIndex(ExprIndex),
    ExprIndexRange(ExprIndexRange),
    ExprWord,
    ExprZext,
    ExprSext,
    ExprAs,
    ExprHole,
    ExprDontcare,

    Assign(Assign),

    PatCtor(PatCtor),
    PatEnumerant(PatEnumerant),
    PatWordLit(PatWordLit),
    PatBitLit(PatBitLit),

    Ofness(Ofness),
    It,
    Path(Path),
}

impl AstNodePayload {
    pub fn kind(&self) -> &str {
        match self {
            AstNodePayload::Error => "Error",
            AstNodePayload::Package(_) => "Package",
            AstNodePayload::Import(_import) => "Import",
            AstNodePayload::ModDef(_mod_def) => "ModDef",
            AstNodePayload::StructDef(_struct_def) => "StructDef",
            AstNodePayload::UnionDef(_union_def) => "UnionDef",
            AstNodePayload::EnumDef(_enum_def) => "EnumDef",
            AstNodePayload::BuiltinDef(_builtin_def) => "BuiltinDef",
            AstNodePayload::FnDef(_fn_def) => "FnDef",
            AstNodePayload::SocketDef(_socket_def) => "SocketDef",
            AstNodePayload::Component(_component) => "Component",
            AstNodePayload::Driver(_driver) => "Driver",
            AstNodePayload::BidirectionalDriver => "BidirectionalDriver",
            AstNodePayload::Submodule(_module) => "Module",
            AstNodePayload::ModDefStmtBlock => "ModDefStmtBlock",
            AstNodePayload::ModDefStmtIf => "ModDefStmtIf",
            AstNodePayload::ModDefStmtMatch => "ModDefStmtMatch",
            AstNodePayload::ModDefStmtUnused => "ModDefStmtUnused",
            AstNodePayload::Socket(_socket) => "Socket",
            AstNodePayload::Field(_field) => "Field",
            AstNodePayload::Ctor(_ctor) => "Ctor",
            AstNodePayload::Enumerant(_enumerant) => "Enumerant",
            AstNodePayload::Channel(_channel) => "Channel",
            AstNodePayload::Param(_param) => "Param",
            AstNodePayload::GenericsParams(_params) => "GenericsParams",
            AstNodePayload::Generics => "Generics",
            AstNodePayload::Kind(_kind) => "Kind",
            AstNodePayload::Type(_) => "Type",
            AstNodePayload::ExprReference => "ExprReference",
            AstNodePayload::ExprParen => "ExprParen",
            AstNodePayload::ExprIf => "ExprIf",
            AstNodePayload::ExprMatch => "ExprMatch",
            AstNodePayload::ExprBitLit(_expr_bit_lit) => "ExprBitLit",
            AstNodePayload::ExprWordLit(_expr_word_lit) => "ExprWordLit",
            AstNodePayload::ExprStrLit(_expr_str_lit) => "ExprStrLit",
            AstNodePayload::ExprBinOp(_expr_bin_op) => "ExprBinOp",
            AstNodePayload::ExprUnOp(_expr_un_op) => "ExprUnOp",
            AstNodePayload::ExprField(_expr_field) => "ExprField",
            AstNodePayload::ExprFn => "ExprFn",
            AstNodePayload::ExprCtor(_expr_ctor) => "ExprCtor",
            AstNodePayload::ExprEnumerant(_expr_enumerant) => "ExprEnumerant",
            AstNodePayload::ExprStruct => "ExprStruct",
            AstNodePayload::ExprIndex(_expr_index) => "ExprIndex",
            AstNodePayload::ExprIndexRange(_expr_index_range) => "ExprIndexRange",
            AstNodePayload::ExprWord => "ExprWord",
            AstNodePayload::ExprZext => "ExprZext",
            AstNodePayload::ExprSext => "ExprSext",
            AstNodePayload::Assign(_assign) => "Assign",
            AstNodePayload::PatCtor(_pat_ident) => "PatCtor",
            AstNodePayload::PatEnumerant(_pat_enumerant) => "PatEnumerant",
            AstNodePayload::PatWordLit(_pat_word_lit) => "PatWordLit",
            AstNodePayload::PatBitLit(_pat_bit_lit) => "PatBitLit",
            AstNodePayload::Ofness(_ofness) => "Ofness",
            AstNodePayload::It => "It",
            AstNodePayload::Path(_path) => "Path",
            AstNodePayload::ExprAs => "ExprAs",
            AstNodePayload::ExprHole => "ExprHole",
            AstNodePayload::ExprDontcare => "ExprDontcare",
        }
    }
}

#[derive(Clone, Debug)]
pub struct PackagePayload {
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub package: InternedString,
}

#[derive(Clone, Debug)]
pub struct ModDef {
    pub name: InternedString,
    pub is_ext: bool,
    pub is_export: bool,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct UnionDef {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: InternedString,
    pub width: Width,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct BuiltinDef {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct SocketDef {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Component {
    pub name: InternedString,
    pub kind: ComponentKind,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Driver {
    pub driver_type: DriverType,
}

#[derive(Clone, Debug)]
pub struct Submodule {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct ModDefStmtBlock;

#[derive(Clone, Debug)]
pub struct Socket {
    pub name: InternedString,
    pub role: SocketRole,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Ctor {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Enumerant {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Channel {
    pub name: InternedString,
    pub dir: ChannelDir,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct GenericsParams {
    pub value: InternedString,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: InternedString,
    pub doc_string: Option<InternedString>,
}

#[derive(Clone, Debug)]
pub struct Kind {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub num_generics: u16,
}

#[derive(Clone, Debug)]
pub struct ExprBitLit {
    pub literal: bool,
}

#[derive(Clone, Debug)]
pub struct ExprWordLit {
    pub literal: InternedString,
}

#[derive(Clone, Debug)]
pub struct ExprStrLit {
    pub literal: InternedString,
}

#[derive(Clone, Debug)]
pub struct ExprBinOp {
    pub op: BinOp,
}

#[derive(Clone, Debug)]
pub struct ExprUnOp {
    pub op: UnOp,
}

#[derive(Clone, Debug)]
pub struct ExprField {
    pub field: InternedString,
}

#[derive(Clone, Debug)]
pub struct ExprCtor {
    pub ctor: InternedString,
}

#[derive(Clone, Debug)]
pub struct ExprEnumerant {
    pub enumerant: InternedString,
}

#[derive(Clone, Debug)]
pub struct ExprIndex {
    pub index: u16,
}

#[derive(Clone, Debug)]
pub struct ExprIndexRange {
    pub index_hi: u16,
    pub index_lo: u16,
}

#[derive(Clone, Debug)]
pub struct Assign {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct PatCtor {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct PatEnumerant {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct PatWordLit {
    pub literal: InternedString,
}

#[derive(Clone, Debug)]
pub struct PatBitLit {
    pub literal: bool,
}

#[derive(Clone, Debug)]
pub struct Ofness {
    pub package: Option<InternedString>,
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Path {
    pub path: InternedString,
}
