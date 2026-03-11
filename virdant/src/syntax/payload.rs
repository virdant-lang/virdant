use crate::syntax::parsing::InternedString;
use crate::common::{BinOp, ChannelDir, ComponentKind, DriverType, SocketRole, UnOp};

#[derive(Debug, Clone)]
pub enum AstNodePayload {
    Error,

    Package,

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
    Module(Module),
    ModDefStmtIf,
    ModDefStmtMatch,
    Socket(Socket),

    Field(Field),
    Ctor(Ctor),
    Enumerant(Enumerant),
    Channel(Channel),

    Param(Param),

    Kind(Kind),
    Type(Type),

    ExprReference,
    ExprParen,
    ExprIf,
    ExprMatch,
    ExprBitLit(ExprBitLit),
    ExprWordLit(ExprWordLit),
    ExprBinOp(ExprBinOp),
    ExprUnOp(ExprUnOp),
    ExprMethod(ExprMethod),
    ExprFn,
    ExprCtor(ExprCtor),
    ExprEnumerant(ExprEnumerant),
    ExprStruct,
    ExprIndex(ExprIndex),
    ExprIndexRange(ExprIndexRange),
    ExprWord,
    ExprZext,
    ExprSext,

    Assign(Assign),

    PatIdent(PatIdent),
    PatEnumerant(PatEnumerant),
    PatElse,

    Ofness(Ofness),
    It,
    Path,
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
            AstNodePayload::ModDefStmtIf => "ModDefStmtIf",
            AstNodePayload::ModDefStmtMatch => "ModDefStmtMatch",
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
            AstNodePayload::It => "It",
            AstNodePayload::Path => "Path",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Import {
    pub package: InternedString,
}

#[derive(Clone, Debug)]
pub struct ModDef {
    pub name: InternedString,
    pub is_ext: bool,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct UnionDef {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: InternedString,
    pub width: u16,
}

#[derive(Clone, Debug)]
pub struct BuiltinDef {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct SocketDef {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Component {
    pub name: InternedString,
    pub kind: ComponentKind,
}

#[derive(Clone, Debug)]
pub struct Driver {
    pub driver_type: DriverType,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Socket {
    pub name: InternedString,
    pub role: SocketRole,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Ctor {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Enumerant {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Channel {
    pub name: InternedString,
    pub dir: ChannelDir,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: InternedString,
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
pub struct ExprBinOp {
    pub op: BinOp,
}

#[derive(Clone, Debug)]
pub struct ExprUnOp {
    pub op: UnOp,
}

#[derive(Clone, Debug)]
pub struct ExprMethod {
    pub method: InternedString,
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
pub struct PatIdent {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct PatEnumerant {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Ofness {
    pub package: Option<InternedString>,
    pub name: InternedString,
}
