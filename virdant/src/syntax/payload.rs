use crate::syntax::parse::InternedString;
use crate::common::{BinOp, ChannelDir, ComponentKind, DriverType, SocketRole, UnOp};

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
