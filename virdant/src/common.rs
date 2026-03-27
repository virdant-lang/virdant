pub mod math;
pub mod graph;
pub mod union;
pub mod source;

pub type Width = u16;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ComponentClass {
    Port,
    SubPort,
    Wire,
    Reg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Flow {
    Source,
    Sink,
    Duplex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DriverType {
    Continuous,
    Latched,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ChannelDir {
    Mosi,
    Miso,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum PortDir {
    Input,
    Output,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum SocketRole {
    Master,
    Slave,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Perspective {
    Exterior,
    Interior,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ItemKind {
    ModDef,
    UnionDef,
    StructDef,
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum TypeScheme {
    BuiltinDef,
    UnionDef,
    StructDef,
    EnumDef,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ComponentKind {
    Incoming,
    Outgoing,
    Reg,
    Wire,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum BinOp {
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    Add,
    Sub,
    And,
    Or,
    Xor,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum UnOp {
    Neg,
    Inv,
    Not,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Radix {
    Dec,
    Hex,
    Bin,
}

impl ItemKind {
    pub fn is_typedef(&self) -> bool {
        match self {
            ItemKind::ModDef => false,
            ItemKind::UnionDef => true,
            ItemKind::StructDef => true,
            ItemKind::EnumDef => true,
            ItemKind::BuiltinDef => true,
            ItemKind::FnDef => false,
            ItemKind::SocketDef => false,
        }
    }
}
