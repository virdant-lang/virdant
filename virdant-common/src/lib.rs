pub mod text;
pub mod location;

pub type WordVal = u64;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ComponentClass {
    Port,
    SubPort,
    Wire,
    Reg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Flow {
    Source,
    Sink,
    Duplex,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
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
pub enum SocketRole {
    Master,
    Slave,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Perspective {
    Exterior,
    Interior,
}

pub fn pow(n: u64, k: u64) -> u64 {
    let mut p = 1;
    for _ in 0..k {
        p *= n
    }
    p
}

pub fn clog2(n: u64) -> u64 {
    let mut result = 0;
    while n > (1 << result) {
        result += 1;
    }
    result
}

pub fn is_pow2(n: u64) -> bool {
    n != 0 && (n & (n - 1)) == 0
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

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub enum PackageSource {
    #[default]
    Builtin,
    File(std::path::PathBuf),
}
