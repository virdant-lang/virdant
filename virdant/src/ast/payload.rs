use crate::common::{BinOp, ComponentKind, DriverType, UnOp};

use super::*;

#[derive(Clone, Debug)]
pub struct Import {
    pub package: InternedString,
}

#[derive(Clone, Debug)]
pub struct ModDef {
    pub name: InternedString,
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
pub struct Ctor {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: InternedString,
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
pub struct PatIdent {
    pub name: InternedString,
}

#[derive(Clone, Debug)]
pub struct Ofness {
    pub name: InternedString,
}
