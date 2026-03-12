use crate::common::{BinOp, PortDir};

use super::typ::Type as VirIrType;

#[derive(Clone)]
pub(super) enum Type {
    Bit,
    Word(u16),
    Clock,
}

impl Type {
    pub(super) fn key(&self) -> String {
        match self {
            Type::Bit => "builtin::Bit".to_string(),
            Type::Word(width) => format!("builtin::Word[{width}]"),
            Type::Clock => "builtin::Clock".to_string(),
        }
    }

    pub(super) fn width(&self) -> u16 {
        match self {
            Type::Bit | Type::Clock => 1,
            Type::Word(width) => *width,
        }
    }

    pub(super) fn to_type(&self) -> VirIrType {
        match self {
            Type::Bit => VirIrType::Bit,
            Type::Word(width) => VirIrType::Word(*width),
            Type::Clock => VirIrType::Clock,
        }
    }
}

pub(super) struct Package {
    pub(super) name: String,
    pub(super) modules: Vec<ModDef>,
}

pub(super) struct ModDef {
    pub(super) is_export: bool,
    pub(super) name: String,
    pub(super) ports: Vec<Port>,
    pub(super) instances: Vec<Instance>,
    pub(super) drivers: Vec<Driver>,
}

pub(super) struct Port {
    pub(super) name: String,
    pub(super) dir: PortDir,
    pub(super) typ: Type,
}

pub(super) struct Instance {
    pub(super) name: String,
    pub(super) module_path: String,
}

pub(super) struct Driver {
    pub(super) name: String,
    pub(super) expr: Expr,
}

pub(super) enum Expr {
    Reference {
        path: String,
        typ: Option<Type>,
    },
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
        typ: Option<Type>,
    },
}

pub(super) enum ModStmt {
    Port(Port),
    Instance(Instance),
    Driver(Driver),
}
