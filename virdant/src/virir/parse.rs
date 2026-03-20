use super::Width;
use crate::common::{BinOp, PortDir, UnOp};

use super::typ::Type as VirIrType;

#[derive(Clone)]
pub(super) enum Type {
    Bit,
    Word(Width),
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

    pub(super) fn width(&self) -> Width {
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
    pub(super) wires: Vec<Wire>,
    pub(super) regs: Vec<Reg>,
    pub(super) instances: Vec<Instance>,
    pub(super) drivers: Vec<Driver>,
    pub(super) on: Option<On>,
}

pub(super) struct Port {
    pub(super) name: String,
    pub(super) dir: PortDir,
    pub(super) typ: Type,
}

pub(super) struct Wire {
    pub(super) name: String,
    pub(super) typ: Type,
}

pub(super) struct Reg {
    pub(super) name: String,
    pub(super) typ: Type,
    pub(super) clock: Option<Expr>,
}

pub(super) struct Instance {
    pub(super) name: String,
    pub(super) module_path: String,
}

pub(super) struct Driver {
    pub(super) name: String,
    pub(super) expr: Expr,
}

pub(super) struct On {
    pub(super) clock: Expr,
    pub(super) commands: Vec<Command>,
}

pub(super) enum Command {
    Assert(Expr),
    Display(Expr),
    Finish,
    Fatal,
}

pub(super) enum Expr {
    Reference {
        path: String,
        typ: Option<Type>,
    },
    WordLit {
        value: u64,
        typ: Type,
    },
    Word {
        exprs: Vec<Expr>,
        typ: Option<Type>,
    },
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    UnOp {
        op: UnOp,
        expr: Box<Expr>,
        typ: Option<Type>,
    },
    Zext {
        expr: Box<Expr>,
        typ: Option<Type>,
    },
    Sext {
        expr: Box<Expr>,
        typ: Option<Type>,
    },
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
        typ: Option<Type>,
    },
    Index {
        subject: Box<Expr>,
        index: Width,
        typ: Option<Type>,
    },
    IndexRange {
        subject: Box<Expr>,
        index_hi: Width,
        index_lo: Width,
        typ: Option<Type>,
    },
}

pub(super) enum ModStmt {
    Port(Port),
    Wire(Wire),
    Reg(Reg),
    Instance(Instance),
    Driver(Driver),
    On(On),
}
