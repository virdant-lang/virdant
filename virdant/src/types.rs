use crate::id::*;
use crate::common::*;

pub type Nat = u64;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Type(TypeScheme, Option<Nat>);

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum TypeScheme {
    StructDef(Id<StructDef>),
    UnionDef(Id<UnionDef>),
    EnumDef(Id<EnumDef>),
    BuiltinDef(Id<BuiltinDef>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CtorSig(Id<Ctor>, Vec<(String, Type)>, Type);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FnSig(Id<FnDef>, Vec<(String, Type)>, Type);

impl CtorSig {
    pub fn new(ctor: Id<Ctor>, params: Vec<(String, Type)>, ret_typ: Type) -> Self {
        CtorSig(ctor, params, ret_typ)
    }

    pub fn ctor(&self) -> Id<Ctor> {
        self.0
    }

    pub fn params(&self) -> &[(String, Type)] {
        &self.1
    }

    pub fn ret(&self) -> Type {
        self.2
    }
}

impl FnSig {
    pub fn new(fndef: Id<FnDef>, params: Vec<(String, Type)>, ret_typ: Type) -> Self {
        FnSig(fndef, params, ret_typ)
    }

    pub fn fndef(&self) -> Id<FnDef> {
        self.0
    }

    pub fn params(&self) -> &[(String, Type)] {
        &self.1
    }

    pub fn ret(&self) -> Type {
        self.2
    }
}

impl Type {
    pub fn structdef(structdef: Id<StructDef>) -> Self {
        Type(TypeScheme::StructDef(structdef), None)
    }

    pub fn uniondef(uniondef: Id<UnionDef>) -> Self {
        Type(TypeScheme::UnionDef(uniondef), None)
    }

    pub fn enumdef(enumdef: Id<EnumDef>) -> Self {
        Type(TypeScheme::EnumDef(enumdef), None)
    }

    pub fn builtindef(builtindef: Id<BuiltinDef>, arg: Option<Nat>) -> Self {
        Type(TypeScheme::BuiltinDef(builtindef), arg)
    }

    fn itemdef(&self) -> Id<Item> {
        match self.0 {
            TypeScheme::StructDef(structdef) => structdef.as_item(),
            TypeScheme::UnionDef(uniondef) => uniondef.as_item(),
            TypeScheme::EnumDef(enumdef) => enumdef.as_item(),
            TypeScheme::BuiltinDef(builtindef) => builtindef.as_item(),
        }
    }

    pub fn args(&self) -> Option<Vec<Nat>> {
        self.1.map(|n| vec![n]).clone()
    }

    pub fn scheme(&self) -> TypeScheme {
        self.0
    }

    pub(crate) fn is_clock(&self) -> bool {
        if let TypeScheme::BuiltinDef(builtindef) = &self.0 {
            *builtindef == Id::new("builtin::Clock")
        } else {
            false
        }
    }

    pub(crate) fn is_bit(&self) -> bool {
        if let TypeScheme::BuiltinDef(builtindef) = &self.0 {
            *builtindef == Id::new("builtin::Bit")
        } else {
            false
        }
    }

    pub(crate) fn is_word(&self) -> bool {
        if let TypeScheme::BuiltinDef(builtindef) = &self.0 {
            *builtindef == Id::new("builtin::Word")
        } else {
            false
        }
    }

    pub(crate) fn width(&self) -> Width {
        self.args().unwrap()[0]
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let item_def = self.itemdef();
        let arg_str = match self.args() {
            None => format!(""),
            Some(args) => {
                assert_eq!(args.len(), 1);
                format!("[{}]", args[0])
            }
        };
        write!(f, "{item_def}{arg_str}")
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for FnSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.fndef();
        write!(f, "{name}(")?;
        for (i, (name, typ)) in self.params().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{name} : {typ}")?;
        }
        write!(f, ") : {}", self.ret())?;
        Ok(())
    }
}

impl std::fmt::Debug for FnSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for CtorSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.ctor();
        write!(f, "{name}(")?;
        for (i, (name, typ)) in self.params().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{name} : {typ}")?;
        }
        write!(f, ") : {}", self.ret())?;
        Ok(())
    }
}

impl std::fmt::Debug for CtorSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
