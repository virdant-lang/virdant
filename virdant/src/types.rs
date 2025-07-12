#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    typedef: String,
    args: Option<Vec<Generic>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Generic {
    Nat(u64),
    Type(Type),
}

impl Type {
    pub fn typedef(&self) -> String {
        self.typedef.clone()
    }

    pub fn args(&self) -> Option<Vec<Generic>> {
        self.args.clone()
    }

    pub fn word(n: u64) -> Self {
        Type {
            typedef: "builtin::Word".to_string(),
            args: Some(vec![Generic::Nat(n)]),
        }
    }

    pub fn bit() -> Self {
        Type {
            typedef: "builtin::Bit".to_string(),
            args: None,
        }
    }

    pub fn clock() -> Self {
        Type {
            typedef: "builtin::Clock".to_string(),
            args: None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(args) = &self.args {
            write!(f, "{}[{}]", self.typedef, args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", "))
        } else {
            write!(f, "{}", self.typedef)
        }
    }
}

impl std::fmt::Display for Generic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Generic::Nat(n) => write!(f, "{n}"),
            Generic::Type(typ) => write!(f, "{typ}"),
        }
    }
}
