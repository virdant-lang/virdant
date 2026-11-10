//! The `vir value` AST: value expressions and type spellings, as
//! parsed from the command line (see `grammar.lalrpop`).
//!
//! This is a standalone AST — deliberately separate from the
//! compiler's `syntax` AST (see PLAN §3): the value language is a
//! sturdy subset of Virdant's `Expr`, and keeping the types small
//! keeps encode/decode independent of the elaboration machinery.

/// A type spelling as written on the command line or in an
/// ascription: bare names are looked up in the `builtin` package
/// (`Bit`, `Word[8]`, `Valid[Word[8]]`); everything else must be
/// package-qualified (`mypkg::Pair[Word[8], Bit]`).
#[derive(Clone, Debug, PartialEq)]
pub enum VType {
    Bare { name: String, args: Vec<VTypeArg> },
    Qualified { package: String, name: String, args: Vec<VTypeArg> },
}

/// One argument in a type instantiation: a `Nat` literal (for
/// `Nat`-kind parameters, becoming `Word[w]`) or a type (for
/// `Type`-kind parameters).
#[derive(Clone, Debug, PartialEq)]
pub enum VTypeArg {
    Nat(u128),
    Type(VType),
}

/// A value expression: the subset of Virdant's `Expr` accepted by
/// `vir value encode` — literals (all forms), constructor and
/// enumerant applications, struct literals, parens, and ascription.
/// No operators, paths, `if`/`match`/`let`, `dontcare`, or `?`.
#[derive(Clone, Debug, PartialEq)]
pub enum ValueExpr {
    /// A plain `Nat` literal (`42`, `0b1010`, `0x8`) — no fixed width;
    /// it takes its width from the expected type.
    NatLit(u128),
    /// A `Word` literal with explicit width (`0b1010w4`, `42w8`).
    WordLit(u128, u16),
    BitLit(bool),
    /// A constructor or enumerant application: `@Valid(0w8)`,
    /// `@Invalid`.
    Ctor { name: String, args: Vec<ValueExpr> },
    /// A struct literal.  Two spellings (see PLAN §3):
    /// `${Pair(a = 1, b = 2)}` (named, round-trippable decode output)
    /// and `${a = 1, b = 2}` (as written in Virdant source).
    StructLit {
        name: Option<String>,
        fields: Vec<(String, ValueExpr)>,
    },
    /// Type ascription: `0 : Word[8]`.  The ascribed type must equal
    /// the expected type at that position.
    Ascribe { expr: Box<ValueExpr>, typ: VType },
}

/// A bit pattern on the `decode` command line: a plain `Nat` literal
/// (its width comes from the decoded type) or a sized `Word` literal
/// (whose width must match).
#[derive(Clone, Debug, PartialEq)]
pub enum PatternLit {
    Nat(u128),
    Word(u128, u16),
}

/// What one `vir-value` invocation parses into: an `encode`
/// expression, or a `decode` pattern/type pair.
#[derive(Clone, Debug, PartialEq)]
pub enum Top {
    Encode(ValueExpr),
    Decode(PatternLit, VType),
}
