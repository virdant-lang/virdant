//! The `vir poke` scripting language's AST.
//!
//! Deliberately separate from the parser's token stream (see the
//! concept notes, §3): the AST is where struct/union/enum literal
//! constructors will land without rework when they are added.

/// A path as written in a script.  Three starting forms:
///
/// - `top.foo.bar` — absolute, from the top of the elaborated design;
/// - `foo.bar`     — relative to the current module (`cd` location);
/// - `it.foo`      — the previously peeked value (expressions only;
///   field access on `it` is future work, so only bare `it` is
///   meaningful in v1).
#[derive(Clone, Debug, PartialEq)]
pub struct SPath {
    pub start: PathStart,
    pub segments: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PathStart {
    Top,
    Ident(String),
    It,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinKind {
    Add,
    Sub,
    And,
    Or,
    Xor,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    LogAnd,
    LogOr,
    LogXor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Path(SPath),
    /// A plain `Nat` literal (`42`, `0b1010`, `0x8`) — no fixed width;
    /// it takes its width from context.
    NatLit(u128),
    /// A `Word` literal with explicit width (`0b1010w4`, `42w8`).
    WordLit(u128, u16),
    BitLit(bool),
    /// An enum constructor: `#Foo` or `#Foo(arg, ...)`.  Its type comes
    /// from context (the other side of a comparison, or the `poke`
    /// target), resolved at evaluation time.
    Ctor { name: String, args: Vec<Expr> },
    Neg(Box<Expr>),
    Inv(Box<Expr>),
    Not(Box<Expr>),
    Bin(BinKind, Box<Expr>, Box<Expr>),
    /// Single-bit indexing: `x[3]`.
    Index(Box<Expr>, u16),
    /// Bit slicing, Virdant convention: `x[hi..lo]` selects bits
    /// `[lo, hi)` (hi exclusive), MSB-first.
    Slice(Box<Expr>, u16, u16),
}

/// One parsed script line.
#[derive(Clone, Debug)]
pub enum Command {
    Cd(Vec<String>),
    /// `cd top.foo` — jump to an absolute module path from the top.
    CdAbs(Vec<String>),
    /// `cd top` — back to the top module.
    CdTop,
    CdUp,
    Peek { path: SPath },
    Poke { path: SPath, expr: Expr },
    Watch { path: SPath },
    Break { path: SPath },
    /// `advance [n]`: move simulated time forward `n` timesteps
    /// (a full clock period each), driving no clock.  Works even on
    /// designs with no clock, where `tick` cannot.
    Advance { count: u64 },
    Tick { count: u64, clock: Option<SPath> },
    Assert { expr: Expr, line: String },
    Print { expr: Expr },
    Restart,
    Exit,
    /// `ls`: list the current module's components (kind + type) and
    /// submodule/socket instances (ofness).
    Ls,
    /// `show`: print the value of every signal in the design,
    /// grouped by module.
    Show,
    /// `autoshow [on|off]`: set (or toggle, with no argument) whether
    /// the design is shown after every `poke` and `tick`.
    Autoshow(Option<bool>),
    /// `vcd <file>`: attach a VCD trace of every dumpable signal to
    /// the simulation, written to the given file from now on.  The
    /// file name may be quoted (`vcd "my trace.vcd"`) or bare
    /// (`vcd trace.vcd`, `vcd out/trace.vcd`).
    Vcd(String),
    /// `prop [on|off]`: toggle (or set) propagation-delay stamping
    /// for the VCD trace (see `plans/gates_and_prop`): value changes
    /// are stamped at `settlement time + pole delay` instead of the
    /// settlement time.  Reset by `restart` (a fresh `Sim`).
    Prop(Option<bool>),
}
