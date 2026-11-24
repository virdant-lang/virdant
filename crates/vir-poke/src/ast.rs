//! The `vir poke` scripting language's AST.
//!
//! Deliberately separate from the parser's token stream: the AST
//! carries the aggregate literal syntax (struct/union/enum) without
//! reworking the command layer.

/// A path as written in a script.  Three starting forms:
///
/// - `top.foo.bar` — absolute, from the top of the elaborated design;
/// - `foo.bar`     — relative to the current module (`cd` location);
/// - `it.foo`      — the previously peeked value (expressions only;
///   only bare `it` is meaningful — field access on `it` is not
///   part of the grammar).
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
    /// An enum constructor: `@Foo`, `@Foo(arg, ...)`, or the older
    /// `#Foo` spelling.  Its type comes from context (the other side
    /// of a comparison, or the `poke` target), resolved at evaluation
    /// time.
    Ctor { name: String, args: Vec<Expr> },
    /// A struct literal: `${f=expr, ...}`.  (Fields may come in any
    /// order; every declared field must be given exactly once.)  Its
    /// type comes from context, like [`Expr::Ctor`].
    StructLit { fields: Vec<(String, Expr)> },
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

/// A display radix for a Word signal (`format`): how its value
/// renders in `show`, `peek`, `poke`, and watch output.  See
/// `Session::formats` for how the session stores these.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Fmt {
    /// `0b...` — full-width binary (leading zeros included, so the
    /// width is recoverable).
    Bin,
    /// `0x...` — zero-padded to whole nibbles (width recoverable).
    Hex,
    /// Plain decimal digits (the width is not recoverable).
    Dec,
}

impl std::fmt::Display for Fmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fmt::Bin => write!(f, "bin"),
            Fmt::Hex => write!(f, "hex"),
            Fmt::Dec => write!(f, "dec"),
        }
    }
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
    ///
    /// `show <path>` instead selects the signal for the VCD trace:
    /// the first call makes the trace contain *only* that signal,
    /// each further call adds one more (the trace starts off carrying
    /// every signal).  Bare `show` never changes the list.
    Show(Option<SPath>),
    /// `autoshow [on|off]`: set (or toggle, with no argument) whether
    /// the design is shown after every `poke` and `tick`.
    Autoshow(Option<bool>),
    /// `vcd <file>`: attach a VCD trace of the traced signals to the
    /// simulation, written to the given file from now on.  The
    /// file name may be quoted (`vcd "my trace.vcd"`) or bare
    /// (`vcd trace.vcd`, `vcd out/trace.vcd`); with no argument the
    /// default is `<script>.vcd` (the running script's name) or
    /// `trace.vcd` when no script is running.
    Vcd(Option<String>),
    /// `top <module>`: reload the simulator with a new top module
    /// (`package::ModDef`, or a simple name when unambiguous).
    Top(String),
    /// `format <path> hex|bin|dec`: set how the signal at `path`
    /// prints (in `show`, `peek`, `poke`, and watch output).  Any
    /// signal takes a format; the radix only changes how Word values
    /// render — everything else keeps its round-trippable rendering.
    Format { path: SPath, fmt: Fmt },
}
