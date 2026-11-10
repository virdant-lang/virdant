//! The AST node payloads produced by the ideal grammar.
//!
//! This is a redesign, not a mirror: the production node structure
//! (`virdant::syntax::payload`) carries a fair amount of parse-history
//! — wrapper nodes that only ever wrap one child, three different node
//! kinds for what is grammatically the same brace block, count-based
//! child lists for `if`/`match` arms, and name-shaped things (`Path`,
//! `Ofness`) as childless nodes.  The ideal grammar's nodes follow two
//! rules instead:
//!
//! 1. **A node exists when it marks a decision.**  A node's kind plus
//!    its children should tell the whole story: no wrapper-only nodes,
//!    no always-present-but-often-empty wrappers, no redundant
//!    payload/child pairs.
//! 2. **Names are data, not subtrees.**  A path (`it.a.b`) or a
//!    (possibly qualified) name (`xilinx::ArtyA7`) is a leaf: it lives
//!    in the payload as interned text, never as a child node.
//!
//! Everything that is genuinely structural keeps a node: clauses
//! (`DependsonList`, `DefaultClause`, `OnClause`), arm groups
//! (`IfArm`, `MatchArm`), blocks (`Block`), and every expression or
//! pattern form.
//!
//! The shared vocabulary enums (`BinOp`, `ComponentKind`, ...) are
//! reused from `virdant::common` rather than redefined: the ideal
//! grammar varies the *grammar*, not the meaning of the nodes.

// The payload structs are data carriers only: the tree dump reads a
// subset of their fields, and the rest exist so that every node is
// fully self-describing.
#![allow(dead_code)]

use virdant::common::{BinOp, ChannelDir, ComponentKind, DriverType, SocketRole, Strength, UnOp};

/// A node id: an index into `Parsing`'s parallel vectors.
pub type NodeId = usize;

/// An interned string: an index into `Parsing`'s string table.
pub type Sym = usize;

/// A (possibly package-qualified) name: `Ident` or `Ident::Ident`.
/// A value, not a node — it appears in the payloads of whatever
/// references it (types, instance targets, call callees, `for`
/// clauses).
#[derive(Clone, Debug)]
pub struct Ofness {
    pub package: Option<Sym>,
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub enum Payload {
    // ----- Top level --------------------------------------------------
    /// The whole file.  Children: `Import` and item nodes in source
    /// order.
    Package(Package),
    Import(Import),

    // ----- Items ------------------------------------------------------
    /// Children: annotations (if any), `Generics` (if any), then the
    /// body statements in source order.
    ModDef(ModDef),
    StructDef(StructDef),
    UnionDef(UnionDef),
    EnumDef(EnumDef),
    BuiltinDef(BuiltinDef),
    SocketDef(SocketDef),
    PlatformDef(PlatformDef),

    // ----- Declaration statements (module-level contexts only) --------
    /// A component declaration.  Children: annotations, `Type`, then
    /// the optional clauses in written order: `DependsonList`,
    /// `DefaultClause`, `OnClause`, and the body `Block`.
    Component(Component),
    /// A memory declaration.  Children: annotations, the depth `Expr`,
    /// the element `Type`, then the optional `OnClause` and body
    /// `Block`.
    Memory(Memory),
    /// A submodule instance.  Children: annotations, the optional
    /// `GenericsParams`, and the body `Block`.
    Submodule(Submodule),
    /// A socket instance.  Children: annotations, the optional
    /// `GenericsParams`, and the body `Block`.
    SocketInstance(SocketInstance),

    // ----- Executable statements --------------------------------------
    /// Children: the RHS `Expr`.  The LHS path and the operator's
    /// driver type/strength live in the payload.
    Driver(Driver),
    /// A write to one word of a `memory`.  Children: the index `Expr`
    /// and the value `Expr`, always both — a constant index is an
    /// `ExprWordLit` child, a dynamic one (`dyn e`) an arbitrary
    /// `Expr`; `is_dyn` in the payload says which.
    IndexedDriver(IndexedDriver),
    /// A `#=#` / `:=:` connection between two paths; both live in the
    /// payload, so this node has no children.
    BidirectionalDriver(BidirectionalDriver),
    Unused(Unused),
    /// Children: the initial value `Expr`.
    Initial(Initial),

    // ----- Memory bodies ----------------------------------------------
    /// An `on <CLOCK> { ... }` clocked-write region inside a memory
    /// body.  Children: the clock `Expr`, then the region's `Block`.
    OnRegion,

    // ----- Clauses -------------------------------------------------------
    /// A `dependson` clause marking which incoming ports a component
    /// may combinationally depend on.  The paths are payload (names
    /// are data); the node exists to mark the clause.
    DependsonList(DependsonList),
    /// An `on <CLOCK>` clause.  Children: the clock `Expr`.
    OnClause,
    /// A `default <EXPR>` clause.  Children: the `Expr`.
    DefaultClause,

    // ----- Control flow ------------------------------------------------
    /// Children: the arms in source order.
    If,
    /// One arm.  Children: the guard `Expr` and the body (`Expr` or
    /// `Block`) for a `when` arm; just the body for an `else` arm.
    IfArm,
    /// Children: the scrutinee `Expr`, then the arms in source order.
    Match,
    /// One arm.  Children: the pattern `Pat` and the body for a `with`
    /// arm; just the body for an `else` arm.
    MatchArm,
    /// Children: the `LetBinding`s in source order, then the body
    /// (`Expr` or `Block`).
    Let,
    /// One binding.  Children: the optional `Type`, then the value
    /// `Expr`.
    LetBinding(LetBinding),
    /// A brace-delimited block of statements — the body of a
    /// component, an `if`/`match`/`let` arm, an `on` region, or a
    /// memory.  Children: the statements in source order.
    Block,

    // ----- Definition bodies ------------------------------------------
    /// One annotation: `@name` or `@name(args)`.  Children: the
    /// argument expressions.
    Annotation(Annotation),
    /// A struct field.  Children: annotations, the `Type`.
    Field(Field),
    /// A union constructor.  Children: annotations, the `Param`s.
    Ctor(Ctor),
    /// An enumerant: `name`, `name = value`, `name(params)`, or
    /// `name(params) = value`.  Children: annotations, the `Param`s,
    /// then the optional value `Expr`.
    Enumerant(Enumerant),
    /// A socket channel (`cosi`/`soci`).  Children: annotations,
    /// `Type`, then the optional `DependsonList`.
    Channel(Channel),
    /// A parameter.  Children: annotations, the `Type`.
    Param(Param),

    // ----- Generics ----------------------------------------------------
    /// A generic parameter declaration list (`[A : Type, n : Nat]`).
    /// Children: one `KindParam` per parameter.
    Generics,
    /// One generic parameter declaration; the declared kind is payload
    /// text (a kind is just a name).
    KindParam(KindParam),
    /// A generic argument list (a type application: `Word[8]`).
    /// Children: the arguments — `GenericsNat` for width arguments,
    /// `Type` for type arguments.
    GenericsParams,
    GenericsNat(GenericsNat),

    // ----- Types -------------------------------------------------------
    /// A type: the (possibly qualified) name is payload; the optional
    /// `GenericsParams` is the only child.
    Type(Type),

    // ----- Expressions --------------------------------------------------
    /// A reference to a path; the path is payload.
    ExprReference,
    /// `tap path`; the path is payload.
    ExprTap(ExprTap),
    ExprParen,
    ExprBitLit(ExprBitLit),
    ExprWordLit(ExprWordLit),
    ExprStrLit(ExprStrLit),
    /// Children: the two operands; the operator is payload.
    ExprBinOp(ExprBinOp),
    /// Children: the operand; the operator is payload.
    ExprUnOp(ExprUnOp),
    /// Children: the base expression.
    ExprField(ExprField),
    /// A function call: the callee name is payload; children are the
    /// argument expressions.
    ExprFn,
    /// A constructor: `@Ident` or `@Ident(args)`; children are the
    /// argument expressions.
    ExprCtor(ExprCtor),
    ExprEnumerant(ExprEnumerant),
    /// A struct literal `${ name = expr, ... }`.  Children: the
    /// `Assign`s.
    ExprStruct,
    /// Children: the base expression; the index is payload.
    ExprIndex(ExprIndex),
    /// Children: the base expression; the bounds are payload.
    ExprIndexRange(ExprIndexRange),
    /// A dynamic index `base[dyn e]`.  Children: the base and the
    /// index expressions.
    ExprIndexDyn,
    /// Children: the expression and the ascribed `Type`.
    ExprAscription,
    ExprHole,
    ExprDontcare,
    /// One field of a struct literal.  Children: the value `Expr`.
    Assign(Assign),

    // ----- Patterns -----------------------------------------------------
    PatCtor(PatCtor),
    PatEnumerant(PatEnumerant),
    PatWordLit(PatWordLit),
    PatBitLit(PatBitLit),
    PatIdent(PatIdent),
    PatDontcare,
}

#[derive(Clone, Debug)]
pub struct Package {
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub package: Sym,
}

#[derive(Clone, Debug)]
pub struct ModDef {
    pub name: Sym,
    pub is_ext: bool,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
    pub for_clause: Option<Ofness>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct UnionDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct BuiltinDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct SocketDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct PlatformDef {
    pub name: Sym,
    pub is_pub: bool,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Component {
    pub name: Sym,
    pub kind: ComponentKind,
    pub doc_string: Option<Sym>,
    pub is_async: bool,
}

#[derive(Clone, Debug)]
pub struct Memory {
    pub name: Sym,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Submodule {
    pub name: Sym,
    pub doc_string: Option<Sym>,
    pub target: Ofness,
}

#[derive(Clone, Debug)]
pub struct SocketInstance {
    pub name: Sym,
    pub role: SocketRole,
    pub doc_string: Option<Sym>,
    pub target: Ofness,
}

#[derive(Clone, Debug)]
pub struct Driver {
    pub lhs: Sym,
    pub driver_type: DriverType,
    pub strength: Strength,
}

#[derive(Clone, Debug)]
pub struct IndexedDriver {
    pub lhs: Sym,
    pub driver_type: DriverType,
    pub strength: Strength,
    pub is_dyn: bool,
}

#[derive(Clone, Debug)]
pub struct BidirectionalDriver {
    pub lhs: Sym,
    pub rhs: Sym,
}

#[derive(Clone, Debug)]
pub struct Unused {
    pub path: Sym,
}

#[derive(Clone, Debug)]
pub struct Initial {
    pub lhs: Sym,
}

#[derive(Clone, Debug)]
pub struct DependsonList {
    pub paths: Vec<Sym>,
}

#[derive(Clone, Debug)]
pub struct ExprTap {
    pub path: Sym,
}

#[derive(Clone, Debug)]
pub struct Annotation {
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: Sym,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Ctor {
    pub name: Sym,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Enumerant {
    pub name: Sym,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Channel {
    pub name: Sym,
    pub dir: ChannelDir,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Sym,
    pub doc_string: Option<Sym>,
}

#[derive(Clone, Debug)]
pub struct KindParam {
    pub name: Sym,
    pub kind: Sym,
}

#[derive(Clone, Debug)]
pub struct GenericsNat {
    pub value: Sym,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub package: Option<Sym>,
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct ExprBitLit {
    pub literal: bool,
}

#[derive(Clone, Debug)]
pub struct ExprWordLit {
    pub literal: Sym,
}

#[derive(Clone, Debug)]
pub struct ExprStrLit {
    pub literal: Sym,
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
pub struct ExprField {
    pub field: Sym,
}

#[derive(Clone, Debug)]
pub struct ExprCtor {
    pub ctor: Sym,
}

#[derive(Clone, Debug)]
pub struct ExprEnumerant {
    pub enumerant: Sym,
}

#[derive(Clone, Debug)]
pub struct ExprIndex {
    pub index: u16,
}

#[derive(Clone, Debug)]
pub struct ExprIndexRange {
    pub index_hi: u16,
    pub index_lo: u16,
}

#[derive(Clone, Debug)]
pub struct Assign {
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct LetBinding {
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct PatCtor {
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct PatEnumerant {
    pub name: Sym,
}

#[derive(Clone, Debug)]
pub struct PatWordLit {
    pub literal: Sym,
}

#[derive(Clone, Debug)]
pub struct PatBitLit {
    pub literal: bool,
}

#[derive(Clone, Debug)]
pub struct PatIdent {
    pub name: Sym,
}

impl Payload {
    /// The node kind, for tree dumps.
    pub fn kind(&self) -> &'static str {
        match self {
            Payload::Package(_) => "Package",
            Payload::Import(_) => "Import",
            Payload::ModDef(_) => "ModDef",
            Payload::StructDef(_) => "StructDef",
            Payload::UnionDef(_) => "UnionDef",
            Payload::EnumDef(_) => "EnumDef",
            Payload::BuiltinDef(_) => "BuiltinDef",
            Payload::SocketDef(_) => "SocketDef",
            Payload::PlatformDef(_) => "PlatformDef",
            Payload::Component(_) => "Component",
            Payload::Memory(_) => "Memory",
            Payload::Submodule(_) => "Submodule",
            Payload::SocketInstance(_) => "SocketInstance",
            Payload::Driver(_) => "Driver",
            Payload::IndexedDriver(_) => "IndexedDriver",
            Payload::BidirectionalDriver(_) => "BidirectionalDriver",
            Payload::Unused(_) => "Unused",
            Payload::Initial(_) => "Initial",
            Payload::OnRegion => "OnRegion",
            Payload::DependsonList(_) => "DependsonList",
            Payload::OnClause => "OnClause",
            Payload::DefaultClause => "DefaultClause",
            Payload::If => "If",
            Payload::IfArm => "IfArm",
            Payload::Match => "Match",
            Payload::MatchArm => "MatchArm",
            Payload::Let => "Let",
            Payload::LetBinding(_) => "LetBinding",
            Payload::Block => "Block",
            Payload::Annotation(_) => "Annotation",
            Payload::Field(_) => "Field",
            Payload::Ctor(_) => "Ctor",
            Payload::Enumerant(_) => "Enumerant",
            Payload::Channel(_) => "Channel",
            Payload::Param(_) => "Param",
            Payload::Generics => "Generics",
            Payload::KindParam(_) => "KindParam",
            Payload::GenericsParams => "GenericsParams",
            Payload::GenericsNat(_) => "GenericsNat",
            Payload::Type(_) => "Type",
            Payload::ExprReference => "ExprReference",
            Payload::ExprTap(_) => "ExprTap",
            Payload::ExprParen => "ExprParen",
            Payload::ExprBitLit(_) => "ExprBitLit",
            Payload::ExprWordLit(_) => "ExprWordLit",
            Payload::ExprStrLit(_) => "ExprStrLit",
            Payload::ExprBinOp(_) => "ExprBinOp",
            Payload::ExprUnOp(_) => "ExprUnOp",
            Payload::ExprField(_) => "ExprField",
            Payload::ExprFn => "ExprFn",
            Payload::ExprCtor(_) => "ExprCtor",
            Payload::ExprEnumerant(_) => "ExprEnumerant",
            Payload::ExprStruct => "ExprStruct",
            Payload::ExprIndex(_) => "ExprIndex",
            Payload::ExprIndexRange(_) => "ExprIndexRange",
            Payload::ExprIndexDyn => "ExprIndexDyn",
            Payload::ExprAscription => "ExprAscription",
            Payload::ExprHole => "ExprHole",
            Payload::ExprDontcare => "ExprDontcare",
            Payload::Assign(_) => "Assign",
            Payload::PatCtor(_) => "PatCtor",
            Payload::PatEnumerant(_) => "PatEnumerant",
            Payload::PatWordLit(_) => "PatWordLit",
            Payload::PatBitLit(_) => "PatBitLit",
            Payload::PatIdent(_) => "PatIdent",
            Payload::PatDontcare => "PatDontcare",
        }
    }

    /// Extra detail for a tree dump: names, operators, flags.
    pub fn detail(&self, p: &crate::syntax::parsing::Parsing) -> String {
        let name = |sym: &Sym| String::from_utf8_lossy(p.sym_text(*sym)).into_owned();
        let ofness = |o: &Ofness| match &o.package {
            Some(package) => format!("{}::{}", name(package), name(&o.name)),
            None => name(&o.name),
        };
        match self {
            Payload::Import(import) => name(&import.package),
            Payload::ModDef(m) => format!(
                "{}{}{}{}",
                name(&m.name),
                if m.is_pub { " pub" } else { "" },
                if m.is_ext { " ext" } else { "" },
                m.for_clause.as_ref().map_or(String::new(), |o| format!(" for {}", ofness(o))),
            ),
            Payload::StructDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::UnionDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::EnumDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::BuiltinDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::SocketDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::PlatformDef(s) => format!("{}{}", name(&s.name), if s.is_pub { " pub" } else { "" }),
            Payload::Component(c) => format!(
                "{:?} {}{}",
                c.kind,
                name(&c.name),
                if c.is_async { " async" } else { "" },
            ),
            Payload::Memory(m) => name(&m.name),
            Payload::Submodule(s) => format!("{} of {}", name(&s.name), ofness(&s.target)),
            Payload::SocketInstance(s) => format!("{:?} {} of {}", s.role, name(&s.name), ofness(&s.target)),
            Payload::Driver(d) => format!("{} {:?} {:?}", name(&d.lhs), d.driver_type, d.strength),
            Payload::IndexedDriver(d) => format!(
                "{}[{}] {:?} {:?}",
                name(&d.lhs),
                if d.is_dyn { "dyn" } else { "const" },
                d.driver_type,
                d.strength,
            ),
            Payload::BidirectionalDriver(d) => format!("{} :=: {}", name(&d.lhs), name(&d.rhs)),
            Payload::Unused(u) => name(&u.path),
            Payload::DependsonList(d) => d.paths.iter().map(|p| name(p)).collect::<Vec<_>>().join(", "),
            Payload::ExprTap(t) => name(&t.path),
            Payload::Initial(i) => name(&i.lhs),
            Payload::Annotation(a) => name(&a.name),
            Payload::Field(f) => name(&f.name),
            Payload::Ctor(c) => name(&c.name),
            Payload::Enumerant(e) => name(&e.name),
            Payload::Channel(c) => format!("{:?} {}", c.dir, name(&c.name)),
            Payload::Param(param) => name(&param.name),
            Payload::KindParam(k) => format!("{} : {}", name(&k.name), name(&k.kind)),
            Payload::GenericsNat(n) => name(&n.value),
            Payload::Type(t) => ofness(&Ofness { package: t.package.clone(), name: t.name }),
            Payload::ExprBitLit(l) => format!("{}", l.literal),
            Payload::ExprWordLit(l) => name(&l.literal),
            Payload::ExprStrLit(l) => name(&l.literal),
            Payload::ExprBinOp(op) => format!("{:?}", op.op),
            Payload::ExprUnOp(op) => format!("{:?}", op.op),
            Payload::ExprField(f) => name(&f.field),
            Payload::ExprCtor(c) => name(&c.ctor),
            Payload::ExprEnumerant(e) => name(&e.enumerant),
            Payload::ExprIndex(i) => format!("{}", i.index),
            Payload::ExprIndexRange(r) => format!("{}..{}", r.index_hi, r.index_lo),
            Payload::Assign(a) => name(&a.name),
            Payload::LetBinding(b) => name(&b.name),
            Payload::PatCtor(p) => name(&p.name),
            Payload::PatEnumerant(p) => name(&p.name),
            Payload::PatWordLit(p) => name(&p.literal),
            Payload::PatBitLit(p) => format!("{}", p.literal),
            Payload::PatIdent(p) => name(&p.name),
            _ => String::new(),
        }
    }
}
