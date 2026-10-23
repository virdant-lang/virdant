//! Defines every diagnostic type emitted by the compiler

use bstr::{BStr, BString};

use crate::common::{DriverType, Width, WordValue};
use crate::common::PortDir;
use crate::common::source::Region;

pub type Type = BString;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub region: Region,
    pub payload: DiagnosticPayload,
}

#[derive(Clone, Debug)]
pub enum DiagnosticPayload {
    /// `import` statement names a package which does not exist.
    SyntaxError(SyntaxError),
    ImportCycle(ImportCycle),
    /// `import` statement names a package which does not exist.
    UnresolvedImportError(UnresolvedImportError),
    /// Package contains two or more `import` statements naming the same package.
    /// Lists regions to all such statements.
    DuplicateImport(DuplicateImport),
    /// Two items share the same name in the same package.
    /// Lists regions to all such item declarations.
    DuplicateItem(DuplicateItem),
    /// Two items share the same name in the same package.
    /// Lists regions to all such item declarations.
    DuplicateSlot(DuplicateSlot),
    /// Failed to resolve a package.
    UnresolvedPackage(UnresolvedPackage),
    /// Failed to resolve an item.
    UnresolvedItem(UnresolvedItem),
    /// Failed to resolve a type.
    UnresolvedType(UnresolvedType),
    /// A `reg` component is missing an `on` clause.
    MissingOnClause(MissingOnClause),
    /// A non-`reg` component has an unexpected `on` clause.
    UnexpectedOnClause(UnexpectedOnClause),
    /// A `driver` statement used the wrong driver type.
    /// Eg, a `reg` used `:=` or a `wire` used `<=`.
    WrongDriverType(WrongDriverType),
    /// A reg component has no drivers.
    NoRegDrivers(NoRegDrivers),
    /// Component has no drivers.
    NoDrivers(NoDrivers),
    /// Component has multiple drivers.
    MultipleDrivers(MultipleDrivers),
    /// Incoming signal has a driver.
    DriverForSink(DriverForSink),
    /// A component could not be resolved.
    UnresolvedComponent(UnresolvedComponent),
    /// A path uses the component name instead of `it` inside a driver block.
    NotIt(NotIt),
    /// A component could not be resolved.
    UnresolvedCtor(UnresolvedCtor),
    /// A component could not be resolved.
    UnusedSource(UnusedSource),
    /// Read from a component which is a sink.
    /// Eg, a read from an `outgoing` port.
    ReadFromSink(ReadFromSink),
    /// A component could not be resolved.
    UnfilledHole(UnfilledHole),
    ModuleCycle(ModuleCycle),
    /// Failed to resolve a method.
    UnresolvedMethod(UnresolvedMethod),
    WrongType(WrongType),
    Unknown(Unknown),
    DoesntFit(DoesntFit),
    NotWordType(NotWordType),
    CantTruncate(CantTruncate),
    Todo(Todo),
    /// A `match` does not cover every value of its subject.
    MatchNotExhaustive(MatchNotExhaustive),
    /// A `case` arm overlaps with an earlier `case` arm.
    MatchOverlappingArm(MatchOverlappingArm),
    /// A docstring (`//>` or `//!`) whose content does not start with a space.
    /// The convention is `//> text` or `//! text`, not `//>text` or `//!text`.
    InvalidDocstring(InvalidDocstring),
    /// Two enumerants of the same enum type have the same value.
    DuplicateEnumValue(DuplicateEnumValue),
    /// An enum type specifies values for some but not all enumerants.
    EnumPartiallySpecified(EnumPartiallySpecified),
    /// A single-bit index `a[i]` where `i >= width` of the subject `Word[n]`.
    IndexOutOfBounds(IndexOutOfBounds),
    /// A bit range `a[hi..lo]` where the bounds exceed the width of the subject.
    IndexRangeOutOfBounds(IndexRangeOutOfBounds),
    /// A bit range `a[hi..lo]` where `hi < lo` (empty/invalid range).
    InvalidIndexRange(InvalidIndexRange),
    /// A dynamic word index `a[i]` where `a: Word[n]`, `i: Word[k]`,
    /// but `n != 2^k`.
    InvalidWordIndexWidth(InvalidWordIndexWidth),
    /// A dynamic word index `a[i]` where the subject or index is not a Word type.
    IndexNotWordType(IndexNotWordType),
    /// A single component is unused multiple times
    RedundantUnused(RedundantUnused),
    /// Latched driver from a component to itself
    RedundantDriver(RedundantDriver),
    /// An `initial` statement targets a component which is not a register.
    InitialForNonReg(InitialForNonReg),
    /// A register has more than one `initial` statement.
    InitialMultiple(InitialMultiple),
    /// An `initial` statement appears inside a `when` or `match` block.
    InitialInsideWhenMatch(InitialInsideWhenMatch),
    /// A combinational loop was detected in a module's dependency graph.
    /// All edges in the cycle are combinational, which would causal, which would cause
    /// simulation to never converge.
    CombinationalLoop(CombinationalLoop),
    ImportNotAtTopError,
    InvalidIndentation,
    TrailingSpaces,
    /// `it` keyword used outside of an `ItBlock`.
    ItNotInItBlock,
    CantInfer,
    WrongArgCount,
    /// The `else` arm covers no values not already covered by earlier `case` arms.
    MatchRedundantElse,
    /// A `match` has more than one `else` arm.
    MatchMultipleElse,
    /// An `else` arm appears before the last position in a `match`.
    MatchElseNotLast,
    /// An enum type's first enumerant does not have an inferrable width.
    EnumUnknownWidth,
    /// A driver block is empty.
    EmptyDriverBlock,
    /// A `when`/`match` arm uses a `{ ... }` block for a value where the
    /// when/match is used as an expression; the idiomatic arm form is
    /// `case <cond> => <expr>`.
    ArmBlockNeedsArrow,
    /// A `when`/`match` arm produces a value where the when/match is used
    /// as a statement; the idiomatic arm form is `case <cond> { <stmts> }`
    /// with driver statements.
    ArmNeedsBlock,
    /// A module's `for` clause names an item which is not a platform.
    ExpectedPlatform(ExpectedPlatform),
    /// A module's port set is missing a port required by its platform.
    PlatformMissingPort(PlatformMissingPort),
    /// A module declares a port which its platform does not have.
    PlatformExtraPort(PlatformExtraPort),
    /// A module port's direction does not match its platform's port direction.
    PlatformPortDirMismatch(PlatformPortDirMismatch),
    /// A module port's type does not match its platform's port type.
    PlatformPortTypeMismatch(PlatformPortTypeMismatch),
    /// A mod definition (not a platform) declares an `outgoing` port
    /// with a `default` clause.
    DefaultOutsidePlatformPort(DefaultOutsidePlatformPort),
    /// A parsed feature whose semantics are not implemented yet
    /// (e.g. generic socket/mod defs in the types-only generics slice).
    NotYetImplemented(NotYetImplemented),
    /// An `ext mod` declares generic parameters, or an ext-mod
    /// instantiation supplies type arguments.  This is a hard rule:
    /// ext mods never support generics (they parameterize through
    /// `@parameter` annotations instead).
    ExtModGenerics(ExtModGenerics),
    /// An `export mod` declares generic parameters: a top module must
    /// be a concrete instantiation, so it cannot be generic.
    ExportModGenerics(ExportModGenerics),
    /// A `:=:` connects two socket instances of the same socketdef
    /// instantiated at different type arguments (different mono
    /// sockets).  Names both instantiations.
    SocketConnectMismatch(SocketConnectMismatch),
    /// A type reference supplies the wrong number of type arguments.
    TypeArgCount(TypeArgCount),
    /// A type argument does not have the kind its parameter declares
    /// (e.g. `Valid[8]`: a `Nat` literal given to a `Type` parameter).
    TypeArgKind(TypeArgKind),
    /// A generic parameter is declared with an unknown kind
    /// (neither `Type` nor `Nat`).
    UnknownKind(UnknownKind),
    /// A ctor or enumerant carries payload parameters but was used
    /// without them (e.g. `#Name` where `@Name(...)` is required).
    CtorRequiresArgs(CtorRequiresArgs),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    pub message: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportCycle {
    pub package_cycle: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedImportError {
    pub imported_package: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateImport {
    pub imported_package: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateItem {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateSlot {
    pub item: BString,
    pub slot: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedPackage {
    pub package: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedItem {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedType {
    pub typ: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingOnClause {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedOnClause {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongDriverType {
    pub target: BString,
    pub expected_driver_type: DriverType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoRegDrivers {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoDrivers {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultipleDrivers {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DriverForSink {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedComponent {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotIt {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedCtor {
    pub ctor: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedSource {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadFromSink {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnfilledHole {
    pub name: Option<BString>,
    pub typ: Option<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleCycle {
    pub module_cycle: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedMethod {
    pub method: BString,
    pub subject_typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongType {
    pub expected: Type,
    pub actual: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unknown {
    pub message: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoesntFit {
    pub value: WordValue,
    pub width: Width,
    pub minwidth: Width,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotWordType {
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CantTruncate {
    pub source_width: Width,
    pub target_width: Width,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Todo {
    pub message: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchNotExhaustive {
    pub subject_typ: Type,
    pub missing: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchOverlappingArm {
    pub overlap: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidDocstring {
    pub content: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateEnumValue {
    pub enum_name: BString,
    pub value: WordValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumPartiallySpecified {
    pub enum_name: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexOutOfBounds {
    pub array_width: Width,
    pub index: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexRangeOutOfBounds {
    pub array_width: Width,
    pub index_hi: u16,
    pub index_lo: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidIndexRange {
    pub array_width: Width,
    pub index_hi: u16,
    pub index_lo: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidWordIndexWidth {
    pub array_width: Width,
    pub index_width: Width,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexNotWordType {
    pub typ: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RedundantUnused {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RedundantDriver {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitialForNonReg {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitialMultiple {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitialInsideWhenMatch {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CombinationalLoop {
    pub components: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedPlatform {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlatformMissingPort {
    pub platform: BString,
    pub port: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlatformExtraPort {
    pub platform: BString,
    pub port: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultOutsidePlatformPort {
    pub port: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlatformPortDirMismatch {
    pub port: BString,
    pub expected: PortDir,
    pub actual: PortDir,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlatformPortTypeMismatch {
    pub port: BString,
    pub expected: crate::types::Type,
    pub actual: crate::types::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotYetImplemented {
    pub feature: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtModGenerics {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportModGenerics {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SocketConnectMismatch {
    /// The rendered mono socket of the left-hand side,
    /// e.g. `Pipe[Word[8]]`.
    pub lhs: BString,
    /// The rendered mono socket of the right-hand side.
    pub rhs: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgCount {
    pub typ: BString,
    pub expected: usize,
    pub actual: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeArgKind {
    /// The generic type being instantiated.
    pub typ: BString,
    /// The parameter whose kind was violated.
    pub param: BString,
    /// The kind the parameter declares.
    pub expected: BString,
    /// What was written instead ("a type" or "a Nat literal").
    pub actual: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownKind {
    pub kind: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CtorRequiresArgs {
    pub ctor: BString,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl Diagnostic {
    pub fn new(region: Region, payload: impl Into<DiagnosticPayload>) -> Self {
        Self { region, payload: payload.into() }
    }

    pub fn region(&self) -> Region {
        self.region.clone()
    }

    pub fn message(&self) -> BString {
        use bstr::ByteSlice;
        match &self.payload {
            DiagnosticPayload::SyntaxError(d) => format!("Syntax Error: {}", d.message).into(),
            DiagnosticPayload::ImportNotAtTopError => "Import not at top of file".into(),
            DiagnosticPayload::InvalidIndentation => "Line must be indented by a multiple of 4 spaces".into(),
            DiagnosticPayload::TrailingSpaces => "Line should not end with trailing space".into(),
            DiagnosticPayload::ImportCycle(d) => {
                debug_assert!(d.package_cycle.len() > 0);
                if d.package_cycle.len() > 1 {
                    let package_cycle = d
                        .package_cycle
                        .iter()
                        .map(|package| package.to_string())
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!("Import cycle: {package_cycle}").into()
                } else {
                    format!("Package imports itself: {}", &d.package_cycle[0]).into()
                }
            }
            DiagnosticPayload::UnresolvedImportError(d) => format!("Unresolved import: {}", d.imported_package).into(),
            DiagnosticPayload::DuplicateImport(_) => "Duplicate import".into(),
            DiagnosticPayload::DuplicateItem(_) => "Duplicate Item".to_owned().into(),
            DiagnosticPayload::DuplicateSlot(_) => "Duplicate slot".into(),
            DiagnosticPayload::UnresolvedPackage(d) => format!("Unresolved package {}", d.package).into(),
            DiagnosticPayload::UnresolvedItem(d) => format!("Unresolved item {}", d.item).into(),
            DiagnosticPayload::UnresolvedType(d) => format!("Unresolved type {}", d.typ).into(),
            DiagnosticPayload::MissingOnClause(d) => format!("Missing on clause for reg {}", d.component).into(),
            DiagnosticPayload::UnexpectedOnClause(d) => format!("Unexpected on clause {}", d.component).into(),
            DiagnosticPayload::WrongDriverType(d) => {
                let driver_type_str = match d.expected_driver_type {
                    DriverType::Continuous => ":=",
                    DriverType::Latched => "<=",
                };
                format!(
                    "Wrong driver type for {}, expected {driver_type_str}",
                    d.target,
                ).into()
            }
            DiagnosticPayload::NoRegDrivers(d) => format!("No drivers for {}", d.target).into(),
            DiagnosticPayload::NoDrivers(d) => format!("No drivers for {}", d.target).into(),
            DiagnosticPayload::MultipleDrivers(d) => format!("Multiple drivers for {}", d.target).into(),
            DiagnosticPayload::DriverForSink(d) => format!("Driver for sink {}", d.target).into(),
            DiagnosticPayload::UnresolvedComponent(d) => format!("Unresolved component {}", d.path).into(),
            DiagnosticPayload::ItNotInItBlock => "'it' used outside of an it block".into(),
            DiagnosticPayload::NotIt(d) => {
                format!(
                    "'{}' should be written as 'it' inside of this driver block",
                    d.component,
                ).into()
            }
            DiagnosticPayload::UnresolvedCtor(d) => format!("Unresolved constructor {}", d.ctor).into(),
            DiagnosticPayload::UnusedSource(d) => format!("Unused signal {}", d.path).into(),
            DiagnosticPayload::ReadFromSink(d) => format!("Read from sink {}", d.path).into(),
            DiagnosticPayload::UnfilledHole(d) => {
                let name = if let Some(name) = &d.name {
                    name.as_bstr()
                } else {
                    BStr::new("?")
                };
                if let Some(typ) = &d.typ {
                    format!("Unfilled hole: {name} : {typ}").into()
                } else {
                    format!("Unfilled hole: {name}").into()
                }
            }
            DiagnosticPayload::ModuleCycle(d) => {
                let cycle = d
                    .module_cycle
                    .iter()
                    .map(|m| m.to_string())
                    .collect::<Vec<_>>();
                format!("Module cycle: {}", cycle.join(", ")).into()
            }
            DiagnosticPayload::UnresolvedMethod(d) => format!("Unresovled method: {} on type {}", d.method, d.subject_typ).into(),
            DiagnosticPayload::WrongType(d) => {
                format!(
                    "Wrong type: Expected {} but found {}",
                    d.expected, d.actual,
                ).into()
            }
            DiagnosticPayload::Unknown(d) => d.message.to_string().into(),
            DiagnosticPayload::DoesntFit(d) => {
                format!(
                    "Value doesn't fit: {} is a {}-bit value, but literal is type builtin::Word[{}]",
                    d.value, d.minwidth, d.width,
                ).into()
            }
            DiagnosticPayload::NotWordType(d) => format!("Expected type {} which is not a Word type", d.typ).into(),
            DiagnosticPayload::CantInfer => "Can't infer".into(),
            DiagnosticPayload::WrongArgCount => "Wrong arg count".into(),
            DiagnosticPayload::CantTruncate(d) => {
                format!(
                    "Cannot truncate Word[{}] to the larger type Word[{}]",
                    d.source_width, d.target_width,
                ).into()
            }
            DiagnosticPayload::Todo(d) => format!("TODO: {}", d.message).into(),
            DiagnosticPayload::MatchNotExhaustive(d) => {
                format!(
                    "Non-exhaustive match on {}: not covered: {}",
                    d.subject_typ, d.missing,
                ).into()
            }
            DiagnosticPayload::MatchOverlappingArm(d) => format!("Overlapping match arm: {}", d.overlap).into(),
            DiagnosticPayload::MatchRedundantElse => "Redundant else arm: all values are already covered".into(),
            DiagnosticPayload::MatchMultipleElse => "Multiple else arms in match".into(),
            DiagnosticPayload::MatchElseNotLast => "else arm must be the last arm of the match".into(),
            DiagnosticPayload::InvalidDocstring(d) => {
                format!(
                    "Invalid docstring: content must start with a space, got {:?}",
                    d.content,
                ).into()
            }
            DiagnosticPayload::EnumUnknownWidth => "Enum type's first enumerant does not have an inferrable width. Add an explicit width, e.g. `= 0wN`.".into(),
            DiagnosticPayload::DuplicateEnumValue(d) => {
                format!(
                    "Duplicate enum value: enumerants of {} share value {}",
                    d.enum_name, d.value,
                ).into()
            }
            DiagnosticPayload::EnumPartiallySpecified(d) => {
                format!(
                    "Enum type {} specifies values for some but not all enumerants. Either give every enumerant a value, or none (which defaults to one-hot encoding).",
                    d.enum_name,
                ).into()
            }
            DiagnosticPayload::InvalidWordIndexWidth(d) => {
                format!(
                    "Invalid word index width: Word[{}] indexed by Word[{}] requires {} == 2^{}",
                    d.array_width, d.index_width, d.array_width, d.index_width,
                ).into()
            }
            DiagnosticPayload::IndexOutOfBounds(d) => {
                format!(
                    "Bit index {} out of bound for Word[{}]",
                    d.index, d.array_width,
                ).into()
            }
            DiagnosticPayload::IndexRangeOutOfBounds(d) => {
                format!(
                    "Bit range {}..{} out of bounds for Word[{}]",
                    d.index_hi, d.index_lo, d.array_width,
                ).into()
            }
            DiagnosticPayload::InvalidIndexRange(d) => {
                format!(
                    "Invalid bit range {}..{}: upper bound must be greater than or equal to lower bound",
                    d.index_hi, d.index_lo,
                ).into()
            }
            DiagnosticPayload::IndexNotWordType(d) => format!("Expected a Word type: {}", d.typ).into(),
            DiagnosticPayload::EmptyDriverBlock => "Empty driver block".into(),
            DiagnosticPayload::ArmBlockNeedsArrow => {
                "For expression-based conditionals, use `=>` instead of `{ ... }`".into()
            }
            DiagnosticPayload::ArmNeedsBlock => {
                "For statement-based conditionals, use `{ ... }` instead of `=>`".into()
            }
            DiagnosticPayload::RedundantUnused(d) => format!("Redundant `unused`: {}", d.path).into(),
            DiagnosticPayload::RedundantDriver(d) => format!("Redundant driver: {}", d.path).into(),
            DiagnosticPayload::InitialForNonReg(d) => {
                format!("initial targets non-register component {}", d.target).into()
            }
            DiagnosticPayload::InitialMultiple(d) => {
                format!("multiple initial statements for register {}", d.target).into()
            }
            DiagnosticPayload::InitialInsideWhenMatch(d) => {
                format!(
                    "initial statement for {} must not appear inside a when/match block",
                    d.target,
                ).into()
            }
            DiagnosticPayload::CombinationalLoop(d) => {
                format!(
                    "Combinational loop detected: {}",
                    d.components
                        .iter()
                        .map(|c| String::from_utf8_lossy(c).into_owned())
                        .collect::<Vec<_>>()
                        .join(" -> "),
                ).into()
            }
            DiagnosticPayload::ExpectedPlatform(d) => format!("Expected a platform: {} is not a platform", d.item).into(),
            DiagnosticPayload::PlatformMissingPort(d) => format!("Module is missing port {} required by platform {}", d.port, d.platform).into(),
            DiagnosticPayload::PlatformExtraPort(d) => format!("Module has port {} which platform {} does not declare", d.port, d.platform).into(),
            DiagnosticPayload::DefaultOutsidePlatformPort(d) => format!("`default` on outgoing port {} is only allowed in a platform definition", d.port).into(),
            DiagnosticPayload::PlatformPortDirMismatch(d) => {
                format!(
                    "Port {} has direction {:?} but platform expects {:?}",
                    d.port, d.actual, d.expected,
                ).into()
            }
            DiagnosticPayload::PlatformPortTypeMismatch(d) => {
                format!(
                    "Port {} has type {:?} but platform expects {:?}",
                    d.port, d.actual, d.expected,
                ).into()
            }
            DiagnosticPayload::NotYetImplemented(d) => {
                format!("Not yet implemented: {}", d.feature).into()
            }
            DiagnosticPayload::ExtModGenerics(d) => {
                format!(
                    "ext mod {} cannot have generics: ext mods do not support type parameters \
                     (use @parameter annotations instead)",
                    d.item,
                ).into()
            }
            DiagnosticPayload::ExportModGenerics(d) => {
                format!(
                    "export mod {} cannot have generic parameters: a top module must be a concrete instantiation",
                    d.item,
                ).into()
            }
            DiagnosticPayload::SocketConnectMismatch(d) => {
                format!(
                    "Cannot connect {} to {}: mismatched socket instantiations",
                    d.lhs, d.rhs,
                ).into()
            }
            DiagnosticPayload::TypeArgCount(d) => {
                format!(
                    "Type {} takes {} type argument{}, but {} {} given",
                    d.typ,
                    d.expected,
                    if d.expected == 1 { "" } else { "s" },
                    d.actual,
                    if d.actual == 1 { "was" } else { "were" },
                ).into()
            }
            DiagnosticPayload::TypeArgKind(d) => {
                format!(
                    "Type argument mismatch: parameter {} of {} expects {}, but found {}",
                    d.param, d.typ, d.expected, d.actual,
                ).into()
            }
            DiagnosticPayload::UnknownKind(d) => {
                format!(
                    "Unknown kind {}: generic parameters must be declared as `: Type` or `: Nat`",
                    d.kind,
                ).into()
            }
            DiagnosticPayload::CtorRequiresArgs(d) => {
                format!(
                    "Ctor {} requires arguments: use @{}(...)",
                    d.ctor, d.ctor,
                ).into()
            }
        }
    }

    pub fn level(&self) -> DiagnosticLevel {
        match self.payload {
            DiagnosticPayload::NoRegDrivers(_)
            | DiagnosticPayload::NotIt(_)
            | DiagnosticPayload::UnusedSource(_)
            | DiagnosticPayload::ReadFromSink(_)
            | DiagnosticPayload::UnfilledHole(_)
            | DiagnosticPayload::EmptyDriverBlock
            | DiagnosticPayload::RedundantDriver(_)
            | DiagnosticPayload::TrailingSpaces => DiagnosticLevel::Warning,
            DiagnosticPayload::Todo(_) => DiagnosticLevel::Info,
            _ => DiagnosticLevel::Error,
        }
    }
}

impl std::fmt::Display for DiagnosticLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            DiagnosticLevel::Error => "Error",
            DiagnosticLevel::Warning => "Warning",
            DiagnosticLevel::Info => "Info",
        };
        write!(f, "{name}")
    }
}

macro_rules! from_diagnostic {
    ($($t:ident),* $(,)?) => {
        $(
            impl From<$t> for DiagnosticPayload {
                fn from(d: $t) -> Self {
                    Self::$t(d)
                }
            }
        )*
    };
}

from_diagnostic! {
    SyntaxError, ImportCycle, UnresolvedImportError, DuplicateImport, DuplicateItem,
    DuplicateSlot, UnresolvedPackage, UnresolvedItem, UnresolvedType, MissingOnClause,
    UnexpectedOnClause, WrongDriverType, NoRegDrivers, NoDrivers, MultipleDrivers,
    DriverForSink, UnresolvedComponent, NotIt, UnresolvedCtor, UnusedSource, ReadFromSink,
    UnfilledHole, ModuleCycle, UnresolvedMethod, WrongType, Unknown, DoesntFit, NotWordType,
    CantTruncate, Todo, MatchNotExhaustive, MatchOverlappingArm, InvalidDocstring,
    DuplicateEnumValue, EnumPartiallySpecified, IndexOutOfBounds, IndexRangeOutOfBounds, InvalidIndexRange,
    InvalidWordIndexWidth, IndexNotWordType, RedundantUnused, RedundantDriver,
    CombinationalLoop, InitialForNonReg, InitialMultiple, InitialInsideWhenMatch,
    ExpectedPlatform, PlatformMissingPort, PlatformExtraPort, PlatformPortDirMismatch,
    PlatformPortTypeMismatch, DefaultOutsidePlatformPort, NotYetImplemented, TypeArgCount,
    TypeArgKind, UnknownKind,
    CtorRequiresArgs, ExtModGenerics, ExportModGenerics, SocketConnectMismatch,
}
