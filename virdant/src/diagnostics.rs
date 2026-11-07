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
    /// A package's name — derived from its `.vir` file's stem — is not
    /// a valid identifier, so no other file can ever `import` it.
    InvalidPackageName(InvalidPackageName),
    /// Failed to resolve an item.
    UnresolvedItem(UnresolvedItem),
    /// Failed to resolve a type.
    UnresolvedType(UnresolvedType),
    /// A `reg` component is missing an `on` clause.
    MissingOnClause(MissingOnClause),
    /// A `reg` component has an explicit `on` clause naming the module's
    /// unique clock. The module's implicit clock already covers it, so
    /// the `on` clause can be omitted.
    RedundantOnClause(RedundantOnClause),
    /// A `driver` statement used the wrong driver type.
    /// Eg, a `reg` used `:=` or a `wire` used `<=`.
    WrongDriverType(WrongDriverType),
    /// A reg component has no drivers.
    NoRegDrivers(NoRegDrivers),
    /// Component has no drivers.
    NoDrivers(NoDrivers),
    /// Component has multiple drivers.
    MultipleDrivers(MultipleDrivers),
    /// A `memory` component's element type is not `Word[n]` with `n` a
    /// power of two.
    MemoryElementNotPowerOf2Word(MemoryElementNotPowerOf2Word),
    /// A `memory` component's depth is not a literal constant
    MemoryDepthNotLiteral(MemoryDepthNotLiteral),
    /// A `memory` component's depth is not a power of two.
    MemoryDepthNotPowerOf2(MemoryDepthNotPowerOf2),
    /// A memory index is not exactly `Word[k]` where `depth == 2^k`.
    MemoryIndexWidth(MemoryIndexWidth),
    /// A constant memory index is not within the declared depth.
    MemoryIndexOutOfRange(MemoryIndexOutOfRange),
    /// A write to a memory appears outside a clocked write context
    /// (an `on` region, or the implicit single-clock context).
    MemoryWriteOutsideOnRegion(MemoryWriteOutsideOnRegion),
    /// Two sibling write clauses with non-exclusive guards write the
    /// same memory in one write site.
    OverlappingMemoryWrites(OverlappingMemoryWrites),
    /// A memory word was written with `:=` instead of `<=`.  Accepted
    /// with this warning so fixing it never changes legality.
    MemoryAssignInsteadOfLassign(MemoryAssignInsteadOfLassign),
    /// A memory component was referenced without an index
    /// (`mem` instead of `mem[dyn i]` / `mem[3]`).
    MemoryUsedWithoutIndex(MemoryUsedWithoutIndex),
    /// An indexed driver targets a component that is not a memory.
    IndexedDriverNotMemory(IndexedDriverNotMemory),
    /// An `on` region inside a memory body names a clock other than
    /// the memory's own clock.
    MemoryOnRegionClockMismatch(MemoryOnRegionClockMismatch),
    /// A statement other than a memory write appeared inside a memory
    /// body's `on` region.
    MemoryOnRegionNonWrite(MemoryOnRegionNonWrite),
    /// An `initial` statement targets a memory (not supported; use
    /// `@image`/`@image_hex`).
    MemoryInitialUnsupported(MemoryInitialUnsupported),
    /// A write to a memory initialized by `@image`/`@image_hex`: its
    /// contents come from the file.
    MemoryImageWrite(MemoryImageWrite),
    /// A memory image file could not be read.
    MemoryImageIo(MemoryImageIo),
    /// A memory image file holds more words than the declared depth
    /// (a shorter file is fine: the rest reads as unspecified).
    MemoryImageTooLong(MemoryImageTooLong),
    /// A memory image in a form not supported (e.g. an element
    /// width above 64 bits).
    MemoryImageNotSupported(MemoryImageNotSupported),
    /// A memory image on a `@noinitial` platform: image initialization
    /// is disallowed, exactly like `initial` statements.
    MemoryImageNoinitial(MemoryImageNoinitial),
    /// A `bus`/`net` component was referenced without `tap`.
    NetReadWithoutTap(NetReadWithoutTap),
    /// `tap` was applied to a component that is not a `bus` or `net`.
    TapNotOnNet(TapNotOnNet),
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
    /// A `with` arm overlaps with an earlier `with` arm.
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
    /// A component marked `unused` is actually used in the module.
    UnusedButUsed(UnusedButUsed),
    /// An `unused` statement marks an `outgoing` (sink) component of
    /// the module itself.
    UnusedOnSink(UnusedOnSink),
    /// An `unused` statement marks an `outgoing wire`.  Declaring an
    /// outgoing wire already signals the intent to use it, so this
    /// warns (it is not an error like a plain `outgoing` mark).
    UnusedOnOutgoingWire(UnusedOnOutgoingWire),
    /// Latched driver from a component to itself
    RedundantDriver(RedundantDriver),
    /// An `initial` statement targets a component which is not a register.
    InitialForNonReg(InitialForNonReg),
    /// A register has more than one `initial` statement.
    InitialMultiple(InitialMultiple),
    /// An `initial` statement appears inside an `if` or `match` block.
    InitialInsideIfMatch(InitialInsideIfMatch),
    /// An `initial` statement appears in a module reachable from a top
    /// module bound to a `@noinitial` platform.  The path names the
    /// instantiation chain from the top module down to the module
    /// containing the `initial` statement.
    InitialNoinitialPlatform(InitialNoinitialPlatform),
    /// A combinational loop was detected in a module's dependency graph.
    /// All edges in the cycle are combinational, which would causal, which would cause
    /// simulation to never converge.
    CombinationalLoop(CombinationalLoop),
    /// A cycle in the `dependson` relation (potentiality graph): the
    /// declared comb dependencies close a loop even if no actual comb
    /// path exists today.
    DependsonCycle(DependsonCycle),
    /// `dependson` used on a component kind that may not carry it.
    DependsonBadCarrier(DependsonBadCarrier),
    /// A `dependson` entry does not resolve to a legal dependee.
    DependsonBadEntry(DependsonBadEntry),
    /// An `outgoing` port has an actual/potential combinational
    /// dependency on an `incoming` port it does not declare.
    DependsonUndeclared(DependsonUndeclared),
    /// An `outgoing` port declares a `dependson` entry it does not (and
    /// cannot potentially) depend on. Warning: an unused `dependson`.
    DependsonUnused(DependsonUnused),
    ImportNotAtTopError,
    InvalidIndentation,
    TrailingSpaces,
    /// `it` keyword used outside of an `ItBlock`.
    ItNotInItBlock,
    CantInfer,
    WrongArgCount,
    /// The `else` arm covers no values not already covered by earlier `with` arms.
    MatchRedundantElse,
    /// A `match` has more than one `else` arm.
    MatchMultipleElse,
    /// An `else` arm appears before the last position in a `match`.
    MatchElseNotLast,
    /// An enum type's first enumerant does not have an inferrable width.
    EnumUnknownWidth,
    /// A driver block is empty.
    EmptyDriverBlock,
    /// An `if`/`match` arm uses a `{ ... }` block for a value where the
    /// if/match is used as an expression; the idiomatic arm form is
    /// `when <cond> => <expr>` (or `with <pat> => <expr>`).
    ArmBlockNeedsArrow,
    /// An `if`/`match` arm produces a value where the if/match is used
    /// as a statement; the idiomatic arm form is `when <cond> { <stmts> }`
    /// (or `with <pat> { <stmts> }`) with driver statements.
    ArmNeedsBlock,
    /// A `let` binding's body is a block (`{ ... }`) but the `let` is
    /// used in an expression position, where the binding's scope must
    /// produce a value (`=> <expr>`).
    LetBlockNeedsArrow,
    /// A `let` binding's body produces a value (`=> <expr>`) but the
    /// `let` is used in a statement position, where the binding's scope
    /// must perform driver statements (`{ <stmts> }`).
    LetArrowNeedsBlock,
    /// A module's `for` clause names an item which is not a platform.
    ExpectedPlatform(ExpectedPlatform),
    /// A module's port set is missing a port required by its platform.
    PlatformMissingPort(PlatformMissingPort),
    /// A module explicitly drives a platform port which it does not
    /// declare.  Synthesized platform ports may be left untouched (the
    /// platform `default` drives them), but driving one requires an
    /// explicit declaration in the mod def.
    DriveUndeclaredPlatformPort(DriveUndeclaredPlatformPort),
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
    NotPub(NotPub),
    ReferencedWhileNotPub(ReferencedWhileNotPub),
    UnusedNonPub(UnusedNonPub),
    TypeNotPub(TypeNotPub),
    TopNotPub(TopNotPub),
    TopGenerics(TopGenerics),
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

/// A package name that is not a valid identifier.  Package names come
/// from `.vir` file stems, so the fix is renaming the file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidPackageName {
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
pub struct RedundantOnClause {
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
pub struct MemoryElementNotPowerOf2Word {
    /// The offending element type, rendered (`Word[12]`, `Bit`, ...).
    pub typ: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryDepthNotLiteral;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryDepthNotPowerOf2 {
    pub depth: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryIndexWidth {
    pub component: BString,
    /// The required index width `k` (`depth == 2^k`).
    pub expected: Width,
    /// The offending index type, rendered.
    pub got: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryIndexOutOfRange {
    pub depth: u32,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryWriteOutsideOnRegion {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OverlappingMemoryWrites {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryAssignInsteadOfLassign {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryUsedWithoutIndex {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexedDriverNotMemory {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryOnRegionClockMismatch {
    pub component: BString,
    /// The memory's resolved clock, rendered.
    pub clock: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryOnRegionNonWrite;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryInitialUnsupported {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImageWrite {
    pub component: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImageIo {
    pub component: BString,
    pub path: BString,
    pub error: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImageTooLong {
    pub component: BString,
    pub path: BString,
    pub depth: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImageNotSupported {
    pub component: BString,
    pub reason: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImageNoinitial {
    pub component: BString,
    pub platform: BString,
    /// Fully-qualified module names, from the top module bound to the
    /// platform down to the module declaring the memory.
    pub path: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NetReadWithoutTap {
    pub source: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TapNotOnNet {
    pub source: BString,
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
pub struct UnusedButUsed {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedOnSink {
    pub path: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedOnOutgoingWire {
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
pub struct InitialInsideIfMatch {
    pub target: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitialNoinitialPlatform {
    pub platform: BString,
    /// Fully-qualified module names, from the top module bound to the
    /// platform down to the module containing the `initial` statement.
    pub path: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CombinationalLoop {
    pub components: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependsonCycle {
    pub components: Vec<BString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependsonBadCarrier {
    pub component: BString,
    pub reason: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependsonBadEntry {
    pub entry: BString,
    pub reason: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependsonUndeclared {
    pub port: BString,
    pub missing: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependsonUnused {
    pub port: BString,
    pub entry: BString,
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
pub struct DriveUndeclaredPlatformPort {
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
pub struct NotPub {
    pub item: BString,
    pub package: BString,
}

/// A non-`pub` item is never referenced anywhere in the project.
/// Reported at the item's *definition* as a warning, so the author
/// knows the item is dead code (or that they forgot to reference it).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedNonPub {
    pub item: BString,
}

/// A non-`pub` item is referenced from another package.
/// Reported at the item's *definition* as a warning, naming the package
/// that references it, so the author knows where to add `pub` (or that
/// their item is being depended on while private).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferencedWhileNotPub {
    pub item: BString,
    pub package: BString,
    pub referencing_package: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopNotPub {
    pub item: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeNotPub {
    pub typ: BString,
    pub owner: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopGenerics {
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
            DiagnosticPayload::InvalidPackageName(d) => {
                format!(
                    "Package name '{}' is not a valid identifier; rename the source file",
                    d.package,
                ).into()
            }
            DiagnosticPayload::UnresolvedItem(d) => format!("Unresolved item {}", d.item).into(),
            DiagnosticPayload::UnresolvedType(d) => format!("Unresolved type {}", d.typ).into(),
            DiagnosticPayload::MissingOnClause(d) => format!("Missing on clause for reg {}", d.component).into(),
            DiagnosticPayload::RedundantOnClause(d) => {
                format!(
                    "Redundant `on` clause: `{}`",
                    d.component,
                ).into()
            }
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
            DiagnosticPayload::MemoryElementNotPowerOf2Word(d) => format!(
                "memory element type must be `Word[n]` with n a power of 2, found {}",
                d.typ,
            ).into(),
            DiagnosticPayload::MemoryDepthNotLiteral(_) =>
                "memory depth must be a literal constant".into(),
            DiagnosticPayload::MemoryDepthNotPowerOf2(d) => format!(
                "memory depth must be a power of 2, found {}",
                d.depth,
            ).into(),
            DiagnosticPayload::MemoryIndexWidth(d) => format!(
                "index into memory {} must be exactly Word[{}] (depth 2^{}), found {}",
                d.component, d.expected, d.expected, d.got,
            ).into(),
            DiagnosticPayload::MemoryIndexOutOfRange(d) => format!(
                "index {} out of range for memory depth {}",
                d.index, d.depth,
            ).into(),
            DiagnosticPayload::MemoryWriteOutsideOnRegion(d) => format!(
                "write to memory {} outside a clocked write context (an `on` region, or a module with exactly one `Clock`)",
                d.component,
            ).into(),
            DiagnosticPayload::OverlappingMemoryWrites(d) => format!(
                "overlapping writes to memory {}: sibling write clauses with non-exclusive guards may both fire in one cycle",
                d.component,
            ).into(),
            DiagnosticPayload::MemoryAssignInsteadOfLassign(d) => format!(
                "use `<=` to write memory {} (a memory write commits on the clock edge)",
                d.component,
            ).into(),
            DiagnosticPayload::MemoryUsedWithoutIndex(d) => format!(
                "memory {} must be indexed: `{}[dyn i]` or `{}[c]`",
                d.component, d.component, d.component,
            ).into(),
            DiagnosticPayload::IndexedDriverNotMemory(d) => format!(
                "indexed driver target {} is not a memory",
                d.target,
            ).into(),
            DiagnosticPayload::MemoryOnRegionClockMismatch(d) => format!(
                "`on` region in memory {} must name the memory's clock {}",
                d.component, d.clock,
            ).into(),
            DiagnosticPayload::MemoryOnRegionNonWrite(_) =>
                "only writes to the memory are allowed inside an `on` region".into(),
            DiagnosticPayload::MemoryInitialUnsupported(d) => format!(
                "`initial` is not supported on memory {}; initialize with @image/@image_hex instead",
                d.component,
            ).into(),
            DiagnosticPayload::MemoryImageWrite(d) => format!(
                "memory {} is initialized by @image/@image_hex; writes to it are not supported",
                d.component,
            ).into(),
            DiagnosticPayload::MemoryImageIo(d) => format!(
                "could not read memory image for {}: {}: {}",
                d.component, d.path, d.error,
            ).into(),
            DiagnosticPayload::MemoryImageTooLong(d) => format!(
                "memory image {} holds more words than the declared depth {}",
                d.path, d.depth,
            ).into(),
            DiagnosticPayload::MemoryImageNotSupported(d) => format!(
                "memory image for {} not supported: {}",
                d.component, d.reason,
            ).into(),
            DiagnosticPayload::MemoryImageNoinitial(d) => format!(
                "memory image initialization for {} is disallowed on `@noinitial` platform {} (reachable via {})",
                d.component,
                d.platform,
                d.path.iter().map(|m| m.to_str_lossy().into_owned()).collect::<Vec<_>>().join(" -> "),
            ).into(),
            DiagnosticPayload::DriverForSink(d) => format!("Driver for sink {}", d.target).into(),
            DiagnosticPayload::UnresolvedComponent(d) => format!("Unresolved component {}", d.path).into(),
            DiagnosticPayload::ItNotInItBlock => "'it' used outside of an it block".into(),
            DiagnosticPayload::NotIt(d) => {
                format!(
                    "'{}' should be written as 'it' inside of this driver block",
                    d.component,
                ).into()
            }
            DiagnosticPayload::NetReadWithoutTap(d) => {
                format!(
                    "{} is not guaranteed to have a value. Use `tap {}` to assert it will.",
                    d.source,
                    d.source,
                ).into()
            }
            DiagnosticPayload::TapNotOnNet(d) => {
                format!(
                    "`tap` may only be applied to a `bus` or `net`: {} is neither",
                    d.source,
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
            DiagnosticPayload::LetBlockNeedsArrow => {
                "For a `let` used as an expression, use `=>` instead of `{ ... }`".into()
            }
            DiagnosticPayload::LetArrowNeedsBlock => {
                "For a `let` used as a statement, use `{ ... }` instead of `=>`".into()
            }
            DiagnosticPayload::RedundantUnused(d) => format!("Redundant `unused`: {}", d.path).into(),
            DiagnosticPayload::UnusedButUsed(d) => format!("Marked `unused` but used: {}", d.path).into(),
            DiagnosticPayload::UnusedOnSink(d) => format!("`unused` on outgoing {}", d.path).into(),
            DiagnosticPayload::UnusedOnOutgoingWire(d) =>
                format!("Marked `unused` but declared `outgoing wire`: {}", d.path).into(),
            DiagnosticPayload::RedundantDriver(d) => format!("Redundant driver: {}", d.path).into(),
            DiagnosticPayload::InitialForNonReg(d) => {
                format!("initial targets non-register component {}", d.target).into()
            }
            DiagnosticPayload::InitialMultiple(d) => {
                format!("multiple initial statements for register {}", d.target).into()
            }
            DiagnosticPayload::InitialInsideIfMatch(d) => {
                format!(
                    "initial statement for {} must not appear inside an if/match block",
                    d.target,
                ).into()
            }
            DiagnosticPayload::InitialNoinitialPlatform(d) => {
                let path = d
                    .path
                    .iter()
                    .map(|module| module.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(
                    "initial statement appears in platform {} with @noinitial: {}",
                    d.platform,
                    path,
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
            DiagnosticPayload::DependsonCycle(d) => {
                format!(
                    "Cycle in the `dependson` relation: {}",
                    d.components
                        .iter()
                        .map(|c| String::from_utf8_lossy(c).into_owned())
                        .collect::<Vec<_>>()
                        .join(" -> "),
                ).into()
            }
            DiagnosticPayload::DependsonBadCarrier(d) => {
                format!(
                    "`dependson` is not allowed on `{}`: {}",
                    d.component, d.reason,
                ).into()
            }
            DiagnosticPayload::DependsonBadEntry(d) => {
                format!(
                    "Bad `dependson` entry `{}`: {}",
                    d.entry, d.reason,
                ).into()
            }
            DiagnosticPayload::DependsonUndeclared(d) => {
                format!(
                    "`{}` could combinationally depend on `{}` but does not declare it; add `{}` to `{}`'s `dependson`",
                    d.port, d.missing, d.missing, d.port,
                ).into()
            }
            DiagnosticPayload::DependsonUnused(d) => {
                format!(
                    "Unused `dependson`: `{}` declares it may depend on `{}` but does not",
                    d.port, d.entry,
                ).into()
            }
            DiagnosticPayload::ExpectedPlatform(d) => format!("Expected a platform: {} is not a platform", d.item).into(),
            DiagnosticPayload::PlatformMissingPort(d) => format!("Module is missing port {} required by platform {}", d.port, d.platform).into(),
            DiagnosticPayload::DriveUndeclaredPlatformPort(d) => format!("`{}` is declared in platform `{}`, but not declared in this module; declare it explicitly to drive it", d.port, d.platform).into(),
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
            DiagnosticPayload::NotPub(d) => {
                format!(
                    "`{}` is not pub in package `{}`; mark it `pub` to reference it from other packages",
                    d.item,
                    d.package,
                ).into()
            }
            DiagnosticPayload::ReferencedWhileNotPub(d) => {
                format!(
                    "`{}` is not pub but referenced from package `{}`",
                    d.item,
                    d.referencing_package,
                ).into()
            }
            DiagnosticPayload::UnusedNonPub(d) => {
                format!(
                    "Item `{}` is unused and not `pub`",
                    d.item,
                ).into()
            }
            DiagnosticPayload::TopNotPub(d) => {
                format!(
                    "Top module {} must be `pub`",
                    d.item,
                ).into()
            }
            DiagnosticPayload::TypeNotPub(d) => {
                format!(
                    "`{}` is not pub: the public surface of `{}` must be transitively pub",
                    d.typ,
                    d.owner,
                ).into()
            }
            DiagnosticPayload::TopGenerics(d) => {
                format!(
                    "top module {} cannot have generic parameters: a top module must be a concrete instantiation",
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
            | DiagnosticPayload::TrailingSpaces
            | DiagnosticPayload::DependsonUnused(_)
            | DiagnosticPayload::RedundantOnClause(_)
            | DiagnosticPayload::UnusedOnOutgoingWire(_)
            | DiagnosticPayload::ReferencedWhileNotPub(_)
            | DiagnosticPayload::MemoryAssignInsteadOfLassign(_)
            | DiagnosticPayload::UnusedNonPub(_) => DiagnosticLevel::Warning,
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
    DuplicateSlot, UnresolvedPackage, InvalidPackageName, UnresolvedItem, UnresolvedType, MissingOnClause,
    RedundantOnClause, WrongDriverType, NoRegDrivers, NoDrivers, MultipleDrivers,
    MemoryElementNotPowerOf2Word, MemoryDepthNotLiteral, MemoryDepthNotPowerOf2, MemoryIndexWidth,
    MemoryIndexOutOfRange, MemoryWriteOutsideOnRegion, OverlappingMemoryWrites,
    MemoryAssignInsteadOfLassign, MemoryUsedWithoutIndex, IndexedDriverNotMemory,
    MemoryOnRegionClockMismatch, MemoryOnRegionNonWrite, MemoryInitialUnsupported,
    MemoryImageWrite, MemoryImageIo, MemoryImageTooLong, MemoryImageNotSupported,
    MemoryImageNoinitial,
    NetReadWithoutTap, TapNotOnNet,
    DriverForSink, UnresolvedComponent, NotIt, UnresolvedCtor, UnusedSource, ReadFromSink,
    UnfilledHole, ModuleCycle, UnresolvedMethod, WrongType, Unknown, DoesntFit, NotWordType,
    CantTruncate, Todo, MatchNotExhaustive, MatchOverlappingArm, InvalidDocstring,
    DuplicateEnumValue, EnumPartiallySpecified, IndexOutOfBounds, IndexRangeOutOfBounds, InvalidIndexRange,
    InvalidWordIndexWidth, IndexNotWordType, RedundantUnused, RedundantDriver, UnusedButUsed,
    UnusedOnSink, UnusedOnOutgoingWire,
    CombinationalLoop, DependsonCycle, DependsonBadCarrier, DependsonBadEntry,
    DependsonUndeclared, DependsonUnused,
    InitialForNonReg, InitialMultiple, InitialInsideIfMatch, InitialNoinitialPlatform,
    ExpectedPlatform, PlatformMissingPort, PlatformExtraPort, PlatformPortDirMismatch,
    DriveUndeclaredPlatformPort,
    PlatformPortTypeMismatch, DefaultOutsidePlatformPort, NotYetImplemented, TypeArgCount,
    TypeArgKind, UnknownKind,
    CtorRequiresArgs, ExtModGenerics, NotPub, ReferencedWhileNotPub, UnusedNonPub, TypeNotPub, TopNotPub, TopGenerics, SocketConnectMismatch,
}
