use bstr::{BStr, BString};
use std::sync::Arc;

use crate::fqn::PackageFqn;
use crate::common::source::Region;

pub type Type = BString;

#[derive(Clone, Debug)]
pub struct Diagnostic(Arc<dyn IsDiagnostic + Send + Sync>);

trait IsDiagnostic: std::fmt::Debug + 'static + Send + Sync {
    fn region(&self) -> Region;
    fn message(&self) -> BString;
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

/// `import` statement names a package which does not exist.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    pub region: Region,
    //pub error: VirParseError, // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportNotAtTopError {
    pub region: Region,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportCycle {
    pub region: Region,
    pub package_cycle: Vec<BString>,
}

/// `import` statement names a package which does not exist.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedImportError {
    pub region: Region,
    pub imported_package: PackageFqn,
}

/// Package contains two or more `import` statements naming the same package.
/// Lists regions to all such statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateImport {
    pub region: Region,
    pub imported_package: PackageFqn,
}

/// Two items share the same name in the same package.
/// Lists regions to all such item declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateItem {
    pub region: Region,
    pub item: BString,
}

/// Two items share the same name in the same package.
/// Lists regions to all such item declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateSlot {
    pub item: BString,
    pub region: Region,
    pub slot: BString,
}

/// Failed to resolve a package.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedPackage {
    pub region: Region,
    pub package: BString,
}

/// Failed to resolve an item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedItem {
    pub region: Region,
    pub item: BString,
}

/// Failed to resolve a type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedType {
    pub region: Region,
    pub typ: BString,
}

/// A `reg` component is missing an `on` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingOnClause {
    pub region: Region,
    pub component: BString,
}

/// A non-`reg` component has an unexpected `on` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedOnClause {
    pub region: Region,
    pub component: BString,
}

/// A `driver` statement used the wrong driver type.
/// Eg, a `reg` used `:=` or a `wire` used `<=`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongDriverType;

/// Component has no drivers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoDrivers {
    pub region: Region,
    pub target: BString,
}

/// Component has multiple drivers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultipleDrivers {
    pub region: Region,
    pub target: BString,
}

/// Incoming signal has a driver.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DriverForSink {
    pub region: Region,
    pub target: BString,
}

/// A component could not be resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedComponent {
    pub region: Region,
    pub path: BString,
}

/// A component could not be resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnusedSource {
    pub region: Region,
    pub path: BString,
}

/// Read from a component which is a sink.
/// Eg, a read from an `outgoing` port.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadFromSink {
    pub region: Region,
    pub path: BString,
}

/// A component could not be resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnfilledHole {
    pub region: Region,
    pub name: Option<BString>,
    pub typ: Option<BString>,
}

/// Failed to resolve a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedMethod {
    pub region: Region,
    pub method: BString,
    pub subject_typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongType {
    pub region: Region,
    pub expected: Type,
    pub actual: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unknown {
    pub region: Region,
    pub message: BString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoesntFit {
    pub region: Region,
    pub value: u64,
    pub width: u64,
    pub minwidth: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotWordType {
    pub region: Region,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CantInfer {
    pub region: Region,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongArgCount {
    pub region: Region,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Todo {
    pub region: Region,
    pub message: BString,
}


#[derive(Debug, Clone)]
pub struct Soften {
    pub inner: Diagnostic,
    pub level: DiagnosticLevel,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl Diagnostic {
    pub fn region(&self) -> Region {
        self.0.region()
    }

    pub fn message(&self) -> BString {
        self.0.message()
    }

    pub fn level(&self) -> DiagnosticLevel {
        self.0.level()
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

impl<E: IsDiagnostic> From<E> for Diagnostic {
    fn from(value: E) -> Self {
        Diagnostic(Arc::new(value))
    }
}

impl IsDiagnostic for UnresolvedMethod {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresovled method: {} on type {}", self.method, self.subject_typ).into()
    }
}

impl IsDiagnostic for Unknown {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        self.message.to_string().into()
    }
}

impl IsDiagnostic for DuplicateItem {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        "Duplicate Item".to_owned().into()
    }
}

impl IsDiagnostic for DuplicateImport {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Duplicate import").into()
    }
}

impl IsDiagnostic for SyntaxError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Syntax Error").into()
    }
}

impl IsDiagnostic for ImportNotAtTopError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Import not at top of file").into()
    }
}

impl IsDiagnostic for ImportCycle {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        debug_assert!(self.package_cycle.len() > 0);

        if self.package_cycle.len() > 1 {
            let package_cycle = self
                .package_cycle
                .iter()
                .map(|package| package.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            format!("Import cycle: {package_cycle}").into()
        } else {
            format!("Package imports itself: {}", &self.package_cycle[0]).into()
        }
    }
}

impl IsDiagnostic for UnresolvedImportError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresolved import: {}", self.imported_package).into()
    }
}

impl IsDiagnostic for DuplicateSlot {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Duplicate slot").into()
    }
}

impl IsDiagnostic for NoDrivers {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("No drivers for {}", &self.target).into()
    }
}

impl IsDiagnostic for MultipleDrivers {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Multiple drivers for {}", &self.target).into()
    }
}

impl IsDiagnostic for DriverForSink {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Driver for sink {}", &self.target).into()
    }
}

impl IsDiagnostic for UnresolvedComponent {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresolved component {}", &self.path).into()
    }
}

impl IsDiagnostic for UnusedSource {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unused signal {}", &self.path).into()
    }

    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }
}

impl IsDiagnostic for ReadFromSink {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Read from sink {}", &self.path).into()
    }

    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }
}

impl IsDiagnostic for UnfilledHole {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        use bstr::ByteSlice;
        let name = if let Some(name) = &self.name {
            name.as_bstr()
        } else {
            BStr::new("?")
        };

        if let Some(typ) = &self.typ {
            format!("Unfilled hole: {name} : {typ}").into()
        } else {
            format!("Unfilled hole: {name}").into()
        }
    }

    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }
}

impl IsDiagnostic for UnresolvedItem {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresolved item {}", &self.item).into()
    }
}

impl IsDiagnostic for UnresolvedType {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresolved type {}", &self.typ).into()
    }
}

impl IsDiagnostic for UnresolvedPackage {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Unresolved package {}", &self.package).into()
    }
}

impl IsDiagnostic for WrongType {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Wrong type: Expected {} but found {}", self.expected, self.actual).into()
    }
}

impl IsDiagnostic for DoesntFit {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Value doesn't fit: {} is a {}-bit value, but literal is type builtin::Word[{}]", self.value, self.minwidth, self.width).into()
    }
}

impl IsDiagnostic for NotWordType {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Expected type {} which is not a Word type", &self.typ).into()
    }
}

impl IsDiagnostic for CantInfer {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Can't infer").into()
    }
}

impl IsDiagnostic for WrongArgCount {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("Wrong arg count").into()
    }
}

impl IsDiagnostic for Todo {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> BString {
        format!("TODO: {}", self.message).into()
    }

    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Info }
}

impl IsDiagnostic for Soften {
    fn region(&self) -> Region {
        self.inner.region()
    }

    fn message(&self) -> BString {
        self.inner.message()
    }

    fn level(&self) -> DiagnosticLevel {
        self.level
    }
}

impl Diagnostic {
    pub fn to_warning(self) -> Diagnostic {
        (Soften {
            inner: self,
            level: DiagnosticLevel::Warning,
        }).into()
    }

    pub fn to_info(self) -> Diagnostic {
        (Soften {
            inner: self,
            level: DiagnosticLevel::Info,
        }).into()
    }
}
