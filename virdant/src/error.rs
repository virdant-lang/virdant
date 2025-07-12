use std::sync::Arc;

use crate::fqn::PackageFqn;
use crate::source::Region;
use crate::types::Type;

#[derive(Clone, Debug)]
pub struct VirError(Arc<dyn IsVirError>);

trait IsVirError: std::fmt::Debug + 'static {
    fn region(&self) -> Region;
    fn message(&self) -> String;
}

/// `import` statement names a package which does not exist.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub region: Region,
    //pub error: VirParseError, // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportNotAtTopError {
    pub region: Region,
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
pub struct DuplicateImportError {
    pub regions: Vec<Region>,
    pub imported_package: PackageFqn,
}

/// Two items share the same name in the same package.
/// Lists regions to all such item declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateItem {
    pub region: Region,
    pub item: String,
}

/// Two items share the same name in the same package.
/// Lists regions to all such item declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateSlot {
    pub item: String,
    pub region: Region,
    pub slot: String,
}

/// Failed to resolve a package.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedPackage {
    pub region: Region,
    pub package: String,
}

/// Failed to resolve an item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedItem {
    pub region: Region,
    pub item: String,
}

/// A `reg` component is missing an `on` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingOnClause {
    pub region: Region,
    pub component: String,
}

/// A non-`reg` component has an unexpected `on` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedOnClause {
    pub region: Region,
    pub component: String,
}

/// A `driver` statement used the wrong driver type.
/// Eg, a `reg` used `:=` or a `wire` used `<=`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrongDriverType;

/// Component has no drivers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoDrivers {
    pub region: Region,
    pub target: String,
}

/// Component has multiple drivers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultipleDrivers {
    pub region: Region,
    pub target: String,
}

/// A component could not be resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedComponent {
    pub region: Region,
    pub path: String,
}

/// Read from a component which is a sink.
/// Eg, a read from an `outgoing` port.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadFromSink;

/// Failed to resolve a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedMethod {
    pub region: Region,
    pub method: String,
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
    pub msg: String,
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

////////////////////////////////////////////////////////////////////////////////////////////////////

impl VirError {
    pub fn region(&self) -> Region {
        self.0.region()
    }

    pub fn message(&self) -> String {
        self.0.message()
    }
}

impl<E: IsVirError> From<E> for VirError {
    fn from(value: E) -> Self {
        VirError(Arc::new(value))
    }
}

impl IsVirError for UnresolvedMethod {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Unresovled method: {} on type {}", self.method, self.subject_typ)
    }
}

impl IsVirError for Unknown {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        self.msg.to_string()
    }
}

impl IsVirError for DuplicateItem {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        "Duplicate Item".to_owned()
    }
}

impl IsVirError for DuplicateImportError {
    fn region(&self) -> Region {
        self.regions[0].clone()
    }

    fn message(&self) -> String {
        format!("Duplicate import")
    }
}

impl IsVirError for ParseError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Parse Error")
    }
}

impl IsVirError for ImportNotAtTopError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Import not at top of file")
    }
}

impl IsVirError for UnresolvedImportError {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Unresolved import: {}", self.imported_package)
    }
}

impl IsVirError for DuplicateSlot {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Duplicate slot")
    }
}

impl IsVirError for NoDrivers {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("No drivers for {}", &self.target)
    }
}

impl IsVirError for MultipleDrivers {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Multiple drivers for {}", &self.target)
    }
}

impl IsVirError for UnresolvedComponent {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Unresolved component {}", &self.path)
    }
}

impl IsVirError for UnresolvedItem {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Unresolved item {}", &self.item)
    }
}

impl IsVirError for UnresolvedPackage {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Unresolved package {}", &self.package)
    }
}

impl IsVirError for WrongType {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Wrong type: Expected {} but found {}", self.expected, self.actual)
    }
}

impl IsVirError for DoesntFit {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Value doesn't fit: {} is a {}-bit value, but literal is type builtin::Word[{}]", self.value, self.minwidth, self.width)
    }
}

impl IsVirError for NotWordType {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        // TODO
        format!("Expected type TODO which is not a Word type")
//        format!("Expected type {} which is not a Word type", self.typ)
    }
}

impl IsVirError for CantInfer {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Can't infer")
    }
}

impl IsVirError for WrongArgCount {
    fn region(&self) -> Region {
        self.region.clone()
    }

    fn message(&self) -> String {
        format!("Wrong arg count")
    }
}
