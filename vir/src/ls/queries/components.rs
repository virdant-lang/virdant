//! The components query: the components of a module or platform.

use bstr::ByteSlice as _;

use virdant::analysis::symbols::Symbol;
use virdant::common::ComponentKind;
use virdant::db::Db;

use super::types::type_to_source;

/// One component of a module, as served by the `components` query.
pub struct ComponentEntry {
    pub path: String,
    /// The component kind (`incoming`, `outgoing`, `reg`, `wire`, ...),
    /// or `None` for socket channels.
    pub kind: Option<&'static str>,
    /// The component's type rendered as Virdant source syntax.
    pub typ: Option<String>,
    /// True for components synthesized from a platform `for` clause.
    pub synthetic: bool,
}

/// The canonical spelling of a component kind.
pub fn component_kind_str(kind: ComponentKind) -> &'static str {
    match kind {
        ComponentKind::Incoming => "incoming",
        ComponentKind::Outgoing => "outgoing",
        ComponentKind::OutgoingWire => "outgoing-wire",
        ComponentKind::OutgoingReg => "outgoing-reg",
        ComponentKind::Reg => "reg",
        ComponentKind::Memory => "memory",
        ComponentKind::Wire => "wire",
        ComponentKind::Bus => "bus",
        ComponentKind::Net => "net",
    }
}

/// The components of a module (or platform) definition.
pub fn components(db: &Db, item: &Symbol) -> Vec<ComponentEntry> {
    let package = item.location.package();
    let analysis = db.get_component_analysis(item.id);
    analysis.components().into_iter()
        .map(|(_path, component)| ComponentEntry {
            path: component.path().to_str_lossy().into_owned(),
            kind: component.kind().map(component_kind_str),
            typ: component.typ().map(|typ| type_to_source(db, package, &typ)),
            synthetic: component.synthetic(),
        })
        .collect()
}
