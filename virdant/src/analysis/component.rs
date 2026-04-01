use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice;
use hashbrown::HashSet;

use crate::analysis::Location;
use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::common::{ComponentKind, Flow};
use crate::diagnostics::Diagnostic;

#[derive(Debug)]
pub struct ComponentAnalysis {
    moddef: SymbolId,
    components: Vec<(BString, Component)>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ComponentId {
    item_id: SymbolId,
    index: usize,
}


#[derive(Debug, Clone)]
pub struct Component {
    id: ComponentId,
    path: BString,
    location: Location,
    typ: Option<Type>,
    flow: Flow,
}

impl Component {
    pub fn id(&self) -> ComponentId {
        return self.id;
    }

    pub fn path(&self) -> BString {
        self.path.clone()
    }

    pub fn typ(&self) -> Option<Type> {
        self.typ.clone()
    }

    pub fn flow(&self) -> Flow {
        self.flow.clone()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn can_sink(&self) -> bool {
        matches!(self.flow, Flow::Sink | Flow::Duplex)
    }


    pub fn can_source(&self) -> bool {
        matches!(self.flow, Flow::Source | Flow::Duplex)
    }
}

impl ComponentAnalysis {
    pub fn component(&self, component_id: ComponentId) -> Option<&Component> {
        for (_name, component) in &self.components {
            if component.id == component_id {
                return Some(component);
            }
        }
        None
    }

    pub fn moddef_symbol_id(&self) -> SymbolId {
        self.moddef.clone()
    }

    pub fn type_of(&self, path: &BStr) -> Option<Type> {
        for (path_, component) in &self.components {
            if path == path_ {
                return component.typ.clone();
            }
        }
        None
    }

    pub fn components(&self) -> Vec<(BString, Component)> {
        self.components.clone()
    }

    pub fn resolve(&self, path: &BStr) -> Option<Component> {
        for (path_, component) in &self.components {
            if path_ == path {
                return Some(component.clone());
            }
        }
        None
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}

pub(crate) fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
        diagnostics: vec![],
    };

    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();

    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let mut components_seen: HashSet<BString> = HashSet::new();

    // ensure all dependent packages have been created
    // TODO this doesn't recurse though?
    let package_analysis = builder.get_package_analysis(location.package());
    for import in package_analysis.imports() {
        builder.get_package_analysis(import);
    }

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let id = ComponentId {
                    item_id: moddef,
                    index: component_analysis.components.len(),
                };
                let path = parsing.string(component.name).to_owned();
                let typ_node = stmt.typ().unwrap();
                let typ = match builder.get_type_at(typ_node.location()) {
                    Ok(typ) => Some(typ),
                    Err(_) => None,
                };

                let flow = match component.kind {
                    ComponentKind::Incoming => Flow::Source,
                    ComponentKind::Outgoing => Flow::Sink,
                    ComponentKind::Reg => Flow::Duplex,
                    ComponentKind::Wire => Flow::Duplex,
                };
                let component = Component {
                    id,
                    path: path.clone(),
                    location: stmt.location(),
                    typ,
                    flow,
                };
                if !components_seen.contains(&path) {
                    components_seen.insert(path.clone());
                    component_analysis.components.push((path.clone(), component));
                }
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name);
                let ofness_node = stmt.child(0);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let submodule_package = ofness
                    .package
                    .map(|pkg| PackageFqn::new(bstr::BString::from(parsing.string(pkg).to_vec())))
                    .unwrap_or_else(|| location.package());
                let submodule_name = parsing.string(ofness.name);
                let submodule_symbol = match symboltable.resolve_item_in_package(submodule_name, submodule_package) {
                    Some(symbol) => symbol.clone(),
                    None => continue,
                };
                let submodule_location = submodule_symbol.location();
                let submodule_parsing = builder.get_parsing(submodule_location.package());
                let submodule_ast = submodule_parsing.ast_node(submodule_location.ast_node_id());
                for submodule_stmt in submodule_ast.children() {
                    let id = ComponentId {
                        item_id: moddef,
                        index: component_analysis.components.len(),
                    };
                    let AstNodePayload::Component(component) = submodule_stmt.payload() else {
                        continue;
                    };
                    if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing) {
                        continue;
                    }
                    let port_name = submodule_parsing.string(component.name);
                    let path = bstr::BString::from(
                        format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes()
                    );
                    let typ_node = submodule_stmt.typ().unwrap();
                    let typ = match builder.get_type_at(typ_node.location()) {
                        Ok(typ) => Some(typ),
                        Err(_) => None,
                    };
                    let flow = match component.kind {
                        ComponentKind::Incoming => Flow::Sink,
                        ComponentKind::Outgoing => Flow::Source,
                        ComponentKind::Reg => Flow::Duplex,
                        ComponentKind::Wire => Flow::Duplex,
                    };
                    let component = Component {
                        id,
                        path: path.clone(),
                        location: stmt.location(),
                        typ,
                        flow,
                    };
                    if !components_seen.contains(&path) {
                        components_seen.insert(path.clone());
                        component_analysis.components.push((path, component))
                    }
                }
            }
            _ => (),
        }
    }

    Arc::new(component_analysis)
}

pub(crate) fn build_component(
    builder: &mut Builder,
    component_id: ComponentId,
) -> Arc<Component> {
    let component_analysis = builder.get_component_analysis(component_id.item_id);
    Arc::new(component_analysis.component(component_id).unwrap().clone())
}
