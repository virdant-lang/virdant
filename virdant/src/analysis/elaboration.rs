use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::component::ComponentAnalysis;
use crate::analysis::drivers::{Driver, DriverAnalysis};
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

#[derive(Debug)]
pub struct Elaboration {
    components: Vec<ElaboratedComponent>,
}

#[derive(Debug)]
pub struct ElaboratedComponent {
    id: ElaboratedComponentId,
    path: BString,
    typ: Type,
    component_kind: ComponentKind,
    driver_type: DriverType,
    driver: Option<Driver>,
    alias: Option<ElaboratedComponentId>,
    clock: Option<ElaboratedComponentId>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ElaboratedComponentId(usize);

impl Elaboration {
    pub fn resolve<P: AsRef<BStr>>(&self, path: P) -> Option<&ElaboratedComponent> {
        let path = path.as_ref();
        for component in &self.components {
            if component.path == path {
                return Some(&component);
            }
        }
        None
    }

    pub fn components(&self) -> &[ElaboratedComponent] {
        &self.components
    }

    pub fn component(
        &self,
        elaborated_component_id: ElaboratedComponentId,
    ) -> &ElaboratedComponent {
        &self.components[elaborated_component_id.0]
    }

    pub fn dump(&self) {
        for component in &self.components {
            let alias_str = if let Some(alias_id) = &component.alias {
                let alias = &self.components[alias_id.0];
                format!(" (alias: {})", &alias.path)
            } else {
                String::new()
            };

            let clock_str = if let Some(clock_id) = &component.clock {
                let clock = &self.components[clock_id.0];
                format!(" (on: {})", &clock.path)
            } else {
                String::new()
            };

            println!("{} : {:?}{alias_str}{clock_str}", component.path, component.typ);
        }
    }
}

impl ElaboratedComponent {
    pub fn id(&self) -> ElaboratedComponentId {
        self.id
    }

    pub fn path(&self) -> &BString {
        &self.path
    }

    pub fn typ(&self) -> Type {
        self.typ.clone()
    }

    pub fn driver_type(&self) -> DriverType {
        self.driver_type
    }

    pub fn driver(&self) -> Option<&Driver> {
        self.driver.as_ref()
    }

    pub fn is_reg(&self) -> bool {
        self.driver_type == DriverType::Latched
    }

    pub fn clock(&self) -> Option<ElaboratedComponentId> {
        self.clock
    }

    pub fn component_kind(&self) -> ComponentKind {
        self.component_kind
    }
}

/// Context supplied by the parent module when recursing into a submodule.
/// Needed so that `incoming` ports can look up their driver in the parent scope.
struct ParentCtx {
    component_analysis: Arc<ComponentAnalysis>,
    driver_analysis: Arc<DriverAnalysis>,
    /// The instance name as it appears in the parent (e.g. `"gcd"`), used to
    /// construct the sub-port lookup path `"{instance}.{port}"`.
    instance_name: String,
}

/// Recursively traverse the module at `moddef`, collecting all fully-qualified
/// component paths into `components`.  `prefix` is the dotted path so far
/// (e.g. `"top"` or `"top.foo"`).
fn elaborate_module(
    builder: &mut Builder,
    moddef: SymbolId,
    prefix: &str,
    components: &mut Vec<ElaboratedComponent>,
    parent_ctx: Option<ParentCtx>,
) {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(moddef);
    let driver_analysis = builder.get_driver_analysis(moddef);

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let name = parsing.string(component.name);
                let path: BString = format!("{prefix}.{}", name.to_str_lossy()).into();

                let typ_node = stmt.typ().unwrap();
                let typ = builder.get_type_at(typ_node.location()).unwrap();

                let driver_type = match component.kind {
                    ComponentKind::Reg => DriverType::Latched,
                    _ => DriverType::Continuous,
                };

                // `incoming` ports are driven from the parent scope.
                // All other kinds (outgoing, wire, reg) are driven within this module.
                let driver = if component.kind == ComponentKind::Incoming {
                    parent_ctx.as_ref().and_then(|ctx| {
                        let sub_port_path: BString =
                            format!("{}.{}", ctx.instance_name, name.to_str_lossy()).into();
                        let comp = ctx.component_analysis.resolve(sub_port_path.as_ref())?;
                        ctx.driver_analysis
                            .drivers()
                            .get(&comp.id())?
                            .first()
                            .cloned()
                    })
                } else {
                    component_analysis
                        .resolve(name)
                        .and_then(|c| driver_analysis.drivers().get(&c.id())?.first().cloned())
                };

                let id = ElaboratedComponentId(components.len());
                let clock = if let Some(clock_node) = stmt.clock() {
                    // Resolve the clock reference (e.g. `clock` in `reg x : T on clock`)
                    // to the ElaboratedComponentId of the already-accumulated clock component
                    // (e.g. `top.clock`).  The incoming clock port is always declared before
                    // any reg in the source, so it is guaranteed to be in `components` already.
                    clock_node.path().and_then(|interned| {
                        let clock_name = parsing.string(interned).to_str_lossy().into_owned();
                        let full_path: BString = format!("{prefix}.{clock_name}").into();
                        components
                            .iter()
                            .position(|c| c.path == full_path)
                            .map(ElaboratedComponentId)
                    })
                } else {
                    None
                };
                components.push(ElaboratedComponent {
                    id,
                    path,
                    typ,
                    component_kind: component.kind,
                    driver_type,
                    driver,
                    alias: None,
                    clock,
                });
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name).to_str_lossy().into_owned();
                let ofness_node = stmt.child(0);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let submodule_package = ofness
                    .package
                    .map(|pkg| PackageFqn::new(bstr::BString::from(parsing.string(pkg).to_vec())))
                    .unwrap_or_else(|| location.package());
                let submodule_name = parsing.string(ofness.name);
                let symboltable = builder.get_symboltable();
                let submodule_symbol =
                    match symboltable.resolve_item_in_package(submodule_name, submodule_package) {
                        Some(symbol) => symbol.clone(),
                        None => continue,
                    };
                let child_prefix = format!("{prefix}.{instance_name}");
                let ctx = ParentCtx {
                    component_analysis: component_analysis.clone(),
                    driver_analysis: driver_analysis.clone(),
                    instance_name: instance_name.clone(),
                };
                elaborate_module(
                    builder,
                    submodule_symbol.id,
                    &child_prefix,
                    components,
                    Some(ctx),
                );
            }
            _ => {}
        }
    }
}

pub(crate) fn build_elaboration(builder: &mut Builder, top: SymbolId) -> Arc<Elaboration> {
    let mut components: Vec<ElaboratedComponent> = vec![];
    elaborate_module(builder, top, "top", &mut components, None);

    // Build a reverse index: fully-elaborated path -> ElaboratedComponentId.
    let path_to_id: IndexMap<BString, ElaboratedComponentId> = components
        .iter()
        .enumerate()
        .map(|(i, c)| (c.path.clone(), ElaboratedComponentId(i)))
        .collect();

    // Resolve aliases: when a component's driver is a bare reference expression,
    // find the ElaboratedComponentId it names.
    let aliases: Vec<Option<ElaboratedComponentId>> = components
        .iter()
        .map(|c| resolve_alias(c, &path_to_id, builder))
        .collect();

    for (component, alias) in components.iter_mut().zip(aliases) {
        component.alias = alias;
    }

    Arc::new(Elaboration { components })
}

/// Returns the `ElaboratedComponentId` that `component`'s driver references, if
/// the driver is a simple `ExprReference` to another elaborated component.
fn resolve_alias(
    component: &ElaboratedComponent,
    path_to_id: &IndexMap<BString, ElaboratedComponentId>,
    builder: &mut Builder,
) -> Option<ElaboratedComponentId> {
    // Only `Driver::Expr` can be a bare reference; `Driver::If` cannot.
    let Driver::Expr(_, loc) = component.driver.as_ref()? else {
        return None;
    };

    let parsing = builder.get_parsing(loc.package());
    let node = parsing.ast_node(loc.ast_node_id());

    // The driver expression must be a plain reference (not a binop, literal, etc.)
    if !matches!(node.payload(), AstNodePayload::ExprReference) {
        return None;
    }

    let ref_path = parsing.string(node.path()?).to_str_lossy().into_owned();

    // The scope in which the driver expression was written depends on the component kind:
    //   - `incoming` ports are driven from the *parent* module (one level up from the
    //     submodule instance), so we must drop two path segments to reach that prefix.
    //     e.g. `top.gcd.clock` -> driver scope prefix is `top`  (not `top.gcd`)
    //   - All other kinds are driven within the module they belong to, so we drop one.
    //     e.g. `top.gcd.result` -> driver scope prefix is `top.gcd`
    let last_dot = component.path.iter().rposition(|&b| b == b'.')?;
    let module_prefix = if component.component_kind == ComponentKind::Incoming {
        let second_last_dot = component.path[..last_dot]
            .iter()
            .rposition(|&b| b == b'.')?;
        component.path[..second_last_dot]
            .to_str_lossy()
            .into_owned()
    } else {
        component.path[..last_dot].to_str_lossy().into_owned()
    };

    let full_path: BString = format!("{module_prefix}.{ref_path}").into();
    path_to_id.get(&full_path).copied()
}
