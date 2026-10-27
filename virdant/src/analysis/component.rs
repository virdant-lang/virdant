//! Defines `ComponentAnalysis`, collecting every component (signal,
//! port, register, etc.) declared in a module with its type, flow,
//! kind, and location, plus references to components (uses, driver
//! targets, `unused` marks) and associated diagnostics.

use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::Location;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolKind};
use crate::db::Builder;
use crate::package::PackageId;
use crate::syntax::ast::{AstNode, AstNodeId, match_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;
use crate::types::substitute::substitute;
use crate::types::Type;
use crate::common::{ChannelDir, ComponentKind, Flow, PortDir, SocketRole};
use crate::diagnostics::{self, Diagnostic};

#[derive(Debug)]
pub struct ComponentAnalysis {
    moddef: SymbolId,
    components: Vec<(BString, Component)>,
    diagnostics: Vec<Diagnostic>,

    references: IndexMap<Location, (ComponentId, ReferenceKind)>,
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
    flow: Flow,
    kind: Option<ComponentKind>,

    // Type may be absent when `builder.get_type_at()` fails during resolution
    // We keep the component anyway -- path, flow, kind, and location are independently useful.
    typ: Option<Type>,

    /// True for components synthesized from a platform (`for` clause):
    /// outgoing ports with a platform `default`, and incoming ports the
    /// module does not declare. Synthesized incoming ports suppress the
    /// unused-signal warning; synthesized ports are otherwise identical
    /// to hand-written ones.
    synthetic: bool,

    /// For a synthesized outgoing port: the location of the platform's
    /// `default <EXPR>` expression, which driver analysis turns into
    /// the port's driver.
    default_expr: Option<Location>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReferenceKind {
    /// Component used as an ExprReference
    Expr,
    /// Component used as the target of a Driver
    DriverTarget,
    /// Component marked unused with `unused` statement (ModDefStmtUnused)
    Unused,
}

impl ComponentId {
    pub fn item_id(&self) -> SymbolId {
        self.item_id
    }
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
        self.flow
    }

    pub fn kind(&self) -> Option<ComponentKind> {
        self.kind
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn synthetic(&self) -> bool {
        self.synthetic
    }

    pub fn default_expr(&self) -> Option<Location> {
        self.default_expr.clone()
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

    pub fn component_at(&self, location: Location) -> Option<&Component> {
        let (component_id, _) = self.references.get(&location)?;
        self.component(*component_id)
    }

    pub fn moddef_symbol_id(&self) -> SymbolId {
        self.moddef
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

fn drives_channel(role: SocketRole, dir: ChannelDir) -> bool {
    matches!(
        (role, dir),
        (SocketRole::Client, ChannelDir::Cosi) | (SocketRole::Server, ChannelDir::Soci)
    )
}

/// The `GenericsParams` node of an instantiation (`mod f of Fifo[Word[8]]`,
/// `client socket s of Pipe[Bit]`), if it has type arguments.
fn instantiation_args_node<'p>(node: &'p AstNode<'p>) -> Option<AstNode<'p>> {
    node.children()
        .into_iter()
        .find(|child| matches!(child.payload(), AstNodePayload::GenericsParams(_)))
}

/// Whether the mod def owning `def_symbol` is an `ext mod`.
fn moddef_is_ext(builder: &mut Builder, def_symbol: &Symbol) -> bool {
    let location = def_symbol.location();
    let parsing = builder.get_parsing(location.package());
    let def_node = parsing.ast_node(location.ast_node_id());
    matches!(def_node.payload(), AstNodePayload::ModDef(moddef) if moddef.is_ext)
}

/// Resolve the type arguments of a socket or submodule instantiation,
/// checking them against the instantiated def's generic parameters.
///
/// * Arity and kind errors are reported at the instantiation site
///   (PLANv2 §10) through the shared `resolve_instantiation_args`
///   routine.
/// * For an `ext mod` target, *any* generics — declared parameters or
///   written arguments — are a hard error: ext mods never support
///   generics.
///
/// Returns the resolved (mono) argument types, or `None` after
/// reporting, in which case the caller must skip registering the
/// instance's channels/ports (a diagnostic already explains why).
fn checked_instantiation_args(
    builder: &mut Builder,
    args_node: Option<&AstNode<'_>>,
    instantiation_region: crate::common::source::Region,
    def_symbol: &Symbol,
    def_is_ext: bool,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<Vec<Type>> {
    if def_is_ext {
        let def_has_generics = {
            let location = def_symbol.location();
            let parsing = builder.get_parsing(location.package());
            let def_node = parsing.ast_node(location.ast_node_id());
            crate::types::typedef::generics_decl_of(&def_node).is_some()
        };
        if def_has_generics || args_node.is_some() {
            let region = args_node
                .map(|node| node.region())
                .unwrap_or(instantiation_region);
            diagnostics.push(Diagnostic::new(
                region,
                diagnostics::ExtModGenerics {
                    item: def_symbol.fqn().to_owned(),
                },
            ));
            return None;
        }
        return Some(vec![]);
    }
    crate::types::typedef::resolve_instantiation_args(
        builder,
        args_node,
        def_symbol,
        instantiation_region,
        diagnostics,
    )
}

pub(crate) fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
        diagnostics: vec![],
        references: IndexMap::new(),
    };

    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();

    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let mut components_seen: IndexSet<BString> = IndexSet::new();

    // ensure all dependent packages have been created
    // TODO this doesn't recurse though?
    let package_analysis = builder.get_package_analysis(location.package());
    for import in package_analysis.imports() {
        if import == location.package() {
            continue;
        }
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
                    ComponentKind::OutgoingWire => Flow::Duplex,
                    ComponentKind::OutgoingReg => Flow::Duplex,
                    ComponentKind::Reg => Flow::Duplex,
                    ComponentKind::Wire => Flow::Duplex,
                    ComponentKind::Bus | ComponentKind::Net => Flow::Duplex,
                };
                let component_kind = component.kind;
                let component = Component {
                    id,
                    path: path.clone(),
                    location: stmt.location(),
                    typ,
                    flow,
                    kind: Some(component_kind),
                    synthetic: false,
                    default_expr: None,
                };
                if !components_seen.contains(&path) {
                    components_seen.insert(path.clone());
                    component_analysis.components.push((path.clone(), component));
                }
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name);
                let ofness_node = stmt.child(1);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let packages = builder.get_packages();
                let submodule_package: Option<PackageId> = match ofness.package {
                    Some(pkg) => packages.id(parsing.string(pkg)),
                    None => Some(location.package()),
                };
                let submodule_name = parsing.string(ofness.name);
                let submodule_symbol = match submodule_package.and_then(|pkg| symboltable.resolve_item(submodule_name, pkg)) {
                    Some(symbol) => symbol.clone(),
                    None => {
                        component_analysis.diagnostics.push(
                            Diagnostic::new(
                                ofness_node.region(),
                                diagnostics::UnresolvedItem {
                                    item: submodule_name.to_owned(),
                                },
                            )
                        );
                        continue;
                    }
                };
                let submodule_location = submodule_symbol.location();
                crate::queries::pubcheck::check_item_reference(
                    builder,
                    &submodule_symbol,
                    stmt.package(),
                    ofness_node.region(),
                    &mut component_analysis.diagnostics,
                );
                let submodule_is_ext = moddef_is_ext(builder, &submodule_symbol);
                // Resolve and kind-check this instance's type arguments
                // (arity/kind errors are reported at the site); skip
                // registration of the instance's ports on failure.
                let Some(submodule_args) = checked_instantiation_args(
                    builder,
                    instantiation_args_node(&stmt).as_ref(),
                    stmt.region(),
                    &submodule_symbol,
                    submodule_is_ext,
                    &mut component_analysis.diagnostics,
                ) else {
                    continue;
                };
                let submodule_parsing = builder.get_parsing(submodule_location.package());
                let submodule_ast = submodule_parsing.ast_node(submodule_location.ast_node_id());
                for submodule_stmt in submodule_ast.children() {
                    match submodule_stmt.payload() {
                        AstNodePayload::Component(component) => {
                            if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing | ComponentKind::OutgoingWire | ComponentKind::OutgoingReg | ComponentKind::Bus) {
                                continue;
                            }
                            let id = ComponentId {
                                item_id: moddef,
                                index: component_analysis.components.len(),
                            };
                            let port_name = submodule_parsing.string(component.name);
                            let path = bstr::BString::from(
                                format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes()
                            );
                            let typ_node = submodule_stmt.typ().unwrap();
                            // The port type as written in the def,
                            // substituted with this instance's type
                            // arguments.
                            let typ = match builder.get_type_at(typ_node.location()) {
                                Ok(typ) => Some(substitute(&typ, &submodule_args)),
                                Err(_) => None,
                            };
                            let flow = match component.kind {
                                ComponentKind::Incoming => Flow::Sink,
                                ComponentKind::Outgoing => Flow::Source,
                                ComponentKind::OutgoingWire => Flow::Source,
                                ComponentKind::OutgoingReg => Flow::Source,
                                ComponentKind::Reg => Flow::Duplex,
                                ComponentKind::Wire => Flow::Duplex,
                                ComponentKind::Bus | ComponentKind::Net => Flow::Duplex,
                            };
                            let component_kind = component.kind;
                            let component = Component {
                                id,
                                path: path.clone(),
                                location: stmt.location(),
                                typ,
                                flow,
                                kind: Some(component_kind),
                                synthetic: false,
                                default_expr: None,
                            };
                            if !components_seen.contains(&path) {
                                components_seen.insert(path.clone());
                                component_analysis.components.push((path, component))
                            }
                        }
                        AstNodePayload::Socket(submodule_socket) => {
                            // Handle sockets in submodules - treat each channel as a component
                            let socket_instance_name = submodule_parsing.string(submodule_socket.name);
                            let socket_ofness_node = submodule_stmt.child(1);
                            let AstNodePayload::Ofness(socket_ofness) = socket_ofness_node.payload() else {
                                continue;
                            };
                            let socket_package: Option<PackageId> = match socket_ofness.package {
                                Some(pkg) => packages.id(submodule_parsing.string(pkg)),
                                None => Some(submodule_location.package()),
                            };
                            let socket_name = submodule_parsing.string(socket_ofness.name);
                            let Some(socket_symbol) = socket_package.and_then(|pkg| symboltable.resolve_item(socket_name, pkg)) else {
                                continue;
                            };

                            let socket_symbol = socket_symbol.clone();
                            crate::queries::pubcheck::check_item_reference(
                                builder,
                                &socket_symbol,
                                submodule_stmt.package(),
                                socket_ofness_node.region(),
                                &mut component_analysis.diagnostics,
                            );
                            // Resolve and kind-check the socket
                            // instance's own type arguments, then
                            // compose them with the enclosing submodule
                            // instance's arguments: a socket written
                            // `of Pipe[T]` inside a generic def, whose
                            // def is instantiated at `Word[8]`, is the
                            // mono socket `Pipe[Word[8]]` here.
                            let Some(socket_args) = checked_instantiation_args(
                                builder,
                                instantiation_args_node(&submodule_stmt).as_ref(),
                                submodule_stmt.region(),
                                &socket_symbol,
                                false,
                                &mut component_analysis.diagnostics,
                            ) else {
                                continue;
                            };
                            let mono_socket_args: Vec<Type> = socket_args
                                .iter()
                                .map(|arg| substitute(arg, &submodule_args))
                                .collect();

                            let socket_def_location = socket_symbol.location();
                            let socket_def_parsing = builder.get_parsing(socket_def_location.package());
                            let socket_def_ast = socket_def_parsing.ast_node(socket_def_location.ast_node_id());

                            for channel_stmt in socket_def_ast.children() {
                                let id = ComponentId {
                                    item_id: moddef,
                                    index: component_analysis.components.len(),
                                };
                                let AstNodePayload::Channel(channel) = channel_stmt.payload() else {
                                    continue;
                                };

                                let channel_name = socket_def_parsing.string(channel.name);
                                let path = bstr::BString::from(
                                    format!("{}.{}.{}",
                                        instance_name.to_str_lossy(),
                                        socket_instance_name.to_str_lossy(),
                                        channel_name.to_str_lossy()).into_bytes()
                                );
                                let typ_node = channel_stmt.typ().unwrap();
                                // The channel type as written in the
                                // socketdef, substituted with the
                                // instance's (composed) type arguments.
                                let typ = match builder.get_type_at(typ_node.location()) {
                                    Ok(typ) => Some(substitute(&typ, &mono_socket_args)),
                                    Err(_) => None,
                                };
                                let flow = if drives_channel(submodule_socket.role, channel.dir) {
                                    Flow::Source
                                } else {
                                    Flow::Sink
                                };
                                let kind = match flow {
                                    Flow::Sink => Some(ComponentKind::Incoming),
                                    Flow::Source => Some(ComponentKind::Outgoing),
                                    Flow::Duplex => None,
                                };
                                let component = Component {
                                    id,
                                    path: path.clone(),
                                    location: stmt.location(),
                                    typ,
                                    flow,
                                    kind,
                                    synthetic: false,
                                    default_expr: None,
                                };
                                if !components_seen.contains(&path) {
                                    components_seen.insert(path.clone());
                                    component_analysis.components.push((path, component))
                                }
                            }
                        }
                        _ => continue,
                    }
                }
            }
            AstNodePayload::Socket(socket) => {
                let instance_name = parsing.string(socket.name);
                let ofness_node = stmt.child(1);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let packages = builder.get_packages();
                let socket_package: Option<PackageId> = match ofness.package {
                    Some(pkg) => packages.id(parsing.string(pkg)),
                    None => Some(location.package()),
                };
                let socket_name = parsing.string(ofness.name);
                let Some(socket_symbol) = socket_package.and_then(|pkg| symboltable.resolve_item(socket_name, pkg)) else {
                    continue;
                };
                let socket_symbol = socket_symbol.clone();
                crate::queries::pubcheck::check_item_reference(
                    builder,
                    &socket_symbol,
                    stmt.package(),
                    ofness_node.region(),
                    &mut component_analysis.diagnostics,
                );

                // Resolve and kind-check this socket instance's type
                // arguments (arity/kind errors are reported at the
                // site); skip registering its channels on failure.
                let Some(socket_args) = checked_instantiation_args(
                    builder,
                    instantiation_args_node(&stmt).as_ref(),
                    stmt.region(),
                    &socket_symbol,
                    false,
                    &mut component_analysis.diagnostics,
                ) else {
                    continue;
                };

                let socket_location = socket_symbol.location();
                let socket_parsing = builder.get_parsing(socket_location.package());
                let socket_ast = socket_parsing.ast_node(socket_location.ast_node_id());
                for socket_stmt in socket_ast.children() {
                    let id = ComponentId {
                        item_id: moddef,
                        index: component_analysis.components.len(),
                    };
                    let AstNodePayload::Channel(channel) = socket_stmt.payload() else {
                        continue;
                    };

                    let port_name = socket_parsing.string(channel.name);
                    let path = bstr::BString::from(
                        format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes()
                    );
                    let typ_node = socket_stmt.typ().unwrap();
                    // The channel type as written in the socketdef,
                    // substituted with this instance's type arguments.
                    let typ = match builder.get_type_at(typ_node.location()) {
                        Ok(typ) => Some(substitute(&typ, &socket_args)),
                        Err(_) => None,
                    };
                    let flow = if drives_channel(socket.role, channel.dir) {
                        Flow::Sink
                    } else {
                        Flow::Source
                    };
                    let component = Component {
                        id,
                        path: path.clone(),
                        location: stmt.location(),
                        typ,
                        flow,
                        kind: None,
                        synthetic: false,
                        default_expr: None,
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

    // Platform-default synthesis: a module bound to a platform via a
    // `for` clause is considered to declare every platform port. A
    // missing `outgoing` port with a platform `default <EXPR>` is
    // synthesized here (driver analysis turns the default expression
    // into its driver); a missing `incoming` port is synthesized as a
    // never-read input whose unused-signal warning is suppressed.
    // Ports missing without a `default` remain missing, so
    // `check_platform_ports_for` still reports `PlatformMissingPort`
    // for them.
    if matches!(symboltable.symbol(moddef).kind(), SymbolKind::ModDef) {
        match crate::analysis::platform::resolve_platform_for(builder, moddef) {
            Ok(Some(platform_id)) => {
                let platform_ports =
                    crate::analysis::platform::platform_ports_detailed(builder, platform_id);
                for platform_port in platform_ports {
                    if components_seen.contains(&platform_port.name) {
                        continue;
                    }
                    let id = ComponentId {
                        item_id: moddef,
                        index: component_analysis.components.len(),
                    };
                    let (flow, kind, default_expr) = match platform_port.dir {
                        PortDir::Input => (Flow::Source, Some(ComponentKind::Incoming), None),
                        PortDir::Output => match platform_port.default_expr {
                            Some(expr_location) =>
                                (Flow::Sink, Some(ComponentKind::Outgoing), Some(expr_location)),
                            // No `default`: the port stays missing and
                            // `check_platform_ports_for` reports it.
                            None => continue,
                        },
                        PortDir::Bidir => continue,
                    };
                    let component = Component {
                        id,
                        path: platform_port.name.clone(),
                        location: platform_port.location,
                        typ: platform_port.typ,
                        flow,
                        kind,
                        synthetic: true,
                        default_expr,
                    };
                    components_seen.insert(platform_port.name.clone());
                    component_analysis.components.push((platform_port.name, component));
                }
            }
            // No `for` clause (or an `ext mod`): nothing to synthesize.
            Ok(None) => {}
            // `UnresolvedItem`/`ExpectedPlatform` are reported by
            // `check_platform_ports_for`; do not duplicate them here.
            Err(_) => {}
        }
    }

    // Collect references after all components are registered.
    collect_references(&item_ast.children(), &mut component_analysis, None);

    Arc::new(component_analysis)
}

pub(crate) fn build_component(
    builder: &mut Builder,
    component_id: ComponentId,
) -> Arc<Component> {
    let component_analysis = builder.get_component_analysis(component_id.item_id);
    Arc::new(component_analysis.component(component_id).unwrap().clone())
}

// ---------------------------------------------------------------------------
// Reference collection
// ---------------------------------------------------------------------------

/// Walk the enclosing match arms to determine whether `name` at `start_id` is
/// shadowed by a pattern-bound variable (PatIdent or PatCtor sub-variable).
fn is_shadowed_by_pattern(
    parsing: &Parsing,
    start_id: AstNodeId,
    name: &BStr,
) -> bool {
    let mut child_id = start_id;
    loop {
        let child = parsing.ast_node(child_id);
        let parent_node = match child.parent() {
            Some(p) => p,
            None => return false,
        };
        let parent_id = parent_node.id();
        let parent = parsing.ast_node(parent_id);
        match parent.payload() {
            AstNodePayload::Match => {
                let children = parent.children();
                for (pat_opt, body) in match_arm_children(&children) {
                    if body.id() == child_id {
                        if let Some(pat) = pat_opt {
                            if let AstNodePayload::PatIdent(pat_ident) = pat.payload() {
                                if parsing.string(pat_ident.name) == name {
                                    return true;
                                }
                            }
                            if let AstNodePayload::PatCtor(_) = pat.payload() {
                                for var in pat.children() {
                                    if let Some(interned) = var.path() {
                                        if parsing.string(interned) == name {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        if parent.is_item() {
            return false;
        }
        child_id = parent_id;
    }
}

/// Walk every `ExprReference` inside `expr` and insert the locations that
/// resolve to a known component into `references`.  References that are
/// shadowed by a match-arm pattern binding are skipped.
fn collect_references_in_expr(
    expr: &AstNode,
    component_analysis: &mut ComponentAnalysis,
    it_context: Option<&BStr>,
) {
    let parsing = expr.parsing;
    let mut stack: Vec<AstNodeId> = vec![expr.id()];
    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        if matches!(node.payload(), AstNodePayload::ExprTap) {
            let path_node = node.child(0);
            if let Some(component) =
                crate::analysis::dependency::resolve_path_component(
                    &path_node,
                    &parsing,
                    component_analysis,
                    it_context,
                )
            {
                component_analysis.references.insert(
                    path_node.location(),
                    (component.id(), ReferenceKind::Expr),
                );
            }
        }
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(path) = node.path() {
                let mut name = parsing.string(path).to_owned();

                // Resolve `it` keyword.
                if name.starts_with(b"it.") || name == b"it" {
                    if let Some(ctx) = it_context {
                        if name == b"it" {
                            name = ctx.to_owned();
                        } else {
                            let suffix: BString = name[3..].to_owned().into();
                            name = ctx.to_owned();
                            name.push(b'.');
                            name.extend_from_slice(&suffix);
                        }
                    } else {
                        // `it` outside an it block is an error.
                        component_analysis.diagnostics.push(
                            Diagnostic::new(
                                node.region(),
                                diagnostics::DiagnosticPayload::ItNotInItBlock,
                            ),
                        );
                        continue;
                    }
                }

                // Skip if the name is shadowed by a match-arm pattern binding.
                if is_shadowed_by_pattern(parsing, node_id, name.as_bstr()) {
                    continue;
                }

                if let Some(component) = component_analysis.resolve(name.as_bstr()) {
                    component_analysis.references.insert(
                        node.location(),
                        (component.id(), ReferenceKind::Expr),
                    );
                }
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
}

/// Walk moddef-level statements and collect reference locations for every
/// driver target and every `ExprReference` that resolves to a known component.
fn collect_references(
    stmts: &[AstNode],
    component_analysis: &mut ComponentAnalysis,
    it_context: Option<&BStr>,
) {
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::Driver(_driver) => {
                // Record the driver target.
                let parsing = stmt.parsing;
                if let Some(target) = stmt.target() {
                    let mut target_str = parsing.string(target).to_owned();
                    if let Some(ctx) = it_context {
                        if target_str.starts_with(b"it.") {
                            let suffix: BString = target_str[3..].to_owned().into();
                            target_str = ctx.to_owned();
                            target_str.push(b'.');
                            target_str.extend_from_slice(&suffix);
                        } else if target_str == b"it" {
                            target_str = ctx.to_owned();
                        }
                    }
                    if let Some(component) = component_analysis.resolve(target_str.as_bstr()) {
                        let target_loc = stmt.child(0).location();
                        component_analysis.references.insert(target_loc, (component.id(), ReferenceKind::DriverTarget));
                    }
                }
                // Walk the driver expression.
                if let Some(expr_node) = stmt.driver() {
                    collect_references_in_expr(
                        &expr_node,
                        component_analysis,
                        it_context,
                    );
                }
            }
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);
                for side_node in [&lhs_node, &rhs_node] {
                    if let Some(side_path) = side_node.path() {
                        let mut side_str = parsing.string(side_path).to_owned();
                        if let Some(ctx) = it_context {
                            if side_str.starts_with(b"it.") {
                                let suffix: BString = side_str[3..].to_owned().into();
                                side_str = ctx.to_owned();
                                side_str.push(b'.');
                                side_str.extend_from_slice(&suffix);
                            } else if side_str == b"it" {
                                side_str = ctx.to_owned();
                            }
                        }
                        if let Some(component) =
                            component_analysis.resolve(side_str.as_bstr())
                        {
                            component_analysis.references
                                .insert(side_node.location(), (component.id(), ReferenceKind::Expr));
                        }
                    }
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                // The It-block is found by payload rather than by index,
                // so the optional GenericsParams child of a type-argument
                // instantiation does not shift it out from under us.
                if let Some(it_block) = children
                    .iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It))
                {
                    let block = &it_block.children()[0];
                    let name_with_context =
                        if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };
                    collect_references(
                        &block.children(),
                        component_analysis,
                        Some(name_with_context.as_bstr()),
                    );
                }
            }
            AstNodePayload::Component(component) => {
                let name = stmt.parsing.string(component.name);
                // Walk clock/reset expressions embedded in the component
                // declaration (e.g., `reg state : State on clock`).
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::ExprReference) {
                        if let Some(path) = child.path() {
                            let mut ref_name = stmt.parsing.string(path).to_owned();
                            if let Some(ctx) = it_context {
                                if ref_name.starts_with(b"it.") {
                                    let suffix: BString = ref_name[3..].to_owned().into();
                                    ref_name = ctx.to_owned();
                                    ref_name.push(b'.');
                                    ref_name.extend_from_slice(&suffix);
                                } else if ref_name == b"it" {
                                    ref_name = ctx.to_owned();
                                }
                            }
                            if let Some(component) =
                                component_analysis.resolve(ref_name.as_bstr())
                            {
                                component_analysis.references.insert(
                                    child.location(),
                                    (component.id(), ReferenceKind::Expr),
                                );
                            }
                        }
                    } else if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        let name_with_context =
                            if let Some(ctx) = it_context {
                                let mut new_name = BString::from(ctx);
                                new_name.push(b'.');
                                new_name.extend_from_slice(name);
                                new_name
                            } else {
                                BString::from(name)
                            };
                        collect_references(
                            &block.children(),
                            component_analysis,
                            Some(name_with_context.as_bstr()),
                        );
                    }
                }
            }
            AstNodePayload::Socket(socket) => {
                let name = stmt.parsing.string(socket.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        let name_with_context =
                            if let Some(ctx) = it_context {
                                let mut new_name = BString::from(ctx);
                                new_name.push(b'.');
                                new_name.extend_from_slice(name);
                                new_name
                            } else {
                                BString::from(name)
                            };
                        collect_references(
                            &block.children(),
                            component_analysis,
                            Some(name_with_context.as_bstr()),
                        );
                    }
                }
            }
            AstNodePayload::When => {
                let children = stmt.children();
                let mut idx = 0;
                while let Some(first) = children.get(idx) {
                    if first.is_expr() {
                        // case arm: guard + body
                        collect_references_in_expr(first, component_analysis, it_context);
                        if let Some(body) = children.get(idx + 1) {
                            if matches!(body.payload(), AstNodePayload::ArmBlock) {
                                collect_references(
                                    &body.children(),
                                    component_analysis,
                                    it_context,
                                );
                            } else {
                                // nested when/match or bare expr - recurse as single stmt
                                collect_references(&[body.clone()], component_analysis, it_context);
                            }
                        }
                        idx += 2;
                    } else {
                        // else arm: body only
                        if matches!(first.payload(), AstNodePayload::ArmBlock) {
                            collect_references(
                                &first.children(),
                                component_analysis,
                                it_context,
                            );
                        } else {
                            collect_references(&[first.clone()], component_analysis, it_context);
                        }
                        idx += 1;
                    }
                }
            }
            AstNodePayload::Match => {
                let children = stmt.children();
                // Walk references in the match subject expression (first child).
                if let Some(subject) = children.first() {
                    collect_references_in_expr(subject, component_analysis, it_context);
                }
                for (_pat_opt, body) in match_arm_children(&children) {
                    match body.payload() {
                        AstNodePayload::ArmBlock => {
                            collect_references(
                                &body.children(),
                                component_analysis,
                                it_context,
                            );
                        }
                        _ => {
                            // For When, Match and other
                            // non-block bodies, pass the body node itself so
                            // the appropriate handler processes guards etc.
                            collect_references(
                                &[body.clone()],
                                component_analysis,
                                it_context,
                            );
                        }
                    }
                }
            }
            AstNodePayload::ModDefStmtUnused => {
                let parsing = stmt.parsing;
                let path_node = stmt.child(0);
                if let Some(path_interned) = path_node.path() {
                    let mut resolved =
                        parsing.string(path_interned).to_owned();
                    if resolved.starts_with(b"it.")
                        || resolved == b"it"
                    {
                        // Walk up to find the enclosing it-context.
                        let mut ctx_name: Option<BString> = None;
                        let mut current_id = stmt.id();
                        while let Some(pid) =
                            parsing.ast_node(current_id).parent
                        {
                            let parent = parsing.ast_node(pid);
                            if let AstNodePayload::Submodule(m) =
                                parent.payload()
                            {
                                ctx_name = Some(
                                    parsing.string(m.name).to_owned(),
                                );
                                break;
                            } else if let AstNodePayload::Component(
                                c,
                            ) = parent.payload()
                            {
                                ctx_name = Some(
                                    parsing.string(c.name).to_owned(),
                                );
                                break;
                            } else if let AstNodePayload::Socket(
                                s,
                            ) = parent.payload()
                            {
                                ctx_name = Some(
                                    parsing.string(s.name).to_owned(),
                                );
                                break;
                            }
                            current_id = pid;
                        }
                        if let Some(ctx) = ctx_name {
                            if resolved.starts_with(b"it.") {
                                let suffix: BString =
                                    resolved[3..].to_owned().into();
                                resolved = ctx;
                                resolved.push(b'.');
                                resolved.extend_from_slice(&suffix);
                            } else {
                                resolved = ctx;
                            }
                        }
                    }
                    if let Some(component) =
                        component_analysis.resolve(resolved.as_bstr())
                    {
                        component_analysis.references
                            .insert(path_node.location(), (component.id(), ReferenceKind::Unused));
                    }
                }
            }
            AstNodePayload::ModDefStmtBlock => {
                collect_references(
                    &stmt.children(),
                    component_analysis,
                    it_context,
                );
            }
            _ => {}
        }
    }
}
