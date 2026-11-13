//! Defines `ComponentAnalysis`, collecting every component (signal,
//! port, register, etc.) declared in a module with its type, flow,
//! kind, and location, plus references to components (uses, driver
//! targets, `unused` marks) and associated diagnostics.

use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::Location;
use crate::analysis::dependency::{find_it_context, resolve_it_path};
use crate::analysis::symbols::{Symbol, SymbolId, SymbolKind};
use crate::common::source::Region;
use crate::db::Builder;
use crate::package::PackageId;
use crate::syntax::ast::{AstNode, AstNodeId, if_arm_children, match_arm_children};
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
    /// The module's implicit clock.  When the module declares *exactly
    /// one* component whose resolved type is the builtin `Clock`,
    /// `reg`/`outgoing reg` components may omit their `on <CLOCK>`
    /// clause, and it is as if the user had written
    /// `on <the_one_clock>`.  This holds that component's id.  `None`
    /// when the module declares zero or more than one Clock-typed
    /// component (or is not a mod definition).  Only components declared
    /// by the module itself count; submodule instance ports, socket
    /// channels, and platform-synthesized ports do not.
    implicit_clock: Option<ComponentId>,

    /// The platform this mod def is bound to via its `for` clause, when
    /// the clause resolved to a platform.  Set when platform ports are
    /// synthesized into this module; used by driver analysis to explain
    /// that an explicitly driven synthesized port must be declared.
    platform: Option<SymbolId>,

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

    /// For a `memory` component: the number of words it holds (the
    /// declared depth, `memory mem[1024] : ...`).  `None` for every
    /// other kind.  A power of two when `Some`: a non-literal or
    /// non-power-of-two depth is diagnosed and leaves this `None`.
    depth: Option<u32>,

    /// For a `memory` component: the path of its resolved clock — the
    /// `on` clause's clock, or the module's unique `Clock` when the
    /// clause is omitted (the same resolution as a `reg`'s).  `None`
    /// for every other kind, and for a
    /// memory whose slot could not be resolved (zero or ≥2 clocks,
    /// no clause — already reported as `MissingOnClause`).
    clock: Option<BString>,

    /// True for the channels of a socket instance (the module's own or
    /// a submodule instance's), false for every other component.  A
    /// socket channel in its driving role has kind `Outgoing`, and one
    /// in its receiving role has kind `Incoming`, but unlike a plain
    /// port a channel's `dependson` obligations come from the
    /// socketdef's channel list rather than a `dependson` list written
    /// in this module, and its exhaustiveness is checked against
    /// receiving *channels* rather than plain `incoming` ports.
    socket_channel: bool,
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

    /// Whether this component is a channel of a socket instance.
    pub fn socket_channel(&self) -> bool {
        self.socket_channel
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

    /// The declared depth of a `memory` component (`1024` in
    /// `memory mem[1024] : Word[32]`), or `None` for any other kind.
    pub fn depth(&self) -> Option<u32> {
        self.depth
    }

    /// The resolved clock path of a `memory` component, or `None`
    /// for any other kind (and for an unresolved memory clock slot).
    pub fn clock(&self) -> Option<BString> {
        self.clock.clone()
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

    /// The ids of the components that belong to `name`: the component
    /// named `name` itself, plus, when `name` names a submodule
    /// instance or a socket, each of its channels (components whose
    /// path is `name.x`).
    pub fn components_owned_by(&self, name: &BStr) -> IndexSet<ComponentId> {
        self.components
            .iter()
            .filter(|(path, _)| {
                path.as_bstr() == name
                    || (path.starts_with(name.as_bytes())
                        && path.len() > name.len()
                        && path.as_bytes()[name.len()] == b'.')
            })
            .map(|(_, component)| component.id())
            .collect()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    /// The module's implicit clock, if it declares exactly one
    /// Clock-typed component (see the field docs).
    pub fn implicit_clock(&self) -> Option<ComponentId> {
        self.implicit_clock
    }

    /// The platform bound by the module's `for` clause, if it resolved
    /// (see the field docs).
    pub fn platform(&self) -> Option<SymbolId> {
        self.platform
    }
}

fn drives_channel(role: SocketRole, dir: ChannelDir) -> bool {
    matches!(
        (role, dir),
        (SocketRole::Client, ChannelDir::Cosi) | (SocketRole::Server, ChannelDir::Soci)
    )
}

/// The numeric value of a word-literal spelling (`1024`, `10w1024`,
/// `0x40`, `0b10`), ignoring `_` separators.  `None` when the spelling
/// is not a plain literal value (or does not fit in a `u32`).
fn word_lit_value(spelling: &[u8]) -> Option<u32> {
    let text = std::str::from_utf8(spelling).ok()?;
    let text: String = text.chars().filter(|c| *c != '_').collect();
    let value_text = if let Some(rest) = text.strip_prefix("0b") {
        return u32::from_str_radix(rest, 2).ok();
    } else if let Some(rest) = text.strip_prefix("0x") {
        return u32::from_str_radix(rest, 16).ok();
    } else if let Some((_, value)) = text.split_once('w') {
        // A width-annotated literal (`10w1024`): the value part is
        // what names the depth.
        value.to_owned()
    } else {
        text
    };
    value_text.parse::<u32>().ok()
}

/// Validate the shape of a `memory` declaration:
/// the depth must be a literal power of two, and the element type
/// must be `Word[n]` with `n` a power of two.  Diagnostics go to
/// `diagnostics`; the declared depth is returned when valid (`None`
/// after an error, so downstream stages can skip the memory).
fn check_memory_shape(
    stmt: &AstNode<'_>,
    typ: Option<&Type>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<u32> {
    // Element type: `Word[n]` with `n` a power of two.
    let typ_node = stmt.typ().unwrap();
    match typ {
        Some(Type::Word(n)) => {
            if *n == 0 || (*n & (*n - 1)) != 0 {
                diagnostics.push(Diagnostic::new(
                    typ_node.region(),
                    diagnostics::MemoryElementNotPowerOf2Word {
                        typ: format!("Word[{n}]").into(),
                    },
                ));
                return None;
            }
        }
        Some(other) => {
            diagnostics.push(Diagnostic::new(
                typ_node.region(),
                diagnostics::MemoryElementNotPowerOf2Word {
                    typ: format!("{other}").into(),
                },
            ));
            return None;
        }
        None => return None, // unresolved type: already diagnosed
    }

    // Depth: a literal power of two.
    let depth_expr = stmt.depth_expr()?;
    let depth = match depth_expr.payload() {
        AstNodePayload::ExprWordLit(_) => {
            word_lit_value(stmt.parsing.text(depth_expr.span()))
        }
        _ => None,
    };
    let Some(depth) = depth else {
        diagnostics.push(Diagnostic::new(
            depth_expr.region(),
            diagnostics::MemoryDepthNotLiteral,
        ));
        return None;
    };
    if depth == 0 || (depth & (depth - 1)) != 0 {
        diagnostics.push(Diagnostic::new(
            depth_expr.region(),
            diagnostics::MemoryDepthNotPowerOf2 { depth },
        ));
        return None;
    }
    Some(depth)
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
        implicit_clock: None,
        platform: None,
        references: IndexMap::new(),
    };

    // Implicit-clock bookkeeping (resolved once the module's own
    // components have all been walked):
    // - ids of the module's own components whose resolved type is the
    //   builtin `Clock`;
    // - regs/outgoing-regs written without an `on` clause, which need
    //   one unless the module has a unique implicit clock;
    // - components of every kind written with an explicit `on` clause,
    //   which are redundant when the module has a unique implicit
    //   clock (and the clause names that clock).
    let mut clock_ids: Vec<ComponentId> = vec![];
    let mut regs_without_on: Vec<(BString, Region)> = vec![];
    let mut written_ons: Vec<(BString, Region, BString)> = vec![];

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

                // A `memory` declares its depth as a literal
                // (`memory mem[1024] : Word[32]`); validate the shape
                // (both powers of two, depth a literal) and record it.
                let depth = if component.kind == ComponentKind::Memory {
                    check_memory_shape(&stmt, typ.as_ref(), &mut component_analysis.diagnostics)
                } else {
                    None
                };

                let flow = match component.kind {
                    ComponentKind::Incoming => Flow::Source,
                    ComponentKind::Outgoing => Flow::Sink,
                    ComponentKind::OutgoingWire => Flow::Duplex,
                    ComponentKind::OutgoingReg => Flow::Duplex,
                    ComponentKind::Reg => Flow::Duplex,
                    ComponentKind::Wire => Flow::Duplex,
                    ComponentKind::Memory => Flow::Duplex,
                    ComponentKind::Bus | ComponentKind::Net => Flow::Duplex,
                };
                let component_kind = component.kind;
                let component = Component {
                    id,
                    path: path.clone(),
                    location: stmt.location(),
                    typ: typ.clone(),
                    flow,
                    kind: Some(component_kind),
                    synthetic: false,
                    default_expr: None,
                    socket_channel: false,
                    depth,
                    // The resolved clock slot is filled in below, once
                    // the module's implicit clock is known.
                    clock: None,
                };
                // An image ROM never writes, so it needs no clock:
                // keep it out of the `MissingOnClause` machinery.
                let memory_needs_clock = component_kind != ComponentKind::Memory
                    || stmt.children().first().map_or(true, |annotations| {
                        !annotations.children().iter().any(|annotation| {
                            matches!(
                                annotation.payload(),
                                AstNodePayload::Annotation(annotation)
                                    if matches!(
                                        parsing.string(annotation.name).as_bytes(),
                                        b"image" | b"image_hex" | b"image_inline"
                                    )
                            )
                        })
                    });
                if matches!(component_kind, ComponentKind::Reg | ComponentKind::OutgoingReg)
                    || (component_kind == ComponentKind::Memory && memory_needs_clock)
                {
                    match stmt.clock() {
                        None => regs_without_on.push((path.clone(), stmt.region())),
                        Some(clock_node) => {
                            let written_clock = clock_node
                                .path()
                                .map(|interned| parsing.string(interned).to_owned());
                            if let Some(written_clock) = written_clock {
                                written_ons.push((path.clone(), clock_node.region(), written_clock));
                            }
                        }
                    }
                } else if let Some(clock_node) = stmt.on_clause() {
                    // Non-reg kinds also carry `on` clauses in the
                    // grammar (though the clause does nothing there),
                    // so they take part in the redundancy check too.
                    let written_clock = clock_node
                        .path()
                        .map(|interned| parsing.string(interned).to_owned());
                    if let Some(written_clock) = written_clock {
                        written_ons.push((path.clone(), clock_node.region(), written_clock));
                    }
                }
                if !components_seen.contains(&path) {
                    components_seen.insert(path.clone());
                    if typ.as_ref() == Some(&Type::Clock) {
                        clock_ids.push(id);
                    }
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
                let submodule_name = parsing.string(ofness.name);
                let qualifier = ofness
                    .package
                    .and_then(|pkg| packages.id(parsing.string(pkg)));
                let submodule_symbol = match crate::analysis::package::resolve_item_reference(
                    builder,
                    submodule_name,
                    location.package(),
                    qualifier,
                ) {
                    Some(symbol) => symbol,
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
                                // A memory is module-local and never a
                                // port; this arm is unreachable (the
                                // kind filter above excludes it) but
                                // keeps the match exhaustive.
                                ComponentKind::Memory => Flow::Duplex,
                            };
                            let component_kind = component.kind;
                            let component = Component {
                                id,
                                path: path.clone(),
                                location: stmt.location(),
                                typ,
                                flow,
                                kind: Some(component_kind),
                                socket_channel: false,
                                synthetic: false,
                                default_expr: None,
                                depth: None,
                                clock: None,
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
                                    socket_channel: true,
                                    synthetic: false,
                                    default_expr: None,
                                    depth: None,
                                    clock: None,
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
                    // The channel in its driving role (`client` for
                    // `cosi`, `server` for `soci`) is an outgoing port of
                    // this module; in the receiving role it is an incoming
                    // one.  (The submodule-socket arm below assigns the
                    // same kinds from the parent's opposite perspective.)
                    let drives = drives_channel(socket.role, channel.dir);
                    let flow = if drives {
                        Flow::Sink
                    } else {
                        Flow::Source
                    };
                    let kind = if drives {
                        Some(ComponentKind::Outgoing)
                    } else {
                        Some(ComponentKind::Incoming)
                    };
                    let component = Component {
                        id,
                        path: path.clone(),
                        location: stmt.location(),
                        typ,
                        flow,
                        kind,
                        socket_channel: true,
                        synthetic: false,
                        default_expr: None,
                        depth: None,
                        clock: None,
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

    // Resolve the implicit clock now that the module's own components
    // have all been walked.  With exactly one Clock-typed component,
    // regs may omit their `on` clause and are implicitly clocked by it;
    // otherwise every reg still requires an explicit `on` clause.
    if clock_ids.len() == 1 {
        let implicit_clock_id = clock_ids.pop();
        component_analysis.implicit_clock = implicit_clock_id;
        let implicit_clock_path = implicit_clock_id
            .and_then(|id| component_analysis.component(id))
            .map(|component| component.path());
        // An explicit `on` clause naming the module's unique clock is
        // redundant: omitting it has exactly the same meaning — on a
        // `reg` because the implicit clock covers it, and on every other
        // kind because the clause is inert.  An `on` clause referencing
        // anything else (eg, a submodule's clock output) is load-bearing
        // on a reg and stays silent.
        for (component, region, written_clock) in written_ons {
            if Some(&written_clock) == implicit_clock_path.as_ref() {
                component_analysis.diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::RedundantOnClause { component },
                ));
            }
        }
    } else {
        for (component, region) in regs_without_on {
            component_analysis.diagnostics.push(Diagnostic::new(
                region,
                diagnostics::MissingOnClause { component },
            ));
        }
    }

    // Resolve each `memory`'s clock slot, exactly
    // like a `reg`'s: an explicit `on <CLOCK>` clause wins; otherwise
    // the module's unique `Clock` covers it; otherwise the slot is
    // unresolved (`MissingOnClause` was reported above) and memory
    // writes report `MemoryWriteOutsideOnRegion`.
    {
        let implicit_clock_path = component_analysis
            .implicit_clock()
            .and_then(|id| component_analysis.component(id))
            .map(|component| component.path());
        for (_path, component) in component_analysis.components.iter_mut() {
            if component.kind != Some(ComponentKind::Memory) {
                continue;
            }
            let decl = parsing.ast_node(component.location().ast_node_id());
            let clock = match decl.clock() {
                Some(clock_node) => clock_node
                    .path()
                    .map(|interned| parsing.string(interned).to_owned()),
                None => implicit_clock_path.clone(),
            };
            component.clock = clock;
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
    //
    // Synthesized ports are otherwise left alone: a module may read
    // them freely, but an *explicit driver* on a synthesized port is an
    // error (`DriveUndeclaredPlatformPort`, reported by driver
    // analysis): driving an `outgoing` requires declaring it in the
    // mod def.
    if matches!(symboltable.symbol(moddef).kind(), SymbolKind::ModDef) {
        match crate::analysis::platform::resolve_platform_for(builder, moddef) {
            Ok(Some(platform_id)) => {
                component_analysis.platform = Some(platform_id);
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
                        socket_channel: false,
                        synthetic: true,
                        default_expr,
                        depth: None,
                        clock: None,
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
pub(crate) fn is_shadowed_by_pattern(
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
            AstNodePayload::Let => {
                // A `let` binding shadows module-level components of the
                // same name within its scope.  The scope of binding `i` is
                // the bodies of bindings `i+1..` and the let's body; the
                // RHS of binding `i` sees only bindings `0..i`.
                let bindings = parent.let_bindings();
                let scope_limit = bindings
                    .iter()
                    .position(|b| b.id() == child_id)
                    .unwrap_or(bindings.len());
                for binding in bindings.iter().take(scope_limit) {
                    if let Some(binding_name) = binding.let_binding_name() {
                        if parsing.string(binding_name) == name {
                            return true;
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
                    } else if component.kind == ComponentKind::Memory
                        && matches!(child.payload(), AstNodePayload::ModDefStmtBlock)
                    {
                        // A memory body: `it` refers to the memory,
                        // exactly like an It-block.
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
                            &child.children(),
                            component_analysis,
                            Some(name_with_context.as_bstr()),
                        );
                    }
                }
            }
            AstNodePayload::OnRegion => {
                // An `on <CLOCK> { ... }` region of a memory body:
                // record the clock reference and walk the region's
                // statements with the enclosing `it` context.
                let children = stmt.children();
                collect_references_in_expr(&children[0], component_analysis, it_context);
                collect_references(&children[1].children(), component_analysis, it_context);
            }
            AstNodePayload::IndexedDriver(_) => {
                // An indexed write's target: the LHS `Path` (child 0).
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
                // The index and the written value are expressions.
                if let Some(index_node) = stmt.driver_index() {
                    collect_references_in_expr(
                        &index_node,
                        component_analysis,
                        it_context,
                    );
                }
                if let Some(expr_node) = stmt.driver() {
                    collect_references_in_expr(
                        &expr_node,
                        component_analysis,
                        it_context,
                    );
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
            AstNodePayload::Let => {
                // Walk each binding's RHS expression and the let's body.
                // References inside a binding's scope are skipped when
                // shadowed by a binding name (`is_shadowed_by_pattern`
                // handles the `let` scoping rules).
                for binding in stmt.let_bindings() {
                    if let Some(rhs) = binding.let_binding_expr() {
                        collect_references_in_expr(&rhs, component_analysis, it_context);
                    }
                }
                if let Some(body) = stmt.let_body() {
                    if matches!(body.payload(), AstNodePayload::ArmBlock) {
                        collect_references(&body.children(), component_analysis, it_context);
                    } else {
                        collect_references(&[body.clone()], component_analysis, it_context);
                    }
                }
            }
            AstNodePayload::If => {
                // Pair children via `if_arm_children` (2 per `when` arm, 1 per
                // `else` arm); an `is_expr()` test cannot distinguish arms
                // because an `else` body may itself be an `If`/`Match` node.
                let children = stmt.children();
                for (guard, body) in if_arm_children(&children) {
                    if let Some(guard) = guard {
                        collect_references_in_expr(guard, component_analysis, it_context);
                    }
                    if matches!(body.payload(), AstNodePayload::ArmBlock) {
                        collect_references(
                            &body.children(),
                            component_analysis,
                            it_context,
                        );
                    } else {
                        // nested if/match or bare expr - recurse as single stmt
                        collect_references(&[body.clone()], component_analysis, it_context);
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
                            // For If, Match and other
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
                    resolve_it_path(
                        &mut resolved,
                        find_it_context(&stmt).as_deref().map(BStr::new),
                    );
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
