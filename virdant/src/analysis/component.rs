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
use crate::analysis::domain::Domain;
use crate::common::{ChannelDir, ComponentKind, Flow, PortDir, SocketRole};
use crate::diagnostics::{self, Diagnostic};

#[derive(Debug)]
pub struct ComponentAnalysis {
    moddef: SymbolId,
    components: Vec<(BString, Component)>,
    diagnostics: Vec<Diagnostic>,
    /// The module's unique clock: the canonical source of its single
    /// sourced clock class, when there is exactly one.
    /// A module's clock sources are its incoming clock ports (its own
    /// declared `incoming` components typed the builtin `Clock`, and
    /// the inward (receiving) channels of its own socket instances),
    /// and the clock output ports of the submodule and `ext mod`
    /// instances it registers.  When there is *exactly one* sourced
    /// class, `reg`/`outgoing reg` components may omit their
    /// `on <CLOCK>` clause, and every local component without an
    /// explicit domain clause is clocked by the unique clock.  `None`
    /// when the module has zero or more than one sourced class (or is
    /// not a mod definition).
    implicit_clock: Option<ComponentId>,

    /// The canonical source of every sourced clock class of the
    /// module's view, one entry per class.
    clock_sources: Vec<ComponentId>,

    /// Maps every sourced Clock-typed component of the module's view to
    /// its class's canonical source.  A Clock-typed component absent
    /// from the map has a *sourceless* class — a clock wire the module
    /// never defines (driver analysis diagnoses that).
    clock_canonical: IndexMap<ComponentId, ComponentId>,

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

    /// True when the component was registered from a submodule
    /// instance — an instance port, or a channel of one of the
    /// instance's sockets.  False for the module's own declarations,
    /// the channels of the module's own sockets, and synthesized
    /// platform ports.  Only non-submodule-owned components belong to
    /// the module proper: they are the ones its clocks clock, and the
    /// ones counted by `incoming_clock_ports`.
    submodule_owned: bool,

    /// The component's clock domain, when the declaration alone
    /// determines it; `None` when it does not (the clock checker
    /// assigns those).  See the clock checker for the full rules.
    domain: Option<Domain>,
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

    /// Whether this component was registered from a submodule
    /// instance (a port of the instance, or a channel of one of the
    /// instance's sockets); see the field docs.
    pub fn submodule_owned(&self) -> bool {
        self.submodule_owned
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

    /// The component's clock domain; see the `domain` field docs.
    pub fn domain(&self) -> Option<&Domain> {
        self.domain.as_ref()
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

    /// The collected references, keyed by the referencing AST node's
    /// location.  See `collect_references`.
    pub fn references(
        &self,
    ) -> &IndexMap<Location, (ComponentId, ReferenceKind)> {
        &self.references
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

    /// The module's unique clock, if it has exactly one sourced clock
    /// class (see the field docs).
    pub fn implicit_clock(&self) -> Option<ComponentId> {
        self.implicit_clock
    }

    /// The module's clock count: the number of sourced clock classes.
    /// A module may acquire clocks internally — from
    /// the clock outputs of its instances — and such clocks count.
    pub fn clock_count(&self) -> usize {
        self.clock_sources.len()
    }

    /// The canonical source of the clock class `component_id` denotes,
    /// or `None` when the component is not Clock-typed or its class has
    /// no source.
    pub fn clock_canonical(&self, component_id: ComponentId) -> Option<ComponentId> {
        self.clock_canonical.get(&component_id).copied()
    }

    /// The canonical source of the clock class denoted by the component
    /// at `path`, or `None`.
    pub fn clock_canonical_by_path(&self, path: &BStr) -> Option<ComponentId> {
        self.resolve(path)
            .and_then(|component| self.clock_canonical(component.id()))
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
        (SocketRole::Client, ChannelDir::Outgoing) | (SocketRole::Server, ChannelDir::Incoming)
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

fn join_paths(parts: &[&BStr]) -> BString {
    let mut joined = BString::new(Vec::new());
    for (i, part) in parts.iter().enumerate() {
        if i > 0 {
            joined.push(b'.');
        }
        joined.extend_from_slice(part);
    }
    joined
}

pub(crate) fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
        diagnostics: vec![],
        implicit_clock: None,
        clock_sources: vec![],
        clock_canonical: IndexMap::new(),
        platform: None,
        references: IndexMap::new(),
    };

    // Implicit-clock bookkeeping (resolved once every component has
    // been walked and registered):
    // - regs/outgoing-regs written without an `on` clause, which need
    //   one unless the module has a unique implicit clock;
    // - components of every kind written with an explicit `on` clause,
    //   which are redundant when the module has a unique implicit
    //   clock (and the clause names that clock).
    // Incoming clock ports are classified after the walk by
    // `incoming_clock_ports`.
    let mut regs_without_on: Vec<(BString, Region)> = vec![];
    let mut written_ons: Vec<(BString, Region, BString)> = vec![];
    // Domains not fixed by the declaration itself (`on` clauses and
    // unique-clock inference) are resolved in the post-pass below.
    let mut ons_to_resolve: Vec<ComponentId> = vec![];
    // `on <CLOCK>` clauses on synthesized platform ports and socketdef
    // channels, resolved in the post-pass below.
    let mut deferred_ons: Vec<(ComponentId, Vec<BString>)> = vec![];
    // Channels of single-Clock-channel socket instances that carry no
    // explicit clause: implicitly `on` the socket's unique Clock
    // channel, whose instance path is kept here and resolved to a
    // component id in the post-pass, once every channel is registered.
    let mut deferred_socket_clocks: Vec<(ComponentId, BString)> = vec![];

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
                let is_async = component.is_async;
                // `async` components get a declaration-fixed domain;
                // Clock-typed components are clocked by their own clock
                // classes, resolved in the post-pass below (see
                // `compute_clock_classes`).
                let mut domain = None;
                if component.is_async {
                    domain = Some(Domain::Async);
                } else if typ.as_ref() != Some(&Type::Clock)
                    && stmt.on_clause().is_some()
                {
                    ons_to_resolve.push(id);
                }
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
                    submodule_owned: false,
                    depth,
                    domain,
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
                    // `async` already supplies the domain clause.
                    if is_async {
                        ()
                    } else {
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
                    component_analysis.components.push((path.clone(), component));
                }
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name);
                let Some(ofness_node) = stmt.ofness() else {
                    continue;
                };
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
                                submodule_owned: true,
                                synthetic: false,
                                default_expr: None,
                                depth: None,
                                clock: None,
                                domain: None,
                            };
                            if !components_seen.contains(&path) {
                                components_seen.insert(path.clone());
                                component_analysis.components.push((path, component))
                            }
                        }
                        AstNodePayload::Socket(submodule_socket) => {
                            // Handle sockets in submodules - treat each channel as a component
                            let socket_instance_name = submodule_parsing.string(submodule_socket.name);
                            let Some(socket_ofness_node) = submodule_stmt.ofness() else {
                                continue;
                            };
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

                            // The socket's clock structure decides the
                            // channels' domains, exactly like the
                            // module-level socket arm below.
                            let mut channel_decls: Vec<(AstNode<'_>, crate::syntax::payload::Channel, Option<Type>)> = vec![];
                            for channel_stmt in socket_def_ast.children() {
                                let AstNodePayload::Channel(channel) = channel_stmt.payload() else {
                                    continue;
                                };
                                let typ_node = channel_stmt.typ().unwrap();
                                // The channel type as written in the
                                // socketdef, substituted with the
                                // instance's (composed) type arguments.
                                let typ = match builder.get_type_at(typ_node.location()) {
                                    Ok(typ) => Some(substitute(&typ, &mono_socket_args)),
                                    Err(_) => None,
                                };
                                channel_decls.push((channel_stmt.clone(), channel, typ));
                            }
                            let socket_clock_count = channel_decls
                                .iter()
                                .filter(|(_, _, typ)| typ.as_ref() == Some(&Type::Clock))
                                .count();
                            let unique_clock_path: Option<BString> = if socket_clock_count == 1 {
                                channel_decls
                                    .iter()
                                    .find(|(_, _, typ)| typ.as_ref() == Some(&Type::Clock))
                                    .map(|(_, channel, _)| {
                                        join_paths(&[
                                            instance_name.as_bstr(),
                                            socket_instance_name.as_bstr(),
                                            socket_def_parsing.string(channel.name).as_bstr(),
                                        ])
                                    })
                            } else {
                                None
                            };
                            for (channel_stmt, channel, typ) in channel_decls {
                                let id = ComponentId {
                                    item_id: moddef,
                                    index: component_analysis.components.len(),
                                };

                                let channel_name = socket_def_parsing.string(channel.name);
                                let path = bstr::BString::from(
                                    format!("{}.{}.{}",
                                        instance_name.to_str_lossy(),
                                        socket_instance_name.to_str_lossy(),
                                        channel_name.to_str_lossy()).into_bytes()
                                );
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
                                // Domain rules per the socket's clock
                                // structure (see
                                // `check_socketdef_domains`): `async` →
                                // `Async`, Clock-typed → the channel's
                                // own clock class (post-pass), explicit
                                // `on` → deferred, else a clock variable
                                // (0 Clock channels), implicit (1),
                                // unclad (≥2).
                                let domain = if channel.is_async {
                                    Some(Domain::Async)
                                } else if typ.as_ref() == Some(&Type::Clock) {
                                    None
                                } else if channel_stmt.on_clause().is_some() {
                                    None
                                } else {
                                    match socket_clock_count {
                                        0 => Some(Domain::sync_var(0)),
                                        1 => {
                                            deferred_socket_clocks.push((
                                                id,
                                                unique_clock_path.clone().unwrap(),
                                            ));
                                            None
                                        }
                                        _ => None,
                                    }
                                };
                                if let Some(on_clause) = channel_stmt.on_clause()
                                    && let Some(path) = on_clause.path()
                                {
                                    deferred_ons.push((
                                        id,
                                        vec![join_paths(&[
                                            instance_name,
                                            socket_instance_name,
                                            socket_def_parsing.string(path),
                                        ])],
                                    ));
                                }
                                let component = Component {
                                    id,
                                    path: path.clone(),
                                    location: stmt.location(),
                                    typ,
                                    flow,
                                    kind,
                                    socket_channel: true,
                                    submodule_owned: true,
                                    synthetic: false,
                                    default_expr: None,
                                    depth: None,
                                    clock: None,
                                    domain,
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
                let Some(ofness_node) = stmt.ofness() else {
                    continue;
                };
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

                // The socket's clock structure decides the channels'
                // domains: a channel typed `Clock` is a clock (always
                // `Async`); with no Clock channels the other channels are
                // flexible; with exactly one, they are implicitly `on`
                // it (unless declared `async`); with several, they need
                // explicit clauses (reported at the socketdef).
                let mut channel_decls: Vec<(AstNode<'_>, crate::syntax::payload::Channel, Option<Type>)> = vec![];
                for socket_stmt in socket_ast.children() {
                    let AstNodePayload::Channel(channel) = socket_stmt.payload() else {
                        continue;
                    };
                    let typ_node = socket_stmt.typ().unwrap();
                    // The channel type as written in the socketdef,
                    // substituted with this instance's type arguments.
                    let typ = match builder.get_type_at(typ_node.location()) {
                        Ok(typ) => Some(substitute(&typ, &socket_args)),
                        Err(_) => None,
                    };
                    channel_decls.push((socket_stmt.clone(), channel, typ));
                }
                let socket_clock_count = channel_decls
                    .iter()
                    .filter(|(_, _, typ)| typ.as_ref() == Some(&Type::Clock))
                    .count();
                let unique_clock_path: Option<BString> = if socket_clock_count == 1 {
                    channel_decls
                        .iter()
                        .find(|(_, _, typ)| typ.as_ref() == Some(&Type::Clock))
                        .map(|(_, channel, _)| {
                            join_paths(&[
                                instance_name.as_bstr(),
                                socket_parsing.string(channel.name).as_bstr(),
                            ])
                        })
                } else {
                    None
                };
                for (socket_stmt, channel, typ) in channel_decls {
                    let id = ComponentId {
                        item_id: moddef,
                        index: component_analysis.components.len(),
                    };

                    let port_name = socket_parsing.string(channel.name);
                    let path = bstr::BString::from(
                        format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes()
                    );
                    // The channel in its driving role (`client` for
                    // `outgoing`, `server` for `incoming`) is an outgoing port of
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
                    // Domain rules per the socket's clock structure
                    // (see `check_socketdef_domains`): `async` →
                    // `Async`, Clock-typed → the channel's own clock
                    // class (post-pass), explicit `on` → deferred, else
                    // a clock variable (0 Clock channels), implicit
                    // (1), unclad (≥2).
                    let domain = if channel.is_async {
                        Some(Domain::Async)
                    } else if typ.as_ref() == Some(&Type::Clock) {
                        None
                    } else if socket_stmt.on_clause().is_some() {
                        None
                    } else {
                        match socket_clock_count {
                            0 => Some(Domain::sync_var(0)),
                            1 => {
                                deferred_socket_clocks
                                    .push((id, unique_clock_path.clone().unwrap()));
                                None
                            }
                            _ => None,
                        }
                    };
                    if let Some(on_clause) = socket_stmt.on_clause()
                        && let Some(on_path) = on_clause.path()
                    {
                        deferred_ons.push((
                            id,
                            vec![join_paths(&[instance_name, socket_parsing.string(on_path)])],
                        ));
                    }
                    let component = Component {
                        id,
                        path: path.clone(),
                        location: stmt.location(),
                        typ,
                        flow,
                        kind,
                        socket_channel: true,
                        submodule_owned: false,
                        synthetic: false,
                        default_expr: None,
                        depth: None,
                        clock: None,
                        domain,
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
    //
    // Synthesized ports may otherwise only be left untouched: *using*
    // one — any expression reference, including a reg's `on <CLOCK>`
    // clock — is an error (`UseUndeclaredPlatformPort`, reported below
    // from the collected references), and an *explicit driver* on a
    // synthesized port is an error (`DriveUndeclaredPlatformPort`,
    // reported by driver analysis).  Both require declaring the port in
    // the mod def: synthesis exists so a module may ignore platform
    // ports, not so module code may reach through the `for` clause and
    // read undeclared names.
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
                    let domain = if platform_port.is_async {
                        Some(Domain::Async)
                    } else {
                        None
                    };
                    if let Some(on_path) = &platform_port.on_path {
                        deferred_ons.push((id, vec![on_path.clone()]));
                    }
                    let component = Component {
                        id,
                        path: platform_port.name.clone(),
                        location: platform_port.location,
                        typ: platform_port.typ,
                        flow,
                        kind,
                        socket_channel: false,
                        submodule_owned: false,
                        synthetic: true,
                        default_expr,
                        depth: None,
                        clock: None,
                        domain,
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

    // Clock classes: resolve every
    // Clock-typed component in the module's view to the canonical
    // source of its class — the incoming clock port, instance clock
    // output, or generator call that introduces the clock identity —
    // or to nothing when its class has no source (a clock wire the
    // module never defines; driver analysis diagnoses that).  The
    // equivalence relation is generated by the *sources* (each
    // incoming clock port, instance clock output, and generating
    // builtin is a source of its own; distinct sources are distinct
    // clocks however the platform ultimately wires them, so a module's
    // clock structure never depends on its instantiation context) and
    // by drivers: a driver whose target is a Clock-typed component
    // and whose driving expression is a plain reference to a
    // Clock-typed thing asserts the two are the same clock.  See
    // `collect_guarded_defs` for how guarded drivers contribute.
    let clock_classes = compute_clock_classes(&item_ast, &mut component_analysis);
    let clock_count = clock_classes.sources.len();
    // The module's unique clock: the single sourced class's source,
    // when there is exactly one.  It may arise from any kind of source
    // — an incoming clock port, an instance's clock output — and it
    // needs no naming: local components may omit their `on` clause and
    // are clocked by it anyway.
    let implicit_clock_id = if clock_count == 1 {
        Some(clock_classes.sources[0])
    } else {
        None
    };
    component_analysis.implicit_clock = implicit_clock_id;
    component_analysis.clock_sources = clock_classes.sources.clone();
    component_analysis.clock_canonical = clock_classes.canonical.clone();

    // Clock provenance: an outgoing Clock-typed port declares whether it
    // aliases an incoming clock (`dependson <clock>`) or originates a
    // fresh clock (no `dependson`, no definition).  A visible
    // definition rooted at an incoming clock port without the
    // declaration is reported here.
    check_clock_provenance(&parsing, &item_ast, &mut component_analysis);

    // Under `@unsafe_clocks`, an explicit `on` clause is never
    // redundant: omitting it is a `MissingDomainClause` error, so the
    // clause is load-bearing even when it names the unique clock.
    let has_unsafe_clocks = item_ast.annotations().into_iter().any(|annotation| {
        matches!(
            annotation.payload(),
            AstNodePayload::Annotation(annotation)
                if parsing.string(annotation.name).as_bytes() == b"unsafe_clocks"
        )
    });

    // Every Clock-typed component is clocked by the very class it
    // denotes: its domain is its class's canonical source.  An
    // instance's unbound incoming clock port is the instance's abstract
    // clock variable — never sourced, never equal to a concrete class.
    let mut clock_domains: Vec<(ComponentId, Option<Domain>)> = vec![];
    for (_, component) in &component_analysis.components {
        if component.typ.as_ref() != Some(&Type::Clock) {
            continue;
        }
        let domain = match component_analysis.clock_canonical.get(&component.id) {
            Some(source) => Some(Domain::sync(*source)),
            None if component.submodule_owned
                && component.kind == Some(ComponentKind::Incoming) =>
            {
                // Unbound instance clock port: the instance's abstract
                // clock variable.  No definition binds it, so only
                // guard comparisons can reach this domain, and they
                // fail.
                Some(Domain::sync_var(0))
            }
            None => None,
        };
        clock_domains.push((component.id, domain));
    }
    for (component_id, domain) in clock_domains {
        if let Some((_, component)) = component_analysis
            .components
            .iter_mut()
            .find(|(_, component)| component.id == component_id)
        {
            component.domain = domain;
        }
    }

    if clock_count == 1 && !has_unsafe_clocks {
        let unique_class = implicit_clock_id;
        for (component, region, written_clock) in written_ons {
            // An explicit `on` clause naming the module's unique clock
            // — directly, or through any alias of it — is redundant:
            // omitting it has exactly the same meaning.  An `on` clause
            // naming a different clock class is load-bearing and stays
            // silent.
            let written_class = component_analysis
                .resolve(written_clock.as_bstr())
                .filter(|clock| clock.typ.as_ref() == Some(&Type::Clock))
                .and_then(|clock| component_analysis.clock_canonical.get(&clock.id).copied());
            if written_class == unique_class {
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

    // Resolve each `memory`'s clock slot, exactly like a `reg`'s: an
    // explicit `on <CLOCK>` clause wins; otherwise the module's unique
    // clock covers it; otherwise the slot is unresolved
    // (`MissingOnClause` was reported above) and memory writes report
    // `MemoryWriteOutsideOnRegion`.  The slot is a clock class, stored
    // as its canonical source's path.
    {
        let implicit_clock_path = implicit_clock_id
            .and_then(|id| component_analysis.component(id))
            .map(|component| component.path());
        let mut slots: Vec<Option<BString>> = vec![];
        for (_path, component) in component_analysis.components.iter() {
            if component.kind != Some(ComponentKind::Memory) {
                slots.push(None);
                continue;
            }
            let decl = parsing.ast_node(component.location().ast_node_id());
            let clock: Option<BString> = match decl.clock() {
                Some(clock_node) => clock_node
                    .path()
                    .map(|interned| parsing.string(interned).to_owned())
                    .and_then(|written| {
                        // The slot is the named clock's class, stored as
                        // its canonical source's path.
                        component_analysis
                            .resolve(written.as_bstr())
                            .filter(|clock| clock.typ.as_ref() == Some(&Type::Clock))
                            .and_then(|clock| component_analysis.clock_canonical(clock.id()))
                            .and_then(|source| component_analysis.component(source))
                            .map(|component| component.path())
                    }),
                None => implicit_clock_path.clone(),
            };
            slots.push(clock);
        }
        let mut slot_index = 0;
        for (_path, component) in component_analysis.components.iter_mut() {
            component.clock = slots[slot_index].clone();
            slot_index += 1;
        }
    }

    // Resolve each `on <CLOCK>` clause to the class of the clock it
    // names.  In a zero-clock module the clause has no clocks to name
    // and the domain stays unset (the clock checker diagnoses it).
    if clock_count > 0 {
        for &owner_id in &ons_to_resolve {
            let clock_class = component_analysis
                .component(owner_id)
                .map(|component| component.location.ast_node_id())
                .and_then(|ast_node_id| {
                    let decl = parsing.ast_node(ast_node_id);
                    decl.on_clause()
                        .and_then(|clock_node| clock_node.path())
                })
                .and_then(|interned| {
                    component_analysis.resolve(parsing.string(interned))
                })
                .filter(|clock| clock.typ.as_ref() == Some(&Type::Clock))
                .and_then(|clock| component_analysis.clock_canonical.get(&clock.id).copied());
            if let Some(class_source) = clock_class {
                if let Some((_, component)) = component_analysis
                    .components
                    .iter_mut()
                    .find(|(_, component)| component.id == owner_id)
                {
                    component.domain = Some(Domain::sync(class_source));
                }
            }
        }
    }
    // Channels implicitly `on` a single-Clock-channel socket's
    // unique Clock channel: the channel is a source of its own, so its
    // class is itself.
    for (component_id, clock_path) in &deferred_socket_clocks {
        let clock_class = component_analysis
            .resolve(clock_path.as_bstr())
            .filter(|clock| clock.typ.as_ref() == Some(&Type::Clock))
            .and_then(|clock| component_analysis.clock_canonical.get(&clock.id).copied());
        if let Some(class_source) = clock_class
            && let Some((_, component)) = component_analysis
                .components
                .iter_mut()
                .find(|(_, component)| component.id == *component_id)
        {
            component.domain = Some(Domain::sync(class_source));
        }
    }

    // Unique-clock inference: every local component whose domain is
    // still unset after its declaration and any `on` clause — the
    // declared components and the channels of the module's own sockets
    // alike — is clocked by the module's unique clock.  (Components
    // whose declaration fixed `Async`, and `on` clauses resolved above,
    // keep those; Clock-typed components carry their own classes.)
    // Domains left as `None` — the inherited components, and every
    // component of a zero- or multi-clock module — are the clock
    // checker's business.
    if let Some(implicit_clock_id) = component_analysis.implicit_clock() {
        for (_, component) in component_analysis.components.iter_mut() {
            if component.submodule_owned {
                continue;
            }
            if component.typ.as_ref() == Some(&Type::Clock) {
                continue;
            }
            if component.domain.is_none() {
                component.domain = Some(Domain::sync(implicit_clock_id));
            }
        }
    }

    // Flexible zero-clock inference: with no sourced clock class and no
    // `async` component, the module is clock-polymorphic — the
    // component analysis gives it a clock variable `α`, and every
    // still-unset local component is clocked by it (`Sync(α)`), to be
    // substituted per instantiation site.  Registers and memories are
    // excluded: `MissingOnClause` has already diagnosed them, and a
    // flexible module is clock-generic in its combinational structure
    // only.
    let has_async_component = item_ast.children().into_iter().any(|child| {
        matches!(
            child.payload(),
            AstNodePayload::Component(payload) if payload.is_async
        )
    });
    if clock_count == 0 && !has_async_component {
        for (_, component) in component_analysis.components.iter_mut() {
            if component.submodule_owned {
                continue;
            }
            if component.typ.as_ref() == Some(&Type::Clock) {
                continue;
            }
            if matches!(
                component.kind,
                Some(ComponentKind::Reg | ComponentKind::OutgoingReg | ComponentKind::Memory)
            ) {
                continue;
            }
            if component.domain.is_none() {
                component.domain = Some(Domain::sync_var(0));
            }
        }
    }

    // `on <CLOCK>` clauses written on synthesized platform ports and
    // on socketdef channels resolve here, once every component of
    // the module — including the named clocks — is registered: the
    // declared name denotes a sibling Clock-typed port/channel of the
    // same item.  A clause naming none leaves the
    // component's domain unset.
    for (component_id, candidates) in &deferred_ons {
        let clock_class = candidates
            .iter()
            .find_map(|clock_path| {
                component_analysis
                    .resolve(clock_path.as_bstr())
                    .filter(|clock| clock.typ.as_ref() == Some(&Type::Clock))
                    .and_then(|clock| component_analysis.clock_canonical.get(&clock.id).copied())
            });
        if let Some(class_source) = clock_class
            && let Some((_, component)) = component_analysis
                .components
                .iter_mut()
                .find(|(_, component)| component.id == *component_id)
        {
            component.domain = Some(Domain::sync(class_source));
        }
    }

    // Collect references after all components are registered.
    collect_references(&item_ast.children(), &mut component_analysis, None);

    // Flag every expression reference to a platform port synthesized
    // into this module (`UseUndeclaredPlatformPort`).  Synthesis makes
    // the port exist so the module may ignore it; actually reading it
    // (as a value or as a reg's clock) requires declaring it in the mod
    // def.  Driving a synthesized port is reported separately as
    // `DriveUndeclaredPlatformPort` by driver analysis, which does not
    // record the driver, so the two checks do not cascade.
    if let Some(platform_id) = component_analysis.platform {
        let platform_name: BString =
            symboltable.symbol(platform_id).name().to_owned().into();
        for (location, (component_id, kind)) in component_analysis.references.iter() {
            if !matches!(kind, ReferenceKind::Expr) {
                continue;
            }
            let component = component_analysis.component(*component_id).unwrap();
            if !component.synthetic() {
                continue;
            }
            component_analysis.diagnostics.push(Diagnostic::new(
                builder.get_location_region(location.clone()),
                diagnostics::UseUndeclaredPlatformPort {
                    platform: platform_name.clone(),
                    port: component.path(),
                },
            ));
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

// ---------------------------------------------------------------------------
// Clocks
// ---------------------------------------------------------------------------

/// The clock classes of one module view.
#[derive(Debug, Default)]
pub struct ClockClasses {
    /// The canonical source of every sourced class, one entry per
    /// class, in registration order.
    pub sources: Vec<ComponentId>,
    /// Maps every sourced Clock-typed component of the view to its
    /// class's canonical source.  Clock-typed components absent from
    /// the map have a *sourceless* class.
    pub canonical: IndexMap<ComponentId, ComponentId>,
}

/// Computes the module's clock classes.
///
/// A *clock source* is a construct that introduces a fresh clock
/// identity.  Clocks coming from the outside are each distinct sources
/// — they have the *potential* to differ, regardless of how the
/// platform ultimately wires them, so a module's clock structure never
/// depends on its instantiation context:
///
/// 1. each incoming clock port — a declared `incoming` component typed
///    `Clock`, or the receiving-side Clock channel of one of the
///    module's own socket instances — is a source of its own;
/// 2. the clock output port of a submodule or `ext mod` instance (and
///    the driving-side Clock channel of one of the instance's sockets)
///    is a source of its own, unless it declares a clock `dependson`;
/// 3. a call to a clock-generating builtin (none wired through here;
///    the checker mints fresh classes for `asclock` calls).
///
/// The equivalence relation additionally relates a Clock-typed
/// component to the clock its drivers name: a driver whose target is
/// a Clock-typed component and whose driving expression is a plain
/// reference to a Clock-typed thing asserts the two are the same
/// clock.  A guarded driver contributes an equivalence only through
/// `collect_guarded_defs`.  Clock identity is static: it is fixed by
/// the module's structure, never by runtime behavior, and never by
/// instantiation-site usage.
fn compute_clock_classes(
    item_ast: &AstNode<'_>,
    component_analysis: &mut ComponentAnalysis,
) -> ClockClasses {
    let mut classes = ClockClasses::default();

    // Every Clock-typed component of the view, in registration order.
    let clock_ids: Vec<ComponentId> = component_analysis
        .components
        .iter()
        .filter(|(_, component)| component.typ.as_ref() == Some(&Type::Clock))
        .map(|(_, component)| component.id)
        .collect();

    // Union-find over the Clock-typed components.
    let mut parent: IndexMap<ComponentId, ComponentId> =
        clock_ids.iter().map(|&id| (id, id)).collect();

    // The sources: each is the canonical source of its own class.
    let mut is_source: IndexSet<ComponentId> = IndexSet::new();
    for (_, component) in &component_analysis.components {
        if component.typ.as_ref() != Some(&Type::Clock) {
            continue;
        }
        let source = match component.submodule_owned {
            // A local incoming clock port — declared, or the
            // receiving-side channel of one of the module's own socket
            // instances — is a source of its own.  (Synthesized
            // platform ports are excluded: the module cannot reference
            // one without declaring it, so it cannot source anything in
            // this view.)
            false => {
                !component.synthetic
                    && component.kind == Some(ComponentKind::Incoming)
            }
            // The clock output of a submodule or `ext mod` instance —
            // an outgoing port, or the driving-side Clock channel of
            // one of the instance's sockets — is a source of its own.
            true => component.flow == Flow::Source,
        };
        if source {
            is_source.insert(component.id());
        }
    }

    // The definitions union the target with the clock its driving
    // expression names.
    let mut definitions: Vec<(ComponentId, ComponentId)> = vec![];
    collect_clock_definitions(item_ast, component_analysis, &mut definitions);
    for (target, source) in &definitions {
        let root_target = clock_class_root(&parent, *target);
        let root_source = clock_class_root(&parent, *source);
        if root_target != root_source {
            // Keep the earlier-registered component as the root, for
            // determinism.
            if root_target.index <= root_source.index {
                parent.insert(root_source, root_target);
            } else {
                parent.insert(root_target, root_source);
            }
        }
    }

    // The module's unique clock, as a class root: when every source
    // lies in one class, a data component without an `on` clause is
    // clocked by that class.  Guard conditions reference such
    // components, so their classes resolve through this root.
    let implicit_root: Option<ComponentId> = {
        let mut source_roots: IndexSet<ComponentId> = IndexSet::new();
        for &id in &clock_ids {
            if is_source.contains(&id) {
                source_roots.insert(clock_class_root(&parent, id));
            }
        }
        if source_roots.len() == 1 {
            source_roots.into_iter().next()
        } else {
            None
        }
    };

    let parsing = item_ast.parsing();
    let mut guarded_merges: Vec<(ComponentId, ComponentId)> = vec![];
    let mut mixed_conditionals: Vec<(Region, ComponentId, ComponentId)> = vec![];
    let mut top_level: Vec<(ComponentId, ComponentId)> = vec![];
    for stmt in item_ast.children() {
        collect_guarded_defs(
            &stmt,
            parsing,
            component_analysis,
            &parent,
            implicit_root,
            &mut guarded_merges,
            &mut mixed_conditionals,
            &mut top_level,
        );
    }
    for (target, root) in &guarded_merges {
        let root_target = clock_class_root(&parent, *target);
        let root_source = clock_class_root(&parent, *root);
        if root_target != root_source {
            if root_target.index <= root_source.index {
                parent.insert(root_source, root_target);
            } else {
                parent.insert(root_target, root_source);
            }
        }
    }

    // A class's canonical source is its single source.  Classes are
    // precluded from having two sources (a component is driven at most
    // once, and a source is never driven), and a class with no source
    // is a clock name with no definition: its members get no domain,
    // and driver analysis diagnoses the undriven clock wire.
    let mut root_source: IndexMap<ComponentId, ComponentId> = IndexMap::new();
    for &id in &clock_ids {
        if !is_source.contains(&id) {
            continue;
        }
        let root = clock_class_root(&parent, id);
        if root_source.contains_key(&root) {
            continue;
        }
        root_source.insert(root, id);
        classes.sources.push(id);
    }
    for &id in &clock_ids {
        let root = clock_class_root(&parent, id);
        if let Some(&source) = root_source.get(&root) {
            classes.canonical.insert(id, source);
        }
    }

    // A conditional whose guards and clock drivers denote more than
    // one clock class mixes domains: reported here, where the
    // conditional's single-clock-domain rule is decided (the clock
    // checker, which runs against these classes, cannot see it).
    for (region, left, right) in mixed_conditionals {
        let render = |root: ComponentId| -> BString {
            let source = root_source.get(&root).copied().unwrap_or(root);
            format!("clocked by `{}`", component_path(component_analysis, source)).into()
        };
        component_analysis.diagnostics.push(Diagnostic::new(
            region,
            diagnostics::DomainMismatch {
                left: render(left),
                right: render(right),
            },
        ));
    }
    classes
}

/// Collects the module's clock *definitions*: drivers whose target is a
/// Clock-typed component and whose driving expression is a plain
/// reference to a Clock-typed thing, each as `(target, source)`.
/// Everything beneath a statement-position `if`/`match` arm is
/// guarded, and is collected by `collect_guarded_defs` instead, which
/// decides whether the enclosing conditional is single-class.
fn collect_clock_definitions<'p>(
    node: &AstNode<'p>,
    component_analysis: &ComponentAnalysis,
    out: &mut Vec<(ComponentId, ComponentId)>,
) {
    match node.payload() {
        // Every driver beneath a statement-position `when`/`match` arm
        // is guarded.  It is collected by `collect_guarded_defs`,
        // which judges the enclosing conditional as a whole.
        AstNodePayload::If | AstNodePayload::Match => {}
        AstNodePayload::Driver(_) | AstNodePayload::InitialDriver | AstNodePayload::IndexedDriver(_) => {
            if let Some((target, source)) = clock_driver_pair(node, component_analysis) {
                out.push((target.id(), source.id()));
            }
        }
        _ => {
            for child in node.children() {
                collect_clock_definitions(&child, component_analysis, out);
            }
        }
    }
}

/// The root of a Clock-typed component's class in the union-find: the
/// canonical representative used to compare classes while the class
/// relation is still being built.
fn clock_class_root(
    parent: &IndexMap<ComponentId, ComponentId>,
    mut id: ComponentId,
) -> ComponentId {
    while parent[&id] != id {
        id = parent[&id];
    }
    id
}

/// Resolves a driver statement to `(target, source)` when its target
/// is a Clock-typed component and its driving expression is a plain
/// reference to a Clock-typed thing; `None` otherwise.
fn clock_driver_pair(
    node: &AstNode<'_>,
    component_analysis: &ComponentAnalysis,
) -> Option<(Component, Component)> {
    let parsing = node.parsing();
    let it_context = find_it_context(node);
    let resolve = |interned| -> Option<Component> {
        let mut name = parsing.string(interned).to_owned();
        resolve_it_path(
            &mut name,
            it_context.as_ref().map(|context| context.as_bstr()),
        );
        component_analysis.resolve(name.as_bstr())
    };
    let target = node
        .target()
        .and_then(|path| resolve(path))
        .or_else(|| {
            node.driver_target_node()
                .and_then(|target_node| {
                    component_analysis.component_at(target_node.location())
                })
                .cloned()
        });
    let target = target?;
    if target.typ().as_ref() != Some(&Type::Clock) {
        return None;
    }
    let rhs = node.driver()?;
    if !matches!(rhs.payload(), AstNodePayload::ExprReference) {
        return None;
    }
    let rhs_path = rhs.path()?;
    let source = resolve(rhs_path)?;
    if source.typ().as_ref() != Some(&Type::Clock) {
        return None;
    }
    Some((target, source))
}

/// The clock class a component's domain is fixed by: a Clock-typed
/// component denotes its own class; a data component denotes the
/// class of the clock its `on` clause names, or the module's unique
/// clock when it carries no clause.  `None` when no class can be
/// attributed (a multi-clock module with no clause).
fn component_clock_root(
    component: &Component,
    parsing: &Parsing,
    component_analysis: &ComponentAnalysis,
    parent: &IndexMap<ComponentId, ComponentId>,
    implicit_root: Option<ComponentId>,
) -> Option<ComponentId> {
    if component.typ().as_ref() == Some(&Type::Clock) {
        return Some(clock_class_root(parent, component.id()));
    }
    let decl = parsing.ast_node(component.location().ast_node_id());
    decl.on_clause()
        .and_then(|clock_node| clock_node.path())
        .and_then(|interned| component_analysis.resolve(parsing.string(interned)))
        .filter(|clock| clock.typ().as_ref() == Some(&Type::Clock))
        .map(|clock| clock_class_root(parent, clock.id()))
        .or(implicit_root)
}

/// The clock classes a condition expression denotes: the class of
/// every component it references, each resolved through
/// `component_clock_root`.  A condition referencing no clock (a
/// constant) constrains nothing.
fn expr_clock_roots(
    node: &AstNode<'_>,
    parsing: &Parsing,
    component_analysis: &ComponentAnalysis,
    parent: &IndexMap<ComponentId, ComponentId>,
    implicit_root: Option<ComponentId>,
) -> Vec<ComponentId> {
    match node.payload() {
        AstNodePayload::ExprReference => {
            let it_context = find_it_context(node);
            let Some(path) = node.path() else { return vec![] };
            let mut name = parsing.string(path).to_owned();
            resolve_it_path(&mut name, it_context.as_ref().map(|context| context.as_bstr()));
            component_analysis
                .resolve(name.as_bstr())
                .and_then(|component| {
                    component_clock_root(&component, parsing, component_analysis, parent, implicit_root)
                })
                .into_iter()
                .collect()
        }
        _ => node
            .children()
            .into_iter()
            .flat_map(|child| {
                expr_clock_roots(&child, parsing, component_analysis, parent, implicit_root)
            })
            .collect(),
    }
}

/// Collects the clock *merges* implied by guarded drivers, and the
/// conditionals that mix clock classes.
///
/// A driver to a Clock-typed target beneath a statement-position
/// `if`/`match` merges its target into the driving clock's class iff
/// the whole conditional — its guards (`match` subjects) and every
/// clock driver in its arms, an `else` arm's included — denotes a
/// single clock class: the conditional then acts on one clock domain
/// and is equivalent to its drivers written unconditionally.  A
/// conditional whose guards and clock drivers denote more than one
/// class mixes domains: it contributes no merge and is reported
/// `DomainMismatch`.  A conditional whose guards and drivers denote
/// no class at all constrains nothing.
///
/// The traversal follows statements only: driver RHS subtrees are
/// never descended, and `if`/`match` in expression position (a `let`
/// binding's value, a default expression) is skipped, so an
/// expression-position conditional is judged by the expression join
/// rules, not as a statement.
fn collect_guarded_defs<'p>(
    node: &AstNode<'p>,
    parsing: &Parsing,
    component_analysis: &ComponentAnalysis,
    parent: &IndexMap<ComponentId, ComponentId>,
    implicit_root: Option<ComponentId>,
    merges: &mut Vec<(ComponentId, ComponentId)>,
    mixed: &mut Vec<(Region, ComponentId, ComponentId)>,
    level: &mut Vec<(ComponentId, ComponentId)>,
) {
    match node.payload() {
        AstNodePayload::If | AstNodePayload::Match => {
            if node.is_expression_position() {
                return;
            }
            let mut cond_roots: IndexSet<ComponentId> = IndexSet::new();
            let mut arm_pairs: Vec<(ComponentId, ComponentId)> = vec![];
            match node.payload() {
                AstNodePayload::If => {
                    let children = node.children();
                    for (guard, body) in if_arm_children(&children) {
                        if let Some(guard) = guard {
                            cond_roots.extend(expr_clock_roots(
                                guard,
                                parsing,
                                component_analysis,
                                parent,
                                implicit_root,
                            ));
                        }
                        let mut sub_level = vec![];
                        collect_guarded_defs(
                            body,
                            parsing,
                            component_analysis,
                            parent,
                            implicit_root,
                            merges,
                            mixed,
                            &mut sub_level,
                        );
                        arm_pairs.extend(sub_level);
                    }
                }
                AstNodePayload::Match => {
                    if let Some(subject) = node.subject() {
                        cond_roots.extend(expr_clock_roots(
                            &subject,
                            parsing,
                            component_analysis,
                            parent,
                            implicit_root,
                        ));
                    }
                    let children = node.children();
                    for (_pat, body) in match_arm_children(&children) {
                        let mut sub_level = vec![];
                        collect_guarded_defs(
                            body,
                            parsing,
                            component_analysis,
                            parent,
                            implicit_root,
                            merges,
                            mixed,
                            &mut sub_level,
                        );
                        arm_pairs.extend(sub_level);
                    }
                }
                _ => unreachable!(),
            }
            let mut all_roots = cond_roots.clone();
            for (_, root) in &arm_pairs {
                all_roots.insert(*root);
            }
            if cond_roots.len() > 1 {
                // The conditions themselves denote more than one class:
                // the guard join in the clock checker diagnoses the
                // conditional; do not cascade.
                return;
            }
            match all_roots.len() {
                0 => {}
                1 => {
                    let root = *all_roots.iter().next().unwrap();
                    for (target, _) in arm_pairs {
                        merges.push((target, root));
                    }
                }
                _ => {
                    let mut roots = all_roots.iter();
                    let left = *roots.next().unwrap();
                    let right = *roots.next().unwrap();
                    mixed.push((node.region(), left, right));
                }
            }
        }
        AstNodePayload::Driver(_) | AstNodePayload::InitialDriver | AstNodePayload::IndexedDriver(_) => {
            if let Some((target, source)) = clock_driver_pair(node, component_analysis) {
                level.push((target.id(), clock_class_root(parent, source.id())));
            }
        }
        _ => {
            for child in node.children() {
                collect_guarded_defs(
                    &child,
                    parsing,
                    component_analysis,
                    parent,
                    implicit_root,
                    merges,
                    mixed,
                    level,
                );
            }
        }
    }
}

/// Clock provenance: an outgoing Clock-typed port of the module declares whether it
/// *aliases* an incoming clock — `dependson <clock>`, and then exactly
/// one unconditional definition from that clock's class — or
/// *originates* a fresh clock: no `dependson`, and no definition (driver
/// analysis's no-driver rule does not apply to it).  A visible
/// definition rooted at an incoming clock port without the declaration
/// is reported `MissingClockDependson`; a definition rooted at an
/// internal source (e.g. `clkout := pll.clkout`) needs no declaration,
/// and is conservatively treated as a fresh source outside the module.
/// The `dependson` clause of a clock is singleton: a clock that depends
/// on two clocks merges two classes.
fn check_clock_provenance(
    parsing: &Parsing,
    item_ast: &AstNode<'_>,
    component_analysis: &mut ComponentAnalysis,
) {
    for stmt in item_ast.children() {
        let AstNodePayload::Component(payload) = stmt.payload() else {
            continue;
        };
        if !matches!(
            payload.kind,
            ComponentKind::Outgoing | ComponentKind::OutgoingWire
        ) {
            continue;
        }
        let name = parsing.string(payload.name);
        let Some(component) = component_analysis.resolve(name.as_bstr()) else {
            continue;
        };
        if component.typ().as_ref() != Some(&Type::Clock) {
            continue;
        }
        let deps: Vec<Component> = stmt
            .dependson_entries()
            .into_iter()
            .filter_map(|entry| entry.path())
            .filter_map(|interned| component_analysis.resolve(parsing.string(interned)))
            .filter(|dep| dep.typ().as_ref() == Some(&Type::Clock))
            .collect();
        if deps.len() > 1 {
            // A clock that depends on two clocks merges two classes.
            component_analysis.diagnostics.push(Diagnostic::new(
                stmt.region(),
                diagnostics::DomainMismatch {
                    left: render_clock_class(component_analysis, &deps[0]),
                    right: render_clock_class(component_analysis, &deps[1]),
                },
            ));
            continue;
        }
        let declared = deps.first().map(|dep| dep.id());
        let class_source = component_analysis.clock_canonical.get(&component.id).copied();
        match (declared, class_source) {
            (Some(dep), Some(source)) => {
                // A declared alias must be defined from the declared
                // clock's class.
                if source != dep {
                    component_analysis.diagnostics.push(Diagnostic::new(
                        stmt.region(),
                        diagnostics::MissingClockDependson {
                            component: name.to_owned(),
                            clock: component_path(component_analysis, source),
                        },
                    ));
                }
            }
            (None, Some(source)) => {
                // An undeclared alias of an incoming clock port must
                // declare its provenance.  A definition rooted at an
                // internal source (an instance's clock output) needs no
                // declaration.
                let is_incoming = component_analysis
                    .component(source)
                    .map(|source| {
                        !source.submodule_owned
                            && source.kind() == Some(ComponentKind::Incoming)
                    })
                    .unwrap_or(false);
                if is_incoming {
                    component_analysis.diagnostics.push(Diagnostic::new(
                        stmt.region(),
                        diagnostics::MissingClockDependson {
                            component: name.to_owned(),
                            clock: component_path(component_analysis, source),
                        },
                    ));
                }
            }
            _ => {}
        }
    }
}

/// The path of a component, or `<?clock?>` when it cannot be resolved.
fn component_path(component_analysis: &ComponentAnalysis, id: ComponentId) -> BString {
    component_analysis
        .component(id)
        .map(|component| component.path())
        .unwrap_or_else(|| BString::from("<?clock?>"))
}

/// Renders a Clock-typed component's class for a diagnostic.
fn render_clock_class(component_analysis: &ComponentAnalysis, component: &Component) -> BString {
    match component_analysis.clock_canonical.get(&component.id) {
        Some(source) => {
            let path = component_path(component_analysis, *source);
            format!("clocked by `{}`", path).into()
        }
        None => BString::from("<?clock?>"),
    }
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
