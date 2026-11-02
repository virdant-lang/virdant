//! The `dependson` analysis: verifies each module's declared
//! combinational-dependency contract.
//!
//! `dependson` is a list on an `outgoing`/`outgoing wire` component
//! declaration (and on socketdef channels): the module's declaration of
//! which `incoming` ports the port *may* combinationally depend on.  It
//! is part of the module's public interface, so parents analyze
//! children through declarations only, never by looking inside them.
//!
//! Core semantics:
//!
//! * `dependson` does not *create* a combinational dependency; it
//!   declares *potential*.
//! * The declared set must be exhaustive: every `incoming` port the
//!   port can combinationally depend on (through local drivers, local
//!   declared edges, socket obligations, and children's declared
//!   edges) must be declared. Undeclared is an error.
//! * Over-declaration is a warning ("unused"), suppressed for `ext mod`
//!   (whose internals are invisible, so declarations cannot be
//!   justified locally) and for socket-derived obligations.
//! * The combinational cycle check (see
//!   `queries::combinational_cycles`) runs over the *potentiality*
//!   graph: local actual edges + local declared edges + edges imported
//!   from children's declarations.

use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::component::ComponentAnalysis;
use crate::analysis::dependency::EdgeKind;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolKind};
use crate::common::source::Region;
use crate::common::{ChannelDir, ComponentKind, SocketRole};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::package::PackageId;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;

/// The `dependson` contract of a single module.
#[derive(Debug)]
pub struct DependsonAnalysis {
    moddef: SymbolId,

    /// Local declared edges from the module's own `dependson` lists
    /// on `outgoing`/`outgoing wire` components:
    /// `(source path, dependee path)`, both module-local. Only
    /// validated edges are included.
    declared_edges: Vec<(BString, BString)>,

    /// Local socket-obligation edges: for each socket instance in the
    /// module instantiated in its driving role (`client` for `cosi`
    /// channels, `server` for `soci` channels), the channels with
    /// declared lists yield `(drive path, read path)` edges. See the
    /// module-level docs for why these obligations never produce a
    /// unused warning.
    socket_edges: Vec<(BString, BString)>,

    /// Edges imported from immediate submodule instances: the child's
    /// `declared_edges` and `socket_edges`, with every path prefixed by
    /// the instance name. These are *not* module-local; the parent
    /// trusts the child's declarations without looking inside it.
    imported_edges: Vec<(BString, BString)>,

    diagnostics: Vec<Diagnostic>,
}

impl DependsonAnalysis {
    pub fn moddef_symbol_id(&self) -> SymbolId {
        self.moddef
    }

    /// The module's own declared edges (validated).
    pub fn declared_edges(&self) -> &[(BString, BString)] {
        &self.declared_edges
    }

    /// The module's own socket-obligation edges (validated).
    pub fn socket_edges(&self) -> &[(BString, BString)] {
        &self.socket_edges
    }

    /// Edges imported from immediate submodule instances.
    pub fn imported_edges(&self) -> &[(BString, BString)] {
        &self.imported_edges
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}

pub(crate) fn build_dependson_analysis(
    builder: &mut Builder,
    moddef: SymbolId,
) -> Arc<DependsonAnalysis> {
    let symboltable = builder.get_symboltable();
    if symboltable.symbol(moddef).kind() != SymbolKind::ModDef {
        return Arc::new(DependsonAnalysis {
            moddef,
            declared_edges: vec![],
            socket_edges: vec![],
            imported_edges: vec![],
            diagnostics: vec![],
        });
    }

    let component_analysis = builder.get_component_analysis(moddef);
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let is_ext = matches!(item_ast.payload(), AstNodePayload::ModDef(moddef) if moddef.is_ext);

    let mut analysis = DependsonAnalysis {
        moddef,
        declared_edges: vec![],
        socket_edges: vec![],
        imported_edges: vec![],
        diagnostics: vec![],
    };

    // The module's own components, by path (no instance prefixes).
    let own_components: Vec<(BString, crate::analysis::component::Component)> =
        component_analysis
            .components()
            .into_iter()
            .filter(|(path, _)| !path.contains(&b'.'))
            .collect();

    let own_incoming_paths: Vec<BString> = own_components
        .iter()
        .filter(|(_path, component)| {
            component.kind() == Some(ComponentKind::Incoming)
        })
        .map(|(path, _)| path.clone())
        .collect();

    // 1. Local declared edges from `dependson` lists on the module's
    //    own components.
    for stmt in item_ast.children() {
        let AstNodePayload::Component(component_payload) = stmt.payload() else {
            continue;
        };
        let carrier_name: BString = parsing.string(component_payload.name).to_owned();
        if component_analysis.resolve(carrier_name.as_bstr()).is_none() {
            // Duplicate or unregistered declaration: diagnosed elsewhere.
            continue;
        };
        collect_declared_edges_for_component(
            &component_analysis,
            &own_incoming_paths,
            &stmt,
            carrier_name.as_bstr(),
            &mut analysis,
        );
    }

    // 2. Local socket-obligation edges for this module's own socket
    //    instances.
    collect_socket_obligations_for(
        builder,
        &item_ast,
        location.package(),
        &component_analysis,
        &mut analysis.socket_edges,
        None,
        None,
    );

    // 3. Imported declared edges from immediate submodule instances.
    collect_imported_edges(builder, moddef, &component_analysis, &mut analysis);

    // 4. Exhaustiveness and unused entries over the potentiality graph.
    check_exhaustiveness_and_unused(
        builder,
        &component_analysis,
        &own_components,
        &own_incoming_paths,
        is_ext,
        &mut analysis,
    );

    Arc::new(analysis)
}

// ---------------------------------------------------------------------------
// Local declared edges
// ---------------------------------------------------------------------------

/// Validate one component's `dependson` list and record its declared
/// edges.
///
/// Carrier-kind and entry-placement rules (the plan's diagnostics
/// table): only `outgoing` and `outgoing wire` may carry the list;
/// entries must resolve to the module's own `incoming` ports.  The
/// grammar accepts the list on any component kind so that these
/// diagnostics can be precise.
fn collect_declared_edges_for_component(
    component_analysis: &ComponentAnalysis,
    own_incoming_paths: &[BString],
    stmt: &AstNode<'_>,
    carrier_path: &BStr,
    analysis: &mut DependsonAnalysis,
) {
    let Some(carrier) = component_analysis.resolve(carrier_path) else {
        return;
    };
    let carrier_kind = carrier.kind();

    // Carrier-kind check.
    match carrier_kind {
        Some(ComponentKind::Outgoing) | Some(ComponentKind::OutgoingWire) => {}
        Some(ComponentKind::OutgoingReg) => {
            if let Some(list) = stmt.dependson_list() {
                analysis.diagnostics.push(Diagnostic::new(
                    list.region(),
                    diagnostics::DependsonBadCarrier {
                        component: carrier_path.to_owned(),
                        reason: "a registered output (`outgoing reg`) cannot change in the same cycle".into(),
                    },
                ));
            }
            return;
        }
        Some(ComponentKind::Incoming) => {
            if let Some(list) = stmt.dependson_list() {
                analysis.diagnostics.push(Diagnostic::new(
                    list.region(),
                    diagnostics::DependsonBadCarrier {
                        component: carrier_path.to_owned(),
                        reason: "`incoming` ports may not declare `dependson`".into(),
                    },
                ));
            }
            return;
        }
        Some(ComponentKind::Bus) => {
            if let Some(list) = stmt.dependson_list() {
                analysis.diagnostics.push(Diagnostic::new(
                    list.region(),
                    diagnostics::DependsonBadCarrier {
                        component: carrier_path.to_owned(),
                        reason: "`bus` is tristate; `dependson` on tristate components is not yet supported".into(),
                    },
                ));
            }
            return;
        }
        Some(ComponentKind::Net) | Some(ComponentKind::Wire) | Some(ComponentKind::Reg) => {
            if let Some(list) = stmt.dependson_list() {
                analysis.diagnostics.push(Diagnostic::new(
                    list.region(),
                    diagnostics::DependsonBadCarrier {
                        component: carrier_path.to_owned(),
                        reason: "internal signals may not declare `dependson`; only `outgoing` and `outgoing wire` may".into(),
                    },
                ));
            }
            return;
        }
        None => return,
    }

    if stmt.dependson_list().is_none() {
        return;
    }
    for entry_node in stmt.dependson_entries() {
        let Some(entry_path) = entry_node.path() else { continue };
        let entry: BString = stmt.parsing.string(entry_path).to_owned();
        let reason = entry_is_legal(
            component_analysis,
            own_incoming_paths,
            entry.as_bstr(),
        );
        match reason {
            None => {
                analysis
                    .declared_edges
                    .push((carrier_path.to_owned(), entry.clone()));
            }
            Some(reason) => {
                analysis.diagnostics.push(Diagnostic::new(
                    entry_node.region(),
                    diagnostics::DependsonBadEntry {
                        entry: entry.clone(),
                        reason,
                    },
                ));
            }
        }
    }
}

/// Why `entry` is not a legal `dependson` entry, or `None` if it is.
fn entry_is_legal(
    component_analysis: &ComponentAnalysis,
    own_incoming_paths: &[BString],
    entry: &BStr,
) -> Option<BString> {
    let Some(component) = component_analysis.resolve(entry) else {
        return Some("could not be resolved to a component of this module".into());
    };
    if component.kind() != Some(ComponentKind::Incoming) {
        return Some("is not an `incoming` port of this module".into());
    }
    if !own_incoming_paths.iter().any(|path| path == entry) {
        // An `incoming`-kind component of another module (e.g. a
        // submodule instance's socket channel) is not *this* module's
        // incoming port.
        return Some("is not an `incoming` port of this module".into());
    }
    None
}

// ---------------------------------------------------------------------------
// Socket obligations
// ---------------------------------------------------------------------------

/// One channel declaration of a socketdef, with its raw `dependson`
/// entries (unvalidated channel names).
#[derive(Debug, Clone)]
pub struct SocketChannelDecl {
    pub dir: ChannelDir,
    pub name: BString,
    pub entries: Vec<BString>,
}

/// Read a socketdef's channel declarations: `(dir, name, raw entries)`.
pub(crate) fn socketdef_channel_decls(
    builder: &mut Builder,
    socketdef: &Symbol,
) -> Vec<SocketChannelDecl> {
    let location = socketdef.location();
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    let mut decls = vec![];
    for stmt in node.children() {
        let AstNodePayload::Channel(channel) = stmt.payload() else {
            continue;
        };
        let name: BString = parsing.string(channel.name).to_owned();
        let entries: Vec<BString> = stmt
            .dependson_entries()
            .iter()
            .filter_map(|entry| entry.path().map(|path| parsing.string(path).to_owned()))
            .collect();
        decls.push(SocketChannelDecl {
            dir: channel.dir,
            name,
            entries,
        });
    }
    decls
}

/// Record socket-obligation edges for every socket instance declared
/// at the top level of `ast` (the module's own namespace).
///
/// A socketdef channel's `dependson` list applies when the socket is
/// instantiated in the channel's driving role: `client` for `cosi`
/// channels, `server` for `soci` channels.  The obligation is on the
/// *containing* module: its drive on the channel may depend on its
/// reads of the declared opposite-direction channels.  In the
/// receiving role the list is inert.
///
/// `prefix` (for imports from a submodule instance) is prepended to
/// every path; `verify` (the parent's component analysis) is used to
/// check that the prefixed paths exist there. Unresolvable paths are
/// skipped: bad socketdef entries are diagnosed by
/// `check_socketdefs`.
fn collect_socket_obligations_for(
    builder: &mut Builder,
    ast: &AstNode<'_>,
    package: PackageId,
    component_analysis: &ComponentAnalysis,
    out: &mut Vec<(BString, BString)>,
    prefix: Option<&BStr>,
    verify: Option<&ComponentAnalysis>,
) {
    let symboltable = builder.get_symboltable();
    let parsing = builder.get_parsing(package);
    let packages = builder.get_packages();

    for stmt in ast.children() {
        let AstNodePayload::Socket(socket) = stmt.payload() else {
            continue;
        };
        let instance_name: BString = parsing.string(socket.name).to_owned();
        let ofness_node = stmt.child(1);
        let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
            continue;
        };
        let socket_package: Option<PackageId> = match ofness.package {
            Some(pkg) => packages.id(parsing.string(pkg)),
            None => Some(package),
        };
        let socket_name = parsing.string(ofness.name);
        let Some(socket_symbol) =
            socket_package.and_then(|pkg| symboltable.resolve_item(socket_name, pkg))
        else {
            continue;
        };
        if socket_symbol.kind() != SymbolKind::SocketDef {
            continue;
        }
        for decl in socketdef_channel_decls(builder, &socket_symbol) {
            let drives = drives_channel(socket.role, decl.dir);
            if !drives || decl.entries.is_empty() {
                continue;
            }
            for entry in &decl.entries {
                let mut source = BString::from(format!(
                    "{}.{}",
                    instance_name.to_str_lossy(),
                    decl.name.to_str_lossy()
                ));
                let mut target = BString::from(format!(
                    "{}.{}",
                    instance_name.to_str_lossy(),
                    entry.to_str_lossy()
                ));
                if component_analysis.resolve(source.as_bstr()).is_none()
                    || component_analysis.resolve(target.as_bstr()).is_none()
                {
                    continue;
                }
                if let Some(prefix) = prefix {
                    source = BString::from(format!(
                        "{}.{}",
                        prefix.to_str_lossy(),
                        source.to_str_lossy()
                    ));
                    target = BString::from(format!(
                        "{}.{}",
                        prefix.to_str_lossy(),
                        target.to_str_lossy()
                    ));
                    if let Some(verify) = verify {
                        if verify.resolve(source.as_bstr()).is_none()
                            || verify.resolve(target.as_bstr()).is_none()
                        {
                            continue;
                        }
                    }
                }
                out.push((source, target));
            }
        }
    }
}

fn drives_channel(role: SocketRole, dir: ChannelDir) -> bool {
    matches!(
        (role, dir),
        (SocketRole::Client, ChannelDir::Cosi) | (SocketRole::Server, ChannelDir::Soci)
    )
}

// ---------------------------------------------------------------------------
// Imported edges from submodule instances
// ---------------------------------------------------------------------------

/// Record edges imported from the module's immediate submodule
/// instances: each child's own declared edges and socket obligations,
/// prefixed with the instance name.  The parent never looks inside the
/// child: the child's declarations *are* its boundary contract.
fn collect_imported_edges(
    builder: &mut Builder,
    moddef: SymbolId,
    component_analysis: &ComponentAnalysis,
    analysis: &mut DependsonAnalysis,
) {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let packages = builder.get_packages();

    for stmt in item_ast.children() {
        let AstNodePayload::Submodule(submodule) = stmt.payload() else {
            continue;
        };
        let instance_name: BString = parsing.string(submodule.name).to_owned();
        let ofness_node = stmt.child(1);
        let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
            continue;
        };
        let submodule_package: Option<PackageId> = match ofness.package {
            Some(pkg) => packages.id(parsing.string(pkg)),
            None => Some(location.package()),
        };
        let submodule_name = parsing.string(ofness.name);
        let Some(child_symbol) =
            submodule_package.and_then(|pkg| symboltable.resolve_item(submodule_name, pkg))
        else {
            continue;
        };
        if child_symbol.kind() != SymbolKind::ModDef {
            continue;
        }

        // Import the child's *own* declarations by walking the child's
        // AST directly, not by querying the child's full analysis:
        // with an instantiation cycle, the child's analysis would
        // recurse back into this module and overflow the stack (the
        // instantiation cycle itself is diagnosed elsewhere).
        let child_location = child_symbol.location();
        let child_parsing = builder.get_parsing(child_location.package());
        let child_ast = child_parsing.ast_node(child_location.ast_node_id());
        let child_component_analysis = builder.get_component_analysis(child_symbol.id());
        let child_incoming_paths: Vec<BString> = child_component_analysis
            .components()
            .into_iter()
            .filter(|(path, component)| {
                !path.contains(&b'.')
                    && component.kind() == Some(ComponentKind::Incoming)
            })
            .map(|(path, _)| path.clone())
            .collect();

        // The child's own `dependson` lists on its outgoing and
        // outgoing-wire components.
        for child_stmt in child_ast.children() {
            let AstNodePayload::Component(child_component) = child_stmt.payload() else {
                continue;
            };
            let carrier: BString = child_parsing.string(child_component.name).to_owned();
            let Some(carrier_component) =
                child_component_analysis.resolve(carrier.as_bstr())
            else {
                continue;
            };
            if !matches!(
                carrier_component.kind(),
                Some(ComponentKind::Outgoing) | Some(ComponentKind::OutgoingWire)
            ) {
                continue;
            }
            for entry_node in child_stmt.dependson_entries() {
                let Some(entry_path) = entry_node.path() else { continue };
                let entry: BString = child_parsing.string(entry_path).to_owned();
                if entry_is_legal(
                    &child_component_analysis,
                    &child_incoming_paths,
                    entry.as_bstr(),
                )
                .is_some()
                {
                    // Invalid in the child; the child's own analysis
                    // diagnoses it.
                    continue;
                }
                let source_full = BString::from(format!(
                    "{}.{}",
                    instance_name.to_str_lossy(),
                    carrier.to_str_lossy()
                ));
                let target_full = BString::from(format!(
                    "{}.{}",
                    instance_name.to_str_lossy(),
                    entry.to_str_lossy()
                ));
                if component_analysis.resolve(source_full.as_bstr()).is_some()
                    && component_analysis.resolve(target_full.as_bstr()).is_some()
                {
                    analysis.imported_edges.push((source_full, target_full));
                }
            }
        }

        // The child's own socket obligations.
        collect_socket_obligations_for(
            builder,
            &child_ast,
            child_location.package(),
            &child_component_analysis,
            &mut analysis.imported_edges,
            Some(instance_name.as_bstr()),
            Some(component_analysis),
        );
    }
}

// ---------------------------------------------------------------------------
// Exhaustiveness and unused entries
// ---------------------------------------------------------------------------

/// Check every `outgoing`/`outgoing wire` component of the module
/// against its declared set:
///
/// * *Exhaustiveness (error)*: every own `incoming` port reachable
///   from the port over the potentiality graph (local actual edges,
///   local declared edges, socket obligations, imported declared
///   edges) must be declared.
/// * *Unused (warning)*: a declared entry that is not justified by
///   reachability over local actual edges, imported declared edges,
///   and declared edges of components *other than* the port itself.
///   Suppressed entirely for `ext mod`, whose internals are invisible:
///   the declaration is the only statement of what the port may
///   depend on.
fn check_exhaustiveness_and_unused(
    builder: &mut Builder,
    component_analysis: &ComponentAnalysis,
    own_components: &[(BString, crate::analysis::component::Component)],
    own_incoming_paths: &[BString],
    is_ext: bool,
    analysis: &mut DependsonAnalysis,
) {
    // The potentiality graph, over module-local paths, plus the
    // actual-only subgraph (needed to tell an edge that is *both*
    // actual and declared apart from one that is declared only).
    let mut adj: IndexMap<BString, IndexSet<BString>> = IndexMap::new();
    let mut actual_adj: IndexMap<BString, IndexSet<BString>> = IndexMap::new();
    let add = |adj: &mut IndexMap<BString, IndexSet<BString>>, source: &BString, target: &BString| {
        adj.entry(source.clone())
            .or_default()
            .insert(target.clone());
    };

    // Local actual edges (combinational only; sequential edges break
    // potentiality).  Includes socket-link edges from `:=:` drivers.
    let dependency_graph = builder.get_dependency_graph(analysis.moddef);
    for (source, edge_list) in dependency_graph.edges() {
        let Some(source_component) = component_analysis.component(*source) else {
            continue;
        };
        for edge in edge_list {
            if edge.kind != EdgeKind::Combinational {
                continue;
            }
            let Some(dependee_component) = component_analysis.component(edge.dependee) else {
                continue;
            };
            add(
                &mut adj,
                &source_component.path(),
                &dependee_component.path(),
            );
            add(
                &mut actual_adj,
                &source_component.path(),
                &dependee_component.path(),
            );
        }
    }

    // Local declared edges, socket obligations, and imported declared
    // edges.
    for (source, target) in analysis
        .declared_edges
        .iter()
        .chain(analysis.socket_edges.iter())
        .chain(analysis.imported_edges.iter())
    {
        add(&mut adj, source, target);
    }

    // Declared entries per outgoing carrier.
    let mut declared_by_port: IndexMap<BString, Vec<BString>> = IndexMap::new();
    for (source, target) in &analysis.declared_edges {
        declared_by_port
            .entry(source.clone())
            .or_default()
            .push(target.clone());
    }

    for (path, component) in own_components {
        if !matches!(
            component.kind(),
            Some(ComponentKind::Outgoing) | Some(ComponentKind::OutgoingWire)
        ) {
            continue;
        }

        // Exhaustiveness: reach over the full potentiality graph
        // (including the port's own declared edges).
        let reachable = reachable_from(&adj, path.as_bstr());
        let declared_set: IndexSet<BString> = declared_by_port
            .get(path)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect();
        for incoming in own_incoming_paths {
            if reachable.contains(incoming) && !declared_set.contains(incoming) {
                let region = builder.get_location_region(component.location());
                analysis.diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::DependsonUndeclared {
                        port: path.clone(),
                        missing: incoming.clone(),
                    },
                ));
            }
        }

        // Unused: justified = reach over local actual edges,
        // imported declared edges, and declared edges of components
        // other than the port itself.  The port's own declared edges
        // cannot justify its own entries, but its *actual* edges still
        // participate.
        if is_ext || declared_set.is_empty() {
            continue;
        }
        let mut adj_without_self = adj.clone();
        if let Some(targets) = adj_without_self.get_mut(path) {
            let actual_targets = actual_adj
                .get(path)
                .cloned()
                .unwrap_or_default();
            for target in declared_by_port.get(path).into_iter().flatten() {
                // Only *declared-only* edges are excluded: an edge that
                // is also an actual dependency justifies itself.
                if !actual_targets.contains(target) {
                    targets.shift_remove(target);
                }
            }
        }
        let justified = reachable_from(&adj_without_self, path.as_bstr());
        for entry in declared_by_port.get(path).into_iter().flatten() {
            if !justified.contains(entry) {
                // The entry path's region, for the diagnostic.
                let region =
                    declared_entry_region(builder, analysis.moddef, path.as_bstr(), entry.as_bstr());
                analysis.diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::DependsonUnused {
                        port: path.clone(),
                        entry: entry.clone(),
                    },
                ));
            }
        }
    }
}

/// All paths reachable from `source` (excluding `source` itself unless
/// it lies on a cycle).
fn reachable_from(
    adj: &IndexMap<BString, IndexSet<BString>>,
    source: &BStr,
) -> IndexSet<BString> {
    let mut visited: IndexSet<BString> = IndexSet::new();
    let mut stack: Vec<&BString> = vec![];
    if let Some(targets) = adj.get(source) {
        for target in targets {
            if visited.insert(target.clone()) {
                stack.push(target);
            }
        }
    }
    while let Some(current) = stack.pop() {
        if let Some(targets) = adj.get(current.as_bstr()) {
            for target in targets {
                if visited.insert(target.clone()) {
                    stack.push(target);
                }
            }
        }
    }
    visited
}

/// The source region of `entry` as written in `port`'s `dependson`
/// list, or the moddef's region as a fallback.
fn declared_entry_region(
    builder: &mut Builder,
    moddef: SymbolId,
    port: &BStr,
    entry: &BStr,
) -> Region {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    for stmt in item_ast.children() {
        let AstNodePayload::Component(component_payload) = stmt.payload() else {
            continue;
        };
        let name: BString = parsing.string(component_payload.name).to_owned();
        if name != port {
            continue;
        }
        for entry_node in stmt.dependson_entries() {
            if let Some(path) = entry_node.path() {
                if parsing.string(path) == entry {
                    return entry_node.region();
                }
            }
        }
    }
    item_ast.region()
}

// ---------------------------------------------------------------------------
// Socketdef checks
// ---------------------------------------------------------------------------

/// Check the `dependson` lists of every socketdef's channels: an
/// entry must be a channel of the *same* socketdef with the opposite
/// direction word (`cosi` entries for `soci` channels and vice versa).
pub(crate) fn check_socketdefs(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind() != SymbolKind::SocketDef {
            continue;
        }
        let decls = socketdef_channel_decls(builder, &item);
        for decl in &decls {
            if decl.entries.is_empty() {
                continue;
            }
            for entry in &decl.entries {
                let Some(target) = decls
                    .iter()
                    .find(|other| other.name == *entry)
                else {
                    diagnostics.push(Diagnostic::new(
                        socketdef_entry_region(builder, &item, decl.name.as_bstr(), entry.as_bstr()),
                        diagnostics::DependsonBadEntry {
                            entry: entry.clone(),
                            reason: format!(
                                "is not a channel of the same socket; a `{}` channel may only `dependson` channels of the opposite direction word",
                                if decl.dir == ChannelDir::Cosi { "cosi" } else { "soci" },
                            ).into(),
                        },
                    ));
                    continue;
                };
                if target.dir == decl.dir {
                    diagnostics.push(Diagnostic::new(
                        socketdef_entry_region(builder, &item, decl.name.as_bstr(), entry.as_bstr()),
                        diagnostics::DependsonBadEntry {
                            entry: entry.clone(),
                            reason: format!(
                                "a `{}` channel may only `dependson` channels of the opposite direction word",
                                if decl.dir == ChannelDir::Cosi { "cosi" } else { "soci" },
                            ).into(),
                        },
                    ));
                }
            }
        }
    }
}

/// The source region of one entry of a socketdef channel's `dependson`
/// list, or the socketdef's region as a fallback.
fn socketdef_entry_region(
    builder: &mut Builder,
    socketdef: &Symbol,
    channel: &BStr,
    entry: &BStr,
) -> Region {
    let location = socketdef.location();
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    for stmt in node.children() {
        let AstNodePayload::Channel(channel_payload) = stmt.payload() else {
            continue;
        };
        let name: BString = parsing.string(channel_payload.name).to_owned();
        if name != channel {
            continue;
        }
        for entry_node in stmt.dependson_entries() {
            if let Some(path) = entry_node.path() {
                if parsing.string(path) == entry {
                    return entry_node.region();
                }
            }
        }
    }
    node.region()
}
