use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::Location;
use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId, match_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::common::{ChannelDir, ComponentKind, Flow, SocketRole};
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
    let packages = builder.get_packages();
    for import in package_analysis.imports() {
        if import == location.package() {
            continue;
        }
        if packages.contains(&import) {
            builder.get_package_analysis(import);
        }
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
                };
                let component_kind = component.kind;
                let component = Component {
                    id,
                    path: path.clone(),
                    location: stmt.location(),
                    typ,
                    flow,
                    kind: Some(component_kind),
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
                    None => {
                        component_analysis.diagnostics.push(
                            diagnostics::UnresolvedItem {
                                region: ofness_node.region(),
                                item: submodule_name.to_owned(),
                            }.into()
                        );
                        continue;
                    }
                };
                let submodule_location = submodule_symbol.location();
                let submodule_parsing = builder.get_parsing(submodule_location.package());
                let submodule_ast = submodule_parsing.ast_node(submodule_location.ast_node_id());
                for submodule_stmt in submodule_ast.children() {
                    match submodule_stmt.payload() {
                        AstNodePayload::Component(component) => {
                            if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing | ComponentKind::OutgoingWire | ComponentKind::OutgoingReg) {
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
                            let typ = match builder.get_type_at(typ_node.location()) {
                                Ok(typ) => Some(typ),
                                Err(_) => None,
                            };
                            let flow = match component.kind {
                                ComponentKind::Incoming => Flow::Sink,
                                ComponentKind::Outgoing => Flow::Source,
                                ComponentKind::OutgoingWire => Flow::Source,
                                ComponentKind::OutgoingReg => Flow::Source,
                                ComponentKind::Reg => Flow::Duplex,
                                ComponentKind::Wire => Flow::Duplex,
                            };
                            let component_kind = component.kind;
                            let component = Component {
                                id,
                                path: path.clone(),
                                location: stmt.location(),
                                typ,
                                flow,
                                kind: Some(component_kind),
                            };
                            if !components_seen.contains(&path) {
                                components_seen.insert(path.clone());
                                component_analysis.components.push((path, component))
                            }
                        }
                        AstNodePayload::Socket(submodule_socket) => {
                            // Handle sockets in submodules - treat each channel as a component
                            let socket_instance_name = submodule_parsing.string(submodule_socket.name);
                            let socket_ofness_node = submodule_stmt.child(0);
                            let AstNodePayload::Ofness(socket_ofness) = socket_ofness_node.payload() else {
                                continue;
                            };
                            let socket_package = socket_ofness
                                .package
                                .map(|pkg| PackageFqn::new(bstr::BString::from(submodule_parsing.string(pkg).to_vec())))
                                .unwrap_or_else(|| submodule_location.package());
                            let socket_name = submodule_parsing.string(socket_ofness.name);
                            let socket_symbol = match symboltable.resolve_item_in_package(socket_name, socket_package) {
                                Some(symbol) => symbol.clone(),
                                None => continue,
                            };

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
                                let typ = match builder.get_type_at(typ_node.location()) {
                                    Ok(typ) => Some(typ),
                                    Err(_) => None,
                                };
                                let flow = match (submodule_socket.role, channel.dir) {
                                    (SocketRole::Client, ChannelDir::Cosi) => Flow::Source,
                                    (SocketRole::Client, ChannelDir::Soci) => Flow::Sink,
                                    (SocketRole::Server, ChannelDir::Cosi) => Flow::Sink,
                                    (SocketRole::Server, ChannelDir::Soci) => Flow::Source,
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
                let ofness_node = stmt.child(0);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let socket_package = ofness
                    .package
                    .map(|pkg| PackageFqn::new(bstr::BString::from(parsing.string(pkg).to_vec())))
                    .unwrap_or_else(|| location.package());
                let socket_name = parsing.string(ofness.name);
                let socket_symbol = match symboltable.resolve_item_in_package(socket_name, socket_package) {
                    Some(symbol) => symbol.clone(),
                    None => continue,
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
                    let typ = match builder.get_type_at(typ_node.location()) {
                        Ok(typ) => Some(typ),
                        Err(_) => None,
                    };
                    let flow = match (socket.role, channel.dir) {
                        (SocketRole::Client, ChannelDir::Cosi) => Flow::Sink,
                        (SocketRole::Client, ChannelDir::Soci) => Flow::Source,
                        (SocketRole::Server, ChannelDir::Cosi) => Flow::Source,
                        (SocketRole::Server, ChannelDir::Soci) => Flow::Sink,
                    };
                    let component = Component {
                        id,
                        path: path.clone(),
                        location: stmt.location(),
                        typ,
                        flow,
                        kind: None,
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
            AstNodePayload::ModDefStmtMatch | AstNodePayload::ExprMatch => {
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
                            crate::diagnostics::ItNotInItBlock {
                                region: node.region(),
                            }.into(),
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
                if children.len() == 2 {
                    let it_block = &children[1];
                    if matches!(it_block.payload(), AstNodePayload::It) {
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
            AstNodePayload::ModDefStmtIf => {
                let children = stmt.children();
                let has_else = children.len() % 2 == 1;
                let num_pairs = children.len() / 2;
                for i in 0..num_pairs {
                    let condition = &children[2 * i];
                    // Walk references in the if/else-if condition expression.
                    collect_references_in_expr(condition, component_analysis, it_context);
                    let block = &children[2 * i + 1];
                    collect_references(
                        &block.children(),
                        component_analysis,
                        it_context,
                    );
                }
                if has_else {
                    let else_block = children.last().unwrap();
                    collect_references(
                        &else_block.children(),
                        component_analysis,
                        it_context,
                    );
                }
            }
            AstNodePayload::ModDefStmtMatch => {
                let children = stmt.children();
                // Walk references in the match subject expression (first child).
                if let Some(subject) = children.first() {
                    collect_references_in_expr(subject, component_analysis, it_context);
                }
                for (_pat_opt, body) in match_arm_children(&children) {
                    collect_references(
                        &body.children(),
                        component_analysis,
                        it_context,
                    );
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
