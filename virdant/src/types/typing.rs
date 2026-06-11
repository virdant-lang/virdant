mod check;
mod infer;

use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::symbols::{Symbol, SymbolId};
use crate::common::WordValue;
use crate::common::{BinOp, Flow, UnOp as UnOp, Width};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticLevel};
use crate::analysis::location::Location;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId, match_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::{self, AstNodePayload};
use crate::types::context::Referent;

use super::context::TypingContext;
use super::typ::Type;

#[derive(Debug)]
pub struct Typing {
    item: Symbol,
    exprroot: ExprRoot,
    typs: IndexMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
    use_locations: IndexMap<BString, Vec<Location>>, // TODO should be use for Referents
    tags: IndexMap<Location, Tag>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}

impl ExprRoot {
    pub fn new(location: Location) -> ExprRoot {
        ExprRoot { location }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn package(&self) -> PackageFqn {
        self.location.package()
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Any,
    All,
    Cast,
    Word,
    Sext,
    Zext,
    Trunc,
    Mux,
}


#[derive(Debug, Clone)]
pub enum Tag {
    None,
    SymbolResolution(SymbolId),
    PrimitiveResolution(Primitive),
    ReferentResolution(Referent),
}

impl Tag {
    pub fn symbol_id(&self) -> Option<SymbolId> {
        if let Tag::SymbolResolution(symbol_id) = self {
            Some(*symbol_id)
        } else {
            None
        }
    }

    pub fn primitive(&self) -> Option<Primitive> {
        if let Tag::PrimitiveResolution(primitive) = self {
            Some(primitive.clone())
        } else {
            None
        }
    }
}

impl Typing {
    pub fn item(&self) -> &Symbol {
        &self.item
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn tag(&self, location: Location) -> Tag {
        if let Some(tag) = self.tags.get(&location).cloned() {
            tag
        } else {
            Tag::None
        }
    }

    pub fn reference_use_locations(&self) -> &IndexMap<BString, Vec<Location>> {
        &self.use_locations
    }

    pub fn use_component(&mut self, path: &BStr, location: Location) {
        if !self.use_locations.contains_key(path) {
            self.use_locations.insert(path.to_owned(), vec![]);
        }
        self.use_locations.get_mut(path).unwrap().push(location);
    }

    pub(crate) fn type_of_node(&self, id: AstNodeId) -> Option<&Type> {
        self.typs.get(&id)
    }

    fn flag_wrong_type<'p>(&mut self, node: &AstNode<'p>, expected: &Type, actual: &Type) {
        let diag: Diagnostic = diagnostics::WrongType {
            region: node.region(),
            expected: expected.to_string().into(),
            actual: actual.to_string().into(),
        }.into();
        self.diagnostics.push(diag);
    }

    #[allow(unused)]
    fn flag_todo<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Todo {
            region: node.region(),
            message: message.into(),
        }.into());
    }

    fn flag_unknown<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Unknown {
            region: node.region(),
            message: message.into(),
        }.into());
    }

    fn flag_not_word_type<'p>(&mut self, node: &AstNode<'p>, typ: &Type) {
        self.diagnostics.push(diagnostics::NotWordType {
            region: node.region(),
            typ: typ.to_string().into(),
        }.into());
    }

    fn flag_cant_infer<'p>(&mut self, node: &AstNode<'p>) {
        self.diagnostics.push(diagnostics::CantInfer {
            region: node.region(),
        }.into());
    }

    fn annotate(&mut self, node: &AstNode<'_>, typ: &Type) {
        let previous = self.typs.insert(node.id(), typ.clone());
        if let Some(previous_typ) = previous {
            panic!("Node is already annotated with {previous_typ:?}, can't annotate with {typ:?} at {:?}", node.region());
        }
    }

    // True when any expression resolved to a referent whose type couldn't be determined
    // upstream (e.g. a component whose type annotation failed to resolve). In that case
    // the cascade is already explained by an upstream diagnostic, and any missing
    // annotations downstream are expected.
    fn has_unresolved_referent(&self, builder: &mut Builder) -> bool {
        for tag in self.tags.values() {
            if let Tag::ReferentResolution(Referent::Component(component_id)) = tag {
                if builder.get_component(*component_id).typ().is_none() {
                    return true;
                }
            }
        }
        false
    }

    // Checks that in a clean typecheck, all expressions in the tree have an annotation.
    pub fn validate(&mut self, builder: &mut Builder, parsing: &Parsing) {
        if self.diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
            return;
        }
        if self.has_unresolved_referent(builder) {
            return;
        }

        let root_id = self.exprroot.location().ast_node_id();
        let mut queue = vec![root_id];
        while let Some(node_id) = queue.pop() {
            let node = parsing.ast_node(node_id);
            if node.is_expr() && !self.typs.contains_key(&node.id()) {
                self.flag_unknown(&node, "Missing annotation");
            }

            for child in node.children() {
                queue.push(child.id());
            }
        }
    }
}

pub(crate) fn build_type_at(builder: &mut Builder, location: Location) -> Result<Type, Vec<Diagnostic>> {
    let type_index = builder.get_type_index();
    if let Some(typ) = type_index.type_at(location) {
        Ok(typ.clone())
    } else {
        Err(vec![]) // TODO do I actually need diagnostics here?
    }
}

pub(crate) fn typecheck(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let exprroots = builder.get_exprroots();
    let item = symboltable.symbol(symbol_id);

    let mut use_locations: IndexMap<BString, IndexSet<Location>> = IndexMap::new();

    let parsing = builder.get_parsing(item.package());
    let node: AstNode = parsing.ast_node(item.location().ast_node_id());
    let is_moddef = matches!(node.payload(), AstNodePayload::ModDef(_));
    let component_analysis = if is_moddef {
        Some(builder.get_component_analysis(symbol_id))
    } else {
        None
    };
    if !node.contains_errors() {
        diagnostics.extend(typecheck_item(builder, item, &exprroots, &mut use_locations));
        let mut already_unused: IndexSet<BString> = IndexSet::new();
        collect_unused(node.clone(), &mut use_locations, &mut already_unused, &mut diagnostics);
        if let Some(component_analysis) = &component_analysis {
            collect_bidirectional_drivers(node.clone(), &mut use_locations, component_analysis, &mut diagnostics);
        }
    }

    if let AstNodePayload::ModDef(moddef) = node.payload() && !moddef.is_ext {
        // Unused varibles and read from sink warnings
        let component_analysis = component_analysis.as_ref().unwrap();
        for (path, component) in component_analysis.components() {
            if component.can_source() && !use_locations.contains_key(&path) {
                let region = builder.get_location_region(component.location());
                diagnostics.push(diagnostics::UnusedSource {
                    region,
                    path: path.into(),
                }.into());
            } else if component.flow() == Flow::Sink && use_locations.contains_key(&path) {
                for location in &use_locations[&path] {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::ReadFromSink {
                        region,
                        path: path.clone(),
                    }.into());
                }
            }
        }
    }

    Arc::new(diagnostics)
}

fn typecheck_item(
    builder: &mut Builder,
    item: Symbol,
    exprroots: &[ExprRoot],
    use_locations: &mut IndexMap<BString, IndexSet<Location>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for exprroot in exprroots {
        if exprroot.package() == item.package() && item_for(builder, exprroot.location()).id() == item.id() {
            let typing = builder.get_typing(exprroot.clone());
            diagnostics.extend(typing.diagnostics());

            let typing_use_locations = typing.reference_use_locations();
            for (path, locations) in typing_use_locations.iter() {
                if !use_locations.contains_key(path.as_bstr()) {
                    use_locations.insert(path.clone(), IndexSet::new());
                }
                let t = use_locations.get_mut(path.as_bstr()).unwrap();
                t.extend(locations.clone());
            }
        }
    }
    diagnostics
}

fn collect_unused(
    node: AstNode<'_>,
    use_locations: &mut IndexMap<BString, IndexSet<Location>>,
    already_unused: &mut IndexSet<BString>,
    diagnostics: &mut Vec<crate::diagnostics::Diagnostic>,
) {
    if let AstNodePayload::ModDefStmtUnused = node.payload() {
        let path_node = node.child(0);
        if let Some(path) = path_node.path() {
            let parsing = node.parsing();
            let path_str = parsing.string(path);
            let mut resolved_path = path_str.to_owned();

            // Always walk up to find the nearest enclosing it-context
            // (Submodule / Component / Socket).
            let mut ctx_name: Option<BString> = None;
            let mut current_id = node.id();
            while let Some(parent_id) = parsing.ast_node(current_id).parent {
                let parent = parsing.ast_node(parent_id);
                let mut resolved_name = None;
                if let AstNodePayload::Submodule(module) = parent.payload() {
                    resolved_name = Some(parsing.string(module.name));
                } else if let AstNodePayload::Component(component) = parent.payload() {
                    resolved_name = Some(parsing.string(component.name));
                } else if let AstNodePayload::Socket(socket) = parent.payload() {
                    resolved_name = Some(parsing.string(socket.name));
                }
                if let Some(name) = resolved_name {
                    ctx_name = Some(name.to_owned());
                    break;
                }
                current_id = parent_id;
            }

            if resolved_path.starts_with(b"it.") || resolved_path == b"it" {
                // Rewrite `it` / `it.foo` to the enclosing context name.
                if let Some(ctx) = &ctx_name {
                    if resolved_path.starts_with(b"it.") {
                        let suffix = resolved_path[3..].to_owned();
                        resolved_path.clear();
                        resolved_path.extend_from_slice(ctx);
                        resolved_path.push(b'.');
                        resolved_path.extend_from_slice(&suffix);
                    } else {
                        resolved_path = ctx.to_owned();
                    }
                }
            } else if let Some(ctx) = &ctx_name {
                // Inside an it-block: flag paths that use the component name
                // directly instead of 'it'.
                let mut ctx_dot = ctx.to_owned();
                ctx_dot.push(b'.');
                if resolved_path == ctx.as_bytes()
                    || resolved_path.starts_with(ctx_dot.as_bytes())
                {
                    diagnostics.push(crate::diagnostics::NotIt {
                        region: path_node.region(),
                        component: ctx.to_owned(),
                    }.into());
                }
            }

            if already_unused.contains(&resolved_path) {
                diagnostics.push(crate::diagnostics::RedundantUnused {
                    region: path_node.region(),
                    path: resolved_path.clone(),
                }.into());
            } else {
                already_unused.insert(resolved_path.clone());
            }
            use_locations.entry(resolved_path).or_default().insert(path_node.location());
        }
    }
    for child in node.children() {
        collect_unused(child, use_locations, already_unused, diagnostics);
    }
}


fn collect_bidirectional_drivers(
    node: AstNode<'_>,
    use_locations: &mut IndexMap<BString, IndexSet<Location>>,
    component_analysis: &crate::analysis::component::ComponentAnalysis,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let AstNodePayload::BidirectionalDriver = node.payload() {
        let parsing = node.parsing();

        for child_idx in 0..2 {
            let path_node = node.child(child_idx);
            if let Some(path) = path_node.path() {
                let path_str = parsing.string(path);
                let mut resolved_path = path_str.to_owned();
                let original_path = path_str.to_owned();

                if resolved_path.starts_with(b"it.") || resolved_path == b"it" {
                    let mut current_id = node.id();
                    while let Some(parent_id) = parsing.ast_node(current_id).parent {
                        let parent = parsing.ast_node(parent_id);
                        let mut resolved_name = None;
                        if let AstNodePayload::Submodule(module) = parent.payload() {
                            resolved_name = Some(parsing.string(module.name));
                        } else if let AstNodePayload::Component(component) = parent.payload() {
                            resolved_name = Some(parsing.string(component.name));
                        } else if let AstNodePayload::Socket(socket) = parent.payload() {
                            resolved_name = Some(parsing.string(socket.name));
                        }

                        if let Some(name) = resolved_name {
                            if resolved_path.starts_with(b"it.") {
                                let suffix = resolved_path[3..].to_owned();
                                resolved_path.clear();
                                resolved_path.extend_from_slice(name);
                                resolved_path.push(b'.');
                                resolved_path.extend_from_slice(&suffix);
                            } else {
                                resolved_path = name.to_owned();
                            }
                            break;
                        }
                        current_id = parent_id;
                    }
                }

                if let Some(comp) = component_analysis.resolve(resolved_path.as_slice().into()) {
                    if comp.can_source() {
                        use_locations.entry(resolved_path.clone()).or_default().insert(path_node.location());
                    }
                } else {
                    // If resolution fails, check if it's a socket prefix
                    // (i.e., there are components that start with this path).
                    let mut is_socket_prefix = false;
                    let mut prefix = String::new();
                    prefix.push_str(resolved_path.to_str_lossy().as_ref());
                    prefix.push('.');

                    for (comp_path, _) in component_analysis.components() {
                        if comp_path.to_str_lossy().starts_with(&prefix) {
                            is_socket_prefix = true;
                            break;
                        }
                    }

                    if !is_socket_prefix {
                        // The resolved path is neither a direct component nor a
                        // socket prefix; generate an error.
                        // Use original path if it was an "it" reference
                        let error_path = if path_str.starts_with(b"it.") || path_str == b"it" {
                            original_path
                        } else {
                            resolved_path.clone()
                        };
                        diagnostics.push(crate::diagnostics::Unknown {
                            region: path_node.region(),
                            message: format!("Unknown component {}", error_path.to_str_lossy()).into(),
                        }.into());
                    }
                }

                let mut prefix = String::new();
                prefix.push_str(resolved_path.to_str_lossy().as_ref());
                prefix.push('.');

                for (comp_path, comp) in component_analysis.components() {
                    let comp_path_str = comp_path.to_str_lossy();
                    if comp_path_str.starts_with(&prefix) {
                        if comp.can_source() {
                            use_locations.entry(comp_path).or_default().insert(path_node.location());
                        }
                    }
                }
            }
        }
    }

    for child in node.children() {
        collect_bidirectional_drivers(child, use_locations, component_analysis, diagnostics);
    }
}


pub(crate) fn item_for(builder: &mut Builder, location: Location) -> Symbol {
    let symboltable = builder.get_symboltable();
    let parsing = builder.get_parsing(location.package());

    let mut node = parsing.ast_node(location.ast_node_id());
    loop {
        if node.is_item() {
            let name = node.name().unwrap();
            let name_str = parsing.string(name);
            return symboltable
                .resolve_item_in_package(name_str, location.package())
                .unwrap()
                .clone();
        }

        if let Some(parent) = node.parent() {
            node = parsing.ast_node(parent.id());
        } else {
            let region = builder.get_location_region(location.clone());
            panic!("No containing item found for location {location:?} at {region:?}");
        }
    }
}

pub(crate) fn build_typing(builder: &mut Builder, exprroot: ExprRoot) -> Arc<Typing> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());

    let mut current_id = location.ast_node_id();
    // REVIEW walk up until you find the containing item ast node.
    let item_name = loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            break parsing
                .string(current.name().expect("expected containing item to have a name"))
                .to_owned();
        }
        current_id = current
            .parent()
            .expect("expected expr root to be contained in an item")
            .id();
    };
    let symboltable = builder.get_symboltable();
    let item = symboltable
        .resolve_item_in_package(item_name.as_bstr(), location.package())
        .unwrap();
    let context = builder.get_typing_context(item.id());
    let context = extend_context_for_enclosing_stmt_matches(builder, &parsing, location.ast_node_id(), context);

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = builder.get_expected_type(exprroot.clone());

    let diagnostics = vec![];
    let mut typing = Typing {
        item: item.clone(),
        exprroot: exprroot.clone(),
        typs: IndexMap::new(),
        diagnostics,
        use_locations: IndexMap::new(),
        tags: IndexMap::new(),
    };

    // if there is no expected type, you can't type check the expression.
    if let Some(expected_typ) = expected_typ {
        let _ = typing.check(builder, context, &node, &expected_typ);
    } else {
        // Still run type inference to catch errors like unknown components,
        // even if we don't have an expected type
        match typing.infer(builder, context, &node) {
            Ok(None) => {
                // Only report "can't infer" if it's not a driver expression
                if !matches!(node.parent().unwrap().payload(), AstNodePayload::Driver(_)) {
                    typing.diagnostics.push(diagnostics::Todo {
                        region: node.region(),
                        message: format!("Can't typecheck expression because we don't know what type it should have").into(),
                    }.into());
                }
            }
            Ok(Some(typ)) => {
                typing.typs.insert(node.id(), typ);
            }
            _ => (),
        }
    }

    typing.validate(builder, &parsing);

    Arc::new(typing)
}


/// Walk up from `start_id` through the AST, collecting pattern-bound variable bindings
/// from any enclosing `ModDefStmtMatch` arms, and push them onto `context`.
/// This allows pattern-bound variables (e.g. `r` in `case @Done(r) => { result := r }`)
/// to be resolved when type-checking driver expressions inside statement-level match arms.
fn extend_context_for_enclosing_stmt_matches(
    builder: &mut Builder,
    parsing: &Parsing,
    start_id: AstNodeId,
    mut context: TypingContext,
) -> TypingContext {
    // Collect all ancestor IDs walking upward from start_id, stopping at item boundaries.
    // The vec is innermost-first (closest ancestor at index 0).
    let mut ancestor_ids: Vec<AstNodeId> = vec![];
    let mut current_id = start_id;
    loop {
        let node = parsing.ast_node(current_id);
        if node.is_item() {
            break;
        }
        match node.parent {
            Some(parent_id) => {
                ancestor_ids.push(parent_id);
                current_id = parent_id;
            }
            None => break,
        }
    }

    // For each enclosing ModDefStmtMatch ancestor, determine which arm contains start_id
    // by checking which arm block is itself an ancestor of start_id.
    // Collect (subject_location, Option<pat_node_id>) pairs; ancestors are innermost-first.
    // `else` arms contribute no pattern bindings, so the pat id is None.
    let mut arm_pats: Vec<(Location, Option<AstNodeId>)> = vec![];
    for &ancestor_id in &ancestor_ids {
        let ancestor = parsing.ast_node(ancestor_id);
        if let AstNodePayload::ModDefStmtMatch = ancestor.payload() {
            let children = ancestor.children();
            for (pat_opt, block) in match_arm_children(&children) {
                if ancestor_ids.contains(&block.id()) {
                    arm_pats.push((children[0].location(), pat_opt.map(|p| p.id())));
                    break;
                }
            }
        }
    }

    // ancestor_ids are innermost-first, so arm_pats are innermost-first too.
    // Reverse so outermost bindings are pushed first (innermost can shadow outermost).
    arm_pats.reverse();
    for (subject_location, pat_id_opt) in arm_pats {
        let Some(pat_id) = pat_id_opt else { continue };
        let subject_typ = match builder.get_typeof(subject_location) {
            Ok(t) => t,
            Err(_) => continue,
        };
        let pat_node = parsing.ast_node(pat_id);
        context = extend_context_with_stmt_match_pat(builder, &pat_node, &subject_typ, context);
    }

    context
}

/// Given a `ModDefStmtMatch` arm pattern node and the expected (subject) type, extend the
/// typing context with any variables bound by the pattern.  Only `PatCtor` patterns with
/// named payload variables and `PatIdent` patterns contribute bindings;
/// `PatEnumerant`, `PatDontcare`, and `else` arms do not.
fn extend_context_with_stmt_match_pat(
    builder: &mut Builder,
    pat_node: &AstNode<'_>,
    expected_typ: &Type,
    mut context: TypingContext,
) -> TypingContext {
    match pat_node.payload() {
        AstNodePayload::PatIdent(pat_ident) => {
            // Ident patterns bind the entire subject value to the identifier.
            let var_name = pat_node.parsing().string(pat_ident.name).to_owned();
            context = context.push_local(var_name, pat_node.location(), expected_typ.clone());
        }
        AstNodePayload::PatCtor(pat_ctor) => {
            let ctor_name = pat_node.parsing().string(pat_ctor.name);
            // Handle builtin Valid[T] pattern constructors: @Valid(t) and @Invalid()
            if ctor_name.as_bytes() == b"Valid" || ctor_name.as_bytes() == b"Invalid" {
                let Type::Valid(inner_typ) = expected_typ else { return context; };
                if ctor_name.as_bytes() == b"Valid" {
                    let children = pat_node.children();
                    if children.len() == 1 {
                        let child = &children[0];
                        if let Some(var_name_interned) = child.path() {
                            let var_name = child.parsing().string(var_name_interned).to_owned();
                            context = context.push_local(
                                var_name, child.location(), (**inner_typ).clone(),
                            );
                        }
                    }
                }
                // @Invalid() takes no arguments, so no bindings
            } else {
                let Type::Usual(typedef_id) = expected_typ else { return context; };
                let symboltable = builder.get_symboltable();
                let Some(ctor_symbol) = symboltable.slot(*typedef_id, ctor_name) else { return context; };
                let ctor_symbol_id = ctor_symbol.id();
                let sig = builder.get_ctor_signature(ctor_symbol_id);
                let children = pat_node.children();
                for (child, (_param_name, param_typ)) in children.iter().zip(sig.parameters.iter()) {
                    if let Some(var_name_interned) = child.path() {
                        let var_name = child.parsing().string(var_name_interned).to_owned();
                        context = context.push_local(var_name, child.location(), param_typ.clone());
                    }
                }
            }
        }
        _ => {}
    }
    context
}
