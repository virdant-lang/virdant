//! Top-level diagnostic aggregator running all front-end checks over the
//! database (syntax errors, package-name keyword collisions, per-item
//! driver/match-coverage checks, typechecking, enum checks,
//! instantiation cycles), sorted by source region.

use std::sync::Arc;

use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexMap;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::graph::{Graph, VertIndex};
use crate::common::source::Region;
use crate::common::WordValue;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticPayload};
use crate::diagnostics::EnumPartiallySpecified;
use crate::package::PackageId;
use crate::syntax::payload::AstNodePayload;
use crate::syntax::token::KEYWORDS;

/// A directed edge in the module instantiation graph:
/// `parent_id` has a `mod ... of (target_vert)` statement at `region`.
struct ModEdge {
    parent_id: SymbolId,
    region: Region,
    parent_vert: VertIndex,
    target_vert: VertIndex,
}

fn package_name_is_keyword(name: &str) -> bool {
    KEYWORDS.contains(&name)
}

/// Check that no package name (except `builtin`) collides with a keyword.
fn check_package_name_not_keyword(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let packages = builder.get_packages();
    for package in packages.ids() {
        let package_name = packages.name(package).to_str_lossy().into_owned();
        if package_name == "builtin" {
            continue;
        }
        if package_name_is_keyword(&package_name) {
            let parsing = builder.get_parsing(package);
            let root_node = parsing.root();
            diagnostics.push(Diagnostic::new(
                root_node.region(),
                diagnostics::Unknown {
                    message: format!(
                        "Package name '{}' is a keyword",
                        package_name,
                    ).into(),
                },
            ));
        }
    }
}

pub(crate) fn check(builder: &mut Builder) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors().iter().cloned());

    check_package_name_not_keyword(builder, &mut diagnostics);

    let symboltable = builder.get_symboltable();
    diagnostics.extend(symboltable.diagnostics.clone());

    let type_index = builder.get_type_index();
    diagnostics.extend(type_index.diagnostics());

    // Arm-style validation runs before the driver checks so that
    // style-mismatch arms are reported with their own diagnostic
    // rather than as missing drivers.
    check_when_match_style(builder, &mut diagnostics);

    for item in symboltable.items() {
        // Platform items are unvalidated data (they have no drivers
        // and no match arms); running module checks on them would
        // falsely flag their `outgoing` ports as undriven.
        if item.kind == SymbolKind::Platform {
            continue;
        }
        diagnostics.extend(builder.check_drivers(item.id()).iter().cloned());
        diagnostics.extend(builder.check_initial(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::Platform {
            continue;
        }
        diagnostics.extend(builder.get_match_coverage(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::ModDef {
            let component_analysis = builder.get_component_analysis(item.id());
            diagnostics.extend(component_analysis.diagnostics());
        }
        diagnostics.extend(builder.typecheck(item.id()).iter().cloned());

        // Exact port match for modules with a `for` clause.
        if item.kind == SymbolKind::ModDef {
            crate::analysis::platform::check_platform_ports_for(
                builder, item.id(), &mut diagnostics);
        }

//        let driver_analysis = builder.get_driver_analysis(item.id());
//        diagnostics.extend(driver_analysis); // TODO
    }

    // `default` clauses are grammatically allowed wherever outgoing
    // components are declared (the rule is shared with platforms), but
    // semantically only platform `outgoing` ports may carry one.
    check_default_clause_placement(builder, &mut diagnostics);

    // Check for duplicate enumerant values in enum types.
    // Only the first duplicate per value is reported (as an Error).
    check_duplicate_enum_values(builder, &mut diagnostics);

    // Check that enum types do not mix enumerants with and without explicit values.
    check_enum_partially_specified(builder, &mut diagnostics);

    // Check that enum types have an inferrable width from the first enumerant.
    check_enum_unknown_width(builder, &mut diagnostics);

    // Check that declared generic parameters have known kinds.
    check_generic_param_kinds(builder, &mut diagnostics);

    // Check that `:=:` socket connections match on mono sockets: the
    // socketdef together with its type arguments (PLANv2 §6).
    check_socket_connections(builder, &mut diagnostics);

    // Check that no `ext mod` declares generic parameters: ext mods
    // never support generics (a hard rule, not a missing feature).
    check_ext_mod_generics(builder, &mut diagnostics);

    // Check that no `export mod` declares generic parameters: an
    // exported top must be a concrete instantiation.
    check_export_mod_generics(builder, &mut diagnostics);

    // Check for recursive module instantiations.
    check_mod_cycles(builder, &mut diagnostics);

    // Check for combinational loops in module dependency graphs.
    // The check runs bottom-up per module in submodule-inclusion order,
    // stitching per-module graphs along the instance tree.
    diagnostics.extend(builder.get_combinational_cycle_check().iter().cloned());

    let packages = builder.get_packages();
    diagnostics.sort_by_key(|d| {
        let region = d.region();
        let package_name = packages.name(region.package()).to_owned();
        let start = region.start();
        let end = region.end();
        (package_name, start.line(), start.col(), end.line(), end.col())
    });

    Arc::new(diagnostics)
}

/// Check the arm style of every `when`/`match` against its context:
/// a when/match used as an expression takes value arms (`=> <expr>`),
/// while one used as a statement takes statement arms (`{ <stmts> }`).
/// The grammar accepts both forms everywhere; this pass reports the
/// mismatches as errors.
fn check_when_match_style(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::ModDef {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        check_when_match_style_in(&node, diagnostics);
    }
}

fn check_when_match_style_in(
    node: &crate::syntax::ast::AstNode<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let payload = node.payload();
    if matches!(payload, AstNodePayload::When | AstNodePayload::Match) {
        let is_expression = node.is_expression_position();
        let children = node.children();
        let arms: Vec<(Option<crate::syntax::ast::AstNode<'_>>, crate::syntax::ast::AstNode<'_>)> =
            if matches!(payload, AstNodePayload::When) {
                crate::syntax::ast::when_arm_children(&children)
                    .into_iter()
                    .map(|(guard, body)| (guard.cloned(), body.clone()))
                    .collect()
            } else {
                crate::syntax::ast::match_arm_children(&children)
                    .into_iter()
                    .map(|(pat, body)| (pat.cloned(), body.clone()))
                    .collect()
            };
        for (_guard, body) in arms {
            match body.payload() {
                // `{ <stmts> }`: statements always fit either context.
                // `{ <expr> }` (a single bare expression) is the
                // mismatch to report.
                AstNodePayload::ArmBlock => {
                    let block_children = body.children();
                    if block_children.len() == 1
                        && block_children[0].is_expr()
                        && !matches!(
                            block_children[0].payload(),
                            AstNodePayload::When | AstNodePayload::Match
                        )
                    {
                        diagnostics.push(Diagnostic::new(
                            body.region(),
                            if is_expression {
                                DiagnosticPayload::ArmBlockNeedsArrow
                            } else {
                                DiagnosticPayload::ArmNeedsBlock
                            },
                        ));
                    }
                }
                // A bare `When`/`Match` arm body is indistinguishable
                // from `=> <when/match>` after parsing, and is legal
                // in both contexts; skip it.
                AstNodePayload::When | AstNodePayload::Match => {}
                // Any other expression body comes from a `=>` arm: a
                // value, which only fits expression context.
                _ if body.is_expr() && !is_expression => {
                    diagnostics.push(Diagnostic::new(
                        body.region(),
                        DiagnosticPayload::ArmNeedsBlock,
                    ));
                }
                _ => {}
            }
        }
    }
    for child in node.children() {
        check_when_match_style_in(&child, diagnostics);
    }
}

/// Check that `default` clauses only appear on outgoing ports in
/// platform definitions. The grammar accepts `default` on outgoing
/// components in plain mod definitions too (the rule is shared), so
/// this is a semantic check, not a syntactic one.
fn check_default_clause_placement(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::ModDef {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        check_default_clauses_in(&node, &parsing, diagnostics);
    }
}

fn check_default_clauses_in(
    node: &crate::syntax::ast::AstNode<'_>,
    parsing: &crate::syntax::parsing::Parsing,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for child in node.children() {
        if let AstNodePayload::Component(component) = child.payload() {
            if let Some(clause) = child.default_clause() {
                diagnostics.push(Diagnostic::new(
                    clause.region(),
                    diagnostics::DefaultOutsidePlatformPort {
                        port: parsing.string(component.name).to_owned(),
                    },
                ));
            }
        }
        check_default_clauses_in(&child, parsing, diagnostics);
    }
}

/// Check each port synthesized from a platform `default <EXPR>` clause:
/// the default expression's type must match the platform port's type.
/// The expression itself is type-checked in the platform's own context
/// (it is an exprroot of the platform item); a mismatch is reported
/// here, pointing at the platform's expression.
/// Check for enumerants of the same enum type that share the same value.
/// Only the first duplicate per value is reported (as an Error);
/// further duplicates for the same value are silently ignored.
fn check_duplicate_enum_values(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::EnumDef {
            continue;
        }
        let typedef = builder.get_typedef(item.id());
        // Map value -> (name, region) of the first occurrence.
        let mut first: IndexMap<WordValue, (BString, crate::common::source::Region)> = IndexMap::new();
        let mut already_reported: std::collections::HashSet<WordValue> = std::collections::HashSet::new();
        for (enumerant_id, value) in &typedef.enumerant_values {
            let enumerant_symbol = symboltable.symbol(*enumerant_id);
            if let Some((_prev_name, prev_region)) = first.get(value) {
                if already_reported.insert(*value) {
                    diagnostics.push(Diagnostic::new(
                        prev_region.clone(),
                        diagnostics::DuplicateEnumValue {
                            enum_name: item.name().to_owned(),
                            value: *value,
                        },
                    ));
                }
            } else {
                let parsing = builder.get_parsing(enumerant_symbol.package());
                let enumerant_node = parsing.ast_node(enumerant_symbol.location().ast_node_id());
                first.insert(*value, (enumerant_symbol.name().to_owned(), enumerant_node.region()));
            }
        }
    }
}

/// Check that enum types do not mix enumerants with explicit values and
/// enumerants without. When no enumerant specifies a value, the enum type
/// is one-hot encoded by default; mixing the two styles is ambiguous and
/// reported as an Error.
fn check_enum_partially_specified(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::EnumDef {
            continue;
        }
        let parsing = builder.get_parsing(item.package());
        let enumdef_node = parsing.ast_node(item.location().ast_node_id());
        let mut any_specified = false;
        let mut any_missing = false;
        for enumerant_node in enumdef_node.children() {
            let AstNodePayload::Enumerant(_) = enumerant_node.payload() else { continue; };
            // The value expression, if any, is the first expression child
            // (after the Annotations wrapper and any ctor Param children).
            if enumerant_node.children().into_iter().any(|child| child.is_expr()) {
                any_specified = true;
            } else {
                any_missing = true;
            }
        }
        if any_specified && any_missing {
            diagnostics.push(Diagnostic::new(
                enumdef_node.region(),
                DiagnosticPayload::EnumPartiallySpecified(EnumPartiallySpecified {
                    enum_name: item.name().to_owned(),
                }),
            ));
        }
    }
}

/// Check that every declared generic parameter has a known kind
/// (`Type` or `Nat`), on every item kind which accepts a generic
/// parameter declaration list.
fn check_generic_param_kinds(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        let parsing = builder.get_parsing(item.package());
        let item_node = parsing.ast_node(item.location().ast_node_id());
        let Some(generics_node) = crate::types::typedef::generics_decl_of(&item_node) else {
            continue;
        };
        for param_node in generics_node.children() {
            let AstNodePayload::KindParam(_kind_param) = param_node.payload() else { continue };
            let AstNodePayload::Kind(kind) = param_node.child(0).payload() else { continue };
            let kind_name = parsing.string(kind.name);
            if !matches!(kind_name.as_bytes(), b"Type" | b"Nat") {
                diagnostics.push(Diagnostic::new(
                    param_node.region(),
                    diagnostics::UnknownKind {
                        kind: kind_name.to_owned(),
                    },
                ));
            }
        }
    }
}

/// Check that every enum type has an inferrable width from its first enumerant.
/// When the first enumerant has no explicit width (e.g. `= 0` instead of `= 0w8`),
/// the enum width cannot be determined and an error is reported.
fn check_enum_unknown_width(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::EnumDef {
            continue;
        }
        let typedef = builder.get_typedef(item.id());
        if typedef.width.is_none() {
            let parsing = builder.get_parsing(item.package());
            let enumdef_node = parsing.ast_node(item.location().ast_node_id());
            diagnostics.push(Diagnostic::new(
                enumdef_node.region(),
                DiagnosticPayload::EnumUnknownWidth,
            ));
        }
    }
}

/// Build a graph of ModDef items, with edges for each `mod X of Y` statement.
/// For each edge `parent -> target`, check whether `target` can reach `parent`
/// (i.e. a back-edge).  When it can, we flag the `mod` statement as part of a cycle.
fn check_mod_cycles(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();

    // Graph vertices are SymbolIds of ModDef items.
    let mut graph: Graph<SymbolId> = Graph::new();
    let mut edges: Vec<ModEdge> = vec![];

    // Map SymbolId -> VertIndex so we can reuse existing vertices.
    let mut vertex_map: IndexMap<SymbolId, VertIndex> = IndexMap::new();
    let mut get_or_add_vert = |g: &mut Graph<SymbolId>, id: SymbolId| -> VertIndex {
        *vertex_map.entry(id).or_insert_with(|| g.add_vert(id))
    };

    for item in symboltable.items() {
        if item.kind() != SymbolKind::ModDef {
            continue;
        }
        let parent_id = item.id();
        let parent_vert = get_or_add_vert(&mut graph, parent_id);

        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let ast_node = parsing.ast_node(location.ast_node_id());

        for stmt in ast_node.children() {
            let AstNodePayload::Submodule(_submodule) = stmt.payload() else {
                continue;
            };
            let ofness_node = stmt.child(1);
            let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                continue;
            };
            let packages = builder.get_packages();
            let target_package: Option<PackageId> = match ofness.package {
                Some(pkg) => packages.id(parsing.string(pkg)),
                None => Some(location.package()),
            };
            let target_name = parsing.string(ofness.name);
            let Some(target_symbol) =
                target_package.and_then(|pkg| symboltable.resolve_item(target_name, pkg))
            else {
                continue;
            };
            if target_symbol.kind() != SymbolKind::ModDef {
                continue;
            }
            let target_id = target_symbol.id();
            let target_vert = get_or_add_vert(&mut graph, target_id);

            graph.add_edge(parent_vert, target_vert);
            edges.push(ModEdge {
                parent_id,
                region: stmt.region(),
                parent_vert,
                target_vert,
            });
        }
    }

    // For each edge, check if `target` can reach `parent` (back-edge detection).
    for edge in &edges {
        let Some(back_path) = graph.path(edge.target_vert, edge.parent_vert) else {
            continue;
        };
        // back_path goes target -> ... -> parent (inclusive of parent).
        // The cycle is: parent, target, ..., parent.
        // Drop the duplicate trailing parent for the FQN list.
        let mut cycle_ids: Vec<SymbolId> = vec![edge.parent_id];
        for vi in back_path.iter().take(back_path.len().saturating_sub(1)) {
            cycle_ids.push(graph[*vi]);
        }
        let cycle_fqns: Vec<BString> = cycle_ids
            .iter()
            .map(|id| symboltable.symbol(*id).fqn().to_owned().into())
            .collect();

        diagnostics.push(Diagnostic::new(
            edge.region.clone(),
            diagnostics::ModuleCycle {
                module_cycle: cycle_fqns,
            },
        ));
    }
}

/// Check that every `:=:` socket connection connects two instances of
/// the *same* mono socket: the same socketdef instantiated at the same
/// type arguments (PLANv2 §6).  Two instances of the same socketdef
/// with different arguments are different mono sockets and do not
/// connect; the diagnostic names both instantiations and points at the
/// connection.
fn check_socket_connections(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    let items: Vec<SymbolId> = symboltable
        .items()
        .into_iter()
        .filter(|item| item.kind == SymbolKind::ModDef)
        .map(|item| item.id())
        .collect();
    for item in items {
        let location = builder.get_symboltable().symbol(item).location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        check_connection_nodes(builder, item, &node, None, diagnostics);
    }
}

/// Recursively walk `node`'s statement tree, tracking the `it`-context
/// (the dotted instance name of the enclosing submodule), checking
/// every `BidirectionalDriver`.
fn check_connection_nodes(
    builder: &mut Builder,
    item: SymbolId,
    node: &crate::syntax::ast::AstNode<'_>,
    it_context: Option<&bstr::BStr>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    use crate::syntax::payload::AstNodePayload;

    match node.payload() {
        AstNodePayload::BidirectionalDriver => {
            let parsing = node.parsing();
            let lhs = node.child(0);
            let rhs = node.child(1);
            let lhs_path = rewrite_connection_side(parsing, lhs, it_context);
            let rhs_path = rewrite_connection_side(parsing, rhs, it_context);
            if let (Some(lhs_path), Some(rhs_path)) = (lhs_path, rhs_path) {
                let lhs_socket = resolve_mono_socket(builder, item, lhs_path.as_bstr());
                let rhs_socket = resolve_mono_socket(builder, item, rhs_path.as_bstr());
                if let (Some(lhs_socket), Some(rhs_socket)) = (lhs_socket, rhs_socket) {
                    if lhs_socket.socketdef != rhs_socket.socketdef
                        || lhs_socket.args != rhs_socket.args
                    {
                        diagnostics.push(Diagnostic::new(
                            node.region(),
                            diagnostics::SocketConnectMismatch {
                                lhs: lhs_socket.rendered,
                                rhs: rhs_socket.rendered,
                            },
                        ));
                    }
                }
            }
            return;
        }
        AstNodePayload::Submodule(submodule) => {
            // Recurse into the It-block body, using the submodule
            // instance name as the new `it`-context so `it.foo` paths
            // inside resolve to the instance.
            let parsing = node.parsing();
            let name = parsing.string(submodule.name);
            let new_context = match it_context {
                Some(ctx) => {
                    let mut joined = bstr::BString::from(ctx);
                    joined.push(b'.');
                    joined.extend_from_slice(name);
                    joined
                }
                None => bstr::BString::from(name),
            };
            for child in node.children() {
                if matches!(child.payload(), AstNodePayload::It) {
                    let block = &child.children()[0];
                    for stmt in block.children() {
                        check_connection_nodes(builder, item, &stmt, Some(new_context.as_bstr()), diagnostics);
                    }
                }
            }
            return;
        }
        _ => {}
    }

    for child in node.children() {
        check_connection_nodes(builder, item, &child, it_context, diagnostics);
    }
}

/// Rewrite a `:=:` side path, resolving `it` / `it.*` against the
/// enclosing it-context (mirroring the driver-analysis rewrite).
fn rewrite_connection_side(
    parsing: &crate::syntax::parsing::Parsing,
    side_node: crate::syntax::ast::AstNode<'_>,
    it_context: Option<&bstr::BStr>,
) -> Option<bstr::BString> {
    let path_interned = side_node.path()?;
    let path = parsing.string(path_interned).to_owned();
    let Some(ctx) = it_context else {
        return Some(path);
    };
    if path == b"it" {
        Some(ctx.to_owned())
    } else if path.starts_with(b"it.") {
        let suffix: bstr::BString = path[3..].to_owned().into();
        let mut joined = ctx.to_owned();
        joined.push(b'.');
        joined.extend_from_slice(&suffix);
        Some(joined)
    } else {
        Some(path)
    }
}

/// A socket instance resolved at a `:=:` connection side, together
/// with its mono identity: the socketdef and its type arguments
/// (composed through the enclosing submodule instances).
struct MonoSocket {
    socketdef: SymbolId,
    args: Vec<crate::types::Type>,
    rendered: BString,
}

/// Resolve one `:=:` side path (already it-rewritten) to its mono
/// socket: walk the instance prefix through submodule statements
/// (composing written type arguments with the enclosing instance's
/// arguments), then find the socket instance named by the last path
/// segment in the innermost module.
fn resolve_mono_socket(
    builder: &mut Builder,
    item: SymbolId,
    path: &bstr::BStr,
) -> Option<MonoSocket> {
    use crate::syntax::payload::AstNodePayload;
    use crate::types::substitute::substitute;

    let segments: Vec<&[u8]> = path.split(|b| *b == b'.').collect();
    let (socket_name, instance_segments) = segments.split_last()?;

    let symboltable = builder.get_symboltable();

    // Walk the instance prefix through submodule statements, composing
    // each instance's written arguments with the enclosing environment.
    let mut current_moddef = item;
    let mut env: Vec<crate::types::Type> = vec![];
    for instance_name in instance_segments {
        let location = symboltable.symbol(current_moddef).location();
        let parsing = builder.get_parsing(location.package());
        let def_node = parsing.ast_node(location.ast_node_id());
        let sub_stmt = def_node.children().into_iter().find(|stmt| {
            matches!(
                stmt.payload(),
                AstNodePayload::Submodule(submodule)
                    if parsing.string(submodule.name) == *instance_name
            )
        })?;
        let (target_symbol, written_args) =
            crate::types::typedef::instantiation_target(builder, &sub_stmt)?;
        env = written_args
            .iter()
            .map(|arg| substitute(arg, &env))
            .collect();
        current_moddef = target_symbol.id();
    }

    // Find the socket instance in the innermost module.
    let location = symboltable.symbol(current_moddef).location();
    let parsing = builder.get_parsing(location.package());
    let def_node = parsing.ast_node(location.ast_node_id());
    let socket_stmt = def_node.children().into_iter().find(|stmt| {
        matches!(
            stmt.payload(),
            AstNodePayload::Socket(socket) if parsing.string(socket.name) == socket_name
        )
    })?;
    let (socket_symbol, written_args) =
        crate::types::typedef::instantiation_target(builder, &socket_stmt)?;
    let args: Vec<crate::types::Type> = written_args
        .iter()
        .map(|arg| substitute(arg, &env))
        .collect();

    let params = crate::types::typedef::params_of_item(builder, &socket_symbol);
    let rendered = crate::types::render::render_mono_item(
        builder.db(),
        socket_symbol.id(),
        &params,
        &args,
    );

    Some(MonoSocket {
        socketdef: socket_symbol.id(),
        args,
        rendered: rendered.into(),
    })
}

/// Check that no `ext mod` declares generic parameters: ext mods never
/// support generics (they parameterize through `@parameter`
/// annotations instead).  This is a hard rule, not a missing feature.
fn check_ext_mod_generics(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::ModDef {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        let AstNodePayload::ModDef(moddef) = node.payload() else { continue };
        if !moddef.is_ext {
            continue;
        }
        let Some(decl) = crate::types::typedef::generics_decl_of(&node) else {
            continue;
        };
        diagnostics.push(Diagnostic::new(
            decl.region(),
            diagnostics::ExtModGenerics {
                item: item.fqn().to_owned(),
            },
        ));
    }
}

/// Check that no `export mod` declares generic parameters: an exported
/// top must be a concrete instantiation, and a generic def has none.
fn check_export_mod_generics(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::ModDef {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        let AstNodePayload::ModDef(moddef) = node.payload() else { continue };
        if !moddef.is_export {
            continue;
        }
        let Some(decl) = crate::types::typedef::generics_decl_of(&node) else {
            continue;
        };
        diagnostics.push(Diagnostic::new(
            decl.region(),
            diagnostics::ExportModGenerics {
                item: item.fqn().to_owned(),
            },
        ));
    }
}
