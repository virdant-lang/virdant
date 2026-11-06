//! Performs `DriverAnalysis`, extracting the driver assigned to each
//! component (simple expressions, bidirectional assignments, `if`
//! conditionals, `match` drivers), recording driver type/location and
//! reporting diagnostics.

use std::sync::Arc;

use bstr::BStr;
use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::analysis::component::{ComponentAnalysis, ComponentId};
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType, Flow};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticPayload};
use crate::syntax::ast::{AstNode, if_arm_children, match_arm_children};
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct DriverAnalysis {
    drivers: IndexMap<ComponentId, Vec<Driver>>,
    /// Location of the RHS expression of each `initial` statement, keyed by
    /// the targeted component. Initial statements are not drivers and never
    /// appear in `drivers`.
    initials: IndexMap<ComponentId, Vec<Location>>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub enum Driver {
    Expr(DriverType, Location),
    Bidirectional(Location),
    If(DriverIf),
    Match(DriverMatch),
    /// A `when`-clause position in which the component is *not* driven.
    ///
    /// `collect_block_drivers` builds one `Driver::If` per driven component,
    /// so a clause that assigns to some other component must still be
    /// represented for this one, or the clause's guard is silently dropped
    /// (making later clauses and the `else` arm unconditional). Lowering
    /// treats `Absent` as "no assignment here": sequential contexts hold the
    /// register's value, combinational contexts drive X.
    Absent,
}

impl Driver {
    pub fn driver_type(&self) -> DriverType {
        match self {
            Driver::Expr(dt, _) => *dt,
            Driver::Bidirectional(_) => DriverType::Continuous,
            Driver::If(driver_if) => driver_if.driver_type,
            Driver::Match(driver_match) => driver_match.driver_type,
            // `Absent` never appears at the top level and is skipped by
            // driver-type inference; this value is never observed.
            Driver::Absent => DriverType::Continuous,
        }
    }

    pub fn location(&self) -> Option<Location> {
        match self {
            Driver::Expr(_, loc) => Some(loc.clone()),
            Driver::Bidirectional(loc) => Some(loc.clone()),
            Driver::If(driver_if) => driver_if.clauses.first().map(|(loc, _)| loc.clone()),
            Driver::Match(driver_match) => Some(driver_match.subject.clone()),
            Driver::Absent => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DriverIf {
    pub driver_type: DriverType,
    pub clauses: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}

#[derive(Debug, Clone)]
pub struct DriverMatch {
    pub driver_type: DriverType,
    pub subject: Location,
    pub arms: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}

impl DriverAnalysis {
    pub fn drivers(&self) -> &IndexMap<ComponentId, Vec<Driver>> {
        &self.drivers
    }

    pub fn initials(&self) -> &IndexMap<ComponentId, Vec<Location>> {
        &self.initials
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    #[allow(unused)]
    pub fn dump(&self, component_analysis: &ComponentAnalysis) {
        let mut entries: Vec<_> = self.drivers.iter().collect();
        entries.sort_by_key(|(comp_id, _)| {
            component_analysis
                .component(**comp_id)
                .map(|c| c.path())
                .unwrap_or_default()
        });
        for (comp_id, drivers) in entries {
            let name = component_analysis
                .component(*comp_id)
                .map(|c| c.path().to_str_lossy().into_owned())
                .unwrap_or_else(|| format!("{comp_id:?}"));
//            let drivertype = match c {
//            };
            eprintln!("{name}");
            for driver in drivers {
                dump_driver(driver, 1);
            }
        }
    }
}

fn dump_driver(driver: &Driver, level: usize) {
    let pad = "    ".repeat(level);
    match driver {
        Driver::Expr(driver_type, loc) => eprintln!("{pad} {driver_type:?}{loc:?}"),
        Driver::Bidirectional(loc) => eprintln!("{pad} Bidirectional{loc:?}"),
        Driver::If(driver_if) => {
            eprintln!("{pad}if:");
            for (cond_loc, sub_driver) in driver_if.clauses.iter() {
                eprintln!("{pad}  when {cond_loc:?}:");
                dump_driver(sub_driver, level + 1);
            }
            if let Some(else_driver) = &driver_if.else_clause {
                eprintln!("{pad}  else:");
                dump_driver(else_driver, level + 1);
            }
        }
        Driver::Match(driver_match) => {
            eprintln!("{pad}match {:?}:", driver_match.subject);
            for (pat_loc, sub_driver) in &driver_match.arms {
                eprintln!("{pad}  arm {pat_loc:?}:");
                dump_driver(sub_driver, level + 1);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                eprintln!("{pad}  else:");
                dump_driver(else_driver, level + 1);
            }
        }
        Driver::Absent => eprintln!("{pad} (absent)"),
    }
}

pub(crate) fn build_driver_analysis(
    builder: &mut Builder,
    symbol_id: SymbolId,
) -> Arc<DriverAnalysis> {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(symbol_id).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(symbol_id);

    let mut initials = IndexMap::new();
    // The name of the platform bound by the module's `for` clause, when
    // platform ports were synthesized into this module.  Used to
    // explain `DriveUndeclaredPlatformPort` diagnostics.
    let platform_name = component_analysis
        .platform()
        .map(|platform_id| {
            symboltable.symbol(platform_id).name().to_owned().into()
        });
    let (mut drivers, diagnostics) = collect_block_drivers(item_ast.children(), &component_analysis, platform_name.as_ref(), None, false, &mut initials);

    // Ports synthesized from platform `default <EXPR>` clauses have no
    // driver statement in the module's own AST: their driver is the
    // platform's default expression, lowered verbatim. The expression
    // node lives in the platform's parsing; every consumer of a driver
    // already resolves expressions through the driver's `Location`, so
    // a cross-package location needs no special handling.
    for (_path, component) in component_analysis.components() {
        if !component.synthetic() {
            continue;
        }
        if component.kind() != Some(ComponentKind::Outgoing) {
            continue;
        }
        let Some(expr_location) = component.default_expr() else { continue };
        // The platform default is a *default*: it applies only when the
        // design provides no driver of its own for the synthesized port
        // (e.g. a driver written inside a `let` body).  When the design
        // does drive the port, the default is overridden, not merged.
        let entry = drivers.entry(component.id()).or_default();
        if entry.is_empty() {
            entry.push(Driver::Expr(DriverType::Continuous, expr_location));
        }
    }

//    driver_analysis.dump(&component_analysis);

    Arc::new(DriverAnalysis {
        drivers,
        initials,
        diagnostics,
    })
}

/// Walk an expression subtree and emit a `NotIt` warning for every
/// `ExprReference` whose path starts with `ctx.` or equals `ctx` exactly.
/// These paths should instead use the `it` keyword.
///
/// Uses node IDs for traversal (not owned nodes) to avoid lifetime issues.
fn collect_not_it_in_expr(
    expr: &AstNode<'_>,
    ctx: &BStr,
    diagnostics: &mut Vec<Diagnostic>,
) {
    use crate::syntax::ast::AstNodeId;
    let parsing = expr.parsing;
    let mut ctx_dot = ctx.to_owned();
    ctx_dot.push(b'.');
    let mut stack: Vec<AstNodeId> = vec![expr.id()];
    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(path) = node.path() {
                let path_str = parsing.string(path);
                if path_str == ctx.as_bytes()
                    || path_str.starts_with(ctx_dot.as_bytes())
                {
                    diagnostics.push(Diagnostic::new(
                        node.region(),
                        diagnostics::NotIt {
                            component: ctx.to_owned(),
                        },
                    ));
                }
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
}

/// Emits a `MultipleDrivers` diagnostic for every component driven more
/// than once within a single `when` clause or `match` arm body.
///
/// The per-clause driver map keeps all drivers for a component, but the
/// clause is later collapsed to a single `Driver` via `.first()`, so this
/// is the only point at which intra-clause driver conflicts are visible.
/// (Top-level conflicts are caught separately in `check_drivers.rs` by
/// counting the drivers in `DriverAnalysis.drivers()`.)
fn flag_duplicate_clause_drivers(
    body_drivers: &IndexMap<ComponentId, Vec<Driver>>,
    component_analysis: &ComponentAnalysis,
    stmt: &AstNode<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (comp_id, drivers) in body_drivers {
        if drivers.len() > 1 {
            let Some(component) = component_analysis.component(*comp_id) else { continue };
            if matches!(component.kind(), Some(ComponentKind::Bus) | Some(ComponentKind::Net)) {
                continue;
            }
            let path = component.path();
            for driver in drivers {
                let Some(location) = driver.location() else { continue };
                let region = stmt.parsing.ast_node(location.ast_node_id()).region();
                diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::MultipleDrivers {
                        target: path.clone(),
                    },
                ));
            }
        }
    }
}

fn collect_block_drivers(
    stmts: Vec<AstNode<'_>>,
    component_analysis: &ComponentAnalysis,
    platform_name: Option<&BString>,
    it_context: Option<&BStr>,
    in_conditional: bool,
    initials: &mut IndexMap<ComponentId, Vec<Location>>,
) -> (IndexMap<ComponentId, Vec<Driver>>, Vec<Diagnostic>) {
    let mut result: IndexMap<ComponentId, Vec<Driver>> = IndexMap::new();
    let mut diagnostics: Vec<Diagnostic> = vec![];

    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
                let is_initial = matches!(stmt.payload(), AstNodePayload::InitialDriver);
                let Some(target) = stmt.target() else { continue };
                let mut target_str = stmt.parsing.string(target).to_owned();

                if is_initial && in_conditional {
                    diagnostics.push(Diagnostic::new(
                        stmt.region(),
                        diagnostics::InitialInsideIfMatch {
                            target: target_str,
                        },
                    ));
                    continue;
                }

                if let Some(ctx) = it_context {
                    if target_str.starts_with(b"it.") {
                        let suffix = target_str[3..].to_owned();
                        target_str.clear();
                        target_str.extend_from_slice(ctx);
                        target_str.push(b'.');
                        target_str.extend_from_slice(&suffix);
                    } else if target_str == b"it" {
                        target_str = ctx.to_owned();
                    } else {
                        let mut ctx_dot = ctx.to_owned();
                        ctx_dot.push(b'.');
                        if target_str == ctx.as_bytes()
                            || target_str.starts_with(ctx_dot.as_bytes())
                        {
                            diagnostics.push(Diagnostic::new(
                                stmt.child(0).region(),
                                diagnostics::NotIt {
                                    component: ctx.to_owned(),
                                },
                            ));
                        }
                    }
                    if let Some(expr_node) = stmt.driver() {
                        collect_not_it_in_expr(&expr_node, ctx, &mut diagnostics);
                    }
                } else if target_str.starts_with(b"it.") || target_str == b"it" {
                    diagnostics.push(Diagnostic::new(
                        stmt.region(),
                        DiagnosticPayload::ItNotInItBlock,
                    ));
                }
                let Some(component) = component_analysis.resolve(target_str.as_bstr()) else {
                    if !target_str.starts_with(b"it.") && target_str != b"it" {
                        diagnostics.push(Diagnostic::new(
                            stmt.region(),
                            diagnostics::UnresolvedComponent {
                                path: target_str,
                            },
                        ));
                    }
                    continue;
                };

                // A platform port synthesized into this module (see
                // `build_component_analysis`) may be left untouched —
                // the platform `default` drives it — but an explicit
                // driver requires the port to be declared in the mod
                // def.  The driver is not recorded, so the platform
                // default remains in effect and no undriven-port
                // diagnostics cascade from the error.
                if component.synthetic() {
                    diagnostics.push(Diagnostic::new(
                        stmt.region(),
                        diagnostics::DriveUndeclaredPlatformPort {
                            platform: platform_name.cloned().unwrap_or_default(),
                            port: target_str.clone(),
                        },
                    ));
                    continue;
                }

                // `initial` statements are not drivers.
                if is_initial {
                    let expr = stmt.driver().unwrap().location();
                    initials.entry(component.id()).or_default().push(expr);
                    continue;
                }

                let driver_type = stmt.driver_type().unwrap();

                // For latched drivers, flag reg <= reg as redundant.
                if driver_type == DriverType::Latched {
                    if let Some(rhs_node) = stmt.driver() {
                        if let Some(rhs_path) = rhs_node.path() {
                            let mut rhs_str = stmt.parsing.string(rhs_path).to_owned();
                            if let Some(ctx) = it_context {
                                if rhs_str.starts_with(b"it.") {
                                    let suffix = rhs_str[3..].to_owned();
                                    rhs_str.clear();
                                    rhs_str.extend_from_slice(ctx);
                                    rhs_str.push(b'.');
                                    rhs_str.extend_from_slice(&suffix);
                                } else if rhs_str == b"it" {
                                    rhs_str = ctx.to_owned();
                                }
                            }
                            if rhs_str == target_str {
                                diagnostics.push(Diagnostic::new(
                                    stmt.region(),
                                    diagnostics::RedundantDriver {
                                        path: target_str.clone(),
                                    },
                                ));
                            }
                        }
                    }
                }

                let expr = stmt.driver().unwrap().location();
                result.entry(component.id()).or_default().push(Driver::Expr(driver_type, expr));
            }
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);
                let Some(lhs_path) = lhs_node.path() else { continue };
                let Some(rhs_path) = rhs_node.path() else { continue };
                let mut lhs_str = parsing.string(lhs_path).to_owned();
                let mut rhs_str = parsing.string(rhs_path).to_owned();

                if let Some(ctx) = it_context {
                    if lhs_str.starts_with(b"it.") {
                        let suffix = lhs_str[3..].to_owned();
                        lhs_str.clear();
                        lhs_str.extend_from_slice(ctx);
                        lhs_str.push(b'.');
                        lhs_str.extend_from_slice(&suffix);
                    } else if lhs_str == b"it" {
                        lhs_str = ctx.to_owned();
                    } else {
                        let mut ctx_dot = ctx.to_owned();
                        ctx_dot.push(b'.');
                        if lhs_str == ctx.as_bytes()
                            || lhs_str.starts_with(ctx_dot.as_bytes())
                        {
                            diagnostics.push(Diagnostic::new(
                                lhs_node.region(),
                                diagnostics::NotIt {
                                    component: ctx.to_owned(),
                                },
                            ));
                        }
                    }
                    if rhs_str.starts_with(b"it.") {
                        let suffix = rhs_str[3..].to_owned();
                        rhs_str.clear();
                        rhs_str.extend_from_slice(ctx);
                        rhs_str.push(b'.');
                        rhs_str.extend_from_slice(&suffix);
                    } else if rhs_str == b"it" {
                        rhs_str = ctx.to_owned();
                    } else {
                        let mut ctx_dot = ctx.to_owned();
                        ctx_dot.push(b'.');
                        if rhs_str == ctx.as_bytes()
                            || rhs_str.starts_with(ctx_dot.as_bytes())
                        {
                            diagnostics.push(Diagnostic::new(
                                rhs_node.region(),
                                diagnostics::NotIt {
                                    component: ctx.to_owned(),
                                },
                            ));
                        }
                    }
                } else {
                    if lhs_str.starts_with(b"it.") || lhs_str == b"it" {
                        diagnostics.push(Diagnostic::new(
                            lhs_node.region(),
                            DiagnosticPayload::ItNotInItBlock,
                        ));
                    }
                    if rhs_str.starts_with(b"it.") || rhs_str == b"it" {
                        diagnostics.push(Diagnostic::new(
                            rhs_node.region(),
                            DiagnosticPayload::ItNotInItBlock,
                        ));
                    }
                }

                let lhs_prefix = format!("{}.", lhs_str.to_str_lossy());
                let rhs_prefix = format!("{}.", rhs_str.to_str_lossy());

                for (lhs_full_path, lhs_component) in component_analysis.components() {
                    let lhs_path_str = lhs_full_path.to_str_lossy();
                    if !lhs_path_str.starts_with(&lhs_prefix) {
                        continue;
                    }

                    let suffix = &lhs_path_str[lhs_prefix.len()..];
                    let rhs_full_path = BString::from(format!("{}{}", rhs_prefix, suffix).into_bytes());

                    let Some(rhs_component) = component_analysis.resolve(rhs_full_path.as_bstr()) else {
                        continue;
                    };

                    let location = stmt.location();
                    match (lhs_component.flow(), rhs_component.flow()) {
                        (Flow::Source, Flow::Sink) => {
                            // Driving a synthesized platform port through a
                            // bidirectional connection requires declaring it
                            // (same rule as explicit drivers).
                            if rhs_component.synthetic() {
                                diagnostics.push(Diagnostic::new(
                                    stmt.region(),
                                    diagnostics::DriveUndeclaredPlatformPort {
                                        platform: platform_name.cloned().unwrap_or_default(),
                                        port: rhs_full_path,
                                    },
                                ));
                            } else {
                                result.entry(rhs_component.id()).or_default()
                                    .push(Driver::Bidirectional(location));
                            }
                        }
                        (Flow::Sink, Flow::Source) => {
                            if lhs_component.synthetic() {
                                diagnostics.push(Diagnostic::new(
                                    stmt.region(),
                                    diagnostics::DriveUndeclaredPlatformPort {
                                        platform: platform_name.cloned().unwrap_or_default(),
                                        port: lhs_full_path,
                                    },
                                ));
                            } else {
                                result.entry(lhs_component.id()).or_default()
                                    .push(Driver::Bidirectional(location));
                            }
                        }
                        _ => {}
                    }
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                // The It-block is found by payload rather than by index, so
                // the optional GenericsParams child of a type-argument
                // instantiation does not shift it out from under us.
                if let Some(it_block) = children
                    .iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It))
                {
                    let block = &it_block.children()[0];

                    if block.children().is_empty() {
                        diagnostics.push(Diagnostic::new(
                            it_block.region(),
                            DiagnosticPayload::EmptyDriverBlock,
                        ));
                        continue;
                    }

                    let name_with_context = if let Some(ctx) = it_context {
                        let mut new_name = BString::from(ctx);
                        new_name.push(b'.');
                        new_name.extend_from_slice(name);
                        new_name
                    } else {
                        BString::from(name)
                    };

                    let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, platform_name, Some(name_with_context.as_bstr()), in_conditional, initials);
                    for (comp_id, mut drivers) in block_drivers {
                        result.entry(comp_id).or_default().append(&mut drivers);
                    }
                    diagnostics.append(&mut block_diags);
                }
            }
            AstNodePayload::Socket(socket) => {
                let name = stmt.parsing.string(socket.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];

                        if block.children().is_empty() {
                            diagnostics.push(Diagnostic::new(
                                child.region(),
                                DiagnosticPayload::EmptyDriverBlock,
                            ));
                            continue;
                        }

                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };

                        let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, platform_name, Some(name_with_context.as_bstr()), in_conditional, initials);
                        for (comp_id, mut drivers) in block_drivers {
                            result.entry(comp_id).or_default().append(&mut drivers);
                        }
                        diagnostics.append(&mut block_diags);
                    }
                }
            }
            AstNodePayload::Component(component) => {
                let name = stmt.parsing.string(component.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];

                        if block.children().is_empty() {
                            diagnostics.push(Diagnostic::new(
                                child.region(),
                                DiagnosticPayload::EmptyDriverBlock,
                            ));
                            continue;
                        }

                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };

                        let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, platform_name, Some(name_with_context.as_bstr()), in_conditional, initials);
                        for (comp_id, mut drivers) in block_drivers {
                            result.entry(comp_id).or_default().append(&mut drivers);
                        }
                        diagnostics.append(&mut block_diags);
                    }
                }
            }
            AstNodePayload::If => {
                // Each `when` arm contributes (guard, body) and each `else`
                // arm contributes (None, body).  `if_arm_children` pairs the
                // children by the grammar's encoding (2 per `when`, 1 per
                // `else`); an `is_expr()` test cannot be used to tell arms
                // apart because an `else` body may itself be an `If`/`Match`
                // node, which `is_expr()` reports as an expression.
                let children = stmt.children();
                let mut clause_drivers: Vec<(Location, IndexMap<ComponentId, Vec<Driver>>)> = vec![];
                let mut else_drivers: IndexMap<ComponentId, Vec<Driver>> = IndexMap::new();

                for (guard, body) in if_arm_children(&children) {
                    let (body_drivers, mut body_diags) = if matches!(body.payload(), AstNodePayload::ArmBlock) {
                        collect_block_drivers(body.children(), component_analysis, platform_name, it_context, true, initials)
                    } else {
                        // nested if/match or bare expr - recurse as single stmt
                        collect_block_drivers(vec![body.clone()], component_analysis, platform_name, it_context, true, initials)
                    };
                    diagnostics.append(&mut body_diags);
                    flag_duplicate_clause_drivers(&body_drivers, component_analysis, &stmt, &mut diagnostics);
                    match guard {
                        Some(guard) => clause_drivers.push((guard.location(), body_drivers)),
                        None => else_drivers = body_drivers,
                    }
                }

                // Gather all component IDs driven in any clause or the else block.
                let mut all_components: IndexSet<ComponentId> = IndexSet::new();
                for (_, cd) in &clause_drivers {
                    all_components.extend(cd.keys().copied());
                }
                all_components.extend(else_drivers.keys().copied());

                // Build a Driver::If for each driven component and merge into the result.
                // Every clause is kept, even those that do not drive this component:
                // they lower to `Absent`, which preserves the clause's guard
                // (hold / X) instead of dropping it and making later clauses
                // and the `else` arm unconditional.
                for comp_id in all_components {
                    let clauses: Vec<(Location, Box<Driver>)> = clause_drivers.iter()
                        .map(|(cond_loc, cd)| {
                            let sub_driver = cd.get(&comp_id)
                                .and_then(|ds| ds.first())
                                .cloned()
                                .unwrap_or(Driver::Absent);
                            (cond_loc.clone(), Box::new(sub_driver))
                        })
                        .collect();

                    let else_clause = else_drivers.get(&comp_id)
                        .and_then(|ds| ds.first())
                        .map(|d| Box::new(d.clone()));

                    // Infer the driver type from the first non-`Absent` sub-driver.
                    let driver_type = clauses.iter()
                        .filter(|(_, d)| !matches!(d.as_ref(), Driver::Absent))
                        .map(|(_, d)| d.driver_type())
                        .next()
                        .or_else(|| else_clause.as_ref().map(|d| d.driver_type()))
                        .unwrap_or(DriverType::Continuous);

                    result.entry(comp_id).or_default().push(Driver::If(DriverIf {
                        driver_type,
                        clauses,
                        else_clause,
                    }));
                }
            }
            AstNodePayload::Match => {
                // Children: [subject, arm_0, arm_1, ...] where each `with` arm contributes
                // (pattern, block) and each `else` arm contributes (block) only.
                let children = stmt.children();
                let subject = &children[0];

                // Recursively collect drivers from each arm's block.  `case_arm_drivers`
                // holds the per-case (pat_loc, drivers) pairs; `else_arm_drivers` holds
                // the drivers from a trailing `else => { ... }` arm, if any.
                let mut case_arm_drivers: Vec<(Location, IndexMap<ComponentId, Vec<Driver>>)> = vec![];
                let mut else_arm_drivers: Option<IndexMap<ComponentId, Vec<Driver>>> = None;
                for (pat_opt, body) in match_arm_children(&children) {
                    let (body_drivers, mut body_diags) = if matches!(body.payload(), AstNodePayload::ArmBlock) {
                        collect_block_drivers(body.children(), component_analysis, platform_name, it_context, true, initials)
                    } else {
                        // nested if/match or bare expr - recurse as single stmt
                        collect_block_drivers(vec![body.clone()], component_analysis, platform_name, it_context, true, initials)
                    };
                    diagnostics.append(&mut body_diags);
                    flag_duplicate_clause_drivers(&body_drivers, component_analysis, &stmt, &mut diagnostics);
                    match pat_opt {
                        Some(pat) => case_arm_drivers.push((pat.location(), body_drivers)),
                        None => { else_arm_drivers = Some(body_drivers); }
                    }
                }

                // Gather all component IDs driven in any arm (case or else).
                let mut all_components: IndexSet<ComponentId> = IndexSet::new();
                for (_, ad) in &case_arm_drivers {
                    all_components.extend(ad.keys().copied());
                }
                if let Some(else_ad) = &else_arm_drivers {
                    all_components.extend(else_ad.keys().copied());
                }

                // Build a Driver::Match for each driven component and merge into the result.
                for comp_id in all_components {
                    let arms: Vec<(Location, Box<Driver>)> = case_arm_drivers.iter()
                        .filter_map(|(pat_loc, ad)| {
                            ad.get(&comp_id)?.first().map(|d| (pat_loc.clone(), Box::new(d.clone())))
                        })
                        .collect();

                    let else_clause = else_arm_drivers.as_ref()
                        .and_then(|ad| ad.get(&comp_id))
                        .and_then(|ds| ds.first())
                        .map(|d| Box::new(d.clone()));

                    let driver_type = arms.first()
                        .map(|(_, d)| d.driver_type())
                        .or_else(|| else_clause.as_ref().map(|d| d.driver_type()))
                        .unwrap_or(DriverType::Continuous);

                    result.entry(comp_id).or_default().push(Driver::Match(DriverMatch {
                        driver_type,
                        subject: subject.location(),
                        arms,
                        else_clause,
                    }));
                }
            }
            AstNodePayload::Let => {
                // A `let` is a pure scoping construct: it contributes no
                // drivers itself, and its body's drivers belong to the
                // enclosing context (unlike an `if`/`match` arm, the
                // block is not conditional).  References to the bound
                // names resolve later, at the typing/sim/Verilog
                // frontiers, by beta-reduction to the binding's RHS.
                let Some(body) = stmt.let_body() else { continue };
                let (body_drivers, mut body_diags) = if matches!(body.payload(), AstNodePayload::ArmBlock) {
                    collect_block_drivers(body.children(), component_analysis, platform_name, it_context, in_conditional, initials)
                } else {
                    // `if`/`match` bodies recurse as a single statement.
                    collect_block_drivers(vec![body], component_analysis, platform_name, it_context, in_conditional, initials)
                };
                diagnostics.append(&mut body_diags);
                for (comp_id, mut drivers) in body_drivers {
                    result.entry(comp_id).or_default().append(&mut drivers);
                }
            }
            _ => {}
        }
    }

    (result, diagnostics)
}
