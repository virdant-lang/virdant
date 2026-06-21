//! Clock domain inference and checking.
//!
//! This module implements the domain checking pass described in SYNC_PLAN.md.
//! It runs after type checking and verifies that clock domains are consistent
//! across drivers, binary operations, and control-flow expressions.
//!
//! Key rules:
//! - Binary/unary operators require operands in the same domain.
//! - Driver LHS and RHS must be in the same domain.
//! - `sync(x)` is the escape hatch: it takes any domain and produces
//!   the expected (target) domain.
//! - `async(x)` always produces the Async domain.
//! - Unknown domains (unannotated signals) are inferred from context.

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::component::ComponentAnalysis;
use crate::common::source::Region;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::{AstNode, match_arm_children, when_arm_children};
use crate::syntax::payload::AstNodePayload;
use crate::types::ClockDomain;

/// The inferred domain of an expression.
#[derive(Clone, Debug)]
enum Dom {
    /// A concrete domain (Async or OnClock).
    Known(ClockDomain),
    /// Result of `sync()` - compatible with any expected domain.
    Sync,
    /// Domain-agnostic (literals, enum variants) - compatible with anything.
    Poly,
    /// Could not be determined.
    Unknown,
}

/// Check clock domain consistency for a module.
/// Returns diagnostics for domain violations (E0707) and uninferrable
/// domains (E0708).
pub(super) fn check_domains(
    component_analysis: &ComponentAnalysis,
    moddef_node: &AstNode,
) -> Vec<Diagnostic> {
    let mut diags = vec![];
    let mut inferred: IndexMap<BString, ClockDomain> = IndexMap::new();

    // Fixpoint iteration: keep running the inference pass until no new
    // domains are inferred.  This handles forward references where a
    // wire is declared before the register it depends on.
    loop {
        let prev_len = inferred.len();
        let prev_diags_len = diags.len();
        walk_stmts(
            &moddef_node.children(),
            component_analysis,
            None,
            &mut inferred,
            &mut diags,
        );
        // Remove diagnostics from this pass; we only keep the final
        // round's diagnostics (domain crossings that persist after
        // all inference is done).
        diags.truncate(prev_diags_len);
        if inferred.len() == prev_len {
            break;
        }
    }

    // Final pass: collect domain crossing diagnostics with all
    // inference completed.
    walk_stmts(
        &moddef_node.children(),
        component_analysis,
        None,
        &mut inferred,
        &mut diags,
    );

    // After inference, report E0708 for every component whose domain is
    // still Unknown (no annotation and could not be inferred from context).
    // Clock/Reset types have clock == None and are exempt.
    let parsing = moddef_node.parsing;
    for (path, component) in component_analysis.components() {
        // Skip submodule ports (paths containing a dot).
        if path.contains(&b'.') {
            continue;
        }
        // Skip components that were inferred during the pass.
        if inferred.contains_key(&path) {
            continue;
        }
        // Only report for components with Unknown domain.
        // None means Clock/Reset or error recovery - not our concern.
        if matches!(component.clock(), Some(ClockDomain::Unknown)) {
            let node = parsing.ast_node(component.location().ast_node_id());
            diags.push(
                diagnostics::UninferrableClock {
                    region: node.region(),
                    name: path.clone(),
                }
                .into(),
            );
        }
    }

    diags
}

/// Walk moddef-level statements and check clock domains in drivers.
fn walk_stmts(
    stmts: &[AstNode],
    ca: &ComponentAnalysis,
    it_ctx: Option<&BStr>,
    inferred: &mut IndexMap<BString, ClockDomain>,
    diags: &mut Vec<Diagnostic>,
) {
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::Driver(_) => {
                check_driver(stmt, ca, it_ctx, inferred, diags);
            }
            AstNodePayload::BidirectionalDriver => {
                check_bidirectional_driver(stmt, ca, it_ctx, inferred, diags);
            }
            AstNodePayload::Component(component) => {
                let name = stmt.parsing().string(component.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        walk_stmts(
                            &block.children(),
                            ca,
                            Some(name.as_bstr()),
                            inferred,
                            diags,
                        );
                    }
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing().string(submodule.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        walk_stmts(
                            &block.children(),
                            ca,
                            Some(name.as_bstr()),
                            inferred,
                            diags,
                        );
                    }
                }
            }
            AstNodePayload::ModDefStmtWhen => {
                // Determine the expected domain from the enclosing it-context
                // (the component being driven).  Guards should be in the same
                // domain as the driver target.
                let target_dom = it_ctx.map(|ctx| get_domain(ctx, ca, inferred));
                let expected = match &target_dom {
                    Some(Dom::Known(d)) => Some(d.clone()),
                    _ => None,
                };

                let children = stmt.children();
                let mut idx = 0;
                while let Some(first) = children.get(idx) {
                    if first.is_expr() {
                        // case arm: guard + body.
                        // Infer the guard's domain using the target's domain
                        // as expected, so references in the guard get inferred.
                        let guard_dom = infer_expr(
                            first,
                            expected.as_ref(),
                            ca,
                            it_ctx,
                            inferred,
                            diags,
                        );

                        // If the target is Unknown but the guard is Known,
                        // infer the target's domain from the guard.
                        if matches!(target_dom, Some(Dom::Unknown)) {
                            if let Dom::Known(d) = &guard_dom {
                                if let Some(ctx) = it_ctx {
                                    inferred.insert(ctx.to_owned(), d.clone());
                                }
                            }
                        }

                        if let Some(body) = children.get(idx + 1) {
                            walk_body(body, ca, it_ctx, inferred, diags);
                        }
                        idx += 2;
                    } else {
                        // else arm: body only
                        walk_body(first, ca, it_ctx, inferred, diags);
                        idx += 1;
                    }
                }
            }
            AstNodePayload::ModDefStmtMatch => {
                let children = stmt.children();
                // The match subject's domain should match the enclosing
                // component's domain (if any).
                if let Some(subject) = children.first() {
                    let expected = it_ctx.and_then(|ctx| {
                        let dom = get_domain(ctx, ca, inferred);
                        match dom {
                            Dom::Known(d) => Some(d),
                            _ => None,
                        }
                    });
                    let _ = infer_expr(
                        subject,
                        expected.as_ref(),
                        ca,
                        it_ctx,
                        inferred,
                        diags,
                    );
                }
                for (_pat, body) in match_arm_children(&children) {
                    walk_body(body, ca, it_ctx, inferred, diags);
                }
            }
            AstNodePayload::ModDefStmtBlock => {
                walk_stmts(&stmt.children(), ca, it_ctx, inferred, diags);
            }
            _ => {}
        }
    }
}

fn walk_body(
    body: &AstNode,
    ca: &ComponentAnalysis,
    it_ctx: Option<&BStr>,
    inferred: &mut IndexMap<BString, ClockDomain>,
    diags: &mut Vec<Diagnostic>,
) {
    match body.payload() {
        AstNodePayload::ModDefStmtBlock => {
            walk_stmts(&body.children(), ca, it_ctx, inferred, diags);
        }
        _ => {
            walk_stmts(std::slice::from_ref(body), ca, it_ctx, inferred, diags);
        }
    }
}

/// Check domain compatibility for a single driver: `target := expr` or `target <= expr`.
fn check_driver(
    stmt: &AstNode,
    ca: &ComponentAnalysis,
    it_ctx: Option<&BStr>,
    inferred: &mut IndexMap<BString, ClockDomain>,
    diags: &mut Vec<Diagnostic>,
) {
    let parsing = stmt.parsing();

    // Resolve the target (LHS) path.
    let target_node = stmt.child(0);
    let Some(path_interned) = target_node.path() else {
        return;
    };
    let target_name = parsing.string(path_interned).to_owned();
    let target_resolved = resolve_name(target_name.as_bstr(), it_ctx);

    // Get the target's domain.
    let target_dom = get_domain(target_resolved.as_bstr(), ca, inferred);

    // Get the RHS expression.
    let Some(rhs) = stmt.driver() else {
        return;
    };

    // The expected domain for the RHS is the target's domain.
    let expected = match &target_dom {
        Dom::Known(d) => Some(d.clone()),
        _ => None,
    };

    let rhs_dom = infer_expr(
        &rhs,
        expected.as_ref(),
        ca,
        it_ctx,
        inferred,
        diags,
    );

    // Bidirectional inference: if the target is Unknown but the RHS is
    // Known, infer the target's domain from the RHS.
    if matches!(target_dom, Dom::Unknown) {
        if let Dom::Known(d) = &rhs_dom {
            inferred.insert(target_resolved.clone(), d.clone());
        }
    }

    // Check compatibility between target and RHS.
    check_compat(&target_dom, &rhs_dom, &target_node.region(), &rhs.region(), diags);
}

/// Check domain compatibility for a bidirectional driver: `lhs :=: rhs`.
fn check_bidirectional_driver(
    stmt: &AstNode,
    ca: &ComponentAnalysis,
    it_ctx: Option<&BStr>,
    inferred: &mut IndexMap<BString, ClockDomain>,
    diags: &mut Vec<Diagnostic>,
) {
    let parsing = stmt.parsing();
    let lhs_node = stmt.child(0);
    let rhs_node = stmt.child(1);

    let Some(lhs_path) = lhs_node.path() else { return };
    let Some(rhs_path) = rhs_node.path() else { return };

    let lhs_name = parsing.string(lhs_path).to_owned();
    let rhs_name = parsing.string(rhs_path).to_owned();
    let lhs_resolved = resolve_name(lhs_name.as_bstr(), it_ctx);
    let rhs_resolved = resolve_name(rhs_name.as_bstr(), it_ctx);

    let lhs_dom = get_domain(lhs_resolved.as_bstr(), ca, inferred);
    let rhs_dom = get_domain(rhs_resolved.as_bstr(), ca, inferred);

    check_compat(&lhs_dom, &rhs_dom, &lhs_node.region(), &rhs_node.region(), diags);
}

/// Resolve `it` / `it.x` references to the enclosing context name.
fn resolve_name(name: &BStr, it_ctx: Option<&BStr>) -> BString {
    if name == b"it" || name.starts_with(b"it.") {
        if let Some(ctx) = it_ctx {
            if name == b"it" {
                return ctx.to_owned();
            } else {
                let suffix: BString = name[3..].to_owned().into();
                let mut result = ctx.to_owned();
                result.push(b'.');
                result.extend_from_slice(&suffix);
                return result;
            }
        }
    }
    name.to_owned()
}

/// Get the domain of a component by path.
fn get_domain(
    path: &BStr,
    ca: &ComponentAnalysis,
    inferred: &IndexMap<BString, ClockDomain>,
) -> Dom {
    // Check inferred domains first.
    if let Some(d) = inferred.get(path) {
        return Dom::Known(d.clone());
    }

    // Check component analysis.
    if let Some(component) = ca.resolve(path) {
        return match component.clock() {
            Some(ClockDomain::Unknown) | None => Dom::Unknown,
            Some(d) => Dom::Known(d.clone()),
        };
    }

    Dom::Unknown
}

/// Infer the domain of an expression.
///
/// `expected` is the domain expected by the surrounding context
/// (e.g., the driver target's domain). When a reference has Unknown
/// domain and an expected domain is available, the reference is
/// inferred to the expected domain.
fn infer_expr(
    node: &AstNode,
    expected: Option<&ClockDomain>,
    ca: &ComponentAnalysis,
    it_ctx: Option<&BStr>,
    inferred: &mut IndexMap<BString, ClockDomain>,
    diags: &mut Vec<Diagnostic>,
) -> Dom {
    match node.payload() {
        AstNodePayload::ExprReference => {
            if let Some(path_interned) = node.path() {
                let name = node.parsing().string(path_interned).to_owned();
                let resolved = resolve_name(name.as_bstr(), it_ctx);

                // Check inferred map first.
                if let Some(d) = inferred.get(resolved.as_bstr()) {
                    return Dom::Known(d.clone());
                }

                // Check component analysis.
                if let Some(component) = ca.resolve(resolved.as_bstr()) {
                    match component.clock() {
                        Some(ClockDomain::Unknown) | None => {
                            // Unknown domain - try to infer from expected.
                            if let Some(exp) = expected {
                                if !matches!(exp, ClockDomain::Unknown) {
                                    inferred.insert(resolved.clone(), exp.clone());
                                    return Dom::Known(exp.clone());
                                }
                            }
                            return Dom::Unknown;
                        }
                        Some(d) => return Dom::Known(d.clone()),
                    }
                }
                return Dom::Unknown;
            }
            Dom::Unknown
        }

        // Literals and enum variants are domain-polymorphic.
        AstNodePayload::ExprBitLit(_)
        | AstNodePayload::ExprWordLit(_)
        | AstNodePayload::ExprStrLit(_)
        | AstNodePayload::ExprEnumerant(_) => Dom::Poly,

        AstNodePayload::ExprHole | AstNodePayload::ExprDontcare => Dom::Poly,

        AstNodePayload::ExprParen => {
            infer_expr(&node.child(0), expected, ca, it_ctx, inferred, diags)
        }

        AstNodePayload::ExprBinOp(_) => {
            let left = &node.child(0);
            let right = &node.child(1);
            let left_dom = infer_expr(left, expected, ca, it_ctx, inferred, diags);
            let right_dom = infer_expr(right, expected, ca, it_ctx, inferred, diags);

            match (&left_dom, &right_dom) {
                // Both known and different: E0707.
                (Dom::Known(l), Dom::Known(r)) if l != r => {
                    diags.push(
                        diagnostics::DomainCrossing {
                            region: node.region(),
                            left_region: left.region(),
                            left_domain: l.clone(),
                            right_region: right.region(),
                            right_domain: r.clone(),
                        }
                        .into(),
                    );
                    Dom::Unknown
                }
                // One known: result is that domain.
                (Dom::Known(d), _) | (_, Dom::Known(d)) => Dom::Known(d.clone()),
                // Sync propagates.
                (Dom::Sync, _) | (_, Dom::Sync) => Dom::Sync,
                // Poly is transparent.
                (Dom::Poly, other) | (other, Dom::Poly) => other.clone(),
                _ => Dom::Unknown,
            }
        }

        AstNodePayload::ExprUnOp(_) => {
            infer_expr(&node.child(0), expected, ca, it_ctx, inferred, diags)
        }

        AstNodePayload::ExprWhen => {
            let children = node.children();
            let arms = when_arm_children(&children);
            let mut result = Dom::Unknown;
            for (guard, body) in &arms {
                if let Some(g) = guard {
                    let g_dom = infer_expr(g, expected, ca, it_ctx, inferred, diags);
                    result = combine(result, g_dom, &node.region(), diags);
                }
                let b_dom = infer_expr(body, expected, ca, it_ctx, inferred, diags);
                result = combine(result, b_dom, &node.region(), diags);
            }
            result
        }

        AstNodePayload::ExprMatch => {
            let children = node.children();
            let mut result = Dom::Unknown;
            // Subject expression: no expected domain constraint.
            if let Some(subject) = children.first() {
                let s_dom = infer_expr(subject, None, ca, it_ctx, inferred, diags);
                result = s_dom;
            }
            for (_pat, body) in match_arm_children(&children) {
                let b_dom = infer_expr(body, expected, ca, it_ctx, inferred, diags);
                result = combine(result, b_dom, &node.region(), diags);
            }
            result
        }

        // sync(x): argument can be any domain; result takes the expected domain.
        AstNodePayload::ExprSync => {
            let _ = infer_expr(&node.child(0), None, ca, it_ctx, inferred, diags);
            Dom::Sync
        }

        // async(x): result is always Async, regardless of expected.
        AstNodePayload::ExprAsync => {
            let _ = infer_expr(&node.child(0), None, ca, it_ctx, inferred, diags);
            Dom::Known(ClockDomain::Async)
        }

        // Field access, indexing, ascription: domain comes from the subject.
        AstNodePayload::ExprField(_)
        | AstNodePayload::ExprIndex(_)
        | AstNodePayload::ExprIndexDyn
        | AstNodePayload::ExprIndexRange(_)
        | AstNodePayload::ExprAs => {
            infer_expr(&node.child(0), expected, ca, it_ctx, inferred, diags)
        }

        // Function calls (mux, zext, sext, word, etc.):
        // All arguments should share the same domain.
        // Child 0 is the Ofness node; arguments start at child 1.
        AstNodePayload::ExprFn => {
            let children = node.children();
            let mut result = Dom::Unknown;
            for child in children.iter().skip(1) {
                if !child.is_expr() {
                    continue;
                }
                let c_dom = infer_expr(child, expected, ca, it_ctx, inferred, diags);
                result = combine(result, c_dom, &node.region(), diags);
            }
            result
        }

        // Constructor calls (@Ctor(args)): domain comes from arguments.
        AstNodePayload::ExprCtor(_) => {
            let mut result = Dom::Unknown;
            for child in node.children() {
                if !child.is_expr() {
                    continue;
                }
                let c_dom = infer_expr(&child, expected, ca, it_ctx, inferred, diags);
                result = combine(result, c_dom, &node.region(), diags);
            }
            result
        }

        // Struct construction ${...}: domain comes from field values.
        AstNodePayload::ExprStruct => {
            let mut result = Dom::Unknown;
            for child in node.children() {
                if !child.is_expr() {
                    continue;
                }
                let c_dom = infer_expr(&child, expected, ca, it_ctx, inferred, diags);
                result = combine(result, c_dom, &node.region(), diags);
            }
            result
        }

        // ExprWord is handled as ExprFn in practice, but handle it just in case.
        AstNodePayload::ExprWord => {
            let mut result = Dom::Unknown;
            for child in node.children() {
                if !child.is_expr() {
                    continue;
                }
                let c_dom = infer_expr(&child, expected, ca, it_ctx, inferred, diags);
                result = combine(result, c_dom, &node.region(), diags);
            }
            result
        }

        // Unknown expression types: try children, default to Unknown.
        _ => {
            for child in node.children() {
                if child.is_expr() {
                    let c_dom = infer_expr(&child, expected, ca, it_ctx, inferred, diags);
                    if matches!(c_dom, Dom::Known(_) | Dom::Sync) {
                        return c_dom;
                    }
                }
            }
            Dom::Unknown
        }
    }
}

/// Combine two domains, reporting conflicts between known domains.
fn combine(a: Dom, b: Dom, region: &Region, diags: &mut Vec<Diagnostic>) -> Dom {
    match (&a, &b) {
        (Dom::Known(d1), Dom::Known(d2)) if d1 != d2 => {
            diags.push(
                diagnostics::DomainCrossing {
                    region: region.clone(),
                    left_region: region.clone(),
                    left_domain: d1.clone(),
                    right_region: region.clone(),
                    right_domain: d2.clone(),
                }
                .into(),
            );
            Dom::Unknown
        }
        (Dom::Known(d), _) | (_, Dom::Known(d)) => Dom::Known(d.clone()),
        (Dom::Sync, _) | (_, Dom::Sync) => Dom::Sync,
        (Dom::Poly, other) | (other, Dom::Poly) => other.clone(),
        (Dom::Unknown, Dom::Unknown) => Dom::Unknown,
    }
}

/// Check compatibility between a target domain and an RHS domain.
/// Reports E0707 when two known domains conflict.
fn check_compat(
    target: &Dom,
    rhs: &Dom,
    target_region: &Region,
    rhs_region: &Region,
    diags: &mut Vec<Diagnostic>,
) {
    match (target, rhs) {
        // Both known and same: OK.
        (Dom::Known(t), Dom::Known(r)) if t == r => {}

        // Target known, RHS is sync(): OK (sync takes the target domain).
        (Dom::Known(_), Dom::Sync) => {}

        // Target known, RHS is poly (literal): OK.
        (Dom::Known(_), Dom::Poly) => {}

        // Target known, RHS is known but different: E0707.
        (Dom::Known(t), Dom::Known(r)) => {
            diags.push(
                diagnostics::DomainCrossing {
                    region: rhs_region.clone(),
                    left_region: target_region.clone(),
                    left_domain: t.clone(),
                    right_region: rhs_region.clone(),
                    right_domain: r.clone(),
                }
                .into(),
            );
        }

        // All other cases: OK (unknown, poly, sync, etc.)
        _ => {}
    }
}
