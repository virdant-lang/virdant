//! Implements the per-expression domain inference of the `Clocking`
//! engine, synthesizing a clock [`Domain`] for each subexpression from
//! its structure: literals and constants, references (`let`-binding
//! beta-reduction, and pattern-bound variables taking the enclosing
//! match's subject domain), projections, combinational operator
//! joins, and the `if`/`match`/`let` join rules.

use super::*;
use bstr::ByteSlice;
use crate::diagnostics;
use crate::syntax::ast::{
    if_arm_children, match_arm_children, resolve_binding, NameBinding, AstNode,
};
use crate::syntax::payload::AstNodePayload;

impl Clocking {
    /// Infers the domain of a subexpression (memoized).
    pub(super) fn infer(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        if let Some(memo) = self.domains.get(&node.id()) {
            return *memo;
        }
        if !self.in_progress.insert(node.id()) {
            // Degenerate self-reference (e.g. `let x = x`): refuse to
            // loop; the domain stays unknown.
            return None;
        }
        let domain = self.compute(node);
        self.in_progress.shift_remove(&node.id());
        match domain {
            Some(domain) => self.annotate(node.id(), domain),
            None => self.annotate_unknown(node.id()),
        }
        domain
    }

    fn compute(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        match node.payload() {
            // Literals, enumerants, and holes are clock-independent.
            // A hole (`?`) / dontcare adopts whatever domain the
            // surrounding join produces, which is exactly what
            // `Constant` does in the lattice.
            AstNodePayload::ExprBitLit(_)
            | AstNodePayload::ExprWordLit(_)
            | AstNodePayload::ExprStrLit(_)
            | AstNodePayload::ExprEnumerant(_)
            | AstNodePayload::ExprHole
            | AstNodePayload::ExprDontcare => Some(Domain::Constant),

            AstNodePayload::ExprReference => self.infer_reference(node),

            AstNodePayload::ExprParen => {
                let subject = node.subject()?;
                self.infer(&subject)
            }

            // Projections and casts take their subject's domain.
            AstNodePayload::ExprAs
            | AstNodePayload::ExprUnOp(_)
            | AstNodePayload::ExprField(_)
            | AstNodePayload::ExprIndex(_)
            | AstNodePayload::ExprIndexRange(_) => {
                let subject = node.subject()?;
                self.infer(&subject)
            }

            // `bus.tap`: the tapped net's domain.
            AstNodePayload::ExprTap => self.infer_tap(node),

            // Combinational operators: all operands join.
            AstNodePayload::ExprBinOp(_) => {
                let parts = node.children().iter().map(|child| self.infer(child)).collect();
                self.join(node, parts)
            }

            // Concatenation / extension: operands join.
            AstNodePayload::ExprWord | AstNodePayload::ExprZext | AstNodePayload::ExprSext => {
                let parts = node
                    .args()
                    .unwrap_or_default()
                    .iter()
                    .map(|arg| self.infer(arg))
                    .collect();
                self.join(node, parts)
            }

            // Function calls (`mux`, `any`, `all`, `word`, ...): the
            // selector and the data operands all join.  `asclock`
            // makes a clock, which is `Async` and does not constrain
            // its subject.
            AstNodePayload::ExprFn => self.infer_fn(node),

            // A dynamic memory index reads `mem[dyn idx]`: the index
            // joins with the memory's domain; the result is the
            // memory's.
            AstNodePayload::ExprIndexDyn => {
                let children = node.children();
                let subject = node.subject()?;
                let subject_domain = self.infer(&subject)?;
                let index = children.get(1)?;
                let index_domain = self.infer(index);
                self.join(node, vec![Some(subject_domain), index_domain])?;
                Some(subject_domain)
            }

            // Struct construction: all field expressions join.
            AstNodePayload::ExprStruct => {
                let mut parts = vec![];
                for assign in node.children() {
                    let expr = assign.assign_expr()?;
                    parts.push(self.infer(&expr));
                }
                self.join(node, parts)
            }

            // Ctor application: all arguments join.
            AstNodePayload::ExprCtor(_) => {
                let parts = node.children().iter().map(|arg| self.infer(arg)).collect();
                self.join(node, parts)
            }

            AstNodePayload::If => self.infer_if(node),
            AstNodePayload::Match => self.infer_match(node),
            AstNodePayload::Let => self.infer_let(node),

            // A `{ ... }` arm block is a value only in the
            // single-expression form; anything else is a style error
            // reported by the terminator-style pass.
            AstNodePayload::ArmBlock => {
                let children = node.children();
                if children.len() == 1 && children[0].is_expr() {
                    self.infer(&children[0])
                } else {
                    None
                }
            }

            // Not an expression (or a form without domain rules of
            // its own — its subexpressions are reached through the
            // walk).
            _ => None,
        }
    }

    /// The domain of a reference: the referenced component's declared
    /// domain; else, for a bound name, the binding's domain — a
    /// pattern-bound variable takes the subject's domain of the
    /// innermost enclosing match that binds it, and a `let`-bound
    /// name takes its RHS's domain (beta-reduction, as in the
    /// typechecker).  `resolve_binding` applies the lexical shadowing
    /// rule between pattern bindings and `let`s.
    fn infer_reference(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        if let Some(component) = self.component_analysis().component_at(node.location()) {
            return self.component_domain(&component);
        }
        let path = node.path()?;
        let name = node.parsing().string(path);
        let pattern_match = enclosing_pattern_match(node, name);
        match resolve_binding(node, name, pattern_match.as_ref().map(|m| m.id())) {
            Some(NameBinding::Let(rhs)) => self.infer(&rhs),
            Some(NameBinding::Pattern) => {
                let subject = pattern_match?.children().into_iter().next()?;
                self.infer(&subject)
            }
            // A reference the component analysis did not register —
            // the RHS of an `initial` statement is not walked for
            // references — still names a component of this module:
            // resolve it by name.
            None => self
                .component_analysis()
                .resolve(name)
                .and_then(|component| self.component_domain(&component)),
        }
    }

    /// `bus.tap`: the tapped component's domain.
    fn infer_tap(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        let path_node = node.tapped()?;
        let component = self
            .component_analysis()
            .component_at(path_node.location())?;
        self.component_domain(&component)
    }

    fn infer_fn(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        let children = node.children();
        let fn_ofness = children.first()?;
        let AstNodePayload::Ofness(ofness) = fn_ofness.payload() else {
            return None;
        };
        let fn_name = node.parsing().string(ofness.name);
        match fn_name.as_bytes() {
            // `asclock` produces a clock: a fresh clock class of its
            // own (a generating call site is a clock source),
            // independent of its subject's domain.
            b"asclock" => {
                let subject = node.subject()?;
                self.infer(&subject);
                let var = self.next_var;
                self.next_var += 1;
                Some(Domain::sync_var(var))
            }
            _ => {
                let parts = children[1..]
                    .iter()
                    .map(|arg| self.infer(arg))
                    .collect();
                self.join(node, parts)
            }
        }
    }

    /// `if`/`when`: all guards and all arm *result expressions* join
    /// to one domain.  Statement-position arms (blocks of driver
    /// statements) contribute no value; their guards still join with
    /// the other guards.
    fn infer_if(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        let children = node.children();
        let mut parts = vec![];
        for (guard, body) in if_arm_children(&children) {
            if let Some(guard) = guard {
                parts.push(self.infer(guard));
            }
            if !matches!(body.payload(), AstNodePayload::ArmBlock) {
                parts.push(self.infer(body));
            }
        }
        self.join(node, parts)
    }

    /// `match`: the subject and all arm results join to one domain.
    /// Patterns are not expressions and do not participate; the
    /// variables they bind take the subject's domain (see
    /// `infer_reference`).
    fn infer_match(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        let children = node.children();
        let subject = children.first()?;
        let mut parts = vec![self.infer(subject)];
        for (_pat, body) in match_arm_children(&children) {
            if !matches!(body.payload(), AstNodePayload::ArmBlock) {
                parts.push(self.infer(body));
            }
        }
        self.join(node, parts)
    }

    /// A `let`: each binding's RHS is an independent expression with
    /// its own domain; the let's domain is its body's.
    fn infer_let(&mut self, node: &AstNode<'_>) -> Option<Domain> {
        for binding in node.let_bindings() {
            if let Some(rhs) = binding.let_binding_expr() {
                self.infer(&rhs);
            }
        }
        let body = node.let_body()?;
        if matches!(body.payload(), AstNodePayload::ArmBlock) {
            // Statement-position `let` (or a style error): no value.
            return None;
        }
        self.infer(&body)
    }

    /// Joins operand domains into the expression's domain: every
    /// operand must join to one domain.  `Constant` upcasts to
    /// anything and `Sync(c)` upcasts to `Async`, but two distinct
    /// clocks cannot be combined — reported once, at the expression.
    fn join(&mut self, node: &AstNode<'_>, parts: Vec<Option<Domain>>) -> Option<Domain> {
        let mut result: Option<Domain> = None;
        let mut conflicted = false;
        for part in parts.into_iter().flatten() {
            match result {
                None => result = Some(part),
                Some(current) => {
                    if let (Domain::Sync(clock_a), Domain::Sync(clock_b)) = (current, part) {
                        if clock_a != clock_b && !conflicted {
                            conflicted = true;
                            self.diagnostics.push(Diagnostic::new(
                                node.region(),
                                diagnostics::DomainMismatch {
                                    left: self.render_domain(&current),
                                    right: self.render_domain(&part),
                                },
                            ));
                        }
                    } else {
                        result = Some(Domain::lub(&current, &part));
                    }
                }
            }
        }
        if conflicted {
            None
        } else {
            result
        }
    }
}

/// The innermost ancestor of `start` that is a `match` binding `name`
/// in one of its patterns.  A match's own subject is outside the scope
/// of its pattern variables, so a match reached through its subject
/// does not count.
fn enclosing_pattern_match<'p>(start: &AstNode<'p>, name: &bstr::BStr) -> Option<AstNode<'p>> {
    let mut current = start.clone();
    loop {
        let parent = current.parent()?;
        if matches!(parent.payload(), AstNodePayload::Match) {
            let children = parent.children();
            let in_subject = children
                .first()
                .is_some_and(|subject| subject.id() == current.id());
            if !in_subject && match_binds_name(&parent, name) {
                return Some(parent);
            }
        }
        current = parent;
    }
}

/// Whether any arm pattern of the `match` binds `name`.
fn match_binds_name(match_node: &AstNode<'_>, name: &bstr::BStr) -> bool {
    let children = match_node.children();
    match_arm_children(&children).into_iter().any(|(pat, _)| {
        let Some(pat) = pat else { return false };
        let mut bound = vec![];
        pattern_binding_names(pat, &mut bound);
        bound.iter().any(|bound_name| bound_name.as_bstr() == name)
    })
}

/// The variable names a pattern binds: an ident pattern binds its
/// name; a ctor pattern's payload subpatterns bind recursively;
/// enumerant, literal, and dontcare patterns bind nothing.
fn pattern_binding_names(pat: &AstNode<'_>, names: &mut Vec<BString>) {
    match pat.payload() {
        AstNodePayload::PatIdent(ident) => {
            names.push(pat.parsing().string(ident.name).to_owned());
        }
        AstNodePayload::PatCtor(_) => {
            for child in pat.children() {
                pattern_binding_names(&child, names);
            }
        }
        _ => {}
    }
}
