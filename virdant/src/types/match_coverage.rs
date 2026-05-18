use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::DriverType;
use crate::common::{Width, WordValue};
use crate::common::source::Region;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::{AstNode, match_arm_children};
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

pub(crate) fn check_match_coverage(
    builder: &mut Builder,
    match_node: &AstNode<'_>,
    subject_typ: &Type,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let children = match_node.children();
    let arms = match_arm_children(&children);

    // Structural pass: multiple else, else-not-last.
    let mut else_regions: Vec<Region> = Vec::new();
    let mut first_else_idx: Option<usize> = None;
    for (i, (pat, body)) in arms.iter().enumerate() {
        if pat.is_none() {
            else_regions.push(body.region());
            if first_else_idx.is_none() {
                first_else_idx = Some(i);
            }
        }
    }
    for region in else_regions.iter().skip(1) {
        diagnostics.push(diagnostics::MatchMultipleElse { region: region.clone() }.into());
    }
    if let Some(idx) = first_else_idx {
        if idx + 1 < arms.len() {
            diagnostics.push(diagnostics::MatchElseNotLast { region: arms[idx].1.region() }.into());
        }
    }

    let has_else = first_else_idx.is_some();
    let category = subject_category(builder, subject_typ);
    if matches!(category, Category::Other) {
        return;
    }

    // Coverage pass.  For statement-level matches the typing pass does not
    // validate arm patterns against the subject type, so emit shape mismatch
    // diagnostics inline; expression matches are already covered by check_pat.
    let is_stmt_match = matches!(match_node.payload(), AstNodePayload::ModDefStmtMatch);
    let mut overlap: Option<(Region, BString)> = None;
    let mut covered_syms: IndexMap<SymbolId, Region> = IndexMap::new();
    let mut covered_words: IndexMap<WordValue, Region> = IndexMap::new();
    let mut covered_bits: IndexMap<bool, Region> = IndexMap::new();

    for (pat_opt, _body) in &arms {
        let Some(pat) = pat_opt else { continue };
        if overlap.is_some() { break; }
        let parsing = builder.get_parsing(pat.package());
        match pat.payload() {
            AstNodePayload::PatCtor(pat_ctor) => {
                let Type::Usual(typedef_id) = subject_typ else {
                    if is_stmt_match {
                        diagnostics.push(diagnostics::UnresolvedCtor {
                            region: pat.region(),
                            ctor: parsing.string(pat_ctor.name).to_owned(),
                        }.into());
                    }
                    continue;
                };
                let symboltable = builder.get_symboltable();
                let name = parsing.string(pat_ctor.name);
                let Some(sym) = symboltable.slot(*typedef_id, name) else { continue };
                let sym_id = sym.id();
                if covered_syms.contains_key(&sym_id) {
                    overlap = Some((pat.region(), format!("@{}", name.to_str_lossy()).into()));
                } else {
                    covered_syms.insert(sym_id, pat.region());
                }
            }
            AstNodePayload::PatEnumerant(pat_enum) => {
                let Type::Usual(typedef_id) = subject_typ else {
                    if is_stmt_match {
                        diagnostics.push(diagnostics::Unknown {
                            region: pat.region(),
                            message: "PatEnumerant expects an enum type".into(),
                        }.into());
                    }
                    continue;
                };
                let symboltable = builder.get_symboltable();
                let name = parsing.string(pat_enum.name);
                let Some(sym) = symboltable.slot(*typedef_id, name) else { continue };
                let sym_id = sym.id();
                if covered_syms.contains_key(&sym_id) {
                    overlap = Some((pat.region(), format!("#{}", name.to_str_lossy()).into()));
                } else {
                    covered_syms.insert(sym_id, pat.region());
                }
            }
            AstNodePayload::PatWordLit(pat_word) => {
                if !matches!(subject_typ, Type::Word(_)) {
                    if is_stmt_match {
                        diagnostics.push(diagnostics::NotWordType {
                            region: pat.region(),
                            typ: subject_typ.to_string().into(),
                        }.into());
                    }
                    continue;
                }
                let literal = parsing.string(pat_word.literal).to_str_lossy().into_owned();
                let value = parse_word_literal_value(&literal);
                if covered_words.contains_key(&value) {
                    overlap = Some((pat.region(), format!("{}", value).into()));
                } else {
                    covered_words.insert(value, pat.region());
                }
            }
            AstNodePayload::PatBitLit(pat_bit) => {
                if !matches!(subject_typ, Type::Bit | Type::Reset) {
                    if is_stmt_match {
                        diagnostics.push(diagnostics::WrongType {
                            region: pat.region(),
                            expected: subject_typ.to_string().into(),
                            actual: Type::Bit.to_string().into(),
                        }.into());
                    }
                    continue;
                }
                let value = pat_bit.literal;
                if covered_bits.contains_key(&value) {
                    overlap = Some((pat.region(), format!("{}", value).into()));
                } else {
                    covered_bits.insert(value, pat.region());
                }
            }
            _ => continue,
        }
    }

    if let Some((region, witness)) = overlap {
        diagnostics.push(diagnostics::MatchOverlappingArm { region, overlap: witness }.into());
        return;
    }

    let (missing, exhausted) = compute_missing(
        &category,
        &covered_syms,
        &covered_words,
        &covered_bits,
    );

    // Statement-level matches whose arm bodies contain only latched drivers
    // (<=) are exempt from exhaustiveness and redundant-else checks.
    let latched_exempt = matches!(match_node.payload(), AstNodePayload::ModDefStmtMatch)
        && arms_only_latched_drivers(&arms);
    if latched_exempt {
        return;
    }

    if has_else {
        if exhausted {
            diagnostics.push(diagnostics::MatchRedundantElse {
                region: match_node.region(),
            }.into());
        }
    } else if !missing.is_empty() {
        diagnostics.push(diagnostics::MatchNotExhaustive {
            region: match_node.region(),
            subject_typ: subject_typ.to_string().into(),
            missing: format_missing(&missing),
        }.into());
    }
}

fn arms_only_latched_drivers(arms: &[(Option<&AstNode<'_>>, &AstNode<'_>)]) -> bool {
    for (_pat, body) in arms {
        if !subtree_only_latched_drivers(body) {
            return false;
        }
    }
    true
}

fn subtree_only_latched_drivers(node: &AstNode<'_>) -> bool {
    match node.payload() {
        AstNodePayload::Driver(driver) => {
            matches!(driver.driver_type, DriverType::Latched)
        }
        AstNodePayload::BidirectionalDriver => false,
        _ => {
            for child in node.children() {
                if !subtree_only_latched_drivers(&child) {
                    return false;
                }
            }
            true
        }
    }
}

enum Category {
    Union { slots: Vec<(SymbolId, String)> },
    Enum { slots: Vec<(SymbolId, String)> },
    Bit,
    WordExhaustible { width: Width },
    WordTooWide,
    Other,
}

fn subject_category(builder: &mut Builder, subject_typ: &Type) -> Category {
    match subject_typ {
        Type::Bit | Type::Reset => Category::Bit,
        Type::Word(width) => {
            if *width <= 10 {
                Category::WordExhaustible { width: *width }
            } else {
                Category::WordTooWide
            }
        }
        Type::Usual(typedef_id) => {
            let symboltable = builder.get_symboltable();
            let symbol = symboltable.symbol(*typedef_id);
            let slots: Vec<(SymbolId, String)> = symboltable
                .slots(*typedef_id)
                .iter()
                .map(|s| (s.id(), s.name().to_str_lossy().into_owned()))
                .collect();
            match symbol.kind() {
                SymbolKind::UnionDef => Category::Union { slots },
                SymbolKind::EnumDef => Category::Enum { slots },
                _ => Category::Other,
            }
        }
        _ => Category::Other,
    }
}

fn compute_missing(
    category: &Category,
    covered_syms: &IndexMap<SymbolId, Region>,
    covered_words: &IndexMap<WordValue, Region>,
    covered_bits: &IndexMap<bool, Region>,
) -> (Vec<BString>, bool) {
    match category {
        Category::Union { slots } => {
            let missing: Vec<BString> = slots.iter()
                .filter(|(id, _)| !covered_syms.contains_key(id))
                .map(|(_, name)| format!("@{}", name).into())
                .collect();
            let exhausted = missing.is_empty();
            (missing, exhausted)
        }
        Category::Enum { slots } => {
            let missing: Vec<BString> = slots.iter()
                .filter(|(id, _)| !covered_syms.contains_key(id))
                .map(|(_, name)| format!("#{}", name).into())
                .collect();
            let exhausted = missing.is_empty();
            (missing, exhausted)
        }
        Category::Bit => {
            let mut missing: Vec<BString> = Vec::new();
            for value in [false, true] {
                if !covered_bits.contains_key(&value) {
                    missing.push(format!("{}", value).into());
                }
            }
            let exhausted = missing.is_empty();
            (missing, exhausted)
        }
        Category::WordExhaustible { width } => {
            let total: u64 = 1u64 << width;
            let mut missing: Vec<BString> = Vec::new();
            for v in 0..total {
                if !covered_words.contains_key(&v) {
                    missing.push(format!("{}", v).into());
                }
            }
            let exhausted = missing.is_empty();
            (missing, exhausted)
        }
        Category::WordTooWide => {
            let mut missing: Vec<BString> = Vec::new();
            let mut v: u64 = 0;
            while missing.len() < 4 {
                if !covered_words.contains_key(&v) {
                    missing.push(format!("{}", v).into());
                }
                v += 1;
            }
            (missing, false)
        }
        Category::Other => (Vec::new(), true),
    }
}

fn format_missing(missing: &[BString]) -> BString {
    let mut out: Vec<u8> = Vec::new();
    let cap = std::cmp::min(missing.len(), 3);
    for (i, w) in missing.iter().take(cap).enumerate() {
        if i > 0 { out.extend_from_slice(b", "); }
        out.extend_from_slice(w);
    }
    if missing.len() > 3 {
        out.extend_from_slice(b", ...");
    }
    BString::from(out)
}

fn parse_word_literal_value(literal: &str) -> WordValue {
    let trimmed = if let Some((value, _width)) = literal.split_once('w') {
        value.to_string()
    } else {
        literal.to_string()
    };
    let trimmed = trimmed.replace('_', "");
    if let Some(hex) = trimmed.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap_or(0)
    } else if let Some(bin) = trimmed.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap_or(0)
    } else {
        trimmed.parse().unwrap_or(0)
    }
}
