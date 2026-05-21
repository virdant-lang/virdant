use std::sync::Arc;

use bstr::BString;
use indexmap::IndexMap;

use crate::analysis::symbols::SymbolKind;
use crate::common::WordValue;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::token::KEYWORDS;

fn package_name_is_keyword(name: &str) -> bool {
    KEYWORDS.contains(&name)
}

/// Check that no package name (except `builtin`) collides with a keyword.
fn check_package_name_not_keyword(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    for package in builder.get_packages().iter() {
        let package_name: &str = &package.to_string();
        if package_name == "builtin" {
            continue;
        }
        if package_name_is_keyword(package_name) {
            let parsing = builder.get_parsing(package.clone());
            let root_node = parsing.root();
            diagnostics.push(
                diagnostics::Unknown {
                    region: root_node.region(),
                    message: format!(
                        "Package name '{}' is a keyword",
                        package_name,
                    ).into(),
                }.into(),
            );
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

    for item in symboltable.items() {
        diagnostics.extend(builder.check_drivers(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        diagnostics.extend(builder.get_match_coverage(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::ModDef {
            let component_analysis = builder.get_component_analysis(item.id());
            diagnostics.extend(component_analysis.diagnostics());
        }
        diagnostics.extend(builder.typecheck(item.id()).iter().cloned());

//        let driver_analysis = builder.get_driver_analysis(item.id());
//        diagnostics.extend(driver_analysis); // TODO
    }

    // Check for duplicate enumerant values in enum types.
    // Only the first duplicate per value is reported (as an Error).
    check_duplicate_enum_values(builder, &mut diagnostics);

    diagnostics.sort_by_key(|d| {
        let region = d.region();
        let start = region.start();
        let end = region.end();
        (region.package(), start.line(), start.col(), end.line(), end.col())
    });

    Arc::new(diagnostics)
}

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
                    diagnostics.push(
                        diagnostics::DuplicateEnumValue {
                            region: prev_region.clone(),
                            enum_name: item.name().to_owned(),
                            value: *value,
                        }.into(),
                    );
                }
            } else {
                let parsing = builder.get_parsing(enumerant_symbol.package());
                let enumerant_node = parsing.ast_node(enumerant_symbol.location().ast_node_id());
                first.insert(*value, (enumerant_symbol.name().to_owned(), enumerant_node.region()));
            }
        }
    }
}
