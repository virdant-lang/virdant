//! Renderers for package, symbol, and type-table query kinds.
//! Each function takes the unwrapped query result JSON and returns HTML.

use serde_json::Value as JsonValue;

use super::common::badges;
use super::common::escape_html;
use super::common::location_string;
use super::common::muted;
use super::common::region_string;
use super::common::section;
use super::common::table;
use super::common::type_string;
use super::common::Ctx;

const ROW_LIMIT: usize = 100;

fn diagnostics_count(json: &JsonValue) -> usize {
    json["diagnostics"].as_array().map(|d| d.len()).unwrap_or(0)
}

fn kind_name(json: &JsonValue) -> String {
    match json["kind"].as_object() {
        Some(map) if !map.is_empty() => {
            let (variant, _) = map.iter().next().unwrap();
            escape_html(variant)
        }
        _ => "?".to_string(),
    }
}

fn opt_u64(json: &JsonValue, key: &str) -> String {
    json[key]
        .as_u64()
        .map(|n| n.to_string())
        .unwrap_or_else(|| "?".to_string())
}

fn location_cell(json: &JsonValue) -> String {
    if json["location"].is_null() {
        "-".to_string()
    } else {
        location_string(&json["location"])
    }
}

fn ctx_symbol(ctx: &Ctx, json: &JsonValue, key: &str) -> String {
    match json[key].as_u64() {
        Some(id) => escape_html(&ctx.symbol(id)),
        None => "?".to_string(),
    }
}

fn cap_note(total: usize) -> String {
    if total > ROW_LIMIT {
        muted(&format!("{} more rows omitted.", total - ROW_LIMIT))
    } else {
        String::new()
    }
}

pub fn render_packages(ctx: &Ctx, result: &JsonValue) -> String {
    let _ = ctx;
    let builtin = result["builtin"].as_u64();
    let mut rows = Vec::new();
    for name in result["names"].as_array().cloned().unwrap_or_default() {
        let name_text = match name.as_str() {
            Some(name) => escape_html(name),
            None => "?".to_string(),
        };
        let id = result["by_name"]
            .get(name.as_str().unwrap_or(""))
            .and_then(|id| id.as_u64());
        let marked = match (id, builtin) {
            (Some(id), Some(builtin)) if id == builtin => " (builtin)".to_string(),
            _ => String::new(),
        };
        let id_text = id.map(|n| n.to_string()).unwrap_or_else(|| "?".to_string());
        rows.push(vec![id_text, format!("{name_text}{marked}")]);
    }
    let body = if rows.is_empty() {
        muted("No packages.")
    } else {
        table(&["Id", "Name"], &rows) + &cap_note(rows.len())
    };
    section("Packages", &body)
}

pub fn render_package_analysis(ctx: &Ctx, result: &JsonValue) -> String {
    let package = result["package"]
        .as_u64()
        .map(|id| escape_html(&ctx.package(id)))
        .unwrap_or_else(|| "?".to_string());
    let mut body = format!("<p>Package: {package}</p>\n");

    let mut imports = Vec::new();
    for id in result["imports"].as_array().cloned().unwrap_or_default() {
        match id.as_u64() {
            Some(id) => imports.push(escape_html(&ctx.package(id))),
            None => imports.push("?".to_string()),
        }
    }
    body.push_str(&if imports.is_empty() {
        muted("No imports.")
    } else {
        format!("<p>Imports: {}</p>\n", imports.join(", "))
    });

    let mut item_rows = Vec::new();
    if let Some(items) = result["items"].as_object() {
        for (name, nodes) in items {
            let count = nodes.as_array().map(|n| n.len()).unwrap_or(0);
            let node_ids = nodes
                .as_array()
                .map(|nodes| {
                    nodes
                        .iter()
                        .filter_map(|n| n.as_u64())
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                })
                .unwrap_or_else(|| "?".to_string());
            item_rows.push(vec![escape_html(name), count.to_string(), node_ids]);
        }
    }
    body.push_str(&if item_rows.is_empty() {
        muted("No items.")
    } else {
        table(&["Item", "Decls", "Ast node ids"], &item_rows)
    });

    let roots = result["expr_roots"].as_array().map(|r| r.len()).unwrap_or(0);
    body.push_str(&format!("<p>Expr roots: {roots}</p>\n"));

    let diagnostics = diagnostics_count(result);
    body.push_str(&badges(0, 0, diagnostics));

    section("Package analysis", &body)
}

pub fn render_symbol_table(ctx: &Ctx, result: &JsonValue) -> String {
    let mut rows = Vec::new();
    if let Some(symbols) = result["symbols"].as_object() {
        let mut entries: Vec<(&String, &JsonValue)> = symbols.iter().collect();
        entries.sort_by_key(|(_, symbol)| symbol["id"].as_u64().unwrap_or(u64::MAX));
        for (_, symbol) in entries {
            let fqn = match symbol["fqn"].as_str() {
                Some(fqn) => escape_html(fqn),
                None => "?".to_string(),
            };
            let parent = match symbol["parent_id"].as_u64() {
                Some(id) => escape_html(&ctx.symbol(id)),
                None => "-".to_string(),
            };
            rows.push(vec![
                opt_u64(symbol, "id"),
                fqn,
                kind_name(symbol),
                location_cell(symbol),
                parent,
            ]);
        }
    }
    let table_body = if rows.is_empty() {
        muted("No symbols.")
    } else {
        table(&["Id", "Fqn", "Kind", "Location", "Parent"], &rows)
            + &cap_note(rows.len())
    };
    let diagnostics = diagnostics_count(result);
    let body = format!(
        "{table_body}<p>Diagnostics: {diagnostics}</p>\n{}",
        badges(0, 0, diagnostics)
    );
    section("Symbol table", &body)
}

pub fn render_symbol_ast(_ctx: &Ctx, result: &JsonValue) -> String {
    let id = result.as_u64().unwrap_or(0);
    let body = format!("<p>Ast node id: {id}</p>\n");
    section("Symbol ast", &body)
}

pub fn render_typedefs(ctx: &Ctx, result: &JsonValue) -> String {
    let typedefs = result.as_array().cloned().unwrap_or_default();
    let mut rows = Vec::new();
    for typedef in &typedefs {
        rows.push(typedef_row(ctx, typedef));
    }
    let body = if rows.is_empty() {
        muted("No typedefs.")
    } else {
        table(&["Type", "Kind", "Width", "Enumerants"], &rows) + &cap_note(rows.len())
    };
    section("Type definitions", &body)
}

pub fn render_typedef(ctx: &Ctx, result: &JsonValue) -> String {
    let rows = vec![typedef_row(ctx, result)];
    let body = table(&["Type", "Kind", "Width", "Enumerants"], &rows);
    section("Type definition", &body)
}

fn typedef_row(ctx: &Ctx, typedef: &JsonValue) -> Vec<String> {
    let name = ctx_symbol(ctx, typedef, "symbol_id");
    let kind = kind_name(typedef);
    let width = match typedef["width"].as_u64() {
        Some(width) => width.to_string(),
        None => "-".to_string(),
    };
    let mut enumerants = Vec::new();
    if let Some(map) = typedef["enumerant_values"].as_object() {
        for (key, value) in map {
            let Ok(symbol_id) = key.parse::<u64>() else {
                continue;
            };
            let name = ctx.symbol(symbol_id);
            let short = name.rsplit("::").next().unwrap_or(&name);
            enumerants.push(format!(
                "{} = {}",
                escape_html(short),
                escape_html(&format!("{value}"))
            ));
        }
    }
    vec![name, kind, width, enumerants.join(", ")]
}

pub fn render_type_index(ctx: &Ctx, result: &JsonValue) -> String {
    let _ = ctx;
    let typs = result["typs"].as_array().cloned().unwrap_or_default();
    let typ_strings: Vec<String> = typs.iter().map(type_string).collect();
    let mut body = if typ_strings.is_empty() {
        muted("No types.")
    } else {
        format!("<p>Types: {}</p>\n", escape_html(&typ_strings.join(", ")))
    };

    let mut entries: Vec<(&String, &JsonValue)> = match result["typ_at_location"].as_object() {
        Some(map) => map.iter().collect(),
        None => Vec::new(),
    };
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    let total = entries.len();
    let mut rows = Vec::new();
    for (location, typeid) in entries.iter().take(ROW_LIMIT) {
        let type_text = match typeid.as_u64() {
            Some(id) => typ_strings
                .get(id as usize)
                .cloned()
                .unwrap_or_else(|| format!("typ{id}")),
            None => "?".to_string(),
        };
        rows.push(vec![escape_html(location), escape_html(&type_text)]);
    }
    body.push_str(&if rows.is_empty() {
        muted("No typed locations.")
    } else {
        table(&["Location", "Type"], &rows) + &cap_note(total)
    });
    body.push_str(&format!(
        "<p>Diagnostics: {}</p>\n",
        diagnostics_count(result)
    ));
    section("Type index", &body)
}

pub fn render_expr_roots(_ctx: &Ctx, result: &JsonValue) -> String {
    let roots = result.as_array().cloned().unwrap_or_default();
    let mut rows = Vec::new();
    for root in &roots {
        rows.push(vec![location_string(&root["location"])]);
    }
    let body = if rows.is_empty() {
        muted("No expr roots.")
    } else {
        table(&["Location"], &rows) + &cap_note(rows.len())
    };
    section("Expr roots", &body)
}

pub fn render_all_exprs(_ctx: &Ctx, result: &JsonValue) -> String {
    let exprs = result.as_array().cloned().unwrap_or_default();
    let mut rows = Vec::new();
    for expr in &exprs {
        rows.push(vec![location_string(expr)]);
    }
    let body = if rows.is_empty() {
        muted("No exprs.")
    } else {
        table(&["Location"], &rows) + &cap_note(rows.len())
    };
    section("All exprs", &body)
}

pub fn render_expr_root_for(_ctx: &Ctx, result: &JsonValue) -> String {
    let body = format!("<p>{}</p>\n", location_string(&result["location"]));
    section("Expr root", &body)
}

pub fn render_location_region(ctx: &Ctx, result: &JsonValue) -> String {
    let region = region_string(ctx, result);
    let body = format!("<p>{}</p>\n", escape_html(&region));
    section("Region", &body)
}
