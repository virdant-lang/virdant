//! Renderers for typing, elaboration, source, and parsing query kinds.
//! Each function takes the unwrapped query result JSON and returns HTML.

use std::collections::BTreeMap;

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
const BIG_ROW_LIMIT: usize = 200;
const LISTING_LIMIT: usize = 2000;

fn variant_name(json: &JsonValue) -> String {
    if let Some(variant) = json.as_str() {
        return variant.to_string();
    }
    if let Some(map) = json.as_object() {
        if let Some((variant, _)) = map.iter().next() {
            return variant.to_string();
        }
    }
    "?".to_string()
}

fn is_warning_payload(variant: &str) -> bool {
    matches!(
        variant,
        "NoRegDrivers"
            | "NotIt"
            | "UnusedSource"
            | "ReadFromSink"
            | "UnfilledHole"
            | "EmptyDriverBlock"
            | "RedundantDriver"
    )
}

fn diagnostic_counts(diags: &[JsonValue]) -> (usize, usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;
    let mut infos = 0;
    for diag in diags {
        match variant_name(&diag["payload"]).as_str() {
            "Todo" => infos += 1,
            variant if is_warning_payload(variant) => warnings += 1,
            _ => errors += 1,
        }
    }
    (errors, warnings, infos)
}

fn diagnostics_table(ctx: &Ctx, diags: &[JsonValue]) -> String {
    if diags.is_empty() {
        return muted("No diagnostics.");
    }
    let mut rows = Vec::new();
    for diag in diags {
        rows.push(vec![
            escape_html(&variant_name(&diag["payload"])),
            escape_html(&region_string(ctx, &diag["region"])),
        ]);
    }
    table(&["Diagnostic", "Region"], &rows)
}

fn span_text(span: &JsonValue) -> String {
    let start = &span[0];
    let end = &span[1];
    let start_line = start[0].as_u64().unwrap_or(0);
    let start_col = start[1].as_u64().unwrap_or(0);
    let end_line = end[0].as_u64().unwrap_or(0);
    let end_col = end[1].as_u64().unwrap_or(0);
    if start_line == end_line {
        format!("{start_line}:{start_col}-{end_col}")
    } else {
        format!("{start_line}:{start_col}-{end_line}:{end_col}")
    }
}

fn cap_note_at(total: usize, limit: usize) -> String {
    if total > limit {
        muted(&format!("{} more rows omitted.", total - limit))
    } else {
        String::new()
    }
}

fn listing(text: &str, pkg: Option<u64>) -> String {
    let mut lines: Vec<&str> = text.split('\n').collect();
    if lines.last().map(|line| line.is_empty()).unwrap_or(false) {
        lines.pop();
    }
    let total = lines.len();
    let width = total.to_string().len();
    let mut out = String::from("<pre>");
    for (i, line) in lines.iter().take(LISTING_LIMIT).enumerate() {
        let line_html = match pkg {
            Some(pkg) => {
                let n = i + 1;
                format!("<span id=\"parsing-p{pkg}-L{n}\">{}</span>", escape_html(line))
            }
            None => escape_html(line),
        };
        out.push_str(&format!("{:>width$} | {line_html}\n", i + 1));
    }
    out.push_str("</pre>\n");
    out.push_str(&cap_note_at(total, LISTING_LIMIT));
    out
}

fn source_listing(json: &JsonValue, pkg: Option<u64>) -> String {
    match json["text"].as_str() {
        Some(text) => listing(text, pkg),
        None => muted("Source text unavailable."),
    }
}

fn tag_string(ctx: &Ctx, tag: &JsonValue) -> String {
    let map = match tag.as_object() {
        Some(map) if map.len() == 1 => map,
        _ => return escape_html(&format!("{tag}")),
    };
    let (variant, value) = map.iter().next().unwrap();
    match variant.as_str() {
        "None" => "-".to_string(),
        "SymbolResolution" => match value.as_u64() {
            Some(id) => format!("Symbol: {}", ctx.symbol(id)),
            None => "Symbol: ?".to_string(),
        },
        "PrimitiveResolution" => format!("Primitive: {}", escape_html(&variant_name(value))),
        "ReferentResolution" => match value.as_object() {
            Some(inner) if inner.len() == 1 => {
                let (kind, referent) = inner.iter().next().unwrap();
                match kind.as_str() {
                    "Component" => format!(
                        "Referent: component {}",
                        match referent.as_str() {
                            Some(referent) => ctx.component(referent),
                            None => escape_html(&format!("{referent}")),
                        }
                    ),
                    "Local" => format!("Referent: local {}", ctx.location(referent)),
                    _ => format!("Referent: {}", escape_html(kind)),
                }
            }
            _ => "Referent: ?".to_string(),
        },
        _ => escape_html(variant),
    }
}

fn render_type_result(ctx: &Ctx, result: &JsonValue, label: &str) -> String {
    if let Some(typ) = result.get("Ok") {
        let typ_text = escape_html(&type_string(typ));
        let body = format!("<p class=\"key\">Type: {typ_text}</p>\n");
        return section(label, &body);
    }
    if let Some(diags) = result["Err"].as_array() {
        let (errors, warnings, infos) = diagnostic_counts(diags);
        let mut body = badges(errors, warnings, infos);
        body.push_str(&diagnostics_table(ctx, diags));
        return section(label, &body);
    }
    section(label, &muted("Unknown result shape."))
}

pub fn render_type_at(ctx: &Ctx, result: &JsonValue) -> String {
    render_type_result(ctx, result, "Type at")
}

pub fn render_typeof(ctx: &Ctx, result: &JsonValue) -> String {
    render_type_result(ctx, result, "Type of")
}

pub fn render_expected_type(_ctx: &Ctx, result: &JsonValue) -> String {
    let body = if result.is_null() {
        muted("None.")
    } else {
        let typ_text = escape_html(&type_string(result));
        format!("<p class=\"key\">Expected type: {typ_text}</p>\n")
    };
    section("Expected type", &body)
}

pub fn render_typing(ctx: &Ctx, result: &JsonValue) -> String {
    let item = match result["item"]["id"].as_u64() {
        Some(id) => ctx.symbol(id),
        None => "?".to_string(),
    };
    let exprroot = ctx.location(&result["exprroot"]["location"]);
    let mut body = format!("<p class=\"key\">Item: {item}</p>\n<p>Expr root: {exprroot}</p>\n");

    let diags = result["diagnostics"].as_array().cloned().unwrap_or_default();
    let (errors, warnings, infos) = diagnostic_counts(&diags);
    body.push_str(&badges(errors, warnings, infos));
    if !diags.is_empty() {
        body.push_str(&diagnostics_table(ctx, &diags));
    }

    let mut type_entries: Vec<(u64, &JsonValue)> = Vec::new();
    if let Some(typs) = result["typs"].as_object() {
        for (key, typ) in typs {
            let id = key.parse::<u64>().unwrap_or(u64::MAX);
            type_entries.push((id, typ));
        }
    }
    type_entries.sort_by_key(|(id, _)| *id);
    let total = type_entries.len();
    let mut rows = Vec::new();
    for (id, typ) in type_entries.iter().take(ROW_LIMIT) {
        rows.push(vec![id.to_string(), escape_html(&type_string(typ))]);
    }
    body.push_str(&section(
        "Node types",
        &if rows.is_empty() {
            muted("No typed nodes.")
        } else {
            table(&["Ast node id", "Type"], &rows) + &cap_note_at(total, ROW_LIMIT)
        },
    ));

    let mut tag_entries: Vec<(&String, &JsonValue)> = match result["tags"].as_object() {
        Some(tags) => tags.iter().collect(),
        None => Vec::new(),
    };
    tag_entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    let total = tag_entries.len();
    let mut rows = Vec::new();
    for (location, tag) in tag_entries.iter().take(ROW_LIMIT) {
        rows.push(vec![
            ctx.location(&JsonValue::String(location.to_string())),
            tag_string(ctx, tag),
        ]);
    }
    body.push_str(&section(
        "Tags",
        &if rows.is_empty() {
            muted("No tags.")
        } else {
            table(&["Location", "Tag"], &rows) + &cap_note_at(total, ROW_LIMIT)
        },
    ));

    let use_entries: Vec<(&String, &JsonValue)> = match result["use_locations"].as_object() {
        Some(uses) => uses.iter().collect(),
        None => Vec::new(),
    };
    let total = use_entries.len();
    let mut rows = Vec::new();
    for (path, locations) in use_entries.iter().take(ROW_LIMIT) {
        let locations_text = locations
            .as_array()
            .map(|locations| {
                locations
                    .iter()
                    .map(|location| ctx.location(location))
                    .collect::<Vec<_>>()
                    .join(", ")
            })
            .unwrap_or_else(|| "?".to_string());
        rows.push(vec![escape_html(path), locations_text]);
    }
    body.push_str(&section(
        "Uses",
        &if rows.is_empty() {
            muted("No component uses.")
        } else {
            table(&["Path", "Locations"], &rows) + &cap_note_at(total, ROW_LIMIT)
        },
    ));

    section("Typing", &body)
}

pub fn render_typeof_all(ctx: &Ctx, result: &JsonValue) -> String {
    let mut entries: Vec<(&String, &JsonValue)> = match result.as_object() {
        Some(map) => map.iter().collect(),
        None => Vec::new(),
    };
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    let total = entries.len();
    let mut rows = Vec::new();
    for (location, typ) in entries.iter().take(BIG_ROW_LIMIT) {
        let typ_text = if typ.is_null() {
            "-".to_string()
        } else {
            escape_html(&type_string(typ))
        };
        rows.push(vec![
            ctx.location(&JsonValue::String(location.to_string())),
            typ_text,
        ]);
    }
    let body = if rows.is_empty() {
        muted("No typed locations.")
    } else {
        table(&["Location", "Type"], &rows) + &cap_note_at(total, BIG_ROW_LIMIT)
    };
    section("Types of all locations", &body)
}

pub fn render_elaboration(ctx: &Ctx, result: &JsonValue) -> String {
    let mut path_by_id: BTreeMap<u64, String> = BTreeMap::new();
    if let Some(path_to_id) = result["path_to_id"].as_object() {
        for (path, id) in path_to_id {
            if let Some(id) = id.as_u64() {
                path_by_id.insert(id, path.clone());
            }
        }
    }

    let mut rows = Vec::new();
    let components = result["components"].as_array().cloned().unwrap_or_default();
    let total = components.len();
    for component in components.iter().take(BIG_ROW_LIMIT) {
        let path = match component["path"].as_str() {
            Some(path) => escape_html(path),
            None => "?".to_string(),
        };
        let driver = match component["driver"].as_object() {
            Some(map) if !map.is_empty() => map.iter().next().unwrap(),
            _ => continue,
        };
        let (driver_variant, driver_value) = driver;
        let driver_location = match driver_variant.as_str() {
            "Expr" => driver_value.get(1).cloned().unwrap_or(JsonValue::Null),
            "Bidirectional" => driver_value.clone(),
            "When" | "Match" => driver_value["location"].clone(),
            _ => JsonValue::Null,
        };
        let driver_text = match driver_location.as_str() {
            Some(location) => format!(
                "{} @ {}",
                escape_html(driver_variant),
                location_string(&JsonValue::String(location.to_string())),
            ),
            None => escape_html(driver_variant),
        };
        let signal_text = |id: &JsonValue| -> String {
            match id.as_u64() {
                Some(id) => match path_by_id.get(&id) {
                    Some(path) => escape_html(path),
                    None => format!("signal{id}"),
                },
                None => "-".to_string(),
            }
        };
        rows.push(vec![
            path,
            escape_html(&type_string(&component["typ"])),
            escape_html(&variant_name(&component["component_kind"])),
            escape_html(&variant_name(&component["driver_type"])),
            driver_text,
            signal_text(&component["alias"]),
            signal_text(&component["clock"]),
        ]);
    }
    let mut body = if rows.is_empty() {
        muted("No elaborated components.")
    } else {
        table(
            &["Path", "Type", "Kind", "Driver type", "Driver", "Alias", "Clock"],
            &rows,
        ) + &cap_note_at(total, BIG_ROW_LIMIT)
    };

    let mut module_rows = Vec::new();
    for module in result["modules"].as_array().cloned().unwrap_or_default() {
        let moddef = match module["moddef"].as_u64() {
            Some(id) => escape_html(&ctx.symbol(id)),
            None => "?".to_string(),
        };
        let prefix = match module["prefix"].as_str() {
            Some(prefix) => escape_html(prefix),
            None => "?".to_string(),
        };
        module_rows.push(vec![prefix, moddef]);
    }
    body.push_str(&section(
        "Modules",
        &if module_rows.is_empty() {
            muted("No module instances.")
        } else {
            table(&["Prefix", "Moddef"], &module_rows)
        },
    ));

    let mut socket_rows = Vec::new();
    for socket in result["sockets"].as_array().cloned().unwrap_or_default() {
        let socketdef = match socket["socketdef"].as_u64() {
            Some(id) => escape_html(&ctx.symbol(id)),
            None => "?".to_string(),
        };
        let role = escape_html(&variant_name(&socket["role"]));
        let prefix = match socket["prefix"].as_str() {
            Some(prefix) => escape_html(prefix),
            None => "?".to_string(),
        };
        socket_rows.push(vec![prefix, role, socketdef]);
    }
    body.push_str(&section(
        "Sockets",
        &if socket_rows.is_empty() {
            muted("No socket instances.")
        } else {
            table(&["Prefix", "Role", "Socketdef"], &socket_rows)
        },
    ));

    section("Elaboration", &body)
}

pub fn render_source(ctx: &Ctx, result: &JsonValue) -> String {
    let package = match result["package"].as_u64() {
        Some(id) => escape_html(&ctx.package(id)),
        None => "?".to_string(),
    };
    let mut body = format!("<p>Package: {package}</p>\n");
    body.push_str(&source_listing(result, None));
    section("Source", &body)
}

pub fn render_parsing(ctx: &Ctx, result: &JsonValue) -> String {
    let num_nodes = result["payloads"].as_array().map(|p| p.len()).unwrap_or(0);
    let num_errors = result["errors"].as_array().map(|e| e.len()).unwrap_or(0);
    let num_strings = result["strings"].as_array().map(|s| s.len()).unwrap_or(0);
    let mut body = format!(
        "<p>{num_nodes} ast nodes, {num_errors} error nodes, {num_strings} strings interned.</p>\n"
    );

    let pkg = result["source"]["package"].as_u64().unwrap_or(0);
    body.push_str(&section("Source", &source_listing(&result["source"], Some(pkg))));

    let error_ids = result["errors"].as_array().cloned().unwrap_or_default();
    let mut rows = Vec::new();
    for error_id in error_ids.iter() {
        let id_text = error_id.as_u64().map(|id| id.to_string()).unwrap_or_else(|| "?".to_string());
        let span = match error_id.as_u64() {
            Some(id) => result["spans"][id as usize].clone(),
            None => JsonValue::Null,
        };
        let span_cell = if span.is_null() {
            "?".to_string()
        } else {
            escape_html(&span_text(&span))
        };
        rows.push(vec![id_text, span_cell]);
    }
    body.push_str(&section(
        "Error nodes",
        &if rows.is_empty() {
            muted("No error nodes.")
        } else {
            table(&["Ast node id", "Span"], &rows)
        },
    ));

    let docstring_diags = result["docstring_diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    body.push_str(&section(
        "Docstring diagnostics",
        &diagnostics_table(ctx, &docstring_diags),
    ));

    section("Parsing", &body)
}

pub fn render_string(_ctx: &Ctx, result: &JsonValue) -> String {
    let text = match result.as_str() {
        Some(text) => escape_html(text),
        None => escape_html(&format!("{result}")),
    };
    let body = format!("<pre>{text}</pre>\n");
    section("String", &body)
}
