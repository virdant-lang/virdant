//! HTML renderers for component-related query kinds.
//! This covers component analyses, drivers, dependency graphs, ports,
//! struct fields, typing contexts, and constructor signatures.

use std::collections::BTreeMap;

use serde_json::Value as JsonValue;

use super::common::badges;
use super::common::escape_html;
use super::common::location_string;
use super::common::muted;
use super::common::section;
use super::common::table;
use super::common::type_string;
use super::common::Ctx;

/// Maximum number of table rows rendered before truncation.
const ROW_LIMIT: usize = 100;

pub fn render_component(ctx: &Ctx, result: &JsonValue) -> String {
    let rows = vec![vec![
        escape_html(result["path"].as_str().unwrap_or("?")),
        component_ref(ctx, &result["id"]),
        variant_string(&result["kind"]),
        variant_string(&result["flow"]),
        type_or_dash(&result["typ"]),
        location_string(&result["location"]),
    ]];
    let body = table(
        &["Path", "Id", "Kind", "Flow", "Type", "Location"],
        &rows,
    );
    section("Component", &body)
}

pub fn render_component_analysis(ctx: &Ctx, result: &JsonValue) -> String {
    let moddef = match result["moddef"].as_u64() {
        Some(id) => escape_html(&ctx.symbol(id)),
        None => "?".to_string(),
    };
    let mut body = format!("<p><strong>Moddef</strong>: {moddef}</p>\n");

    let empty = Vec::new();
    let paths = component_paths(result);
    let mut rows = vec![];
    for entry in result["components"].as_array().unwrap_or(&empty) {
        let comp = &entry[1];
        rows.push(vec![
            escape_html(entry[0].as_str().unwrap_or("?")),
            component_ref(ctx, &comp["id"]),
            variant_string(&comp["kind"]),
            variant_string(&comp["flow"]),
            type_or_dash(&comp["typ"]),
            location_string(&comp["location"]),
        ]);
    }
    body.push_str(&section(
        "Components",
        &capped_table(
            &["Path", "Id", "Kind", "Flow", "Type", "Location"],
            &rows,
        ),
    ));

    let empty_map = serde_json::Map::new();
    let references = result["references"].as_object().unwrap_or(&empty_map);
    let mut ref_rows = vec![];
    for (location, value) in references {
        let id_text = value.get(0).and_then(|v| v.as_str()).unwrap_or("");
        let component = match paths.get(id_text) {
            Some(path) => path.clone(),
            None => component_ref(ctx, &value[0]),
        };
        let kind = value
            .get(1)
            .map(variant_string)
            .unwrap_or_else(|| "-".to_string());
        ref_rows.push(vec![escape_html(location), component, kind]);
    }
    body.push_str(&section(
        "References",
        &capped_table(&["Location", "Component", "Kind"], &ref_rows),
    ));

    body.push_str(&diagnostics_badges(result));
    section("Component analysis", &body)
}

pub fn render_ctor_signature(_ctx: &Ctx, result: &JsonValue) -> String {
    let empty = Vec::new();
    let mut rows = vec![];
    for param in result["parameters"].as_array().unwrap_or(&empty) {
        rows.push(vec![
            escape_html(param[0].as_str().unwrap_or("?")),
            type_or_dash(&param[1]),
        ]);
    }
    let mut body = section("Parameters", &capped_table(&["Name", "Type"], &rows));
    body.push_str(&format!(
        "<p><strong>Return type</strong>: {}</p>\n",
        type_or_dash(&result["ret_typ"])
    ));
    body
}

pub fn render_dependency_graph(ctx: &Ctx, result: &JsonValue) -> String {
    let empty = Vec::new();
    let empty_map = serde_json::Map::new();
    let edges = result["edges"].as_object().unwrap_or(&empty_map);
    let mut rows = vec![];
    for node in result["nodes"].as_array().unwrap_or(&empty) {
        let mut dependees = vec![];
        if let Some(id) = node.as_str() {
            if let Some(edge_list) = edges.get(id).and_then(|e| e.as_array()) {
                for edge in edge_list {
                    let dependee = component_ref(ctx, &edge["dependee"]);
                    let kind = variant_string(&edge["kind"]);
                    dependees.push(format!("{dependee} ({kind})"));
                }
            }
        }
        let dependee_cell = if dependees.is_empty() {
            "-".to_string()
        } else {
            dependees.join("<br>")
        };
        rows.push(vec![component_ref(ctx, node), dependee_cell]);
    }
    section("Dependency graph", &capped_table(&["Component", "Dependees"], &rows))
}

pub fn render_driver_analysis(ctx: &Ctx, result: &JsonValue) -> String {
    let empty_vec = Vec::new();
    let empty_map = serde_json::Map::new();
    let drivers = result["drivers"].as_object().unwrap_or(&empty_map);
    let mut rows = vec![];
    for (id, list) in drivers {
        let id_json = JsonValue::String(id.clone());
        for driver in list.as_array().unwrap_or(&empty_vec) {
            rows.push(vec![
                component_ref(ctx, &id_json),
                driver_type_string(driver),
                format!("<ul>{}</ul>", driver_html(driver)),
            ]);
        }
    }
    let mut body = section(
        "Drivers",
        &capped_table(&["Component", "Driver type", "Structure"], &rows),
    );
    body.push_str(&diagnostics_badges(result));
    section("Driver analysis", &body)
}

pub fn render_ports_of(_ctx: &Ctx, result: &JsonValue) -> String {
    let empty = Vec::new();
    let mut rows = vec![];
    for port in result.as_array().unwrap_or(&empty) {
        rows.push(vec![
            escape_html(port["path"].as_str().unwrap_or("?")),
            variant_string(&port["dir"]),
            type_or_dash(&port["typ"]),
        ]);
    }
    section("Ports", &capped_table(&["Path", "Dir", "Type"], &rows))
}

pub fn render_struct_fields(ctx: &Ctx, result: &JsonValue) -> String {
    let empty = Vec::new();
    let mut rows = vec![];
    for field in result.as_array().unwrap_or(&empty) {
        let symbol = match field["field_symbol_id"].as_u64() {
            Some(id) => escape_html(&ctx.symbol(id)),
            None => "?".to_string(),
        };
        rows.push(vec![
            escape_html(field["name"].as_str().unwrap_or("?")),
            symbol,
            type_or_dash(&field["typ"]),
        ]);
    }
    section("Struct fields", &capped_table(&["Name", "Symbol", "Type"], &rows))
}

pub fn render_typing_context(ctx: &Ctx, result: &JsonValue) -> String {
    let empty = Vec::new();
    let mut rows = vec![];
    for binding in result["context"].as_array().unwrap_or(&empty) {
        let name = escape_html(
            binding
                .get(0)
                .and_then(|v| v.as_str())
                .unwrap_or("?"),
        );
        let pair = &binding[1];
        rows.push(vec![
            name,
            referent_string(ctx, &pair[0]),
            type_or_dash(&pair[1]),
        ]);
    }
    section("Typing context", &capped_table(&["Name", "Referent", "Type"], &rows))
}

fn capped_table(headers: &[&str], rows: &[Vec<String>]) -> String {
    if rows.is_empty() {
        return muted("No entries.");
    }
    if rows.len() <= ROW_LIMIT {
        return table(headers, rows);
    }
    let mut out = table(headers, &rows[..ROW_LIMIT]);
    let omitted = rows.len() - ROW_LIMIT;
    out.push_str(&muted(&format!("{omitted} more rows omitted.")));
    out
}

fn diagnostics_badges(result: &JsonValue) -> String {
    let count = result["diagnostics"]
        .as_array()
        .map(|d| d.len())
        .unwrap_or(0);
    let mut out = badges(0, 0, count);
    if count > 0 {
        out.push_str(&muted(
            "Diagnostic severities are not classified in this view.",
        ));
    }
    out
}

fn component_paths(result: &JsonValue) -> BTreeMap<String, String> {
    let mut paths = BTreeMap::new();
    let empty = Vec::new();
    for entry in result["components"].as_array().unwrap_or(&empty) {
        let id = entry[1]["id"].as_str().unwrap_or("").to_string();
        if !id.is_empty() {
            let path = escape_html(entry[0].as_str().unwrap_or("?"));
            paths.insert(id, path);
        }
    }
    paths
}

fn component_ref(ctx: &Ctx, json: &JsonValue) -> String {
    let text = json.as_str().unwrap_or("?");
    match text.split_once('.') {
        Some((item, index)) => {
            let fqn = match item.parse::<u64>() {
                Ok(id) => ctx.symbol(id),
                Err(_) => item.to_string(),
            };
            escape_html(&format!("{fqn}[{index}]"))
        }
        None => escape_html(text),
    }
}

fn referent_string(ctx: &Ctx, json: &JsonValue) -> String {
    match variant(json) {
        Some("Component") => component_ref(ctx, &json["Component"]),
        Some("Local") => format!("local @ {}", location_string(&json["Local"])),
        _ => "?".to_string(),
    }
}

fn driver_type_string(driver: &JsonValue) -> String {
    match variant(driver) {
        Some("Expr") => driver["Expr"]
            .get(0)
            .map(variant_string)
            .unwrap_or_else(|| "?".to_string()),
        Some("Bidirectional") => "Continuous".to_string(),
        Some("When") => variant_string(&driver["When"]["driver_type"]),
        Some("Match") => variant_string(&driver["Match"]["driver_type"]),
        _ => "?".to_string(),
    }
}

fn driver_html(driver: &JsonValue) -> String {
    match variant(driver) {
        Some("Expr") => {
            let parts = &driver["Expr"];
            let driver_type = parts
                .get(0)
                .map(variant_string)
                .unwrap_or_else(|| "?".to_string());
            let location = parts
                .get(1)
                .map(location_string)
                .unwrap_or_else(|| "?".to_string());
            format!("<li>expr {driver_type} @ {location}</li>")
        }
        Some("Bidirectional") => {
            let location = location_string(&driver["Bidirectional"]);
            format!("<li>bidirectional @ {location}</li>")
        }
        Some("When") => {
            let when = &driver["When"];
            let driver_type = variant_string(&when["driver_type"]);
            let mut items = format!("<li>when ({driver_type})<ul>");
            let empty = Vec::new();
            for clause in when["clauses"].as_array().unwrap_or(&empty) {
                let location = clause
                    .get(0)
                    .map(location_string)
                    .unwrap_or_else(|| "?".to_string());
                let sub = clause.get(1).map(driver_html).unwrap_or_default();
                items.push_str(&format!("<li>case @ {location}<ul>{sub}</ul></li>"));
            }
            items.push_str(&else_html(&when["else_clause"]));
            items.push_str("</ul></li>");
            items
        }
        Some("Match") => {
            let m = &driver["Match"];
            let driver_type = variant_string(&m["driver_type"]);
            let subject = location_string(&m["subject"]);
            let mut items = format!("<li>match {subject} ({driver_type})<ul>");
            let empty = Vec::new();
            for arm in m["arms"].as_array().unwrap_or(&empty) {
                let location = arm
                    .get(0)
                    .map(location_string)
                    .unwrap_or_else(|| "?".to_string());
                let sub = arm.get(1).map(driver_html).unwrap_or_default();
                items.push_str(&format!("<li>arm @ {location}<ul>{sub}</ul></li>"));
            }
            items.push_str(&else_html(&m["else_clause"]));
            items.push_str("</ul></li>");
            items
        }
        _ => format!("<li>{}</li>", escape_html(&format!("{driver}"))),
    }
}

fn else_html(else_clause: &JsonValue) -> String {
    if else_clause.is_null() {
        "<li>else: (none)</li>".to_string()
    } else {
        let sub = driver_html(else_clause);
        format!("<li>else<ul>{sub}</ul></li>")
    }
}

fn variant(json: &JsonValue) -> Option<&str> {
    json.as_object()
        .and_then(|map| map.keys().next())
        .map(|key| key.as_str())
}

fn variant_string(json: &JsonValue) -> String {
    match variant(json) {
        Some(name) => escape_html(name),
        None => "-".to_string(),
    }
}

fn type_or_dash(json: &JsonValue) -> String {
    if json.is_null() {
        "-".to_string()
    } else {
        escape_html(&type_string(json))
    }
}
