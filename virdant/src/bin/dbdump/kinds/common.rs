//! Shared helpers for the per-query-kind HTML renderers.

use std::collections::BTreeMap;

use serde_json::Value as JsonValue;

/// Reference tables for making ids readable in the HTML.
pub struct Ctx {
    /// Package name by package id.
    pub packages: Vec<String>,
    /// Fully-qualified symbol name by symbol id.
    pub symbols: BTreeMap<u32, String>,
}

impl Ctx {
    pub fn package(&self, id: u64) -> String {
        self.packages
            .get(id as usize)
            .cloned()
            .unwrap_or_else(|| format!("pkg{id}"))
    }

    pub fn symbol(&self, id: u64) -> String {
        self.symbols
            .get(&(id as u32))
            .cloned()
            .unwrap_or_else(|| format!("sym{id}"))
    }
}

/// Escapes text for use in HTML.
pub fn escape_html(text: &str) -> String {
    text.replace('&', "\u{26}amp;")
        .replace('<', "\u{26}lt;")
        .replace('>', "\u{26}gt;")
        .replace('"', "\u{26}quot;")
}

/// Renders a type JSON value (`{"Bit":null}`, `{"Word":32}`,
/// `{"Usual":5}`, `{"Valid":{...}}`) as a readable string.
pub fn type_string(json: &JsonValue) -> String {
    let map = match json.as_object() {
        Some(map) if map.len() == 1 => map,
        _ => return format!("{json}"),
    };
    let (variant, value) = map.iter().next().unwrap();
    match variant.as_str() {
        "Bit" => "Bit".to_string(),
        "Clock" => "Clock".to_string(),
        "Reset" => "Reset".to_string(),
        "Word" => format!("Word[{}]", value.as_u64().unwrap_or(0)),
        "Usual" => format!("Usual({})", value.as_u64().unwrap_or(0)),
        "Valid" => format!("Valid[{}]", type_string(value)),
        _ => format!("{variant}"),
    }
}

/// Renders a location JSON value (a string like `"1:5"`) escaped.
pub fn location_string(json: &JsonValue) -> String {
    match json.as_str() {
        Some(location) => escape_html(location),
        None => escape_html(&format!("{json}")),
    }
}

/// Renders a region JSON value
/// (`{"package": id, "span": [[line, col], [line, col]]}`) as
/// `name[line:col-col]`.
pub fn region_string(ctx: &Ctx, json: &JsonValue) -> String {
    let package = json["package"].as_u64().map(|id| ctx.package(id));
    let span = &json["span"];
    let start = &span[0];
    let end = &span[1];
    let start_line = start[0].as_u64().unwrap_or(0);
    let start_col = start[1].as_u64().unwrap_or(0);
    let end_line = end[0].as_u64().unwrap_or(0);
    let end_col = end[1].as_u64().unwrap_or(0);
    let span_text = if start_line == end_line {
        format!("{start_line}:{start_col}-{end_col}")
    } else {
        format!("{start_line}:{start_col}-{end_line}:{end_col}")
    };
    match package {
        Some(package) => format!("{package}[{span_text}]"),
        None => format!("[{span_text}]"),
    }
}

/// Pretty-prints a JSON value as an escaped HTML `<pre>` block.
pub fn pre_json(json: &JsonValue) -> String {
    let pretty = serde_json::to_string_pretty(json).unwrap_or_else(|_| "<error>".to_string());
    format!("<pre>{}</pre>", escape_html(&pretty))
}

/// Renders an HTML table. Cell contents are raw HTML, so callers must
/// escape user-derived text themselves.
pub fn table(headers: &[&str], rows: &[Vec<String>]) -> String {
    let mut out = String::from("<table><tr>");
    for header in headers {
        out.push_str(&format!("<th>{}</th>", escape_html(header)));
    }
    out.push_str("</tr>\n");
    for row in rows {
        out.push_str("<tr>");
        for cell in row {
            out.push_str(&format!("<td>{cell}</td>"));
        }
        out.push_str("</tr>\n");
    }
    out.push_str("</table>\n");
    out
}

/// Renders a section with an escaped heading.
pub fn section(title: &str, body: &str) -> String {
    format!("<h2>{}</h2>\n{}", escape_html(title), body)
}

/// Renders a muted paragraph note.
pub fn muted(text: &str) -> String {
    format!("<p class=\"muted\">{}</p>\n", escape_html(text))
}

/// Renders a diagnostics count badge line.
pub fn badges(num_errors: usize, num_warnings: usize, num_infos: usize) -> String {
    format!(
        "<p><span class=\"badge error\">{num_errors} errors</span> \
         <span class=\"badge warning\">{num_warnings} warnings</span> \
         <span class=\"badge muted\">{num_infos} infos</span></p>\n"
    )
}
