//! The `virdb` CLI entry point: builds a static dark-mode HTML site
//! under `build/db/` dumping every cached compiler database query
//! result, with a raw JSON dump per query kind plus an overview index.

use bstr::ByteSlice;

use clap::Parser;

use serde_json::Value as JsonValue;

use std::collections::BTreeMap;
use std::path::PathBuf;

use virdant::db::Db;
use virdant::util::{db_from_dir, db_from_files};
use virdant::diagnostics::DiagnosticLevel;

mod kinds;

use kinds::common::{escape_html, Ctx};

/// Dump the Virdant compiler database as a static HTML site
#[derive(Parser, Debug)]
#[command(name = "virdb", author, version, about)]
struct Args {
    /// Project directory containing Virdant.toml (exclusive with -F)
    #[arg(short = 'C', conflicts_with = "virfile")]
    cwd: Option<PathBuf>,

    /// Load .vir file(s) as a self-contained project (comma-separated, no spaces; exclusive with -C)
    #[arg(short = 'F', conflicts_with = "cwd")]
    virfile: Option<String>,
}

fn main() {
    let args = Args::parse();
    let (db, builddir) = project_db(&args);
    run(&db, &builddir);
}

fn project_db(args: &Args) -> (Db, PathBuf) {
    if let Some(virfile) = &args.virfile {
        let paths: Vec<PathBuf> = virfile
            .split(',')
            .map(|s| PathBuf::from(s.trim()))
            .collect();
        for path in &paths {
            if !path.is_file() {
                eprintln!("ERROR: file not found: {}", path.display());
                std::process::exit(1);
            }
        }
        let builddir = std::env::current_dir().unwrap().join("build").join("db");
        return (db_from_files(paths), builddir);
    }

    let cwd = if let Some(cwd) = &args.cwd {
        match std::fs::canonicalize(cwd) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot resolve directory {}: {e}", cwd.display());
                std::process::exit(1);
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot determine current directory: {e}");
                std::process::exit(1);
            }
        }
    };

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let src_dir = cwd.join("src");
    if !src_dir.is_dir() {
        eprintln!("ERROR: source directory not found: {}", src_dir.display());
        std::process::exit(1);
    }

    let builddir = cwd.join("build").join("db");
    (db_from_dir(src_dir), builddir)
}

fn run(db: &Db, builddir: &std::path::Path) {
    let diagnostics = db.check();
    for diagnostic in diagnostics.iter() {
        let region = diagnostic.region.display(db);
        let message = diagnostic.message().to_str_lossy().into_owned();
        eprintln!("{}: {}: {}", diagnostic.level(), region, message);
    }
    let num_errors = diagnostics.iter().filter(|d| d.level() == DiagnosticLevel::Error).count();
    if num_errors > 0 {
        eprintln!(
            "WARNING: project has {num_errors} errors. \
            The database dump may be incomplete, but dumping anyway.",
        );
    }

    let dump = db.dump_query_results_json();

    match generate_site(&dump, builddir) {
        Ok(()) => println!("Wrote database dump to {}", builddir.to_string_lossy()),
        Err(e) => {
            eprintln!("Database dump generation error: {e}");
            std::process::exit(1);
        }
    }
}

// ---------------------------------------------------------------------------
// Site generation
// ---------------------------------------------------------------------------

fn generate_site(dump: &JsonValue, builddir: &std::path::Path) -> Result<(), String> {
    let data_dir = builddir.join("data");
    let query_dir = builddir.join("query");
    std::fs::create_dir_all(&data_dir).map_err(|e| e.to_string())?;
    std::fs::create_dir_all(&query_dir).map_err(|e| e.to_string())?;

    let entries: Vec<&JsonValue> = dump
        .as_array()
        .map(|array| array.iter().collect())
        .ok_or("Dump is not an array")?;

    // Lookups for making debug keys readable.
    let packages = package_names(&entries);
    let symbols = symbol_fqns(&entries);
    let components = component_names(&entries, &symbols);
    let locations = location_lines(&entries);
    let ctx = Ctx { packages, symbols, components, locations };

    // Write the full dump as JSON.
    let full_json = serde_json::to_string_pretty(dump).map_err(|e| e.to_string())?;
    std::fs::write(data_dir.join("db.json"), full_json).map_err(|e| e.to_string())?;

    // Group entries by query kind, and write per-kind JSON and HTML.
    let mut groups: BTreeMap<&str, Vec<&JsonValue>> = BTreeMap::new();
    for entry in &entries {
        let name = entry_name(entry);
        groups.entry(name).or_default().push(entry);
    }

    let mut kind_rows: Vec<(String, usize, f64)> = vec![];
    for (kind, kind_entries) in &groups {
        let json_text = serde_json::to_string_pretty(kind_entries).map_err(|e| e.to_string())?;
        std::fs::write(data_dir.join(format!("{kind}.json")), json_text)
            .map_err(|e| e.to_string())?;

        let total_secs: f64 = kind_entries
            .iter()
            .map(|e| e["duration_secs"].as_f64().unwrap_or(0.0))
            .sum();
        kind_rows.push((kind.to_string(), kind_entries.len(), total_secs));

        let page = render_kind_page(kind, kind_entries, &ctx);
        std::fs::write(query_dir.join(format!("{kind}.html")), page)
            .map_err(|e| e.to_string())?;
    }

    let index = render_index(&kind_rows, &entries, &ctx.packages);
    std::fs::write(builddir.join("index.html"), index).map_err(|e| e.to_string())?;

    std::fs::write(builddir.join("style.css"), STYLE_CSS).map_err(|e| e.to_string())?;

    Ok(())
}

fn entry_name<'a>(entry: &'a JsonValue) -> &'a str {
    entry["name"].as_str().unwrap_or("?")
}

/// Package names indexed by package id, from the cached `Packages`
/// result.
fn package_names(entries: &[&JsonValue]) -> Vec<String> {
    let mut names: Vec<String> = vec![];
    for entry in entries {
        if entry_name(entry) == "Packages" {
            if let Some(list) = entry["result"]["Packages"]["names"].as_array() {
                names = list
                    .iter()
                    .map(|v| v.as_str().unwrap_or("?").to_string())
                    .collect();
            }
            break;
        }
    }
    names
}

/// Fully-qualified symbol names indexed by symbol id, from the cached
/// `SymbolTable` result.
fn symbol_fqns(entries: &[&JsonValue]) -> BTreeMap<u32, String> {
    let mut symbols: BTreeMap<u32, String> = BTreeMap::new();
    for entry in entries {
        if entry_name(entry) == "SymbolTable" {
            if let Some(map) = entry["result"]["SymbolTable"]["symbols"].as_object() {
                for (fqn, symbol) in map {
                    if let Some(id) = symbol["id"].as_u64() {
                        symbols.insert(id as u32, fqn.to_string());
                    }
                }
            }
            break;
        }
    }
    symbols
}

/// Full component names (`pkg::ModDef::path`) indexed by component id
/// (`item.index`), from the cached `ComponentAnalysis` results.
fn component_names(
    entries: &[&JsonValue],
    symbols: &BTreeMap<u32, String>,
) -> BTreeMap<String, String> {
    let mut components: BTreeMap<String, String> = BTreeMap::new();
    for entry in entries {
        if entry_name(entry) == "ComponentAnalysis" {
            if let Some(list) = entry["result"]["ComponentAnalysis"]["components"].as_array() {
                for component in list {
                    let path = component.get(0).and_then(|p| p.as_str());
                    let id = component.get(1).and_then(|c| c["id"].as_str());
                    if let (Some(path), Some(id)) = (path, id) {
                        let full = match id.split_once('.') {
                            Some((item, _)) => {
                                let fqn = match item.parse::<u32>() {
                                    Ok(symbol_id) => symbols
                                        .get(&symbol_id)
                                        .cloned()
                                        .unwrap_or_else(|| format!("sym{item}")),
                                    Err(_) => item.to_string(),
                                };
                                format!("{fqn}::{path}")
                            }
                            None => path.to_string(),
                        };
                        components.insert(id.to_string(), full);
                    }
                }
            }
        }
    }
    components
}

/// Line-col strings (`"12:5"`) indexed by (package, ast node), from
/// the cached `Parsing` results.
fn location_lines(entries: &[&JsonValue]) -> BTreeMap<(u32, u32), String> {
    let mut map: BTreeMap<(u32, u32), String> = BTreeMap::new();
    for entry in entries {
        if entry_name(entry) != "Parsing" {
            continue;
        }
        let Some(pkg) = entry["key"]["Parsing"].as_u64().map(|id| id as u32) else {
            continue;
        };
        let Some(spans) = entry["result"]["Parsing"]["spans"].as_array() else {
            continue;
        };
        for (node, span) in spans.iter().enumerate() {
            let line = span[0][0].as_u64().unwrap_or(0);
            let col = span[0][1].as_u64().unwrap_or(0);
            map.entry((pkg, node as u32))
                .or_insert_with(|| format!("{line}:{col}"));
        }
    }
    map
}

/// Rewrites `Location(...)`, `PackageId(N)`, and `SymbolId(N)` tokens
/// in a debug string into readable HTML with links.
fn prettify_debug_html(debug: &str, ctx: &Ctx) -> String {
    let mut out = String::new();
    let mut rest = debug;
    loop {
        let location_idx = rest.find("Location(PackageId(");
        let symbol_idx = rest.find("SymbolId(");
        let package_idx = rest.find("PackageId(");
        let next = [location_idx, symbol_idx, package_idx]
            .into_iter()
            .flatten()
            .min();
        let Some(start) = next else {
            out.push_str(&escape_html(rest));
            break;
        };
        out.push_str(&escape_html(&rest[..start]));
        rest = &rest[start..];
        if rest.starts_with("Location(PackageId(") {
            match parse_location_token(rest, ctx) {
                Some((html, len)) => {
                    out.push_str(&html);
                    rest = &rest[len..];
                }
                None => {
                    out.push_str(&escape_html("Location("));
                    rest = &rest["Location(".len()..];
                }
            }
        } else if rest.starts_with("SymbolId(") {
            match parse_digits_token(rest, "SymbolId(") {
                Some((id, len)) => {
                    out.push_str(&ctx.symbol(id));
                    rest = &rest[len..];
                }
                None => {
                    out.push_str(&escape_html("SymbolId("));
                    rest = &rest["SymbolId(".len()..];
                }
            }
        } else {
            match parse_digits_token(rest, "PackageId(") {
                Some((id, len)) => {
                    out.push_str(&escape_html(&format!("Package({})", ctx.package(id))));
                    rest = &rest[len..];
                }
                None => {
                    out.push_str(&escape_html("PackageId("));
                    rest = &rest["PackageId(".len()..];
                }
            }
        }
    }
    out
}

/// Parses a `Location(PackageId(N), AstNodeId(M))` token at the start
/// of `rest`, returning `(html, consumed_len)`.
fn parse_location_token(rest: &str, ctx: &Ctx) -> Option<(String, usize)> {
    let prefix = "Location(PackageId(";
    let after = rest.strip_prefix(prefix)?;
    let (pkg, pkg_len) = parse_digits(after)?;
    let mid = "), AstNodeId(";
    let after = after[pkg_len..].strip_prefix(mid)?;
    let (node, node_len) = parse_digits(after)?;
    let after = after[node_len..].strip_prefix("))")?;
    let _ = after;
    let len = prefix.len() + pkg_len + mid.len() + node_len + 2;
    let pkg_id = u32::try_from(pkg).ok()?;
    let node_id = u32::try_from(node).ok()?;
    let name = ctx.package(pkg as u64);
    let html = match ctx.locations.get(&(pkg_id, node_id)) {
        Some(linecol) => {
            let line = linecol.split(':').next().unwrap_or("0");
            format!(
                "<a href=\"Parsing.html#parsing-p{pkg_id}-L{line}\">{}[{linecol}]</a>",
                escape_html(&name),
            )
        }
        None => escape_html(&format!("{name}[node {node_id}]")),
    };
    Some((html, len))
}

/// Parses `(digits, consumed_len)` at the start of `rest`.
fn parse_digits(rest: &str) -> Option<(u64, usize)> {
    let end = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
    if end == 0 {
        return None;
    }
    let digits = &rest[..end];
    let value = digits.parse().ok()?;
    Some((value, end))
}

/// Parses `prefix<digits>)` at the start of `rest`, returning
/// `(value, consumed_len)`.
fn parse_digits_token(rest: &str, prefix: &str) -> Option<(u64, usize)> {
    let after = rest.strip_prefix(prefix)?;
    let (value, digits_len) = parse_digits(after)?;
    after[digits_len..].strip_prefix(')')?;
    Some((value, prefix.len() + digits_len + 1))
}

const WARNING_PAYLOADS: [&str; 7] = [
    "NoRegDrivers",
    "NotIt",
    "UnusedSource",
    "ReadFromSink",
    "UnfilledHole",
    "EmptyDriverBlock",
    "RedundantDriver",
];

/// Classifies a diagnostic JSON object into "error", "warning", or
/// "info" based on its payload variant.
fn diagnostic_level(diagnostic: &JsonValue) -> &'static str {
    let payload = &diagnostic["payload"];
    let variant = payload
        .as_object()
        .and_then(|map| map.keys().next())
        .map(|key| key.as_str())
        .unwrap_or("");
    if WARNING_PAYLOADS.contains(&variant) {
        "warning"
    } else if variant == "Todo" {
        "info"
    } else {
        "error"
    }
}

fn render_index(
    kind_rows: &[(String, usize, f64)],
    entries: &[&JsonValue],
    packages: &[String],
) -> String {
    let mut out = String::new();
    out.push_str(&page_header("style.css"));

    let total_duration: f64 = kind_rows.iter().map(|(_, _, s)| s).sum();
    out.push_str("<h1>Virdant Database Dump</h1>\n");
    out.push_str(&format!(
        "<p>{} cached query results across {} query kinds. \
        Total build time: {:.1} ms.</p>\n",
        entries.len(),
        kind_rows.len(),
        total_duration * 1000.0,
    ));

    if !packages.is_empty() {
        let links: Vec<String> = packages
            .iter()
            .enumerate()
            .map(|(id, name)| {
                format!(
                    "<a href=\"query/Source.html#source-p{id}\">{}</a>",
                    escape_html(name),
                )
            })
            .collect();
        out.push_str(&format!("<p class=\"muted\">Packages: {}</p>\n", links.join(", ")));
    }

    // Diagnostics summary from the cached `Check` result.
    let mut num_errors = 0;
    let mut num_warnings = 0;
    let mut num_infos = 0;
    let mut has_check = false;
    for entry in entries {
        if entry_name(entry) == "Check" {
            has_check = true;
            if let Some(diagnostics) = entry["result"]["Check"].as_array() {
                for diagnostic in diagnostics {
                    match diagnostic_level(diagnostic) {
                        "warning" => num_warnings += 1,
                        "info" => num_infos += 1,
                        _ => num_errors += 1,
                    }
                }
            }
            break;
        }
    }
    if has_check {
        out.push_str("<h2>Diagnostics</h2>\n<p>");
        out.push_str(&format!(
            "<span class=\"badge error\">{} errors</span> \
             <span class=\"badge warning\">{} warnings</span> \
             <span class=\"badge muted\">{} infos</span>\n",
            num_errors, num_warnings, num_infos,
        ));
        out.push_str(&format!(
            "<p><a href=\"query/Check.html\">Full check results</a></p>\n"
        ));
    }

    out.push_str("<h2>Query kinds</h2>\n");
    out.push_str("<table><tr><th>Query</th><th>Cached results</th><th>Time</th></tr>\n");
    for (kind, count, secs) in kind_rows {
        out.push_str(&format!(
            "<tr><td><a href=\"query/{kind}.html\">{kind}</a></td><td>{count}</td><td>{:.1} ms</td></tr>\n",
            secs * 1000.0,
        ));
    }
    out.push_str("</table>\n");

    out.push_str(&format!(
        "<p class=\"muted\">Raw JSON: <a href=\"data/db.json\">db.json</a> \
        (also per query kind under <a href=\"data/\">data/</a>).</p>\n"
    ));

    out.push_str(&page_footer());
    out
}

fn render_kind_page(
    kind: &str,
    entries: &[&JsonValue],
    ctx: &Ctx,
) -> String {
    let mut out = String::new();
    out.push_str(&page_header("../style.css"));

    let total_secs: f64 = entries
        .iter()
        .map(|e| e["duration_secs"].as_f64().unwrap_or(0.0))
        .sum();

    out.push_str(&format!("<h1>{kind}</h1>\n"));
    out.push_str(&format!(
        "<nav><a href=\"../index.html\">&larr; Index</a> \
        &middot; <a href=\"../data/{kind}.json\">JSON</a></nav>\n",
    ));
    out.push_str(&format!(
        "<p>{} cached result(s). Total time: {:.1} ms.</p>\n",
        entries.len(),
        total_secs * 1000.0,
    ));

    for entry in entries {
        let debug = entry["debug"].as_str().unwrap_or("?");
        let rev = entry["rev"].as_u64().unwrap_or(0);
        let secs = entry["duration_secs"].as_f64().unwrap_or(0.0);
        let num_deps = entry["deps"]
            .as_array()
            .map(|deps| deps.len())
            .unwrap_or(0);

        let body = match kinds::render_result(ctx, kind, &entry["result"]) {
            Some(html) => html,
            None => generic_entry_body(&entry["result"], &entry["key"]),
        };

        let anchor_attr = match entry_anchor(kind, entry) {
            Some(anchor) => format!(" id=\"{anchor}\""),
            None => String::new(),
        };

        out.push_str(&format!("<div class=\"entry\"{anchor_attr}>\n"));
        out.push_str(&format!(
            "<div><span class=\"key\">{}</span> \
            <span class=\"meta\">rev {} | {:.1} ms | {} deps</span></div>\n",
            prettify_debug_html(debug, ctx),
            rev,
            secs * 1000.0,
            num_deps,
        ));
        out.push_str(&body);
        out.push_str(&format!(
            "<details><summary>Deps ({num_deps})</summary>{}</details>\n",
            deps_html(ctx, &entry["deps"]),
        ));
        out.push_str("</div>\n");
    }

    out.push_str(&page_footer());
    out
}

/// The raw-JSON fallback used for query kinds without a smart
/// renderer.
fn generic_entry_body(result: &JsonValue, key: &JsonValue) -> String {
    let mut out = String::new();
    let result_pretty = serde_json::to_string_pretty(result)
        .unwrap_or_else(|_| "<serialization error>".to_string());
    let key_pretty = serde_json::to_string_pretty(key)
        .unwrap_or_else(|_| "<serialization error>".to_string());
    let open = if result_pretty.len() < 2000 { " open" } else { "" };
    out.push_str(&format!(
        "<details{open}><summary>Result</summary><pre>{}</pre></details>\n",
        escape_html(&result_pretty),
    ));
    out.push_str(&format!(
        "<details><summary>Key</summary><pre>{}</pre></details>\n",
        escape_html(&key_pretty),
    ));
    out
}

/// The HTML anchor id for an entry whose page other entries may link
/// to, when the key is a simple id (`Parsing`, `Source`,
/// `ComponentAnalysis`).
fn entry_anchor(kind: &str, entry: &JsonValue) -> Option<String> {
    let id = entry["key"].get(kind)?.as_u64()?;
    let prefix = match kind {
        "ComponentAnalysis" => "ca",
        "Parsing" => "parsing-p",
        "Source" => "source-p",
        _ => return None,
    };
    Some(format!("{prefix}{id}"))
}

/// Renders the deps list as linked query names with readable
/// arguments.
fn deps_html(ctx: &Ctx, deps: &JsonValue) -> String {
    let empty = Vec::new();
    let mut out = String::from("<ul class=\"deps\">");
    for dep in deps.as_array().unwrap_or(&empty) {
        let (name, args) = match dep.as_object().and_then(|map| map.iter().next()) {
            Some((name, args)) => (name.as_str(), args),
            None => ("?", &JsonValue::Null),
        };
        let args_html = query_args_html(ctx, name, args);
        let args_part = if args_html.is_empty() {
            String::new()
        } else {
            format!("({args_html})")
        };
        out.push_str(&format!(
            "<li><a href=\"{name}.html\">{name}</a>{args_part}</li>"
        ));
    }
    out.push_str("</ul>");
    out
}

/// Renders a query's key arguments as readable linked HTML.
fn query_args_html(ctx: &Ctx, name: &str, args: &JsonValue) -> String {
    // Location-keyed queries take an ExprRoot-like struct or a raw
    // location string.
    if let Some(location) = args.get("location") {
        return ctx.location(location);
    }
    if let Some((pkg, id)) = args
        .get("package")
        .and_then(|p| p.as_u64())
        .zip(args.get("id").and_then(|i| i.as_u64()))
    {
        return escape_html(&format!("{}#str{id}", ctx.package(pkg)));
    }
    if let Some(text) = args.as_str() {
        return match name {
            "Component" => ctx.component(text),
            "TypeAt" | "Typeof" | "ExprRootFor" | "LocationRegion" => ctx.location(args),
            _ => escape_html(text),
        };
    }
    if let Some(id) = args.as_u64() {
        return match name {
            "Parsing" | "Source" => escape_html(&ctx.package(id)),
            _ => ctx.symbol(id),
        };
    }
    if args.is_null() {
        return String::new();
    }
    escape_html(&format!("{args}"))
}

fn page_header(css_href: &str) -> String {
    format!(
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>Virdant Database Dump</title>\n\
         <link rel=\"stylesheet\" href=\"{css_href}\">\n</head>\n<body>\n<main>\n"
    )
}

fn page_footer() -> String {
    "</main>\n</body>\n</html>\n".to_string()
}

const STYLE_CSS: &str = r#":root {
    color-scheme: dark;
    --bg: #0d1117;
    --panel: #161b22;
    --border: #30363d;
    --text: #c9d1d9;
    --muted: #8b949e;
    --accent: #58a6ff;
    --error: #f85149;
    --warning: #d29922;
}

* { box-sizing: border-box; }

body {
    margin: 0;
    background: var(--bg);
    color: var(--text);
    font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    font-size: 14px;
    line-height: 1.5;
}

main { max-width: 1100px; margin: 0 auto; padding: 24px 16px 64px; }

h1 { font-size: 22px; border-bottom: 1px solid var(--border); padding-bottom: 8px; }
h2 { font-size: 16px; color: var(--accent); margin-top: 32px; }

a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
a { border-radius: 2px; }
:target { outline: 2px solid var(--accent); background: rgba(88, 166, 255, 0.08); }

nav { margin: 8px 0 16px; color: var(--muted); }

table { border-collapse: collapse; width: 100%; margin: 12px 0; }
th, td { border: 1px solid var(--border); padding: 6px 10px; text-align: left; }
th { background: var(--panel); color: var(--muted); font-weight: 600; }
tr:nth-child(even) td { background: rgba(255, 255, 255, 0.02); }

pre {
    background: var(--panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 12px;
    overflow-x: auto;
    margin: 8px 0;
    font-size: 12.5px;
}

details {
    border: 1px solid var(--border);
    border-radius: 6px;
    margin: 8px 0;
    background: var(--panel);
}
details > summary { cursor: pointer; padding: 8px 12px; color: var(--muted); user-select: none; }
details > pre { margin: 0; border: none; border-top: 1px solid var(--border); border-radius: 0 0 6px 6px; }

.entry {
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 12px 16px;
    margin: 16px 0;
    background: var(--panel);
}
.key { color: var(--accent); word-break: break-all; }
.meta { color: var(--muted); float: right; font-size: 12px; }

.badge {
    display: inline-block;
    padding: 1px 8px;
    border-radius: 10px;
    font-size: 12px;
    margin-right: 8px;
}
.badge.error { background: rgba(248, 81, 73, 0.15); color: var(--error); }
.badge.warning { background: rgba(210, 153, 34, 0.15); color: var(--warning); }
.badge.muted { background: rgba(139, 148, 158, 0.15); color: var(--muted); }
.muted { color: var(--muted); }
"#;
