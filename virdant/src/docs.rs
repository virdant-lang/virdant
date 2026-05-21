use std::io::Write;
use std::path::Path;

use bstr::{BStr, BString, ByteSlice};

use crate::analysis::symbols::{SymbolKind, SymbolTable};
use crate::common::PortDir;
use crate::db::Db;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::syntax::parsing::Parsing;
use crate::fqn::PackageFqn;

// ---------------------------------------------------------------------------
// Module-level struct for item entries
// ---------------------------------------------------------------------------

struct ItemEntry {
    package: PackageFqn,
    name: BString,
    kind: SymbolKind,
    doc_body: BString,
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Generate HTML documentation for a Virdant project.
///
/// Writes files into `out_dir` (e.g. `build/doc/`).  Creates the directory
/// tree as needed.
pub fn generate_docs(
    db: &Db,
    out_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    std::fs::create_dir_all(out_dir)?;

    // Write shared CSS
    write_css(out_dir)?;

    // Collect packages
    let packages = db.get_packages();
    let symboltable = db.get_symboltable();

    // Build a list of all items across all packages for the project index
    let mut all_items: Vec<ItemEntry> = Vec::new();

    // Write package pages
    for pkg in packages.iter() {
        let analysis = db.get_package_analysis(pkg.clone());
        let parsing = db.get_parsing(pkg.clone());

        let pkg_dir = out_dir.join(pkg.as_ref().to_str_lossy().as_ref());
        std::fs::create_dir_all(&pkg_dir)?;

        // Collect items for this package
        // (name, kind, doc_body, html_filename)
        let mut pkg_items: Vec<(BString, SymbolKind, BString, String)> = Vec::new();

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let kind = node_kind(&node);
            let doc_body = extract_doc_body(&node, &parsing);

            let html = match kind {
                SymbolKind::ModDef => moddef_page(&node, &parsing, db, &symboltable, pkg, item_name.as_bstr(), doc_body.as_bstr()),
                SymbolKind::StructDef | SymbolKind::UnionDef | SymbolKind::EnumDef | SymbolKind::BuiltinDef =>
                    type_page(&node, &parsing, kind.clone(), item_name.as_bstr(), doc_body.as_bstr()),
                SymbolKind::SocketDef => socket_page(&node, &parsing, item_name.as_bstr(), doc_body.as_bstr()),
                SymbolKind::FnDef => fn_page(&node, &parsing, item_name.as_bstr(), doc_body.as_bstr()),
                _ => continue,
            };

            let html_filename = format!("{}.html", item_name);
            let item_path = pkg_dir.join(&html_filename);
            let mut file = std::fs::File::create(&item_path)?;
            file.write_all(html.as_bytes())?;

            all_items.push(ItemEntry {
                package: pkg.clone(),
                name: item_name.clone(),
                kind: kind.clone(),
                doc_body: doc_body.clone(),
            });

            pkg_items.push((
                item_name.clone(),
                kind,
                doc_body,
                html_filename,
            ));
        }

        // Write package index
        let package_doc = extract_package_doc(&parsing);
        let pkg_index = package_index_page(pkg, package_doc.as_bstr(), &pkg_items);
        let pkg_index_path = pkg_dir.join("index.html");
        let mut file = std::fs::File::create(&pkg_index_path)?;
        file.write_all(pkg_index.as_bytes())?;
    }

    // Write project index
    let project_index = project_index_page(&packages, &all_items);
    let project_index_path = out_dir.join("index.html");
    let mut file = std::fs::File::create(&project_index_path)?;
    file.write_all(project_index.as_bytes())?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: determine SymbolKind from an item AST node
// ---------------------------------------------------------------------------

fn node_kind(node: &AstNode<'_>) -> SymbolKind {
    match node.payload() {
        AstNodePayload::ModDef(_) => SymbolKind::ModDef,
        AstNodePayload::StructDef(_) => SymbolKind::StructDef,
        AstNodePayload::UnionDef(_) => SymbolKind::UnionDef,
        AstNodePayload::EnumDef(_) => SymbolKind::EnumDef,
        AstNodePayload::BuiltinDef(_) => SymbolKind::BuiltinDef,
        AstNodePayload::FnDef(_) => SymbolKind::FnDef,
        AstNodePayload::SocketDef(_) => SymbolKind::SocketDef,
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Docstring extraction
// ---------------------------------------------------------------------------

/// Extract the body of a docstring (/// ...) from an AST node.
/// Returns the text with the `/// ` prefix stripped from each line.
fn extract_doc_body(node: &AstNode<'_>, parsing: &Parsing) -> BString {
    if let Some(doc) = node.doc_string() {
        let raw = parsing.string(doc.clone());
        let mut body = BString::default();
        for line in raw.lines() {
            // Strip leading "/// " (4 bytes) from each line
            let stripped = if line.len() >= 4 && &line[..4] == b"/// " {
                &line[4..]
            } else {
                line
            };
            body.extend_from_slice(stripped);
            body.push(b'\n');
        }
        // Trim trailing newline
        while body.last() == Some(&b'\n') {
            body.pop();
        }
        body
    } else {
        BString::default()
    }
}

/// Extract the package-level docstring (//! ...) from the parsing.
fn extract_package_doc(parsing: &Parsing) -> BString {
    let root = parsing.root();
    if let AstNodePayload::Package(p) = root.payload() {
        if let Some(doc) = &p.doc_string {
            let raw = parsing.string(doc.clone());
            let mut body = BString::default();
            for line in raw.lines() {
                // Strip leading "//! " (4 bytes) from each line
                let stripped = if line.len() >= 4 && &line[..4] == b"//! " {
                    &line[4..]
                } else {
                    line
                };
                body.extend_from_slice(stripped);
                body.push(b'\n');
            }
            while body.last() == Some(&b'\n') {
                body.pop();
            }
            return body;
        }
    }
    BString::default()
}

// ---------------------------------------------------------------------------
// HTML escaping
// ---------------------------------------------------------------------------

fn html_escape(text: &BStr) -> String {
    let mut out = String::with_capacity(text.len());
    for &b in text.iter() {
        match b {
            b'&' => out.push_str("&amp;"),
            b'<' => out.push_str("&lt;"),
            b'>' => out.push_str("&gt;"),
            b'"' => out.push_str("&quot;"),
            b'\'' => out.push_str("&#39;"),
            _ => out.push(b as char),
        }
    }
    out
}

fn kind_label(kind: &SymbolKind) -> &'static str {
    match kind {
        SymbolKind::ModDef => "mod",
        SymbolKind::StructDef => "struct",
        SymbolKind::UnionDef => "union",
        SymbolKind::EnumDef => "enum",
        SymbolKind::BuiltinDef => "builtin",
        SymbolKind::FnDef => "fn",
        SymbolKind::SocketDef => "socket",
        _ => "?",
    }
}

// ---------------------------------------------------------------------------
// CSS
// ---------------------------------------------------------------------------

fn write_css(out_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let css = r#"body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                 Helvetica, Arial, sans-serif;
    max-width: 960px;
    margin: 0 auto;
    padding: 1em 2em;
    line-height: 1.6;
    color: #1a1a1a;
    background: #fafafa;
}
h1 { border-bottom: 2px solid #ddd; padding-bottom: 0.3em; }
h2 { border-bottom: 1px solid #eee; padding-bottom: 0.2em; }
pre.doc {
    background: #eef;
    padding: 1em;
    border-radius: 4px;
    white-space: pre-wrap;
}
pre.definition {
    background: #f4f4f4;
    padding: 1em;
    border-radius: 4px;
    border: 1px solid #ddd;
    overflow-x: auto;
}
table {
    border-collapse: collapse;
    width: 100%;
    margin: 1em 0;
}
th, td {
    text-align: left;
    padding: 0.5em;
    border-bottom: 1px solid #ddd;
}
tr:hover { background: #f0f0f0; }
th { background: #e8e8e8; font-weight: 600; }
a { color: #0366d6; text-decoration: none; }
a:hover { text-decoration: underline; }
ul.item-list { list-style: none; padding: 0; }
ul.item-list li { padding: 0.3em 0; }
span.kind {
    display: inline-block;
    font-size: 0.8em;
    background: #ddf;
    padding: 0.1em 0.5em;
    border-radius: 3px;
    margin-right: 0.5em;
    font-weight: 600;
}
span.kind-mod    { background: #cdf; }
span.kind-struct { background: #cfc; }
span.kind-union  { background: #fcf; }
span.kind-enum   { background: #fdc; }
span.kind-builtin{ background: #ddd; }
span.kind-fn     { background: #ddf; }
span.kind-socket { background: #fdd; }
"#;
    std::fs::write(out_dir.join("style.css"), css)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Project index page
// ---------------------------------------------------------------------------

fn project_index_page(
    packages: &[PackageFqn],
    items: &[ItemEntry],
) -> String {
    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str("<title>Virdant Project</title>\n");
    html.push_str("<link rel=\"stylesheet\" href=\"style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str("<h1>Virdant Project</h1>\n");

    // Package list
    html.push_str("<h2>Packages</h2>\n<ul class=\"item-list\">\n");
    for pkg in packages.iter() {
        let pkg_str = pkg.as_ref().to_str_lossy().to_owned();
        html.push_str(&format!(
            "<li><a href=\"{pkg_str}/index.html\">{pkg_str}</a></li>\n",
        ));
    }
    html.push_str("</ul>\n");

    // Item list
    html.push_str("<h2>Items</h2>\n<ul class=\"item-list\">\n");
    for entry in items {
        let pkg_str = entry.package.as_ref().to_str_lossy().to_owned();
        let name_str = entry.name.to_str_lossy().to_owned();
        let kind_str = kind_label(&entry.kind);
        let doc_short = if entry.doc_body.is_empty() {
            String::new()
        } else {
            let first_line = entry.doc_body
                .to_str_lossy()
                .lines()
                .next()
                .unwrap_or("")
                .to_owned();
            format!(" &mdash; {}", html_escape(BStr::new(first_line.as_bytes())))
        };
        html.push_str(&format!(
            "<li><a href=\"{pkg_str}/{name_str}.html\">\
             <span class=\"kind kind-{kind_str}\">{kind_str}</span> \
             {pkg_str}::<strong>{name_str}</strong></a>{doc_short}</li>\n",
        ));
    }
    html.push_str("</ul>\n");

    html.push_str("</body>\n</html>\n");
    html
}

// ---------------------------------------------------------------------------
// Package index page
// ---------------------------------------------------------------------------

fn package_index_page(
    pkg: &PackageFqn,
    package_doc: &BStr,
    items: &[(BString, SymbolKind, BString, String)], // (name, kind, doc_body, html_filename)
) -> String {
    let pkg_str = pkg.as_ref().to_str_lossy().to_owned();
    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str(&format!("<title>Package {pkg_str}</title>\n"));
    html.push_str("<link rel=\"stylesheet\" href=\"../style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str(&format!("<h1>Package {pkg_str}</h1>\n"));

    if !package_doc.is_empty() {
        html.push_str("<pre class=\"doc\">");
        html.push_str(&html_escape(package_doc));
        html.push_str("</pre>\n");
    }

    html.push_str("<h2>Items</h2>\n<ul class=\"item-list\">\n");
    for (name, kind, doc_body, html_filename) in items {
        let name_str = name.to_str_lossy().to_owned();
        let kind_str = kind_label(kind);
        let doc_short = if doc_body.is_empty() {
            String::new()
        } else {
            let first_line = doc_body
                .to_str_lossy()
                .lines()
                .next()
                .unwrap_or("")
                .to_owned();
            format!(" &mdash; {}", html_escape(BStr::new(first_line.as_bytes())))
        };
        html.push_str(&format!(
            "<li><a href=\"{html_filename}\">\
             <span class=\"kind kind-{kind_str}\">{kind_str}</span> \
             <strong>{name_str}</strong></a>{doc_short}</li>\n",
        ));
    }
    html.push_str("</ul>\n");

    html.push_str("</body>\n</html>\n");
    html
}

// ---------------------------------------------------------------------------
// Moddef page
// ---------------------------------------------------------------------------

fn moddef_page(
    node: &AstNode<'_>,
    parsing: &Parsing,
    db: &Db,
    symboltable: &SymbolTable,
    pkg: &PackageFqn,
    item_name: &BStr,
    doc_body: &BStr,
) -> String {
    let pkg_str = pkg.as_ref().to_str_lossy().to_owned();
    let name_str = item_name.to_str_lossy().to_owned();

    // Build FQN for symbol lookup
    let fqn: BString = format!("{pkg_str}::{name_str}").into();
    let symbol = symboltable.resolve_item_fqn(fqn.as_bstr())
        .expect("moddef symbol not found");

    // Get ports via the ports query
    let ports = db.get_ports_of(symbol.id());

    // Get sockets by walking the AST
    struct SocketInfo {
        name: String,
        role: String,
        ofness: String,
    }
    let mut sockets: Vec<SocketInfo> = Vec::new();
    for child in node.children() {
        if let AstNodePayload::Socket(socket) = child.payload() {
            let role_str = match socket.role {
                crate::common::SocketRole::Client => "client",
                crate::common::SocketRole::Server => "server",
            };
            // The Ofness is the first child of the Socket node
            let ofness_node = child.child(0);
            let ofness_str = ofness_node.spelling().to_str_lossy().to_string();
            sockets.push(SocketInfo {
                name: parsing.string(socket.name).to_str_lossy().to_string(),
                role: role_str.to_owned(),
                ofness: ofness_str,
            });
        }
    }

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str(&format!("<title>mod {pkg_str}::{name_str}</title>\n"));
    html.push_str("<link rel=\"stylesheet\" href=\"../style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str(&format!("<h1>mod {name_str}</h1>\n"));

    // Docstring
    if !doc_body.is_empty() {
        html.push_str("<pre class=\"doc\">");
        html.push_str(&html_escape(doc_body));
        html.push_str("</pre>\n");
    }

    // Ports table
    html.push_str("<h2>Ports</h2>\n");
    if ports.is_empty() {
        html.push_str("<p><em>No ports.</em></p>\n");
    } else {
        html.push_str("<table>\n<thead><tr>");
        html.push_str("<th>Direction</th><th>Name</th><th>Type</th>");
        html.push_str("</tr></thead>\n<tbody>\n");
        for port in ports.iter() {
            let dir_str = match port.dir {
                PortDir::Input => "incoming",
                PortDir::Output => "outgoing",
            };
            let path_str = port.path.to_str_lossy().to_owned();
            let typ_str = port.typ
                .as_ref()
                .map(|t| format!("{t:?}"))
                .unwrap_or_else(|| "?".to_owned());
            html.push_str(&format!(
                "<tr><td>{dir_str}</td><td>{path_str}</td><td>{typ_str}</td></tr>\n",
            ));
        }
        html.push_str("</tbody>\n</table>\n");
    }

    // Sockets table
    html.push_str("<h2>Sockets</h2>\n");
    if sockets.is_empty() {
        html.push_str("<p><em>No sockets.</em></p>\n");
    } else {
        html.push_str("<table>\n<thead><tr>");
        html.push_str("<th>Role</th><th>Name</th><th>Socket Type</th>");
        html.push_str("</tr></thead>\n<tbody>\n");
        for sock in &sockets {
            html.push_str(&format!(
                "<tr><td>{}</td><td>{}</td><td>{}</td></tr>\n",
                sock.role, sock.name, sock.ofness,
            ));
        }
        html.push_str("</tbody>\n</table>\n");
    }

    html.push_str("</body>\n</html>\n");
    html
}

// ---------------------------------------------------------------------------
// Type page (struct / union / enum / builtin)
// ---------------------------------------------------------------------------

fn type_page(
    node: &AstNode<'_>,
    parsing: &Parsing,
    kind: SymbolKind,
    item_name: &BStr,
    doc_body: &BStr,
) -> String {
    let name_str = item_name.to_str_lossy().to_owned();
    let kind_str = kind_label(&kind);

    // Get the full definition text from the source
    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str(&format!("<title>{kind_str} {name_str}</title>\n"));
    html.push_str("<link rel=\"stylesheet\" href=\"../style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str(&format!("<h1>{kind_str} {name_str}</h1>\n"));

    if !doc_body.is_empty() {
        html.push_str("<pre class=\"doc\">");
        html.push_str(&html_escape(doc_body));
        html.push_str("</pre>\n");
    }

    html.push_str("<h2>Definition</h2>\n");
    html.push_str("<pre class=\"definition\">");
    html.push_str(&html_escape(definition_text));
    html.push_str("</pre>\n");

    html.push_str("</body>\n</html>\n");
    html
}

// ---------------------------------------------------------------------------
// Socket page
// ---------------------------------------------------------------------------

fn socket_page(
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
) -> String {
    let name_str = item_name.to_str_lossy().to_owned();

    // Get the full definition text from the source
    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str(&format!("<title>socket {name_str}</title>\n"));
    html.push_str("<link rel=\"stylesheet\" href=\"../style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str(&format!("<h1>socket {name_str}</h1>\n"));

    if !doc_body.is_empty() {
        html.push_str("<pre class=\"doc\">");
        html.push_str(&html_escape(doc_body));
        html.push_str("</pre>\n");
    }

    html.push_str("<h2>Definition</h2>\n");
    html.push_str("<pre class=\"definition\">");
    html.push_str(&html_escape(definition_text));
    html.push_str("</pre>\n");

    html.push_str("</body>\n</html>\n");
    html
}

// ---------------------------------------------------------------------------
// Fn page
// ---------------------------------------------------------------------------

fn fn_page(
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
) -> String {
    let name_str = item_name.to_str_lossy().to_owned();

    // Get the full definition text from the source
    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str(&format!("<title>fn {name_str}</title>\n"));
    html.push_str("<link rel=\"stylesheet\" href=\"../style.css\">\n");
    html.push_str("</head>\n<body>\n");
    html.push_str(&format!("<h1>fn {name_str}</h1>\n"));

    if !doc_body.is_empty() {
        html.push_str("<pre class=\"doc\">");
        html.push_str(&html_escape(doc_body));
        html.push_str("</pre>\n");
    }

    html.push_str("<h2>Signature</h2>\n");
    html.push_str("<pre class=\"definition\">");
    html.push_str(&html_escape(definition_text));
    html.push_str("</pre>\n");

    html.push_str("</body>\n</html>\n");
    html
}
