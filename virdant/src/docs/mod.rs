use std::io::Write;
use std::path::Path;

use bstr::{BStr, BString, ByteSlice};
use serde::Serialize;
use tera::Tera;

use crate::analysis::symbols::{SymbolKind, SymbolTable};
use crate::common::PortDir;
use crate::db::Db;
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

// ---------------------------------------------------------------------------
// Template context structs (Serializable)
// ---------------------------------------------------------------------------

#[derive(Clone, Serialize)]
struct SidebarPkgCtx {
    name: String,
    is_active: bool,
    items: Vec<SidebarItemCtx>,
}

#[derive(Clone, Serialize)]
struct SidebarItemCtx {
    name: String,
    filename: String,
    kind_label: String,
    is_current: bool,
}

#[derive(Serialize)]
struct ProjectIndexCtx {
    style_href: String,
    all_packages: Vec<SidebarPkgCtx>,
    packages: Vec<PackageEntry>,
    items: Vec<ItemEntryCtx>,
}

#[derive(Serialize)]
struct PackageEntry {
    name: String,
    item_count: usize,
}

#[derive(Serialize)]
struct ItemEntryCtx {
    package: String,
    name: String,
    kind_label: String,
    filename: String,
    doc_first_line: String,
}

#[derive(Serialize)]
struct PackageIndexCtx {
    style_href: String,
    all_packages: Vec<SidebarPkgCtx>,
    package_name: String,
    package_doc: String,
    items: Vec<PackageItemCtx>,
}

#[derive(Clone, Serialize)]
struct PackageItemCtx {
    name: String,
    kind_label: String,
    filename: String,
    doc_first_line: String,
}

#[derive(Clone, Serialize)]
struct ModdefPageCtx {
    style_href: String,
    all_packages: Vec<SidebarPkgCtx>,
    package_name: String,
    name: String,
    doc_body: String,
    ports: Vec<PortCtx>,
    sockets: Vec<SocketCtx>,
}

#[derive(Clone, Serialize)]
struct PortCtx {
    direction: String,
    name: String,
    type_str: String,
}

#[derive(Clone, Serialize)]
struct SocketCtx {
    role: String,
    name: String,
    ofness: String,
}

#[derive(Clone, Serialize)]
struct TypePageCtx {
    style_href: String,
    all_packages: Vec<SidebarPkgCtx>,
    package_name: String,
    kind_label: String,
    name: String,
    doc_body: String,
    definition_text: String,
}

#[derive(Clone, Serialize)]
struct SimplePageCtx {
    style_href: String,
    all_packages: Vec<SidebarPkgCtx>,
    package_name: String,
    name: String,
    doc_body: String,
    definition_text: String,
}

// ---------------------------------------------------------------------------
// Tera engine setup
// ---------------------------------------------------------------------------

fn make_tera() -> Result<Tera, tera::Error> {
    let mut tera = Tera::default();
    tera.add_raw_template(
        "base.html.tera",
        include_str!("templates/base.html.tera"),
    )?;
    tera.add_raw_template(
        "project_index.html.tera",
        include_str!("templates/project_index.html.tera"),
    )?;
    tera.add_raw_template(
        "package_index.html.tera",
        include_str!("templates/package_index.html.tera"),
    )?;
    tera.add_raw_template(
        "moddef_page.html.tera",
        include_str!("templates/moddef_page.html.tera"),
    )?;
    tera.add_raw_template(
        "type_page.html.tera",
        include_str!("templates/type_page.html.tera"),
    )?;
    tera.add_raw_template(
        "socket_page.html.tera",
        include_str!("templates/socket_page.html.tera"),
    )?;
    tera.add_raw_template(
        "fn_page.html.tera",
        include_str!("templates/fn_page.html.tera"),
    )?;
    Ok(tera)
}

fn style_href(nesting_depth: usize) -> String {
    if nesting_depth == 0 {
        String::new()
    } else {
        let mut s = String::new();
        for _ in 0..nesting_depth {
            s.push_str("../");
        }
        s
    }
}


// ---------------------------------------------------------------------------
// Internal data collected during first pass
// ---------------------------------------------------------------------------

struct ItemEntry {
    package: PackageFqn,
    name: BString,
    kind: SymbolKind,
    doc_body: BString,
}

struct PkgData {
    package: PackageFqn,
    package_doc: String,
    pkg_items: Vec<PackageItemCtx>,
}

// ---------------------------------------------------------------------------
// Sidebar tree builder
// ---------------------------------------------------------------------------

fn build_sidebar_tree(
    packages: &[PackageFqn],
    all_items: &[ItemEntry],
    active_pkg: Option<&str>,
    current_item: Option<&str>,
) -> Vec<SidebarPkgCtx> {
    let mut result: Vec<SidebarPkgCtx> = Vec::new();
    for pkg in packages {
        let pkg_name = pkg.as_ref().to_str_lossy().into_owned();
        let is_active = active_pkg == Some(pkg_name.as_str());
        let mut items: Vec<SidebarItemCtx> = Vec::new();
        for entry in all_items {
            let entry_pkg = entry.package.as_ref().to_str_lossy();
            if entry_pkg != pkg_name {
                continue;
            }
            let entry_name = entry.name.to_str_lossy().into_owned();
            let is_current = current_item == Some(entry_name.as_str());
            items.push(SidebarItemCtx {
                name: entry_name,
                filename: format!("{}.html", entry.name.to_str_lossy()),
                kind_label: kind_label(&entry.kind).to_owned(),
                is_current,
            });
        }
        result.push(SidebarPkgCtx {
            name: pkg_name,
            is_active,
            items,
        });
    }
    result
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

    // Build Tera engine (templates embedded via include_str!)
    let tera = make_tera()?;

    // Collect packages
    let packages = db.get_packages();
    let symboltable = db.get_symboltable();

    // -------------------------------------------------------------------
    // First pass: collect all items across all packages
    // -------------------------------------------------------------------
    let mut all_items: Vec<ItemEntry> = Vec::new();
    let mut pkg_datas: Vec<PkgData> = Vec::new();

    for pkg in packages.iter() {
        let analysis = db.get_package_analysis(pkg.clone());
        let parsing = db.get_parsing(pkg.clone());

        let pkg_str: String = pkg.as_ref().to_str_lossy().into_owned();
        let pkg_dir = out_dir.join(&pkg_str);
        std::fs::create_dir_all(&pkg_dir)?;

        let mut pkg_items: Vec<PackageItemCtx> = Vec::new();

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let kind = node_kind(&node);
            let doc_body = extract_doc_body(&node, &parsing);

            // Render to HTML string (without sidebar -- we fill that later)
            let _html = match kind {
                SymbolKind::ModDef => render_moddef_page(
                    &tera,
                    &node,
                    &parsing,
                    db,
                    &symboltable,
                    pkg,
                    item_name.as_bstr(),
                    doc_body.as_bstr(),
                )?,
                SymbolKind::StructDef
                | SymbolKind::UnionDef
                | SymbolKind::EnumDef
                | SymbolKind::BuiltinDef => render_type_page(
                    &tera,
                    &node,
                    &parsing,
                    &kind,
                    item_name.as_bstr(),
                    doc_body.as_bstr(),
                )?,
                SymbolKind::SocketDef => render_socket_page(
                    &tera,
                    &node,
                    &parsing,
                    item_name.as_bstr(),
                    doc_body.as_bstr(),
                )?,
                SymbolKind::FnDef => render_fn_page(
                    &tera,
                    &node,
                    &parsing,
                    item_name.as_bstr(),
                    doc_body.as_bstr(),
                )?,
                _ => continue,
            };

            all_items.push(ItemEntry {
                package: pkg.clone(),
                name: item_name.clone(),
                kind: kind.clone(),
                doc_body: doc_body.clone(),
            });

            pkg_items.push(PackageItemCtx {
                name: item_name.to_str_lossy().into_owned(),
                kind_label: kind_label(&kind).to_owned(),
                filename: format!("{}.html", item_name),
                doc_first_line: first_line(doc_body.as_bstr()),
            });
        }

        let package_doc = extract_package_doc(&parsing);
        pkg_datas.push(PkgData {
            package: pkg.clone(),
            package_doc: package_doc.to_str_lossy().into_owned(),
            pkg_items,
        });
    }

    // -------------------------------------------------------------------
    // Second pass: write all pages with full sidebar
    // -------------------------------------------------------------------

    // Project index (no active package, no current item)
    let project_sidebar = build_sidebar_tree(&packages, &all_items, None, None);
    let project_ctx = ProjectIndexCtx {
        style_href: style_href(0),
        all_packages: project_sidebar,
        packages: packages
            .iter()
            .map(|pkg| {
                let pkg_name = pkg.as_ref().to_str_lossy();
                let count = all_items
                    .iter()
                    .filter(|e| e.package.as_ref().to_str_lossy() == pkg_name)
                    .count();
                PackageEntry {
                    name: pkg_name.into_owned(),
                    item_count: count,
                }
            })
            .collect(),
        items: all_items
            .iter()
            .map(|entry| ItemEntryCtx {
                package: entry.package.as_ref().to_str_lossy().into_owned(),
                name: entry.name.to_str_lossy().into_owned(),
                kind_label: kind_label(&entry.kind).to_owned(),
                filename: format!("{}.html", entry.name.to_str_lossy()),
                doc_first_line: first_line(entry.doc_body.as_bstr()),
            })
            .collect(),
    };
    let project_index_html =
        tera.render("project_index.html.tera", &tera::Context::from_serialize(&project_ctx)?)?;
    let project_index_path = out_dir.join("index.html");
    let mut file = std::fs::File::create(&project_index_path)?;
    file.write_all(project_index_html.as_bytes())?;

    // Per-package pages
    for pkg_data in &pkg_datas {
        let pkg_str: String = pkg_data.package.as_ref().to_str_lossy().into_owned();
        let pkg_dir = out_dir.join(&pkg_str);

        // Package index (active package, no current item)
        let pkg_sidebar = build_sidebar_tree(
            &packages, &all_items, Some(&pkg_str), None,
        );
        let pkg_ctx = PackageIndexCtx {
            style_href: style_href(1),
            all_packages: pkg_sidebar,
            package_name: pkg_str.clone(),
            package_doc: pkg_data.package_doc.clone(),
            items: pkg_data.pkg_items.clone(),
        };
        let pkg_index_html = tera.render(
            "package_index.html.tera",
            &tera::Context::from_serialize(&pkg_ctx)?,
        )?;
        let mut file = std::fs::File::create(&pkg_dir.join("index.html"))?;
        file.write_all(pkg_index_html.as_bytes())?;

        // Item pages (active package + current item)
        for entry in &all_items {
            let entry_pkg = entry.package.as_ref().to_str_lossy();
            if entry_pkg != pkg_str {
                continue;
            }
            let item_name = entry.name.to_str_lossy().into_owned();
            let item_sidebar = build_sidebar_tree(
                &packages, &all_items, Some(&pkg_str), Some(&item_name),
            );

            // Re-render the HTML with sidebar
            // We stored the inner HTML, now we need to inject sidebar
            // and re-render through the template.
            // But the render_* functions build the full page from scratch.
            // They each call tera.render() with their own context.
            // We need the context without sidebar first, then add sidebar.
            //
            // Simplest approach: rebuild the context and re-render.
            let ast_node_id = db
                .get_package_analysis(entry.package.clone())
                .item_ast_node_id(entry.name.as_ref());
            let parsing = db.get_parsing(entry.package.clone());
            let node = parsing.ast_node(ast_node_id);
            let kind = node_kind(&node);

            let html = match kind {
                SymbolKind::ModDef => render_moddef_page_with_sidebar(
                    &tera,
                    &node,
                    &parsing,
                    db,
                    &symboltable,
                    &entry.package,
                    entry.name.as_bstr(),
                    entry.doc_body.as_bstr(),
                    &item_sidebar,
                )?,
                SymbolKind::StructDef
                | SymbolKind::UnionDef
                | SymbolKind::EnumDef
                | SymbolKind::BuiltinDef => render_type_page_with_sidebar(
                    &tera,
                    &node,
                    &parsing,
                    &kind,
                    entry.name.as_bstr(),
                    entry.doc_body.as_bstr(),
                    &item_sidebar,
                    &pkg_str,
                )?,
                SymbolKind::SocketDef => render_socket_page_with_sidebar(
                    &tera,
                    &node,
                    &parsing,
                    entry.name.as_bstr(),
                    entry.doc_body.as_bstr(),
                    &item_sidebar,
                    &pkg_str,
                )?,
                SymbolKind::FnDef => render_fn_page_with_sidebar(
                    &tera,
                    &node,
                    &parsing,
                    entry.name.as_bstr(),
                    entry.doc_body.as_bstr(),
                    &item_sidebar,
                    &pkg_str,
                )?,
                _ => continue,
            };

            let html_filename = format!("{}.html", entry.name.to_str_lossy());
            let mut file = std::fs::File::create(&pkg_dir.join(&html_filename))?;
            file.write_all(html.as_bytes())?;
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn first_line(body: &BStr) -> String {
    body.to_str_lossy().lines().next().unwrap_or("").to_owned()
}

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

/// Extract the body of a docstring (//> ...) from an AST node.
/// Returns the text with the `//> ` prefix stripped from each line.
fn extract_doc_body(node: &AstNode<'_>, parsing: &Parsing) -> BString {
    if let Some(doc) = node.doc_string() {
        let raw = parsing.string(doc.clone());
        let mut body = BString::default();
        for line in raw.lines() {
            // Strip leading "//>" (3 bytes) from each line
            let stripped = if line.len() >= 3 && &line[..3] == b"//>" {
                &line[3..]
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
                // Strip leading "//!" (3 bytes) from each line
                let stripped = if line.len() >= 3 && &line[..3] == b"//!" {
                    &line[3..]
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
    let css = include_str!("templates/style.css");
    std::fs::write(out_dir.join("style.css"), css)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Template rendering helpers
// ---------------------------------------------------------------------------

fn render_moddef_page_with_sidebar(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    db: &Db,
    symboltable: &SymbolTable,
    pkg: &PackageFqn,
    item_name: &BStr,
    doc_body: &BStr,
    all_packages: &[SidebarPkgCtx],
) -> Result<String, Box<dyn std::error::Error>> {
    let pkg_str = pkg.as_ref().to_str_lossy().into_owned();
    let name_str: String = item_name.to_str_lossy().into_owned();

    // Build FQN for symbol lookup
    let fqn: BString = format!("{pkg_str}::{name_str}").into();
    let symbol = symboltable
        .resolve_item_fqn(fqn.as_bstr())
        .expect("moddef symbol not found");

    // Get ports via the ports query
    let ports = db.get_ports_of(symbol.id());

    // Get sockets by walking the AST
    let mut sockets: Vec<SocketCtx> = Vec::new();
    for child in node.children() {
        if let AstNodePayload::Socket(socket) = child.payload() {
            let role_str = match socket.role {
                crate::common::SocketRole::Client => "client",
                crate::common::SocketRole::Server => "server",
            };
            let ofness_node = child.child(1);
            let ofness_str = ofness_node.spelling().to_str_lossy().into_owned();
            sockets.push(SocketCtx {
                name: parsing.string(socket.name).to_str_lossy().into_owned(),
                role: role_str.to_owned(),
                ofness: ofness_str,
            });
        }
    }

    let ctx = ModdefPageCtx {
        style_href: style_href(1),
        all_packages: all_packages.to_vec(),
        package_name: pkg_str.clone(),
        name: name_str,
        doc_body: doc_body.to_str_lossy().into_owned(),
        ports: ports
            .iter()
            .map(|port| {
                let dir_str = match port.dir {
                    PortDir::Input => "incoming",
                    PortDir::Output => "outgoing",
                };
                PortCtx {
                    direction: dir_str.to_owned(),
                    name: port.path.to_str_lossy().into_owned(),
                    type_str: port
                        .typ
                        .as_ref()
                        .map(|t| format!("{t:?}"))
                        .unwrap_or_else(|| "?".to_owned()),
                }
            })
            .collect(),
        sockets,
    };

    Ok(tera.render("moddef_page.html.tera", &tera::Context::from_serialize(&ctx)?)?)
}

fn render_type_page_with_sidebar(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    kind: &SymbolKind,
    item_name: &BStr,
    doc_body: &BStr,
    all_packages: &[SidebarPkgCtx],
    package_name: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let name_str: String = item_name.to_str_lossy().into_owned();

    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let ctx = TypePageCtx {
        style_href: style_href(1),
        all_packages: all_packages.to_vec(),
        package_name: package_name.to_owned(),
        kind_label: kind_label(kind).to_owned(),
        name: name_str,
        doc_body: doc_body.to_str_lossy().into_owned(),
        definition_text: definition_text.to_str_lossy().into_owned(),
    };

    Ok(tera.render("type_page.html.tera", &tera::Context::from_serialize(&ctx)?)?)
}

fn render_socket_page_with_sidebar(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
    all_packages: &[SidebarPkgCtx],
    package_name: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let name_str: String = item_name.to_str_lossy().into_owned();

    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let ctx = SimplePageCtx {
        style_href: style_href(1),
        all_packages: all_packages.to_vec(),
        package_name: package_name.to_owned(),
        name: name_str,
        doc_body: doc_body.to_str_lossy().into_owned(),
        definition_text: definition_text.to_str_lossy().into_owned(),
    };

    Ok(tera.render("socket_page.html.tera", &tera::Context::from_serialize(&ctx)?)?)
}

fn render_fn_page_with_sidebar(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
    all_packages: &[SidebarPkgCtx],
    package_name: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let name_str: String = item_name.to_str_lossy().into_owned();

    let definition_span = node.span();
    let definition_text = parsing.text(definition_span);

    let ctx = SimplePageCtx {
        style_href: style_href(1),
        all_packages: all_packages.to_vec(),
        package_name: package_name.to_owned(),
        name: name_str,
        doc_body: doc_body.to_str_lossy().into_owned(),
        definition_text: definition_text.to_str_lossy().into_owned(),
    };

    Ok(tera.render("fn_page.html.tera", &tera::Context::from_serialize(&ctx)?)?)
}

// These are stubs used during the first pass (no sidebar data yet).
// They still compile because the context defaults to empty sidebar,
// but the output is discarded and regenerated in the second pass.

fn render_moddef_page(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    db: &Db,
    symboltable: &SymbolTable,
    pkg: &PackageFqn,
    item_name: &BStr,
    doc_body: &BStr,
) -> Result<String, Box<dyn std::error::Error>> {
    render_moddef_page_with_sidebar(
        tera, node, parsing, db, symboltable, pkg, item_name, doc_body, &[],
    )
}

fn render_type_page(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    kind: &SymbolKind,
    item_name: &BStr,
    doc_body: &BStr,
) -> Result<String, Box<dyn std::error::Error>> {
    render_type_page_with_sidebar(
        tera, node, parsing, kind, item_name, doc_body, &[], "",
    )
}

fn render_socket_page(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
) -> Result<String, Box<dyn std::error::Error>> {
    render_socket_page_with_sidebar(
        tera, node, parsing, item_name, doc_body, &[], "",
    )
}

fn render_fn_page(
    tera: &Tera,
    node: &AstNode<'_>,
    parsing: &Parsing,
    item_name: &BStr,
    doc_body: &BStr,
) -> Result<String, Box<dyn std::error::Error>> {
    render_fn_page_with_sidebar(
        tera, node, parsing, item_name, doc_body, &[], "",
    )
}
