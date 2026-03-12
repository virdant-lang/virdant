use crate::verilog;
use crate::virir;

#[cfg(test)]
mod tests;

pub fn convert_virir_to_verilog(virir: virir::VirIr) -> verilog::Verilog {
    verilog::Verilog {
        files: virir
            .packages
            .into_iter()
            .enumerate()
            .map(|(package_index, package)| convert_package(package_index, package))
            .collect(),
    }
}

fn convert_package(package_index: usize, package: virir::Package) -> verilog::VerilogFile {
    let file_stem = package_file_stem(package_index, &package);
    let item_count = package.items.len();

    verilog::VerilogFile {
        name: format!("{file_stem}.v"),
        modules: package
            .items
            .into_iter()
            .enumerate()
            .map(|(item_index, item)| convert_item(&file_stem, item_index, item_count, item))
            .collect(),
    }
}

fn convert_item(
    file_stem: &str,
    item_index: usize,
    item_count: usize,
    item: virir::Item,
) -> verilog::Module {
    match item {
        virir::Item::ModDef(mod_def) => convert_mod_def(file_stem, item_index, item_count, mod_def),
    }
}

fn convert_mod_def(
    file_stem: &str,
    item_index: usize,
    item_count: usize,
    mod_def: virir::ModDef,
) -> verilog::Module {
    // VirIr does not currently retain the module's source name, so we derive a
    // plausible Verilog module name from the package/file stem.
    let base_name = default_module_name(file_stem);
    let name = if item_count == 1 {
        base_name
    } else {
        format!("{base_name}{}", item_index + 1)
    };

    verilog::Module {
        name: valid_verilog_name(&name),
        ports: mod_def.ports.into_iter().map(convert_port).collect(),
        elements: mod_def.drivers.into_iter().map(convert_driver).collect(),
    }
}

fn convert_port(port: virir::Port) -> verilog::Port {
    verilog::Port {
        name: valid_verilog_name(&port.name),
        dir: port.dir,
        width: port.width.into(),
    }
}

fn convert_driver(driver: virir::Driver) -> verilog::Element {
    let name = valid_verilog_name(&driver.path);
    verilog::Element::Assign(verilog::Assign {
        name,
        expr: convert_expr(driver.expr.as_ref()),
    })
}

const VERILOG_KEYWORDS: &[&str] = &[
    "always", "and", "assign", "automatic", "begin", "buf", "bufif0", "bufif1", "case",
    "casex", "casez", "cell", "cmos", "config", "deassign", "default", "defparam", "design",
    "disable", "edge", "else", "end", "endcase", "endconfig", "endfunction", "endgenerate",
    "endmodule", "endprimitive", "endspecify", "endtable", "endtask", "event", "for", "force",
    "forever", "fork", "function", "generate", "genvar", "highz0", "highz1", "if", "ifnone",
    "incdir", "include", "initial", "inout", "input", "instance", "integer", "join", "large",
    "liblist", "library", "localparam", "macromodule", "medium", "module", "nand", "negedge",
    "nmos", "nor", "noshowcancelled", "not", "notif0", "notif1", "or", "output", "parameter",
    "pmos", "posedge", "primitive", "pull0", "pull1", "pulldown", "pullup", "pulsestyle_ondetect",
    "pulsestyle_onevent", "rcmos", "real", "realtime", "reg", "release", "repeat", "rnmos",
    "rpmos", "rtran", "rtranif0", "rtranif1", "scalared", "showcancelled", "signed", "small",
    "specify", "specparam", "strong0", "strong1", "supply0", "supply1", "table", "task",
    "time", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand",
    "trior", "trireg", "unsigned", "use", "uwire", "vectored", "wait", "wand",
    "weak0", "weak1", "while", "wire", "wor", "xnor", "xor",
];

/// Takes a Virdant path and converts it to a valid Verilog identifier.
/// For paths that are already valid Verilog identifiers, it it preserves the name.
/// If the path conflicts with a Verilog keyword, it prefixes the name with a \.
/// If the path contains .'s, it will prefix the name with a \.
fn valid_verilog_name(path: &str) -> String {
    if is_simple_verilog_identifier(path) && !VERILOG_KEYWORDS.contains(&path) {
        path.to_string()
    } else {
        format!(r"\{path} ")
    }
}

fn is_simple_verilog_identifier(path: &str) -> bool {
    let mut chars = path.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !matches!(first, 'a'..='z' | 'A'..='Z' | '_') {
        return false;
    }

    chars.all(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
}

fn convert_expr(expr: &virir::expr::Expr) -> verilog::Expr {
    match expr {
        virir::expr::Expr::Reference(reference) => {
            verilog::Expr::Reference(verilog::expr::Reference {
                name: valid_verilog_name(&reference.path),
            })
        }
        virir::expr::Expr::Literal(bit_lit) => {
            verilog::Expr::BitLit(verilog::expr::BitLit {
                value: bit_lit.value(),
            })
        }
        virir::expr::Expr::BinOp(_) => {
            panic!("VirIr::BinOp cannot yet be converted because it does not store operands")
        }
    }
}

fn package_file_stem(package_index: usize, package: &virir::Package) -> String {
    package
        .items
        .iter()
        .map(item_package_name)
        .find(|name| !name.is_empty())
        .unwrap_or_else(|| format!("package_{}", package_index + 1))
}

fn item_package_name(item: &virir::Item) -> String {
    match item {
        virir::Item::ModDef(mod_def) => normalize_name(&mod_def.region.package().to_string()),
    }
}

fn normalize_name(name: &str) -> String {
    name.replace("::", "_").replace('-', "_")
}

fn default_module_name(file_stem: &str) -> String {
    let mut module_name = String::new();
    for part in file_stem.split('_').filter(|part| !part.is_empty()) {
        let mut chars = part.chars();
        if let Some(ch) = chars.next() {
            module_name.extend(ch.to_uppercase());
            module_name.push_str(chars.as_str());
        }
    }

    if module_name.is_empty() {
        "Top".to_string()
    } else {
        module_name
    }
}
