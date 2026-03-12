use std::collections::HashMap;

use crate::verilog;
use crate::virir;

#[cfg(test)]
mod tests;

pub fn convert_virir_to_verilog(virir: virir::VirIr) -> verilog::Verilog {
    verilog::Verilog {
        files: virir
            .packages
            .into_iter()
            .map(convert_package)
            .collect(),
    }
}

fn convert_package(package: virir::Package) -> verilog::VerilogFile {
    let file_stem = package.name;

    verilog::VerilogFile {
        name: format!("{file_stem}.sv"),
        modules: package.items.into_iter().map(convert_item).collect(),
    }
}

fn convert_item(item: virir::Item) -> verilog::Module {
    match item {
        virir::Item::ModDef(mod_def) => convert_mod_def(mod_def),
    }
}

fn convert_mod_def(mod_def: virir::ModDef) -> verilog::Module {
    let mut connects_by_instance: HashMap<String, Vec<(String, String)>> = mod_def
        .instances
        .iter()
        .map(|instance| (instance.name.clone(), vec![]))
        .collect();
    let mut ordinary_drivers = vec![];

    for driver in mod_def.drivers {
        if let Some((instance_name, port_name)) = split_instance_path(&driver.path) {
            if let Some(connects) = connects_by_instance.get_mut(instance_name) {
                connects.push((
                    valid_verilog_name(port_name),
                    convert_connect_expr(driver.expr.as_ref()),
                ));
                continue;
            }
        }

        if let virir::expr::Expr::Reference(reference) = driver.expr.as_ref() {
            if let Some((instance_name, port_name)) = split_instance_path(&reference.path) {
                if let Some(connects) = connects_by_instance.get_mut(instance_name) {
                    connects.push((
                        valid_verilog_name(port_name),
                        valid_verilog_name(&driver.path),
                    ));
                    continue;
                }
            }
        }

        ordinary_drivers.push(driver);
    }

    let mut elements: Vec<verilog::Element> = mod_def
        .instances
        .into_iter()
        .map(|instance| {
            let submodule_name = instance_module_name(&instance.module_path);
            let connects = connects_by_instance.remove(&instance.name).unwrap_or_default();
            verilog::Element::Submodule(verilog::Submodule {
                name: valid_verilog_name(&instance.name),
                submodule_name: valid_verilog_name(&submodule_name),
                connects,
            })
        })
        .collect();
    elements.extend(ordinary_drivers.into_iter().map(convert_driver));

    verilog::Module {
        name: valid_verilog_name(&mod_def.name),
        ports: mod_def.ports.into_iter().map(convert_port).collect(),
        elements,
    }
}

fn convert_port(port: virir::Port) -> verilog::Port {
    verilog::Port {
        name: valid_verilog_name(&port.name),
        kind: verilog::PortKind::Wire,
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

fn convert_connect_expr(expr: &virir::expr::Expr) -> String {
    match expr {
        virir::expr::Expr::Reference(reference) => valid_verilog_name(&reference.path),
        virir::expr::Expr::Literal(bit_lit) => {
            if bit_lit.value() { "1'h1" } else { "1'h0" }.to_string()
        }
        virir::expr::Expr::BinOp(_) => {
            panic!("VirIr::BinOp cannot yet be used as a submodule connection")
        }
    }
}

fn split_instance_path(path: &str) -> Option<(&str, &str)> {
    let (instance_name, port_name) = path.split_once('.')?;
    if port_name.contains('.') {
        None
    } else {
        Some((instance_name, port_name))
    }
}

fn instance_module_name(module_path: &str) -> String {
    module_path
        .rsplit("::")
        .next()
        .unwrap_or(module_path)
        .to_string()
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

