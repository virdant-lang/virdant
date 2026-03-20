use std::collections::HashMap;

use crate::common;
use crate::common::Radix;
use crate::verilog;
use crate::virir;

/// Converts a parsed VirIr program into its Verilog representation.
pub fn convert_virir_to_verilog(virir: virir::VirIr) -> verilog::Verilog {
    let emitted_module_names = collect_emitted_module_names(&virir);
    let type_widths = collect_type_widths(&virir);

    verilog::Verilog {
        files: virir
            .packages
            .into_iter()
            .map(|package| convert_package(&emitted_module_names, &type_widths, package))
            .collect(),
    }
}

/// Converts a single VirIr package into one emitted Verilog file.
fn convert_package(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    package: virir::Package,
) -> verilog::VerilogFile {
    let file_stem = package.name;
    let package_name = file_stem.clone();

    verilog::VerilogFile {
        name: format!("{file_stem}.sv"),
        modules: package
            .items
            .into_iter()
            .map(|item| convert_item(emitted_module_names, type_widths, &package_name, item))
            .collect(),
    }
}

/// Converts one VirIr item within a package into a Verilog module.
fn convert_item(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    package_name: &str,
    item: virir::Item,
) -> verilog::Module {
    match item {
        virir::Item::ModDef(mod_def) => convert_mod_def(emitted_module_names, type_widths, package_name, mod_def),
    }
}

/// Converts a VirIr module definition into a Verilog module, preserving package qualification.
/// REVIEW I don't fully understand what this does.
fn convert_mod_def(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    package_name: &str,
    mod_def: virir::ModDef,
) -> verilog::Module {
    let module_name = emitted_module_name(emitted_module_names, package_name, &mod_def);
    let ports = mod_def
        .ports
        .iter()
        .map(|port| convert_port(type_widths, port))
        .collect();
    let mut connects_by_instance: HashMap<String, Vec<(String, String)>> = mod_def
        .instances
        .iter()
        .map(|instance| (instance.name.clone(), vec![]))
        .collect();

    let ordinary_drivers = collect_instance_connects(
        type_widths,
        mod_def.drivers,
        &mut connects_by_instance,
    );
    let sequential_regs: HashMap<_, _> = mod_def
        .regs
        .iter()
        .filter_map(|reg| {
            reg.clock
                .as_deref()
                .map(|clock| (reg.name.as_str(), clock))
        })
        .collect();
    let mut combinational_drivers = vec![];
    let mut sequential_driver_elements = vec![];
    let reg_reset_elements: Vec<_> = mod_def
        .regs
        .iter()
        .filter(|reg| reg.clock.is_some())
        .map(|reg| convert_reg_reset(type_widths, reg))
        .collect();

    for driver in ordinary_drivers {
        if let Some(clock) = sequential_regs.get(driver.path.as_str()) {
            sequential_driver_elements.push(convert_sequential_reg_driver(type_widths, driver, clock));
        } else {
            combinational_drivers.push(driver);
        }
    }

    let mut elements: Vec<verilog::Element> = mod_def
        .wires
        .into_iter()
        .map(|wire| convert_wire(type_widths, wire))
        .collect();

    elements.extend(mod_def.regs.into_iter().map(|reg| convert_reg(type_widths, reg)));

    elements.extend(reg_reset_elements);

    elements.extend(mod_def
        .instances
        .into_iter()
        .map(|instance| {
            let connects = connects_by_instance.remove(&instance.name).unwrap_or_default();
            let name = valid_verilog_name(&instance.name);
            let submodule_name = emitted_module_name_for_path(
                emitted_module_names,
                &instance.module_path,
            );
            verilog::Element::Submodule(verilog::Submodule {
                name,
                submodule_name,
                connects,
            })
        })
        .collect::<Vec<_>>());

    elements.extend(
        combinational_drivers
            .into_iter()
            .map(|driver| convert_driver(type_widths, driver)),
    );

    elements.extend(sequential_driver_elements);

    if let Some(on) = mod_def.on {
        elements.push(convert_on(type_widths, on));
    }

    verilog::Module {
        name: module_name,
        ports,
        elements,
    }
}

/// Collects the emitted Verilog name for each package-qualified VirIr module path.
fn collect_emitted_module_names(virir: &virir::VirIr) -> HashMap<String, String> {
    let mut emitted_module_names = HashMap::new();

    for package in &virir.packages {
        for item in &package.items {
            match item {
                virir::Item::ModDef(mod_def) => {
                    let module_path = qualified_module_name(&package.name, &mod_def.name);
                    let emitted_name = if mod_def.is_export {
                        exact_verilog_name(&mod_def.name)
                    } else {
                        valid_verilog_name(&module_path)
                    };
                    emitted_module_names.insert(module_path, emitted_name);
                }
            }
        }
    }

    emitted_module_names
}

fn collect_type_widths(virir: &virir::VirIr) -> HashMap<virir::TypeId, virir::Width> {
    virir
        .types
        .iter()
        .enumerate()
        .map(|(i, typ)| (virir::TypeId::new(i.try_into().unwrap()), typ.width()))
        .collect()
}

/// Returns the emitted Verilog name for a package-qualified VirIr module path.
fn emitted_module_name_for_path(
    emitted_module_names: &HashMap<String, String>,
    module_path: &str,
) -> String {
    emitted_module_names
        .get(module_path)
        .cloned()
        .unwrap_or_else(|| valid_verilog_name(module_path))
}

/// Returns the emitted Verilog name for a VirIr module definition.
fn emitted_module_name(
    emitted_module_names: &HashMap<String, String>,
    package_name: &str,
    mod_def: &virir::ModDef,
) -> String {
    emitted_module_name_for_path(
        emitted_module_names,
        &qualified_module_name(package_name, &mod_def.name),
    )
}

/// Collects submodule port connections from drivers and returns the remaining ordinary drivers.
/// REVIEW I don't fully understand what this does.
fn collect_instance_connects(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    drivers: Vec<virir::Driver>,
    connects_by_instance: &mut HashMap<String, Vec<(String, String)>>,
) -> Vec<virir::Driver> {
    let mut ordinary_drivers = vec![];

    for driver in drivers {
        if let Some((instance_name, port_name)) = split_instance_path(&driver.path) {
            if let Some(connects) = connects_by_instance.get_mut(instance_name) {
                let verilog_name = exact_verilog_name(port_name);
                let expr = convert_connect_expr(type_widths, driver.expr.as_ref());
                connects.push((verilog_name, expr));
                continue;
            }
        }

        if let virir::expr::Expr::Reference(reference) = driver.expr.as_ref() {
            if let Some((instance_name, port_name)) = split_instance_path(&reference.path) {
                if let Some(connects) = connects_by_instance.get_mut(instance_name) {
                    connects.push((
                        exact_verilog_name(port_name),
                        valid_verilog_name(&driver.path),
                    ));
                    continue;
                }
            }
        }

        ordinary_drivers.push(driver);
    }

    ordinary_drivers
}

/// Converts a VirIr port declaration into its Verilog port form.
fn convert_port(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    port: &virir::Port,
) -> verilog::Port {
    verilog::Port {
        name: exact_verilog_name(&port.name),
        kind: verilog::PortKind::Wire,
        dir: port.dir,
        width: type_widths[&port.typ],
    }
}

fn convert_wire(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    wire: virir::Wire,
) -> verilog::Element {
    verilog::Element::Wire(verilog::Wire {
        name: exact_verilog_name(&wire.name),
        width: type_widths[&wire.typ],
        expr: None,
    })
}

fn convert_reg(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    reg: virir::Reg,
) -> verilog::Element {
    verilog::Element::Reg(verilog::Reg {
        name: exact_verilog_name(&reg.name),
        width: type_widths[&reg.typ],
        expr: None,
    })
}

fn convert_reg_reset(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    reg: &virir::Reg,
) -> verilog::Element {
    verilog::Element::Initial(verilog::Initial {
        stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
            name: valid_verilog_name(&reg.name),
            expr: reg_default_expr(type_widths[&reg.typ]),
        })],
    })
}

/// Converts a VirIr driver into a Verilog continuous assignment.
fn convert_driver(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    driver: virir::Driver,
) -> verilog::Element {
    let name = valid_verilog_name(&driver.path);
    verilog::Element::Assign(verilog::Assign {
        name,
        expr: convert_expr(type_widths, driver.expr.as_ref()),
    })
}

fn convert_sequential_reg_driver(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    driver: virir::Driver,
    clock: &virir::expr::Expr,
) -> verilog::Element {
    verilog::Element::Always(verilog::Always {
        clock: Some(convert_expr(type_widths, clock)),
        stmts: vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
            name: valid_verilog_name(&driver.path),
            expr: convert_expr(type_widths, driver.expr.as_ref()),
        })],
    })
}

fn zero_expr(width: virir::Width) -> verilog::Expr {
    verilog::Expr::WordLit(verilog::expr::WordLit {
        value: 0,
        width,
        radix: Radix::Dec,
    })
}

fn reg_default_expr(width: virir::Width) -> verilog::Expr {
    verilog::Expr::WordLit(verilog::expr::WordLit {
        value: 0,
        width,
        radix: Radix::Dec,
    })
}

fn convert_on(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    on: virir::On,
) -> verilog::Element {
    verilog::Element::Always(verilog::Always {
        clock: Some(convert_expr(type_widths, on.clock.as_ref())),
        stmts: on.commands.into_iter().map(|command| convert_command(type_widths, command)).collect(),
    })
}

fn convert_command(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    command: virir::Command,
) -> verilog::Stmt {
    match command {
        virir::Command::Assert(expr) => verilog::Stmt::Assert(verilog::Assert {
            exprs: vec![convert_expr(type_widths, expr.as_ref())],
        }),
        virir::Command::Display(expr) => verilog::Stmt::Display(verilog::Display {
            exprs: vec![convert_expr(type_widths, expr.as_ref())],
        }),
        virir::Command::Finish => verilog::Stmt::Finish,
        virir::Command::Fatal => verilog::Stmt::Fatal,
    }
}

/// Converts an expression used in a submodule connection into Verilog source text.
fn convert_connect_expr(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    expr: &virir::expr::Expr,
) -> String {
    match expr {
        virir::expr::Expr::Reference(reference) => valid_verilog_name(&reference.path),
        virir::expr::Expr::WordLit(word_lit) => {
            let width = type_widths[&word_lit.typ];
            format!("{width}'d{}", word_lit.value)
        }
        virir::expr::Expr::Word(word) => format!(
            "{{{}}}",
            word.exprs
                .iter()
                .map(|expr| convert_connect_expr(type_widths, expr.as_ref()))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        virir::expr::Expr::BitLit(bit_lit) => {
            if bit_lit.value() { "1'h1" } else { "1'h0" }.to_string()
        }
        virir::expr::Expr::BinOp(binop) => format!(
            "({} {} {})",
            convert_connect_expr(type_widths, binop.lhs.as_ref()),
            convert_connect_binop(binop.op),
            convert_connect_expr(type_widths, binop.rhs.as_ref())
        ),
        virir::expr::Expr::UnOp(unop) => format!(
            "({}{})",
            convert_connect_unop(unop.op),
            convert_connect_expr(type_widths, unop.expr.as_ref())
        ),
        virir::expr::Expr::Zext(zext) => convert_connect_ext_expr(type_widths, zext.typ, zext.expr.as_ref(), false),
        virir::expr::Expr::Sext(sext) => convert_connect_ext_expr(type_widths, sext.typ, sext.expr.as_ref(), true),
        virir::expr::Expr::If(expr_if) => format!(
            "({} ? {} : {})",
            convert_connect_expr(type_widths, expr_if.cond.as_ref()),
            convert_connect_expr(type_widths, expr_if.then_expr.as_ref()),
            convert_connect_expr(type_widths, expr_if.else_expr.as_ref())
        ),
        virir::expr::Expr::Index(index) => format!(
            "{}[{}]",
            convert_connect_expr(type_widths, index.subject.as_ref()),
            index.index,
        ),
        virir::expr::Expr::IndexRange(index_range) => format!(
            "{}[{}:{}]",
            convert_connect_expr(type_widths, index_range.subject.as_ref()),
            index_range.index_hi,
            index_range.index_lo,
        ),
    }
}

/// Converts a VirIr binary operator into the corresponding textual Verilog operator.
fn convert_connect_binop(op: common::BinOp) -> &'static str {
    match op {
        common::BinOp::Lt => "<",
        common::BinOp::Lte => "<=",
        common::BinOp::Gt => ">",
        common::BinOp::Gte => ">=",
        common::BinOp::Eq => "==",
        common::BinOp::Neq => "!=",
        common::BinOp::Add => "+",
        common::BinOp::Sub => "-",
        common::BinOp::And => "&&",
        common::BinOp::Or => "||",
        common::BinOp::Xor => "^",
    }
}

fn convert_connect_unop(op: common::UnOp) -> &'static str {
    match op {
        common::UnOp::Neg => "-",
        common::UnOp::Inv => "~",
        common::UnOp::Not => "!",
    }
}

/// Splits a `instance.port` path into its instance name and port name components.
fn split_instance_path(path: &str) -> Option<(&str, &str)> {
    let (instance_name, port_name) = path.split_once('.')?;
    if port_name.contains('.') {
        None
    } else {
        Some((instance_name, port_name))
    }
}

/// Builds a package-qualified module name in `package::Module` form.
fn qualified_module_name(package_name: &str, module_name: &str) -> String {
    format!("{package_name}::{module_name}")
}

/// Returns a Verilog identifier only if it can be emitted exactly as written.
fn exact_verilog_name(name: &str) -> String {
    let verilog_name = valid_verilog_name(name);
    if verilog_name != name {
        // TODO Support exported names and port names that require escaping while preserving the source spelling.
        panic!("Name `{name}` cannot be preserved exactly in Verilog");
    }
    verilog_name
}

#[rustfmt::skip]
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

/// Returns whether a path is already a plain, unescaped Verilog identifier.
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

/// Converts a VirIr binary operator into the corresponding Verilog binary operator.
/// TODO I need to add a virir-level BinOp enum, since those are distinct.
fn convert_binop(op: common::BinOp) -> verilog::BinOp {
    match op {
        common::BinOp::Lt => verilog::BinOp::Lt,
        common::BinOp::Lte => verilog::BinOp::Lte,
        common::BinOp::Gt => verilog::BinOp::Gt,
        common::BinOp::Gte => verilog::BinOp::Gte,
        common::BinOp::Eq => verilog::BinOp::Eq,
        common::BinOp::Neq => verilog::BinOp::Ne,
        common::BinOp::Add => verilog::BinOp::Add,
        common::BinOp::Sub => verilog::BinOp::Sub,
        common::BinOp::And => verilog::BinOp::LogAnd,
        common::BinOp::Or => verilog::BinOp::LogOr,
        common::BinOp::Xor => verilog::BinOp::BitXor,
    }
}

fn convert_unop(op: common::UnOp) -> verilog::UnOp {
    match op {
        common::UnOp::Neg => verilog::UnOp::Neg,
        common::UnOp::Inv => verilog::UnOp::BitNot,
        common::UnOp::Not => verilog::UnOp::LogNot,
    }
}

fn expr_type_id(expr: &virir::expr::Expr) -> virir::TypeId {
    match expr {
        virir::expr::Expr::Reference(reference) => reference.typ,
        virir::expr::Expr::BitLit(bit_lit) => bit_lit.typ,
        virir::expr::Expr::WordLit(word_lit) => word_lit.typ,
        virir::expr::Expr::Word(word) => word.typ,
        virir::expr::Expr::BinOp(binop) => binop.typ,
        virir::expr::Expr::UnOp(unop) => unop.typ,
        virir::expr::Expr::Zext(zext) => zext.typ,
        virir::expr::Expr::Sext(sext) => sext.typ,
        virir::expr::Expr::If(expr_if) => expr_if.typ,
        virir::expr::Expr::Index(index) => index.typ,
        virir::expr::Expr::IndexRange(index_range) => index_range.typ,
    }
}

fn expr_width(type_widths: &HashMap<virir::TypeId, virir::Width>, expr: &virir::expr::Expr) -> virir::Width {
    type_widths[&expr_type_id(expr)]
}

fn constant_word_expr(width: virir::Width, value: u128) -> verilog::Expr {
    verilog::Expr::WordLit(verilog::expr::WordLit {
        value,
        width,
        radix: Radix::Dec,
    })
}

fn constant_index_expr(index: virir::Width) -> verilog::Expr {
    constant_word_expr(32, u128::from(index))
}

fn convert_ext_expr(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    target_type: virir::TypeId,
    expr: &virir::expr::Expr,
    signed: bool,
) -> verilog::Expr {
    let subject_width = expr_width(type_widths, expr);
    let target_width = type_widths[&target_type];
    let extend_by = target_width
        .checked_sub(subject_width)
        .unwrap_or_else(|| panic!("cannot extend width {subject_width} into {target_width}"));

    if extend_by == 0 {
        return convert_expr(type_widths, expr);
    }

    let fill = if signed {
        if subject_width == 1 {
            convert_expr(type_widths, expr)
        } else {
            verilog::Expr::Index(verilog::expr::Index {
                subject: Box::new(convert_expr(type_widths, expr)),
                index: Box::new(constant_index_expr(subject_width - 1)),
            })
        }
    } else {
        constant_word_expr(1, 0)
    };

    verilog::Expr::Concat(verilog::expr::Concat {
        exprs: vec![
            verilog::Expr::Repeat(verilog::expr::Repeat {
                count: Box::new(constant_index_expr(extend_by)),
                exprs: vec![fill],
            }),
            convert_expr(type_widths, expr),
        ],
    })
}

fn convert_connect_ext_expr(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    target_type: virir::TypeId,
    expr: &virir::expr::Expr,
    signed: bool,
) -> String {
    let subject_width = expr_width(type_widths, expr);
    let target_width = type_widths[&target_type];
    let extend_by = target_width
        .checked_sub(subject_width)
        .unwrap_or_else(|| panic!("cannot extend width {subject_width} into {target_width}"));
    let subject = convert_connect_expr(type_widths, expr);

    if extend_by == 0 {
        return subject;
    }

    let fill = if signed {
        if subject_width == 1 {
            subject.clone()
        } else {
            format!("{}[{}]", subject, subject_width - 1)
        }
    } else {
        "1'd0".to_string()
    };
    let repeated = format!("{{{}{{{}}}}}", extend_by, fill);
    format!("{{{}, {}}}", repeated, subject)
}

/// Converts a general VirIr expression into the corresponding Verilog expression node.
fn convert_expr(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    expr: &virir::expr::Expr,
) -> verilog::Expr {
    match expr {
        virir::expr::Expr::Reference(reference) => {
            let name = valid_verilog_name(&reference.path);
            debug_assert!(!name.contains(' '));
            verilog::Expr::Reference(verilog::expr::Reference {
                name,
            })
        }
        virir::expr::Expr::WordLit(wordlit) => {
            let width = type_widths[&wordlit.typ];
            verilog::Expr::WordLit(verilog::expr::WordLit {
                value: wordlit.value.into(),
                width,
                radix: Radix::Dec,
            })
        }
        virir::expr::Expr::Word(word) => verilog::Expr::Concat(verilog::expr::Concat {
            exprs: word
                .exprs
                .iter()
                .map(|expr| convert_expr(type_widths, expr.as_ref()))
                .collect(),
        }),
        virir::expr::Expr::BitLit(bit_lit) => {
            verilog::Expr::BitLit(verilog::expr::BitLit {
                value: bit_lit.value(),
            })
        }
        virir::expr::Expr::BinOp(binop) => verilog::Expr::BinOp(verilog::expr::BinOp {
            op: convert_binop(binop.op),
            lhs: Box::new(convert_expr(type_widths, binop.lhs.as_ref())),
            rhs: Box::new(convert_expr(type_widths, binop.rhs.as_ref())),
        }),
        virir::expr::Expr::UnOp(unop) => verilog::Expr::UnOp(verilog::expr::UnOp {
            op: convert_unop(unop.op),
            expr: Box::new(convert_expr(type_widths, unop.expr.as_ref())),
        }),
        virir::expr::Expr::Zext(zext) => convert_ext_expr(type_widths, zext.typ, zext.expr.as_ref(), false),
        virir::expr::Expr::Sext(sext) => convert_ext_expr(type_widths, sext.typ, sext.expr.as_ref(), true),
        virir::expr::Expr::If(expr_if) => verilog::Expr::If(verilog::expr::If {
            cond: Box::new(convert_expr(type_widths, expr_if.cond.as_ref())),
            then_expr: Box::new(convert_expr(type_widths, expr_if.then_expr.as_ref())),
            else_expr: Box::new(convert_expr(type_widths, expr_if.else_expr.as_ref())),
        }),
        virir::expr::Expr::Index(index) => verilog::Expr::Index(verilog::expr::Index {
            subject: Box::new(convert_expr(type_widths, index.subject.as_ref())),
            index: Box::new(constant_index_expr(index.index)),
        }),
        virir::expr::Expr::IndexRange(index_range) => {
            verilog::Expr::IndexRange(verilog::expr::IndexRange {
                subject: Box::new(convert_expr(type_widths, index_range.subject.as_ref())),
                index_hi: Box::new(constant_index_expr(index_range.index_hi)),
                index_lo: Box::new(constant_index_expr(index_range.index_lo)),
            })
        }
    }
}
