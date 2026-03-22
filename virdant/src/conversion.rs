use std::collections::HashMap;

use crate::common;
use crate::common::Radix;
use crate::verilog;
use crate::verilog::{exact_verilog_name, valid_verilog_name};
use crate::virir;

/// Converts a parsed VirIr program into its Verilog representation.
pub fn convert_virir_to_verilog(virir: virir::VirIr) -> verilog::Verilog {
    let emitted_module_names = collect_emitted_module_names(&virir);
    let type_widths = collect_type_widths(&virir);
    let module_ports = collect_module_ports(&virir, &type_widths);

    let mut verilog = verilog::Verilog {
        files: virir
            .packages
            .into_iter()
            .map(|package| convert_package(&emitted_module_names, &type_widths, &module_ports, package))
            .collect(),
    };

    // Lift Concat/Repeat sub-expressions that iverilog cannot handle in-place
    // (e.g. as bit-select subjects or nested inside a concatenation) into SSA
    // temporary wires at the module level.
    verilog.normalize();

    verilog
}

/// Converts a single VirIr package into one emitted Verilog file.
fn convert_package(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    module_ports: &HashMap<String, Vec<(String, virir::Width)>>,
    package: virir::Package,
) -> verilog::VerilogFile {
    let file_stem = package.name;
    let package_name = file_stem.clone();

    verilog::VerilogFile {
        name: format!("{file_stem}.sv"),
        modules: package
            .items
            .into_iter()
            .map(|item| convert_item(emitted_module_names, type_widths, module_ports, &package_name, item))
            .collect(),
    }
}

/// Converts one VirIr item within a package into a Verilog module.
fn convert_item(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    module_ports: &HashMap<String, Vec<(String, virir::Width)>>,
    package_name: &str,
    item: virir::Item,
) -> verilog::Module {
    match item {
        virir::Item::ModDef(mod_def) => convert_mod_def(emitted_module_names, type_widths, module_ports, package_name, mod_def),
    }
}

/// Converts a VirIr module definition into a Verilog module.
fn convert_mod_def(
    emitted_module_names: &HashMap<String, String>,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    module_ports: &HashMap<String, Vec<(String, virir::Width)>>,
    package_name: &str,
    mod_def: virir::ModDef,
) -> verilog::Module {
    let module_name = emitted_module_name(emitted_module_names, package_name, &mod_def);
    let ports = mod_def.ports.iter().map(|port| convert_port(type_widths, port)).collect();

    let sequential_regs: HashMap<_, _> = mod_def
        .regs
        .iter()
        .filter_map(|reg| reg.clock.as_deref().map(|clock| (reg.name.as_str(), clock)))
        .collect();

    let reg_reset_elements: Vec<_> = mod_def
        .regs
        .iter()
        .filter(|reg| reg.clock.is_some())
        .map(|reg| convert_reg_reset(type_widths, reg))
        .collect();

    let mut combinational_drivers = vec![];
    let mut sequential_driver_elements = vec![];

    for driver in mod_def.drivers {
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

    // For each submodule instance: emit one wire per port (using the dotted
    // name, e.g. `\foo.inp `), then the instantiation element itself.
    for instance in mod_def.instances {
        let raw_name = &instance.name;
        let ports_info = module_ports.get(&instance.module_path).cloned().unwrap_or_default();

        for (port_name, width) in &ports_info {
            elements.push(verilog::Element::Wire(verilog::Wire {
                name: valid_verilog_name(&format!("{raw_name}.{port_name}")),
                width: *width,
                expr: None,
            }));
        }

        elements.push(verilog::Element::Submodule(verilog::Submodule {
            name: valid_verilog_name(raw_name),
            submodule_name: emitted_module_name_for_path(emitted_module_names, &instance.module_path),
            ports: ports_info.into_iter().map(|(name, _)| name).collect(),
        }));
    }

    elements.extend(combinational_drivers.into_iter().map(|driver| convert_driver(type_widths, driver)));
    elements.extend(sequential_driver_elements);

    if let Some(on) = mod_def.on {
        elements.push(convert_on(type_widths, on));
    }

    verilog::Module { name: module_name, ports, elements }
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

/// Collects the port names and widths for every module in the VirIr.
/// Returns a map from `package::ModuleName` to a list of `(port_name, width)` pairs.
fn collect_module_ports(
    virir: &virir::VirIr,
    type_widths: &HashMap<virir::TypeId, virir::Width>,
) -> HashMap<String, Vec<(String, virir::Width)>> {
    let mut module_ports = HashMap::new();
    for package in &virir.packages {
        for item in &package.items {
            match item {
                virir::Item::ModDef(mod_def) => {
                    let module_path = qualified_module_name(&package.name, &mod_def.name);
                    let ports = mod_def
                        .ports
                        .iter()
                        .map(|port| (port.name.clone(), type_widths[&port.typ]))
                        .collect();
                    module_ports.insert(module_path, ports);
                }
            }
        }
    }
    module_ports
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


/// Builds a package-qualified module name in `package::Module` form.
fn qualified_module_name(package_name: &str, module_name: &str) -> String {
    format!("{package_name}::{module_name}")
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
                // fill is always 1 bit (BitLit or Index), so repeat width = extend_by * 1
                width: extend_by,
            }),
            convert_expr(type_widths, expr),
        ],
        width: target_width,
    })
}


/// Converts a general VirIr expression into the corresponding Verilog expression node.
fn convert_expr(
    type_widths: &HashMap<virir::TypeId, virir::Width>,
    expr: &virir::expr::Expr,
) -> verilog::Expr {
    match expr {
        virir::expr::Expr::Reference(reference) => {
            verilog::Expr::Reference(verilog::expr::Reference {
                name: valid_verilog_name(&reference.path),
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
        virir::expr::Expr::Word(word) => {
            let width = type_widths[&word.typ];
            verilog::Expr::Concat(verilog::expr::Concat {
                exprs: word
                    .exprs
                    .iter()
                    .map(|expr| convert_expr(type_widths, expr.as_ref()))
                    .collect(),
                width,
            })
        }
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
