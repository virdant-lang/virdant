pub mod expr;
pub mod typ;

mod parse;

#[cfg(test)]
pub mod tests;

lalrpop_util::lalrpop_mod!(grammar, "/virir/grammar.rs");

use std::fmt::Write;
use std::collections::HashMap;
use std::sync::Arc;

use crate::common::PortDir;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Span};
use crate::source::Region;

use crate::virir::expr::{BinOp as VirIrBinOp, Expr, If as VirIrIf, Reference, WordLit};
use crate::virir::typ::Type;

pub use crate::common::Width;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub fn new(id: u32) -> Self {
        TypeId(id)
    }
}

pub fn parse(text: &str) -> Result<VirIr, String> {
    grammar::VirIrParser::new()
        .parse(text)
        .map_err(|err| format!("{err:?}"))
}

#[derive(Debug)]
pub struct VirIr {
    pub packages: Vec<Package>,
    pub types: Vec<Arc<Type>>,
}

impl VirIr {
    pub fn to_text(&self) -> String {
        let mut out = String::new();
        writeln!(&mut out, "virir {{").unwrap();

        for package in &self.packages {
            write_package(&mut out, package, self);
        }

        for typ in &self.types {
            writeln!(&mut out, "    type {};", type_to_text(typ.as_ref())).unwrap();
        }

        writeln!(&mut out, "}}").unwrap();
        out
    }
}

#[derive(Debug)]
pub struct Package {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    ModDef(ModDef),
}

#[derive(Debug)]
pub struct ModDef {
    pub region: Region,
    pub is_export: bool,
    pub name: String,
    pub ports: Vec<Port>,
    pub wires: Vec<Wire>,
    pub regs: Vec<Reg>,
    pub instances: Vec<Instance>,
    pub drivers: Vec<Driver>,
    pub on: Option<On>,
}

#[derive(Debug)]
pub enum Command {
    Assert(Arc<Expr>),
    Display(),
    Finish,
    Fatal,
}

#[derive(Debug)]
pub struct On {
    pub region: Region,
    pub clock: Arc<Expr>,
    pub commands: Vec<Command>,
}


#[derive(Debug)]
pub struct Port {
    pub region: Region,
    pub name: String,
    pub dir: PortDir,
    pub typ: TypeId,
}

#[derive(Debug)]
pub struct Wire {
    pub region: Region,
    pub typ: TypeId,
    pub name: String,
}

#[derive(Debug)]
pub struct Reg {
    pub region: Region,
    pub typ: TypeId,
    pub name: String,
    pub clock: Option<Arc<Expr>>,
}

#[derive(Debug)]
pub struct Instance {
    pub region: Region,
    pub name: String,
    pub module_path: String,
}

#[derive(Debug)]
pub struct Driver {
    pub region: Region,
    pub path: String,
    pub expr: Arc<Expr>,
}

fn dummy_region() -> Region {
    Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    )
}

fn write_package(out: &mut String, package: &Package, virir: &VirIr) {
    writeln!(out, "    package {} {{", package.name).unwrap();
    for item in &package.items {
        match item {
            Item::ModDef(moddef) => write_moddef(out, moddef, virir),
        }
    }
    writeln!(out, "    }}").unwrap();
    writeln!(out).unwrap();
}

fn write_moddef(out: &mut String, moddef: &ModDef, virir: &VirIr) {
    if moddef.is_export {
        writeln!(out, "        export mod {} {{", moddef.name).unwrap();
    } else {
        writeln!(out, "        mod {} {{", moddef.name).unwrap();
    }

    for port in &moddef.ports {
        let dir = match port.dir {
            PortDir::Input => "incoming",
            PortDir::Output => "outgoing",
        };
        writeln!(out, "            {dir} {} : {};", port.name, port_type_to_text(port, virir)).unwrap();
    }

    for wire in &moddef.wires {
        writeln!(out, "            wire {} : {};", wire.name, type_id_to_text(wire.typ, virir)).unwrap();
    }

    for reg in &moddef.regs {
        if let Some(clock) = &reg.clock {
            writeln!(
                out,
                "            reg {} : {} on {};",
                reg.name,
                type_id_to_text(reg.typ, virir),
                expr_to_text(clock.as_ref(), virir),
            ).unwrap();
        } else {
            writeln!(out, "            reg {} : {};", reg.name, type_id_to_text(reg.typ, virir)).unwrap();
        }
    }

    for instance in &moddef.instances {
        writeln!(out, "            mod {} of {};", instance.name, instance.module_path).unwrap();
    }

    for driver in &moddef.drivers {
        writeln!(out, "            {} := {};", driver.path, expr_to_text(driver.expr.as_ref(), virir)).unwrap();
    }

    if let Some(on) = &moddef.on {
        writeln!(out, "            on {} {{", expr_to_text(on.clock.as_ref(), virir)).unwrap();
        for command in &on.commands {
            writeln!(out, "                {}", command_to_text(command, virir)).unwrap();
        }
        writeln!(out, "            }}").unwrap();
    }

    writeln!(out, "        }}").unwrap();
}

fn command_to_text(command: &Command, virir: &VirIr) -> String {
    match command {
        Command::Assert(expr) => format!("assert({});", expr_to_text(expr.as_ref(), virir)),
        Command::Display() => "display();".to_string(),
        Command::Finish => "finish;".to_string(),
        Command::Fatal => "fatal;".to_string(),
    }
}

fn port_type_to_text(port: &Port, virir: &VirIr) -> String {
    type_id_to_text(port.typ, virir)
}

fn type_id_to_text(type_id: TypeId, virir: &VirIr) -> String {
    let typ = virir.types[type_id.0 as usize].as_ref();
    type_to_text(typ)
}

fn type_to_text(typ: &Type) -> String {
    match typ {
        Type::Bit => "builtin::Bit".to_string(),
        Type::Clock => "builtin::Clock".to_string(),
        Type::Word(width) => format!("builtin::Word[{width}]"),
    }
}

fn expr_to_text(expr: &Expr, virir: &VirIr) -> String {
    match expr {
        Expr::Reference(reference) => {
            format!("({} : {})", reference.path, type_id_to_text(reference.typ, virir))
        }
        Expr::BitLit(bit_lit) => {
            let value = if bit_lit.value() { 1 } else { 0 };
            format!("({value} : {})", type_id_to_text(bit_lit.typ, virir))
        }
        Expr::WordLit(word_lit) => {
            format!("({} : {})", word_lit.value, type_id_to_text(word_lit.typ, virir))
        }
        Expr::BinOp(binop) => {
            let op = match binop.op {
                crate::common::BinOp::Lt => "<",
                crate::common::BinOp::Lte => "<=",
                crate::common::BinOp::Gt => ">",
                crate::common::BinOp::Gte => ">=",
                crate::common::BinOp::Eq => "==",
                crate::common::BinOp::Neq => "!=",
                crate::common::BinOp::Add => "+",
                crate::common::BinOp::Sub => "-",
                crate::common::BinOp::And => "&&",
                crate::common::BinOp::Or => "||",
                crate::common::BinOp::Xor => "!=",
            };
            format!(
                "({} {op} {} : {})",
                expr_to_text(binop.lhs.as_ref(), virir),
                expr_to_text(binop.rhs.as_ref(), virir),
                type_id_to_text(binop.typ, virir),
            )
        }
        Expr::If(expr_if) => {
            format!(
                "(if {} {{ {} }} else {{ {} }} : {})",
                expr_to_text(expr_if.cond.as_ref(), virir),
                expr_to_text(expr_if.then_expr.as_ref(), virir),
                expr_to_text(expr_if.else_expr.as_ref(), virir),
                type_id_to_text(expr_if.typ, virir),
            )
        }
    }
}

fn build_mod_def(is_export: bool, name: String, stmts: Vec<parse::ModStmt>) -> parse::ModDef {
    let mut ports = vec![];
    let mut wires = vec![];
    let mut regs = vec![];
    let mut instances = vec![];
    let mut drivers = vec![];
    let mut on = None;

    for stmt in stmts {
        match stmt {
            parse::ModStmt::Port(port) => ports.push(port),
            parse::ModStmt::Wire(wire) => wires.push(wire),
            parse::ModStmt::Reg(reg) => regs.push(reg),
            parse::ModStmt::Instance(instance) => instances.push(instance),
            parse::ModStmt::Driver(driver) => drivers.push(driver),
            parse::ModStmt::On(on_stmt) => {
                assert!(on.is_none(), "VirIr supports at most one on statement per module");
                on = Some(on_stmt);
            }
        }
    }

    parse::ModDef {
        is_export,
        name,
        ports,
        wires,
        regs,
        instances,
        drivers,
        on,
    }
}

fn join_path(head: String, tails: Vec<String>) -> String {
    let mut path = head;
    for tail in tails {
        path.push_str(&tail);
    }
    path
}

fn build_virir(packages: Vec<parse::Package>, parsed_types: Vec<parse::Type>) -> VirIr {
    let type_ids: HashMap<String, TypeId> = parsed_types
        .iter()
        .enumerate()
        .map(|(i, typ)| (typ.key(), TypeId::new(i.try_into().unwrap())))
        .collect();

    let module_signatures = build_module_signatures(&packages, &type_ids);

    VirIr {
        packages: packages
            .into_iter()
            .map(|package| build_package(package, &type_ids, &module_signatures))
            .collect(),
        types: parsed_types
            .into_iter()
            .map(|typ| Arc::new(typ.to_type()))
            .collect(),
    }
}

fn build_module_signatures(
    packages: &[parse::Package],
    type_ids: &HashMap<String, TypeId>,
) -> HashMap<String, HashMap<String, TypeId>> {
    let mut signatures = HashMap::new();

    for package in packages {
        for mod_def in &package.modules {
            let ports = mod_def
                .ports
                .iter()
                .map(|port| (port.name.clone(), lookup_type_id(&port.typ, type_ids)))
                .collect();

            signatures.insert(format!("{}::{}", package.name, mod_def.name), ports);
        }
    }

    signatures
}

fn build_package(
    package: parse::Package,
    type_ids: &HashMap<String, TypeId>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Package {
    let name = package.name;

    Package {
        name,
        items: package
            .modules
            .into_iter()
            .map(|mod_def| Item::ModDef(build_module(mod_def, type_ids, module_signatures)))
            .collect(),
    }
}

fn build_module(
    mod_def: parse::ModDef,
    type_ids: &HashMap<String, TypeId>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> ModDef {
    let clock_type = lookup_type_id(&parse::Type::Clock, type_ids);
    let local_types: HashMap<String, TypeId> = mod_def
        .ports
        .iter()
        .map(|port| (port.name.clone(), lookup_type_id(&port.typ, type_ids)))
        .chain(mod_def.wires.iter().map(|wire| (wire.name.clone(), lookup_type_id(&wire.typ, type_ids))))
        .chain(mod_def.regs.iter().map(|reg| (reg.name.clone(), lookup_type_id(&reg.typ, type_ids))))
        .collect();

    let instance_types: HashMap<String, String> = mod_def
        .instances
        .iter()
        .map(|instance| (instance.name.clone(), instance.module_path.clone()))
        .collect();
    let instances = mod_def
        .instances
        .iter()
        .map(|instance| Instance {
            region: dummy_region(),
            name: instance.name.clone(),
            module_path: instance.module_path.clone(),
        })
        .collect();

    let wires = mod_def
        .wires
        .into_iter()
        .map(|wire| Wire {
            region: dummy_region(),
            typ: lookup_type_id(&wire.typ, type_ids),
            name: wire.name,
        })
        .collect();

    let regs = mod_def
        .regs
        .into_iter()
        .map(|reg| Reg {
            region: dummy_region(),
            typ: lookup_type_id(&reg.typ, type_ids),
            name: reg.name,
            clock: reg.clock.map(|clock| Arc::new(build_expr(
                clock,
                Some(clock_type),
                type_ids,
                &local_types,
                &instance_types,
                module_signatures,
            ))),
        })
        .collect();

    let drivers = mod_def
        .drivers
        .into_iter()
        .map(|driver| {
            let expected_type = resolve_path_type(
                &driver.name,
                &local_types,
                &instance_types,
                module_signatures,
            );

            Driver {
                region: dummy_region(),
                path: driver.name,
                expr: Arc::new(build_expr(
                    driver.expr,
                    expected_type,
                    type_ids,
                    &local_types,
                    &instance_types,
                    module_signatures,
                )),
            }
        })
        .collect();

    let on = mod_def
        .on
        .map(|on_stmt| On {
            region: dummy_region(),
            clock: Arc::new(build_expr(
                on_stmt.clock,
                Some(clock_type),
                type_ids,
                &local_types,
                &instance_types,
                module_signatures,
            )),
            commands: on_stmt
                .commands
                .into_iter()
                .map(|command| build_command(command, type_ids, &local_types, &instance_types, module_signatures))
                .collect(),
        });

    ModDef {
        region: dummy_region(),
        is_export: mod_def.is_export,
        name: mod_def.name,
        ports: mod_def
            .ports
            .into_iter()
            .map(|port| Port {
                region: dummy_region(),
                name: port.name,
                dir: port.dir,
                typ: lookup_type_id(&port.typ, type_ids),
            })
            .collect(),
        wires,
        regs,
        instances,
        drivers,
        on,
    }
}

fn build_expr(
    expr: parse::Expr,
    expected_type: Option<TypeId>,
    type_ids: &HashMap<String, TypeId>,
    local_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Expr {
    match expr {
        parse::Expr::Reference { path, typ } => {
            let typ = typ
                .map(|typ| lookup_type_id(&typ, type_ids))
                .or_else(|| resolve_path_type(&path, local_types, instance_types, module_signatures))
                .or(expected_type)
                .unwrap_or(TypeId::new(0));

            Expr::Reference(Reference {
                region: dummy_region(),
                typ,
                path,
            })
        }
        parse::Expr::WordLit { value, typ } => {
            let typ = Some(typ)
                .map(|typ| lookup_type_id(&typ, type_ids))
                .or(expected_type)
                .unwrap_or(TypeId::new(0));
            Expr::WordLit(WordLit {
                region: dummy_region(),
                value: value,
                typ,
            })
        }
        parse::Expr::BinOp { lhs, op, rhs } => {
            let lhs = Arc::new(build_expr(
                *lhs,
                None,
                type_ids,
                local_types,
                instance_types,
                module_signatures,
            ));
            let rhs = Arc::new(build_expr(
                *rhs,
                None,
                type_ids,
                local_types,
                instance_types,
                module_signatures,
            ));
            let typ = infer_binop_type(op, expected_type, lhs.as_ref(), rhs.as_ref(), type_ids);

            Expr::BinOp(VirIrBinOp {
                region: dummy_region(),
                typ,
                op,
                lhs,
                rhs,
            })
        }
        parse::Expr::If {
            cond,
            then_expr,
            else_expr,
            typ,
        } => {
            let branch_expected_type = typ
                .as_ref()
                .map(|typ| lookup_type_id(typ, type_ids))
                .or(expected_type);
            let cond = Arc::new(build_expr(
                *cond,
                Some(lookup_type_id(&parse::Type::Bit, type_ids)),
                type_ids,
                local_types,
                instance_types,
                module_signatures,
            ));
            let then_expr = Arc::new(build_expr(
                *then_expr,
                branch_expected_type,
                type_ids,
                local_types,
                instance_types,
                module_signatures,
            ));
            let else_expr = Arc::new(build_expr(
                *else_expr,
                branch_expected_type,
                type_ids,
                local_types,
                instance_types,
                module_signatures,
            ));
            let typ = infer_if_type(branch_expected_type, then_expr.as_ref(), else_expr.as_ref());

            Expr::If(VirIrIf {
                region: dummy_region(),
                typ,
                cond,
                then_expr,
                else_expr,
            })
        }
    }
}

fn build_command(
    command: parse::Command,
    type_ids: &HashMap<String, TypeId>,
    local_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Command {
    let bit_type = lookup_type_id(&parse::Type::Bit, type_ids);
    match command {
        parse::Command::Assert(expr) => Command::Assert(Arc::new(build_expr(
            expr,
            Some(bit_type),
            type_ids,
            local_types,
            instance_types,
            module_signatures,
        ))),
        parse::Command::Display() => Command::Display(),
        parse::Command::Finish => Command::Finish,
        parse::Command::Fatal => Command::Fatal,
    }
}

fn infer_binop_type(
    op: crate::common::BinOp,
    expected_type: Option<TypeId>,
    lhs: &Expr,
    rhs: &Expr,
    type_ids: &HashMap<String, TypeId>,
) -> TypeId {
    match op {
        crate::common::BinOp::Lt
        | crate::common::BinOp::Lte
        | crate::common::BinOp::Gt
        | crate::common::BinOp::Gte
        | crate::common::BinOp::Eq
        | crate::common::BinOp::Neq
        | crate::common::BinOp::And
        | crate::common::BinOp::Or
        | crate::common::BinOp::Xor
            => {
            lookup_type_id(&parse::Type::Bit, type_ids)
        }
        crate::common::BinOp::Add | crate::common::BinOp::Sub => {
            expected_type
            .unwrap_or_else(|| expr_type_id(lhs).unwrap_or_else(|| expr_type_id(rhs).unwrap_or(TypeId::new(0))))
        }
    }
}

fn infer_if_type(expected_type: Option<TypeId>, then_expr: &Expr, else_expr: &Expr) -> TypeId {
    expected_type.unwrap_or_else(|| {
        expr_type_id(then_expr)
            .unwrap_or_else(|| expr_type_id(else_expr).unwrap_or(TypeId::new(0)))
    })
}

fn expr_type_id(expr: &Expr) -> Option<TypeId> {
    match expr {
        Expr::Reference(reference) => Some(reference.typ),
        Expr::WordLit(word_lit) => Some(word_lit.typ),
        Expr::BitLit(bit_lit) => Some(bit_lit.typ),
        Expr::BinOp(binop) => Some(binop.typ),
        Expr::If(expr_if) => Some(expr_if.typ),
    }
}

fn lookup_type_id(typ: &parse::Type, type_ids: &HashMap<String, TypeId>) -> TypeId {
    *type_ids
        .get(&typ.key())
        .unwrap_or_else(|| panic!("type not declared at bottom of file: {}", typ.key()))
}

fn resolve_path_type(
    path: &str,
    local_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Option<TypeId> {
    if let Some(typ) = local_types.get(path) {
        return Some(*typ);
    }

    let mut parts = path.split('.');
    let instance_name = parts.next()?;
    let port_name = parts.next()?;

    if parts.next().is_some() {
        return None;
    }

    let module_path = instance_types.get(instance_name)?;
    let signature = module_signatures.get(module_path)?;
    signature.get(port_name).copied()
}

#[cfg(test)]
mod roundtrip_tests {
    use std::fs;
    use std::path::PathBuf;

    use super::parse;

    #[test]
    fn test_roundtrip_all_virir_files() {
        let virir_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../virir");

        let mut paths: Vec<_> = fs::read_dir(&virir_dir)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "virir"))
            .collect();
        paths.sort();

        for path in paths {
            let text = fs::read_to_string(&path).unwrap();
            let virir = parse(&text).unwrap();
            let roundtrip_text = virir.to_text();
            parse(&roundtrip_text).unwrap();
        }
    }
}
