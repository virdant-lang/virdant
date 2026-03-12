pub mod expr;
pub mod typ;

#[cfg(test)]
pub mod tests;

lalrpop_util::lalrpop_mod!(grammar, "/virir/grammar.rs");

use std::collections::HashMap;
use std::sync::Arc;

use crate::common::PortDir;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Span};
use crate::source::Region;

use crate::virir::expr::{BinOp as VirIrBinOp, Expr, If as VirIrIf, Reference};
use crate::virir::typ::Type;

pub type Width = u16;
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
    pub instances: Vec<Instance>,
    pub drivers: Vec<Driver>,
}

#[derive(Debug)]
pub struct Port {
    pub region: Region,
    pub name: String,
    pub dir: PortDir,
    pub width: Width,
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

#[derive(Clone)]
enum ParsedType {
    Bit,
    Word(u16),
    Clock,
}

impl ParsedType {
    fn key(&self) -> String {
        match self {
            ParsedType::Bit => "builtin::Bit".to_string(),
            ParsedType::Word(width) => format!("builtin::Word[{width}]"),
            ParsedType::Clock => "builtin::Clock".to_string(),
        }
    }

    fn width(&self) -> u16 {
        match self {
            ParsedType::Bit | ParsedType::Clock => 1,
            ParsedType::Word(width) => *width,
        }
    }

    fn to_type(&self) -> Type {
        match self {
            ParsedType::Bit => Type::Bit,
            ParsedType::Word(width) => Type::Word(*width),
            ParsedType::Clock => Type::Clock,
        }
    }
}

struct ParsedPackage {
    name: String,
    modules: Vec<ParsedModDef>,
}

struct ParsedModDef {
    is_export: bool,
    name: String,
    ports: Vec<ParsedPort>,
    instances: Vec<ParsedInstance>,
    drivers: Vec<ParsedDriver>,
}

struct ParsedPort {
    name: String,
    dir: PortDir,
    typ: ParsedType,
}

struct ParsedInstance {
    name: String,
    module_path: String,
}

struct ParsedDriver {
    name: String,
    expr: ParsedExpr,
}

enum ParsedExpr {
    Reference { path: String, typ: Option<ParsedType> },
    BinOp {
        lhs: Box<ParsedExpr>,
        op: crate::common::BinOp,
        rhs: Box<ParsedExpr>,
    },
    If {
        cond: Box<ParsedExpr>,
        then_expr: Box<ParsedExpr>,
        else_expr: Box<ParsedExpr>,
        typ: Option<ParsedType>,
    },
}

enum ParsedModStmt {
    Port(ParsedPort),
    Instance(ParsedInstance),
    Driver(ParsedDriver),
}

fn dummy_region() -> Region {
    Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    )
}

fn build_mod_def(is_export: bool, name: String, stmts: Vec<ParsedModStmt>) -> ParsedModDef {
    let mut ports = vec![];
    let mut instances = vec![];
    let mut drivers = vec![];

    for stmt in stmts {
        match stmt {
            ParsedModStmt::Port(port) => ports.push(port),
            ParsedModStmt::Instance(instance) => instances.push(instance),
            ParsedModStmt::Driver(driver) => drivers.push(driver),
        }
    }

    ParsedModDef {
        is_export,
        name,
        ports,
        instances,
        drivers,
    }
}

fn join_path(head: String, tails: Vec<String>) -> String {
    let mut path = head;
    for tail in tails {
        path.push_str(&tail);
    }
    path
}

fn build_virir(packages: Vec<ParsedPackage>, parsed_types: Vec<ParsedType>) -> VirIr {
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
    packages: &[ParsedPackage],
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
    package: ParsedPackage,
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
    mod_def: ParsedModDef,
    type_ids: &HashMap<String, TypeId>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> ModDef {
    let local_port_types: HashMap<String, TypeId> = mod_def
        .ports
        .iter()
        .map(|port| (port.name.clone(), lookup_type_id(&port.typ, type_ids)))
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

    let drivers = mod_def
        .drivers
        .into_iter()
        .map(|driver| {
            let expected_type = resolve_path_type(
                &driver.name,
                &local_port_types,
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
                    &local_port_types,
                    &instance_types,
                    module_signatures,
                )),
            }
        })
        .collect();

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
                width: port.typ.width(),
            })
            .collect(),
        instances,
        drivers,
    }
}

fn build_expr(
    expr: ParsedExpr,
    expected_type: Option<TypeId>,
    type_ids: &HashMap<String, TypeId>,
    local_port_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Expr {
    match expr {
        ParsedExpr::Reference { path, typ } => {
            let typ = typ
                .map(|typ| lookup_type_id(&typ, type_ids))
                .or_else(|| resolve_path_type(&path, local_port_types, instance_types, module_signatures))
                .or(expected_type)
                .unwrap_or(TypeId::new(0));

            Expr::Reference(Reference {
                region: dummy_region(),
                typ,
                path,
            })
        }
        ParsedExpr::BinOp { lhs, op, rhs } => {
            let lhs = Arc::new(build_expr(
                *lhs,
                None,
                type_ids,
                local_port_types,
                instance_types,
                module_signatures,
            ));
            let rhs = Arc::new(build_expr(
                *rhs,
                None,
                type_ids,
                local_port_types,
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
        ParsedExpr::If {
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
                Some(lookup_type_id(&ParsedType::Bit, type_ids)),
                type_ids,
                local_port_types,
                instance_types,
                module_signatures,
            ));
            let then_expr = Arc::new(build_expr(
                *then_expr,
                branch_expected_type,
                type_ids,
                local_port_types,
                instance_types,
                module_signatures,
            ));
            let else_expr = Arc::new(build_expr(
                *else_expr,
                branch_expected_type,
                type_ids,
                local_port_types,
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
        | crate::common::BinOp::Or => lookup_type_id(&ParsedType::Bit, type_ids),
        crate::common::BinOp::Add | crate::common::BinOp::Sub => expected_type
            .unwrap_or_else(|| expr_type_id(lhs).unwrap_or_else(|| expr_type_id(rhs).unwrap_or(TypeId::new(0)))),
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
        Expr::Literal(bit_lit) => Some(bit_lit.typ),
        Expr::BinOp(binop) => Some(binop.typ),
        Expr::If(expr_if) => Some(expr_if.typ),
    }
}

fn lookup_type_id(typ: &ParsedType, type_ids: &HashMap<String, TypeId>) -> TypeId {
    *type_ids
        .get(&typ.key())
        .unwrap_or_else(|| panic!("type not declared at bottom of file: {}", typ.key()))
}

fn resolve_path_type(
    path: &str,
    local_port_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
) -> Option<TypeId> {
    if let Some(typ) = local_port_types.get(path) {
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
