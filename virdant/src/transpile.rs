use std::collections::HashMap;
use std::sync::Arc;

use bstr::ByteSlice;

use crate::db::Db;
use crate::analysis::typecheck::Type;
use crate::analysis::Location;
use crate::common::{BinOp as CommonBinOp, ComponentKind, PortDir};
use crate::fqn::PackageFqn;
use crate::source::Region;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::virir::expr::{BinOp, Expr, If, Reference, WordLit};
use crate::virir::typ::Type as VirType;
use crate::virir::{Driver, Instance, Item, ModDef, Package, Port, Reg, TypeId, VirIr, Wire};

/// Lowers a checked analysis database into a `VirIr` module graph.
pub fn transpile(db: &Db) -> VirIr {
    db.check().unwrap();

    let mut type_ids = HashMap::new();
    let mut all_types = vec![];

    for typ in db.get_type_monomorphizations() {
        intern_type(&mut type_ids, &mut all_types, &typ);
    }

    let module_signatures = build_module_signatures(db, &mut type_ids, &mut all_types);

    let mut packages = vec![];
    for package in db.get_packages() {
        let package = build_package(
            db,
            package,
            &module_signatures,
            &mut type_ids,
            &mut all_types,
        );

        if !package.items.is_empty() {
            packages.push(package);
        }
    }

    VirIr {
        packages,
        types: all_types.into_iter().map(Arc::new).collect(),
    }
}

/// Builds a lookup of `package::module` port names to their lowered VirIr types.
fn build_module_signatures(
    db: &Db,
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
) -> HashMap<String, HashMap<String, TypeId>> {
    let symboltable = db.get_symboltable();
    let mut module_signatures = HashMap::new();

    for package in db.get_packages() {
        let parsing = db.get_parsing(package.clone());

        for item_ast in parsing.root().children() {
            let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                continue;
            };

            let moddef_name = parsing.string(moddef.name);
            let moddef_symbol = symboltable
                .resolve_item_in_package(moddef_name, package.clone())
                .unwrap();
            let component_analysis = db.get_component_analysis(moddef_symbol.id());

            let mut signature = HashMap::new();
            for stmt in item_ast.children() {
                let AstNodePayload::Component(component) = stmt.payload() else {
                    continue;
                };

                if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing) {
                    continue;
                }

                let name = parsing.string(component.name);
                let typ = component_analysis.type_of(name).unwrap();
                let type_id = intern_type(type_ids, all_types, &typ);
                signature.insert(name.to_str_lossy().into_owned(), type_id);
            }

            module_signatures.insert(format!("{package}::{}", moddef_name.to_str_lossy()), signature);
        }
    }

    module_signatures
}

/// Lowers all modules in a package into VirIr items and declarations.
fn build_package(
    db: &Db,
    package: PackageFqn,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
) -> Package {
    let parsing = db.get_parsing(package.clone());
    let symboltable = db.get_symboltable();
    let mut items = vec![];

    for item_ast in parsing.root().children() {
        let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
            continue;
        };

        let moddef_name = parsing.string(moddef.name);
        let moddef_symbol = symboltable
            .resolve_item_in_package(moddef_name, package.clone())
            .unwrap();
        let component_analysis = db.get_component_analysis(moddef_symbol.id());

        let mut ports = vec![];
        let mut wires = vec![];
        let mut regs = vec![];
        let mut instances = vec![];
        let mut drivers = vec![];
        let mut local_types = HashMap::new();
        let mut instance_types = HashMap::new();

        for stmt in item_ast.children() {
            match stmt.payload() {
                AstNodePayload::Component(component) => {
                    let name = parsing.string(component.name);
                    let typ = component_analysis.type_of(name).unwrap();
                    let type_id = intern_type(type_ids, all_types, &typ);
                    let rendered_name = name.to_str_lossy().into_owned();
                    local_types.insert(rendered_name.clone(), type_id);

                    match component.kind {
                        ComponentKind::Incoming | ComponentKind::Outgoing => ports.push(Port {
                            region: stmt.region(),
                            name: rendered_name,
                            dir: if component.kind == ComponentKind::Incoming {
                                PortDir::Input
                            } else {
                                PortDir::Output
                            },
                            width: analysis_type_width(&typ),
                        }),
                        ComponentKind::Wire => wires.push(Wire {
                            region: stmt.region(),
                            typ: type_id,
                            name: rendered_name,
                        }),
                        ComponentKind::Reg => regs.push(Reg {
                            region: stmt.region(),
                            typ: type_id,
                            name: rendered_name,
                        }),
                    }
                }
                AstNodePayload::Module(module) => {
                    let name = parsing.string(module.name).to_str_lossy().into_owned();
                    let module_path = render_ofness_path(stmt.child(0), &package);
                    instance_types.insert(name.clone(), module_path.clone());
                    instances.push(Instance {
                        region: stmt.region(),
                        name,
                        module_path,
                    });
                }
                _ => (),
            }
        }

        for stmt in item_ast.children() {
            let AstNodePayload::Driver(_) = stmt.payload() else {
                continue;
            };

            let path = parsing.string(stmt.child(0).path().unwrap()).to_str_lossy().into_owned();
            let expr_node = stmt.driver().unwrap();
            let expected_type = db
                .get_typeof(Location::new(package.clone(), expr_node.id()))
                .map(|typ| intern_type(type_ids, all_types, &typ))
                .or_else(|| resolve_path_type(&path, &local_types, &instance_types, module_signatures));

            drivers.push(Driver {
                region: stmt.region(),
                path,
                expr: Arc::new(build_expr(
                    &package,
                    expr_node,
                    expected_type,
                    &local_types,
                    &instance_types,
                    module_signatures,
                    type_ids,
                    all_types,
                )),
            });
        }

        items.push(Item::ModDef(ModDef {
            region: item_ast.region(),
            is_export: moddef.is_export,
            name: moddef_name.to_str_lossy().into_owned(),
            ports,
            wires,
            regs,
            instances,
            drivers,
        }));
    }

    Package {
        name: package.to_string(),
        items,
    }
}

/// Recursively lowers an expression AST node into a typed VirIr expression.
fn build_expr(
    package: &PackageFqn,
    node: AstNode<'_>,
    expected_type: Option<TypeId>,
    local_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
) -> Expr {
    match node.payload() {
        AstNodePayload::ExprReference => {
            let path = node.parsing.string(node.path().unwrap()).to_str_lossy().into_owned();
            let typ = resolve_path_type(&path, local_types, instance_types, module_signatures)
                .or(expected_type)
                .unwrap_or_else(|| intern_type(type_ids, all_types, &Type::Bit));

            Expr::Reference(Reference {
                region: node.region(),
                typ,
                path,
            })
        }
        AstNodePayload::ExprBitLit(expr_bit_lit) => Expr::WordLit(WordLit {
            region: node.region(),
            typ: expected_type
                .unwrap_or_else(|| intern_type(type_ids, all_types, &Type::Bit)),
            value: if expr_bit_lit.literal { 1 } else { 0 },
        }),
        AstNodePayload::ExprWordLit(expr_word_lit) => {
            let literal = node.parsing.string(expr_word_lit.literal).to_str_lossy().into_owned();
            let (value, width) = parse_word_literal(&literal);
            let typ = if let Some(width) = width {
                intern_type(type_ids, all_types, &Type::Word(width.into()))
            } else {
                expected_type.unwrap_or_else(|| panic!("word literal {literal} needs a type"))
            };

            Expr::WordLit(WordLit {
                region: node.region(),
                typ,
                value,
            })
        }
        AstNodePayload::ExprBinOp(expr_bin_op) => {
            let lhs = Arc::new(build_expr(
                package,
                node.child(0),
                None,
                local_types,
                instance_types,
                module_signatures,
                type_ids,
                all_types,
            ));
            let rhs_expected_type = Some(expr_type_id(lhs.as_ref()));
            let rhs = Arc::new(build_expr(
                package,
                node.child(1),
                rhs_expected_type,
                local_types,
                instance_types,
                module_signatures,
                type_ids,
                all_types,
            ));
            let typ = infer_binop_type(
                expr_bin_op.op,
                expected_type,
                lhs.as_ref(),
                rhs.as_ref(),
                type_ids,
                all_types,
            );

            Expr::BinOp(BinOp {
                region: node.region(),
                typ,
                op: expr_bin_op.op,
                lhs,
                rhs,
            })
        }
        AstNodePayload::ExprIf => build_if_expr(
            package,
            node.region(),
            &node.children(),
            expected_type,
            local_types,
            instance_types,
            module_signatures,
            type_ids,
            all_types,
        ),
        AstNodePayload::ExprParen => build_expr(
            package,
            node.child(0),
            expected_type,
            local_types,
            instance_types,
            module_signatures,
            type_ids,
            all_types,
        ),
        _ => todo!("unsupported expr in transpile: {}", node.summary()),
    }
}

/// Lowers an `if` expression, including chained `else if` branches.
fn build_if_expr(
    package: &PackageFqn,
    region: Region,
    children: &[AstNode<'_>],
    expected_type: Option<TypeId>,
    local_types: &HashMap<String, TypeId>,
    instance_types: &HashMap<String, String>,
    module_signatures: &HashMap<String, HashMap<String, TypeId>>,
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
) -> Expr {
    let cond = Arc::new(build_expr(
        package,
        children[0].clone(),
        Some(intern_type(type_ids, all_types, &Type::Bit)),
        local_types,
        instance_types,
        module_signatures,
        type_ids,
        all_types,
    ));
    let then_expr = Arc::new(build_expr(
        package,
        children[1].clone(),
        expected_type,
        local_types,
        instance_types,
        module_signatures,
        type_ids,
        all_types,
    ));
    let else_expr = if children.len() == 3 {
        Arc::new(build_expr(
            package,
            children[2].clone(),
            expected_type,
            local_types,
            instance_types,
            module_signatures,
            type_ids,
            all_types,
        ))
    } else {
        Arc::new(build_if_expr(
            package,
            children[2].region(),
            &children[2..],
            expected_type,
            local_types,
            instance_types,
            module_signatures,
            type_ids,
            all_types,
        ))
    };
    let typ = expected_type.unwrap_or_else(|| expr_type_id(then_expr.as_ref()));

    Expr::If(If {
        region,
        typ,
        cond,
        then_expr,
        else_expr,
    })
}

/// Interns a type into the shared VirIr type table and returns its `TypeId`.
fn intern_type(
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
    typ: &Type,
) -> TypeId {
    let vir_type = match typ {
        Type::Bit => VirType::Bit,
        Type::Clock => VirType::Clock,
        Type::Word(width) => VirType::Word((*width).try_into().unwrap()),
        Type::Usual(symbol_id) => panic!("VirIr cannot represent non-builtin type {symbol_id:?}"),
    };
    let key = match vir_type {
        VirType::Bit => "builtin::Bit".to_string(),
        VirType::Clock => "builtin::Clock".to_string(),
        VirType::Word(width) => format!("builtin::Word[{width}]"),
    };

    if let Some(type_id) = type_ids.get(&key) {
        *type_id
    } else {
        let type_id = TypeId::new(all_types.len().try_into().unwrap());
        type_ids.insert(key, type_id);
        all_types.push(vir_type);
        type_id
    }
}

/// Converts an analysis type into the width expected by VirIr ports.
fn analysis_type_width(typ: &Type) -> u16 {
    match typ {
        Type::Bit | Type::Clock => 1,
        Type::Word(width) => (*width).try_into().unwrap(),
        Type::Usual(symbol_id) => panic!("VirIr cannot represent non-builtin type {symbol_id:?}"),
    }
}

/// Renders an instance `of` clause as a fully qualified VirIr module path.
fn render_ofness_path(ofness_node: AstNode<'_>, package: &PackageFqn) -> String {
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        panic!("expected Ofness node")
    };

    let module_package = ofness
        .package
        .map(|package_name| ofness_node.parsing.string(package_name).to_str_lossy().into_owned())
        .unwrap_or_else(|| package.to_string());
    let module_name = ofness_node.parsing.string(ofness.name).to_str_lossy();
    format!("{module_package}::{module_name}")
}

/// Parses a source word literal into its numeric value and optional explicit width.
fn parse_word_literal(literal: &str) -> (u64, Option<u16>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

/// Parses decimal, binary, or hexadecimal naturals after removing separators.
fn parse_nat_literal(literal: &str) -> u64 {
    let literal = literal.replace('_', "");
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

/// Infers the VirIr result type for a lowered binary operation.
fn infer_binop_type(
    op: CommonBinOp,
    expected_type: Option<TypeId>,
    lhs: &Expr,
    rhs: &Expr,
    type_ids: &mut HashMap<String, TypeId>,
    all_types: &mut Vec<VirType>,
) -> TypeId {
    match op {
        CommonBinOp::Lt
        | CommonBinOp::Lte
        | CommonBinOp::Gt
        | CommonBinOp::Gte
        | CommonBinOp::Eq
        | CommonBinOp::Neq
        | CommonBinOp::And
        | CommonBinOp::Or
        | CommonBinOp::Xor => intern_type(type_ids, all_types, &Type::Bit),
        CommonBinOp::Add | CommonBinOp::Sub => expected_type.unwrap_or_else(|| expr_type_id(lhs)),
    }
}

/// Returns the already-lowered type id stored on a VirIr expression node.
fn expr_type_id(expr: &Expr) -> TypeId {
    match expr {
        Expr::Reference(reference) => reference.typ,
        Expr::BitLit(bit_lit) => bit_lit.typ,
        Expr::WordLit(word_lit) => word_lit.typ,
        Expr::BinOp(binop) => binop.typ,
        Expr::If(expr_if) => expr_if.typ,
    }
}

/// Resolves the type of a local name or instance-port path within a module body.
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
