use std::collections::HashMap;
use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::typecheck::Type;
use crate::analysis::Location;
use crate::common::{BinOp as CommonBinOp, ComponentKind, PortDir, Width};
use crate::db::Db;
use crate::fqn::PackageFqn;
use crate::source::Region;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::virir::expr::{BinOp, Expr, If, Reference, WordLit};
use crate::virir::typ::Type as VirType;
use crate::virir::{Command, Driver, Instance, Item, ModDef, On, Package, Port, Reg, TypeId, VirIr, Wire};

/// Lowers a checked analysis database into a `VirIr` module graph.
pub fn transpile(db: &Db) -> VirIr {
    db.check().unwrap();

    let mut transpiler = Transpiler::new(db);
    transpiler.run()
}

struct Transpiler<'d> {
    db: &'d Db,
    module_signatures: HashMap<String, HashMap<String, TypeId>>,
    type_ids: HashMap<String, TypeId>,
    all_types: Vec<VirType>,
}

impl<'d> Transpiler<'d> {
    fn new(db: &Db) -> Transpiler<'_> {
        Transpiler {
            db,
            module_signatures: HashMap::new(),
            type_ids: HashMap::new(),
            all_types: vec![],
        }
    }

    fn run(&mut self) -> VirIr {
        for typ in self.db.get_type_monomorphizations() {
            self.intern_type(&typ);
        }

        self.module_signatures = self.build_module_signatures();

        let mut packages = vec![];
        for package in self.db.get_packages() {
            let package = self.build_package(package);

            if !package.items.is_empty() {
                packages.push(package);
            }
        }

        let types = std::mem::take(&mut self.all_types)
            .into_iter()
            .map(Arc::new)
            .collect();

        VirIr { packages, types }
    }
    /// Builds a lookup of `package::module` port names to their lowered VirIr types.
    fn build_module_signatures(&mut self) -> HashMap<String, HashMap<String, TypeId>> {
        let symboltable = self.db.get_symboltable();
        let mut module_signatures = HashMap::new();

        for package in self.db.get_packages() {
            let parsing = self.db.get_parsing(package.clone());

            for item_ast in parsing.root().children() {
                let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                    continue;
                };

                let moddef_name = parsing.string(moddef.name);
                let moddef_symbol = symboltable
                    .resolve_item_in_package(moddef_name, package.clone())
                    .unwrap();
                let component_analysis = self.db.get_component_analysis(moddef_symbol.id());

                let mut signature = HashMap::new();
                for stmt in item_ast.children() {
                    let AstNodePayload::Component(component) = stmt.payload() else {
                        continue;
                    };

                    if !matches!(
                        component.kind,
                        ComponentKind::Incoming | ComponentKind::Outgoing
                    ) {
                        continue;
                    }

                    let name = parsing.string(component.name);
                    let typ = component_analysis.type_of(name).unwrap();
                    let type_id = self.intern_type(&typ);
                    signature.insert(name.to_str_lossy().into_owned(), type_id);
                }

                module_signatures.insert(
                    format!("{package}::{}", moddef_name.to_str_lossy()),
                    signature,
                );
            }
        }

        module_signatures
    }

    /// Lowers all modules in a package into VirIr items and declarations.
    fn build_package(&mut self, package: PackageFqn) -> Package {
        let parsing = self.db.get_parsing(package.clone());
        let symboltable = self.db.get_symboltable();
        let mut items = vec![];

        for item_ast in parsing.root().children() {
            let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                continue;
            };

            let moddef_name = parsing.string(moddef.name);
            let moddef_symbol = symboltable
                .resolve_item_in_package(moddef_name, package.clone())
                .unwrap();
            let component_analysis = self.db.get_component_analysis(moddef_symbol.id());

            let mut ports = vec![];
            let mut wires = vec![];
            let mut regs = vec![];
            let mut instances = vec![];
            let mut drivers = vec![];
            let mut on = None;
            let mut local_types = HashMap::new();
            let mut instance_types = HashMap::new();

            for stmt in item_ast.children() {
                match stmt.payload() {
                    AstNodePayload::Component(component) => {
                        let name = parsing.string(component.name);
                        let typ = component_analysis.type_of(name).unwrap();
                        let type_id = self.intern_type(&typ);
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
                                typ: type_id,
                            }),
                            ComponentKind::Wire => wires.push(Wire {
                                region: stmt.region(),
                                typ: type_id,
                                name: rendered_name,
                            }),
                            ComponentKind::Reg => {
                                let clock_type = self.intern_type(&Type::Clock);
                                let clock = stmt.clock().map(|clock| {
                                    Arc::new(self.build_expr(
                                        &package,
                                        clock,
                                        Some(clock_type),
                                        &local_types,
                                        &instance_types,
                                    ))
                                });
                                regs.push(Reg {
                                    region: stmt.region(),
                                    typ: type_id,
                                    name: rendered_name,
                                    clock,
                                })
                            }
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
                match stmt.payload() {
                    AstNodePayload::Driver(_) => {
                        let path = parsing
                            .string(stmt.child(0).path().unwrap())
                            .to_str_lossy()
                            .into_owned();
                        let expr_node = stmt.driver().unwrap();
                        let expected_type = self
                            .db
                            .get_typeof(Location::new(package.clone(), expr_node.id()))
                            .map(|typ| self.intern_type(&typ))
                            .or_else(|| self.resolve_path_type(&path, &local_types, &instance_types));

                        drivers.push(Driver {
                            region: stmt.region(),
                            path,
                            expr: Arc::new(self.build_expr(
                                &package,
                                expr_node,
                                expected_type,
                                &local_types,
                                &instance_types,
                            )),
                        });
                    }
                    AstNodePayload::ModDefStmtOn => {
                        assert!(on.is_none(), "Virdant transpilation supports at most one on block per module");
                        on = Some(self.build_on_stmt(
                            &package,
                            stmt,
                            &local_types,
                            &instance_types,
                        ));
                    }
                    _ => (),
                }
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
                on,
            }));
        }

        Package {
            name: package.to_string(),
            items,
        }
    }

    fn build_on_stmt(
        &mut self,
        package: &PackageFqn,
        node: AstNode<'_>,
        local_types: &HashMap<String, TypeId>,
        instance_types: &HashMap<String, String>,
    ) -> On {
        let clock_type = self.intern_type(&Type::Clock);
        let clock_node = node.clock().unwrap();
        let commands = node
            .children()
            .into_iter()
            .skip(1)
            .map(|command| self.build_command(package, command, local_types, instance_types))
            .collect();

        On {
            region: node.region(),
            clock: Arc::new(self.build_expr(
                package,
                clock_node,
                Some(clock_type),
                local_types,
                instance_types,
            )),
            commands,
        }
    }

    fn build_command(
        &mut self,
        package: &PackageFqn,
        node: AstNode<'_>,
        local_types: &HashMap<String, TypeId>,
        instance_types: &HashMap<String, String>,
    ) -> Command {
        let bit_type = self.intern_type(&Type::Bit);
        match node.payload() {
            AstNodePayload::CommandAssert => Command::Assert(Arc::new(self.build_expr(
                package,
                node.child(0),
                Some(bit_type),
                local_types,
                instance_types,
            ))),
            AstNodePayload::CommandDisplay => Command::Display(),
            AstNodePayload::CommandFinish => Command::Finish,
            AstNodePayload::CommandFatal => Command::Fatal,
            _ => panic!("expected command node, found {}", node.summary()),
        }
    }

    /// Recursively lowers an expression AST node into a typed VirIr expression.
    fn build_expr(
        &mut self,
        package: &PackageFqn,
        node: AstNode<'_>,
        expected_type: Option<TypeId>,
        local_types: &HashMap<String, TypeId>,
        instance_types: &HashMap<String, String>,
    ) -> Expr {
        match node.payload() {
            AstNodePayload::ExprReference => {
                let path = node
                    .parsing
                    .string(node.path().unwrap())
                    .to_str_lossy()
                    .into_owned();
                let typ = self
                    .resolve_path_type(&path, local_types, instance_types)
                    .or(expected_type)
                    .unwrap_or_else(|| self.intern_type(&Type::Bit));

                Expr::Reference(Reference {
                    region: node.region(),
                    typ,
                    path,
                })
            }
            AstNodePayload::ExprBitLit(expr_bit_lit) => Expr::WordLit(WordLit {
                region: node.region(),
                typ: expected_type.unwrap_or_else(|| self.intern_type(&Type::Bit)),
                value: if expr_bit_lit.literal { 1 } else { 0 },
            }),
            AstNodePayload::ExprWordLit(expr_word_lit) => {
                let literal = node
                    .parsing
                    .string(expr_word_lit.literal)
                    .to_str_lossy()
                    .into_owned();
                let (value, width) = parse_word_literal(&literal);
                let typ = if let Some(width) = width {
                    self.intern_type(&Type::Word(width))
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
                let lhs = Arc::new(self.build_expr(
                    package,
                    node.child(0),
                    None,
                    local_types,
                    instance_types,
                ));
                let rhs_expected_type = Some(expr_type_id(lhs.as_ref()));
                let rhs = Arc::new(self.build_expr(
                    package,
                    node.child(1),
                    rhs_expected_type,
                    local_types,
                    instance_types,
                ));
                let typ = self.infer_binop_type(
                    expr_bin_op.op,
                    expected_type,
                    lhs.as_ref(),
                    rhs.as_ref(),
                );

                Expr::BinOp(BinOp {
                    region: node.region(),
                    typ,
                    op: expr_bin_op.op,
                    lhs,
                    rhs,
                })
            }
            AstNodePayload::ExprIf => self.build_if_expr(
                package,
                node.region(),
                &node.children(),
                expected_type,
                local_types,
                instance_types,
            ),
            AstNodePayload::ExprParen => self.build_expr(
                package,
                node.child(0),
                expected_type,
                local_types,
                instance_types,
            ),
            _ => todo!("unsupported expr in transpile: {}", node.summary()),
        }
    }

    /// Lowers an `if` expression, including chained `else if` branches.
    fn build_if_expr(
        &mut self,
        package: &PackageFqn,
        region: Region,
        children: &[AstNode<'_>],
        expected_type: Option<TypeId>,
        local_types: &HashMap<String, TypeId>,
        instance_types: &HashMap<String, String>,
    ) -> Expr {
        let bit_type = self.intern_type(&Type::Bit);
        let cond = Arc::new(self.build_expr(
            package,
            children[0].clone(),
            Some(bit_type),
            local_types,
            instance_types,
        ));
        let then_expr = Arc::new(self.build_expr(
            package,
            children[1].clone(),
            expected_type,
            local_types,
            instance_types,
        ));
        let else_expr = if children.len() == 3 {
            Arc::new(self.build_expr(
                package,
                children[2].clone(),
                expected_type,
                local_types,
                instance_types,
            ))
        } else {
            Arc::new(self.build_if_expr(
                package,
                children[2].region(),
                &children[2..],
                expected_type,
                local_types,
                instance_types,
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
    fn intern_type(&mut self, typ: &Type) -> TypeId {
        let vir_type = match typ {
            Type::Bit => VirType::Bit,
            Type::Clock => VirType::Clock,
            Type::Word(width) => VirType::Word((*width).try_into().unwrap()),
            Type::Usual(symbol_id) => {
                panic!("VirIr cannot represent non-builtin type {symbol_id:?}")
            }
        };
        let key = match vir_type {
            VirType::Bit => "builtin::Bit".to_string(),
            VirType::Clock => "builtin::Clock".to_string(),
            VirType::Word(width) => format!("builtin::Word[{width}]"),
        };

        if let Some(type_id) = self.type_ids.get(&key) {
            *type_id
        } else {
            let type_id = TypeId::new(self.all_types.len().try_into().unwrap());
            self.type_ids.insert(key, type_id);
            self.all_types.push(vir_type);
            type_id
        }
    }

    /// Infers the VirIr result type for a lowered binary operation.
    fn infer_binop_type(
        &mut self,
        op: CommonBinOp,
        expected_type: Option<TypeId>,
        lhs: &Expr,
        _rhs: &Expr,
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
            | CommonBinOp::Xor => self.intern_type(&Type::Bit),
            CommonBinOp::Add | CommonBinOp::Sub => {
                expected_type.unwrap_or_else(|| expr_type_id(lhs))
            }
        }
    }

    /// Resolves the type of a local name or instance-port path within a module body.
    fn resolve_path_type(
        &self,
        path: &str,
        local_types: &HashMap<String, TypeId>,
        instance_types: &HashMap<String, String>,
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
        let signature = self.module_signatures.get(module_path)?;
        signature.get(port_name).copied()
    }
}

/// Renders an instance `of` clause as a fully qualified VirIr module path.
fn render_ofness_path(ofness_node: AstNode<'_>, package: &PackageFqn) -> String {
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        panic!("expected Ofness node")
    };

    let module_package = ofness
        .package
        .map(|package_name| {
            ofness_node
                .parsing
                .string(package_name)
                .to_str_lossy()
                .into_owned()
        })
        .unwrap_or_else(|| package.to_string());
    let module_name = ofness_node.parsing.string(ofness.name).to_str_lossy();
    format!("{module_package}::{module_name}")
}

/// Parses a source word literal into its numeric value and optional explicit width.
fn parse_word_literal(literal: &str) -> (u64, Option<Width>) {
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
