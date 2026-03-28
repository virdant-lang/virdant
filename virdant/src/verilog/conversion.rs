use std::collections::HashMap;
use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::location::Location;
use crate::common::{self, ComponentKind, Radix, Width};
use crate::db::Db;
use crate::diagnostics::DiagnosticLevel;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::types::{Type, Typing};
use crate::verilog::{self, exact_verilog_name, valid_verilog_name};

/// Converts the Db (checked analysis database) directly into its Verilog representation.
pub fn convert_db_to_verilog(db: &Db) -> verilog::Verilog {
    let diagnostics = db.check();
    if diagnostics.iter().any(|d| d.level() == DiagnosticLevel::Error) {
        eprintln!("Compilation failed due to errors");
        std::process::exit(1);
    }

    let mut converter = Converter::new(db);
    let mut verilog = converter.run();
    verilog.normalize();
    verilog
}

struct Converter<'d> {
    db: &'d Db,
    /// "package::Module" -> list of (port_name, width)
    module_ports: HashMap<String, Vec<(String, Width)>>,
    /// "package::Module" -> emitted Verilog name
    emitted_module_names: HashMap<String, String>,
}

impl<'d> Converter<'d> {
    fn new(db: &'d Db) -> Self {
        Converter {
            db,
            module_ports: HashMap::new(),
            emitted_module_names: HashMap::new(),
        }
    }

    fn run(&mut self) -> verilog::Verilog {
        self.build_module_info();
        let mut files = vec![];
        for package in self.db.get_packages() {
            if let Some(file) = self.convert_package(package) {
                files.push(file);
            }
        }
        verilog::Verilog { files }
    }

    fn build_module_info(&mut self) {
        let symboltable = self.db.get_symboltable();
        for package in self.db.get_packages() {
            let parsing = self.db.get_parsing(package.clone());
            for item_ast in parsing.root().children() {
                let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                    continue;
                };
                let moddef_name = parsing.string(moddef.name).to_str_lossy().into_owned();
                let module_path = qualified_module_name(&package.to_string(), &moddef_name);
                let emitted_name = if moddef.is_export {
                    exact_verilog_name(&moddef_name)
                } else {
                    valid_verilog_name(&module_path)
                };
                self.emitted_module_names.insert(module_path.clone(), emitted_name);

                let moddef_symbol = symboltable
                    .resolve_item_in_package(parsing.string(moddef.name), package.clone())
                    .unwrap();
                let component_analysis = self.db.get_component_analysis(moddef_symbol.id());
                let mut ports = vec![];
                for stmt in item_ast.children() {
                    let AstNodePayload::Component(component) = stmt.payload() else {
                        continue;
                    };
                    if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing) {
                        continue;
                    }
                    let name = parsing.string(component.name).to_str_lossy().into_owned();
                    let typ = component_analysis.type_of(parsing.string(component.name)).unwrap();
                    ports.push((name, type_width(&typ, self.db)));
                }
                self.module_ports.insert(module_path, ports);
            }
        }
    }

    fn convert_package(&self, package: PackageFqn) -> Option<verilog::VerilogFile> {
        let parsing = self.db.get_parsing(package.clone());
        let package_name = package.to_string();
        let mut modules = vec![];
        for item_ast in parsing.root().children() {
            let AstNodePayload::ModDef(_) = item_ast.payload() else {
                continue;
            };
            modules.push(self.convert_moddef(&package, item_ast));
        }
        if modules.is_empty() {
            return None;
        }
        Some(verilog::VerilogFile {
            name: format!("{package_name}.sv"),
            modules,
        })
    }

    fn convert_moddef(&self, package: &PackageFqn, item_ast: AstNode) -> verilog::Module {
        let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
            panic!("expected ModDef");
        };
        let parsing = self.db.get_parsing(package.clone());
        let moddef_name = parsing.string(moddef.name).to_str_lossy().into_owned();
        let module_path = qualified_module_name(&package.to_string(), &moddef_name);
        let module_name = self.emitted_module_names[&module_path].clone();

        let symboltable = self.db.get_symboltable();
        let moddef_symbol = symboltable
            .resolve_item_in_package(parsing.string(moddef.name), package.clone())
            .unwrap();
        let component_analysis = self.db.get_component_analysis(moddef_symbol.id());

        let mut ports: Vec<verilog::Port> = vec![];
        let mut elements: Vec<verilog::Element> = vec![];
        // reg name -> AstNodeId of its explicit `on clock` expression (if present)
        let mut sequential_regs: HashMap<String, AstNodeId> = HashMap::new();
        let mut on_block: Option<verilog::Element> = None;

        // First pass: ports, wires, regs, instances, on
        for stmt in item_ast.children() {
            match stmt.payload() {
                AstNodePayload::Component(component) => {
                    let name = parsing.string(component.name).to_str_lossy().into_owned();
                    let typ = component_analysis.type_of(parsing.string(component.name)).unwrap();
                    let width = type_width(&typ, self.db);
                    match component.kind {
                        ComponentKind::Incoming => ports.push(verilog::Port {
                            name: exact_verilog_name(&name),
                            kind: verilog::PortKind::Wire,
                            dir: common::PortDir::Input,
                            width,
                        }),
                        ComponentKind::Outgoing => ports.push(verilog::Port {
                            name: exact_verilog_name(&name),
                            kind: verilog::PortKind::Wire,
                            dir: common::PortDir::Output,
                            width,
                        }),
                        ComponentKind::Wire => elements.push(verilog::Element::Wire(verilog::Wire {
                            name: exact_verilog_name(&name),
                            width,
                            expr: None,
                        })),
                        ComponentKind::Reg => {
                            // Regs with `on clock` have 2 children: type + clock expr
                            let children = stmt.children();
                            if children.len() >= 2 {
                                sequential_regs.insert(name.clone(), children[1].id());
                            }
                            elements.push(verilog::Element::Reg(verilog::Reg {
                                name: exact_verilog_name(&name),
                                width,
                                expr: None,
                            }));
                        }
                    }
                }
                AstNodePayload::Module(module) => {
                    let raw_name = parsing.string(module.name).to_str_lossy().into_owned();
                    let mod_path = render_ofness_path(stmt.child(0), package);
                    let ports_info = self.module_ports.get(&mod_path).cloned().unwrap_or_default();
                    let sub_name = self.emitted_module_names.get(&mod_path)
                        .cloned()
                        .unwrap_or_else(|| valid_verilog_name(&mod_path));
                    for (port_name, width) in &ports_info {
                        elements.push(verilog::Element::Wire(verilog::Wire {
                            name: valid_verilog_name(&format!("{raw_name}.{port_name}")),
                            width: *width,
                            expr: None,
                        }));
                    }
                    elements.push(verilog::Element::Submodule(verilog::Submodule {
                        name: valid_verilog_name(&raw_name),
                        submodule_name: sub_name,
                        ports: ports_info.into_iter().map(|(n, _)| n).collect(),
                    }));
                }
                AstNodePayload::ModDefStmtOn => {
                    let clock_expr = self.convert_expr(package, stmt.child(0), None, self.db);
                    let stmts = stmt.children().into_iter().skip(1)
                        .map(|cmd| self.convert_command(package, cmd))
                        .collect();
                    on_block = Some(verilog::Element::Always(verilog::Always {
                        clock: Some(clock_expr),
                        stmts,
                    }));
                }
                _ => {}
            }
        }

        // Second pass: drivers (and hole displays)
        let mut hole_displays: Vec<verilog::Element> = vec![];
        let mut combinational: Vec<verilog::Element> = vec![];
        let mut sequential: Vec<verilog::Element> = vec![];

        for stmt in item_ast.children() {
            let AstNodePayload::Driver(_) = stmt.payload() else { continue; };
            let path = parsing.string(stmt.target().unwrap()).to_str_lossy().into_owned();
            let expr_node = stmt.driver().unwrap();

            // Collect holes from this driver expression
            let mut hole_regions: Vec<String> = vec![];
            collect_ast_holes(expr_node.clone(), &mut hole_regions);
            for region in hole_regions {
                let message = format!("\"HOLE: ? at {region}\"");
                hole_displays.push(verilog::Element::Initial(verilog::Initial {
                    stmts: vec![verilog::Stmt::Display(verilog::Display {
                        message: message.into(),
                        exprs: vec![],
                    })],
                }));
            }

            let expected_type = component_analysis.type_of(path.as_bytes().as_bstr());
            let expr = self.convert_expr(package, expr_node, expected_type.as_ref(), self.db);

            if let Some(&clock_id) = sequential_regs.get(&path) {
                let clock_node = parsing.ast_node(clock_id);
                let clock_expr = self.convert_expr(package, clock_node, None, self.db);
                sequential.push(verilog::Element::Always(verilog::Always {
                    clock: Some(clock_expr),
                    stmts: vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
                        name: valid_verilog_name(&path),
                        expr,
                    })],
                }));
            } else {
                combinational.push(verilog::Element::Assign(verilog::Assign {
                    name: valid_verilog_name(&path),
                    expr,
                }));
            }
        }

        elements.extend(combinational);
        elements.extend(sequential);
        if let Some(on) = on_block {
            elements.push(on);
        }
        elements.extend(hole_displays);

        verilog::Module { name: module_name, is_ext: moddef.is_ext, ports, elements }
    }

    fn convert_expr(
        &self,
        package: &PackageFqn,
        node: AstNode,
        expected_type: Option<&Type>,
        db: &Db,
    ) -> verilog::Expr {
        let typing = self.typing_for(&node);
        match node.payload() {
            AstNodePayload::ExprReference => {
                let path = node.parsing.string(node.path().unwrap()).to_str_lossy().into_owned();
                verilog::Expr::Reference(verilog::expr::Reference {
                    name: valid_verilog_name(&path),
                })
            }
            AstNodePayload::ExprBitLit(expr_bit_lit) => {
                verilog::Expr::BitLit(verilog::expr::BitLit { value: expr_bit_lit.literal })
            }
            AstNodePayload::ExprWordLit(expr_word_lit) => {
                let literal = node.parsing.string(expr_word_lit.literal).to_str_lossy().into_owned();
                let (value, lit_width) = parse_word_literal(&literal);
                let width = lit_width
                    .or_else(|| expected_type.map(|t| type_width(t, self.db)))
                    .or_else(|| self.node_type(package, &node).map(|t| type_width(&t, self.db)))
                    .unwrap_or_else(|| panic!("word literal {literal} needs a type"));
                verilog::Expr::WordLit(verilog::expr::WordLit { value: value.into(), width, radix: Radix::Dec })
            }
            AstNodePayload::ExprWord => {
                let width = self.node_or_expected_width(package, &node, expected_type);
                let exprs = node.children().into_iter()
                    .map(|child| {
                        let child_type = self.node_type(package, &child);
                        self.convert_expr(package, child, child_type.as_ref(), self.db)
                    })
                    .collect();
                verilog::Expr::Concat(verilog::expr::Concat { exprs, width })
            }
            AstNodePayload::ExprBinOp(expr_bin_op) => {
                verilog::Expr::BinOp(verilog::expr::BinOp {
                    op: convert_binop(expr_bin_op.op),
                    lhs: Box::new(self.convert_expr(package, node.child(0), None, self.db)),
                    rhs: Box::new(self.convert_expr(package, node.child(1), None, self.db)),
                })
            }
            AstNodePayload::ExprUnOp(expr_un_op) => {
                verilog::Expr::UnOp(verilog::expr::UnOp {
                    op: convert_unop(expr_un_op.op),
                    expr: Box::new(self.convert_expr(package, node.child(0), None, self.db)),
                })
            }
            AstNodePayload::ExprZext => self.convert_zext(package, node, expected_type),
            AstNodePayload::ExprSext => self.convert_sext(package, node, expected_type),
            AstNodePayload::ExprIf => {
                let children = node.children();
                self.convert_if_expr(package, &children, expected_type)
            }
            AstNodePayload::ExprIndex(expr_index) => verilog::Expr::Index(verilog::expr::Index {
                subject: Box::new(self.convert_expr(package, node.child(0), None, self.db)),
                index: Box::new(constant_index_expr(expr_index.index)),
            }),
            AstNodePayload::ExprIndexRange(expr_index_range) => {
                verilog::Expr::IndexRange(verilog::expr::IndexRange {
                    subject: Box::new(self.convert_expr(package, node.child(0), None, self.db)),
                    index_hi: Box::new(constant_index_expr(expr_index_range.index_hi - 1)),
                    index_lo: Box::new(constant_index_expr(expr_index_range.index_lo)),
                })
            }
            AstNodePayload::ExprAs | AstNodePayload::ExprParen => {
                self.convert_expr(package, node.child(0), expected_type, db)
            }
            AstNodePayload::ExprHole => {
                let width = self.node_or_expected_width(package, &node, expected_type);
                verilog::Expr::XLit(verilog::expr::XLit { width })
            }
            AstNodePayload::ExprCtor(_ctor) => {
                // TODO doesn't handle Ctor payloads
                let symboltable = db.get_symboltable();
                let Some(Type::Usual(typedef_symbol_id)) = expected_type else {
                    unreachable!()
                };
                let slots = symboltable.slots(*typedef_symbol_id);
                let ctor_symbol_id = typing.resolution(node.location()).unwrap();
                let width: u16 = TryInto::<u16>::try_into(slots.len()).unwrap();
                let slot_index = slots.iter().position(|slot_id| slot_id.id() == ctor_symbol_id).unwrap();
                let value = 1 << slot_index;
                verilog::Expr::WordLit(verilog::expr::WordLit { value: value, width, radix: Radix::Dec })
            }
            AstNodePayload::ExprEnumerant(_enumerant) => {
                let symboltable = db.get_symboltable();
                let Some(Type::Usual(typedef_symbol_id)) = expected_type else {
                    unreachable!()
                };
                let typedef = db.get_typedef(*typedef_symbol_id);
                let slots = symboltable.slots(*typedef_symbol_id);
                let enumerant_symbol_id = typing.resolution(node.location()).unwrap();
                let value = *typedef.enumerant_values.get(&enumerant_symbol_id).unwrap();
                let width: u16 = TryInto::<u16>::try_into(slots.len()).unwrap();
                verilog::Expr::WordLit(verilog::expr::WordLit { value: value, width, radix: Radix::Dec })
            }
            _ => panic!("unsupported expr in conversion: {}", node.summary()),
        }
    }

    fn convert_if_expr(
        &self,
        package: &PackageFqn,
        children: &[AstNode],
        expected_type: Option<&Type>,
    ) -> verilog::Expr {
        let cond = self.convert_expr(package, children[0].clone(), None, self.db);
        let then_expr = self.convert_expr(package, children[1].clone(), expected_type, self.db);
        let else_expr = if children.len() == 3 {
            self.convert_expr(package, children[2].clone(), expected_type, self.db)
        } else {
            self.convert_if_expr(package, &children[2..], expected_type)
        };
        verilog::Expr::If(verilog::expr::If {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    fn convert_zext(&self, package: &PackageFqn, node: AstNode, expected_type: Option<&Type>) -> verilog::Expr {
        let inner = node.child(0);
        let target_width = self.node_or_expected_width(package, &node, expected_type);
        let source_width = self.node_width(package, &inner);
        let extend_by = target_width.checked_sub(source_width)
            .unwrap_or_else(|| panic!("cannot zext from {source_width} to {target_width}"));
        if extend_by == 0 {
            return self.convert_expr(package, inner, None, self.db);
        }
        verilog::Expr::Concat(verilog::expr::Concat {
            exprs: vec![
                verilog::Expr::Repeat(verilog::expr::Repeat {
                    count: Box::new(constant_index_expr(extend_by)),
                    exprs: vec![constant_word_expr(1, 0)],
                    width: extend_by,
                }),
                self.convert_expr(package, inner, None, self.db),
            ],
            width: target_width,
        })
    }

    fn convert_sext(&self, package: &PackageFqn, node: AstNode, expected_type: Option<&Type>) -> verilog::Expr {
        let inner = node.child(0);
        let target_width = self.node_or_expected_width(package, &node, expected_type);
        let source_width = self.node_width(package, &inner);
        let extend_by = target_width.checked_sub(source_width)
            .unwrap_or_else(|| panic!("cannot sext from {source_width} to {target_width}"));
        if extend_by == 0 {
            return self.convert_expr(package, inner, None, self.db);
        }
        let fill = if source_width == 1 {
            self.convert_expr(package, inner.clone(), None, self.db)
        } else {
            verilog::Expr::Index(verilog::expr::Index {
                subject: Box::new(self.convert_expr(package, inner.clone(), None, self.db)),
                index: Box::new(constant_index_expr(source_width - 1)),
            })
        };
        verilog::Expr::Concat(verilog::expr::Concat {
            exprs: vec![
                verilog::Expr::Repeat(verilog::expr::Repeat {
                    count: Box::new(constant_index_expr(extend_by)),
                    exprs: vec![fill],
                    width: extend_by,
                }),
                self.convert_expr(package, inner, None, self.db),
            ],
            width: target_width,
        })
    }

    fn convert_command(&self, package: &PackageFqn, node: AstNode) -> verilog::Stmt {
        match node.payload() {
            AstNodePayload::CommandAssert => verilog::Stmt::Assert(verilog::Assert {
                exprs: vec![self.convert_expr(package, node.child(0), None, self.db)],
            }),
            AstNodePayload::CommandDisplay(s) => {
                let message = node.parsing.string(s).to_owned();
                let expr = self.convert_expr(package, node.child(0), None, self.db);
                verilog::Stmt::Display(verilog::Display { message, exprs: vec![expr] })
            }
            AstNodePayload::CommandFinish => verilog::Stmt::Finish,
            AstNodePayload::CommandFatal => verilog::Stmt::Fatal,
            AstNodePayload::CommandIf => {
                let children = node.children();
                let cond = self.convert_expr(package, children[0].clone(), None, self.db);
                let stmts = children[1..].iter()
                    .map(|cmd| self.convert_command(package, cmd.clone()))
                    .collect();
                verilog::Stmt::If(verilog::If { cond, stmts })
            }
            _ => panic!("expected command node, found {}", node.summary()),
        }
    }

    fn typing_for(&self, node: &AstNode) -> Arc<Typing> {
        let exprroot = self.db.get_exprroot_for(node.location());
        self.db.get_typing(exprroot)
    }

    fn node_type(&self, package: &PackageFqn, node: &AstNode) -> Option<Type> {
        self.db.get_typeof(Location::new(package.clone(), node.id())).ok()
    }

    fn node_width(&self, package: &PackageFqn, node: &AstNode) -> Width {
        self.node_type(package, node).map(|t| type_width(&t, self.db)).unwrap_or(0)
    }

    fn node_or_expected_width(&self, package: &PackageFqn, node: &AstNode, expected: Option<&Type>) -> Width {
        self.node_type(package, node)
            .or_else(|| expected.cloned())
            .map(|t| type_width(&t, self.db))
            .unwrap_or(0)
    }
}


fn qualified_module_name(package_name: &str, module_name: &str) -> String {
    format!("{package_name}::{module_name}")
}

fn type_width(typ: &Type, db: &Db) -> Width {
    match typ {
        Type::Bit | Type::Clock => 1,
        Type::Word(w) => *w,
        Type::Usual(typedef_symbol_id) => {
            let symboltable = db.get_symboltable();
            let slots = symboltable.slots(*typedef_symbol_id);
            slots.len().try_into().unwrap()
        }
    }
}

fn render_ofness_path(ofness_node: AstNode, package: &PackageFqn) -> String {
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        panic!("expected Ofness node");
    };
    let module_package = ofness
        .package
        .map(|pkg| ofness_node.parsing.string(pkg).to_str_lossy().into_owned())
        .unwrap_or_else(|| package.to_string());
    let module_name = ofness_node.parsing.string(ofness.name).to_str_lossy();
    format!("{module_package}::{module_name}")
}

fn parse_word_literal(literal: &str) -> (u64, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

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

fn constant_word_expr(width: Width, value: u128) -> verilog::Expr {
    verilog::Expr::WordLit(verilog::expr::WordLit { value, width, radix: Radix::Dec })
}

fn constant_index_expr(index: Width) -> verilog::Expr {
    constant_word_expr(32, u128::from(index))
}

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
        common::BinOp::And => verilog::BinOp::BitAnd,
        common::BinOp::Or => verilog::BinOp::BitOr,
        common::BinOp::Xor => verilog::BinOp::BitXor,
        common::BinOp::LogicalAnd => verilog::BinOp::BitAnd,
        common::BinOp::LogicalOr => verilog::BinOp::BitOr,
        common::BinOp::LogicalXor => verilog::BinOp::BitXor,
    }
}

fn convert_unop(op: common::UnOp) -> verilog::UnOp {
    match op {
        common::UnOp::Neg => verilog::UnOp::Neg,
        common::UnOp::Inv => verilog::UnOp::BitNot,
        common::UnOp::Not => verilog::UnOp::LogNot,
    }
}

/// Recursively collects the region strings of all `?` (hole) nodes within an expression.
fn collect_ast_holes(node: AstNode<'_>, holes: &mut Vec<String>) {
    match node.payload() {
        AstNodePayload::ExprHole => holes.push(format!("{}", node.region())),
        _ => {
            for child in node.children() {
                collect_ast_holes(child, holes);
            }
        }
    }
}
