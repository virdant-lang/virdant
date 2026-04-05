use indexmap::IndexMap;
use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::drivers::{Driver, DriverIf};
use crate::analysis::location::Location;
use crate::common::{self, ComponentKind, DriverType, Radix, TypeScheme, Width};
use crate::db::Db;
use crate::diagnostics::DiagnosticLevel;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::types::{Type, Typing};
use crate::verilog::{self, exact_verilog_name, valid_verilog_name};

/// Accumulates Verilog elements (regs, always blocks) generated as side-effects of
/// converting expressions that cannot be expressed inline (e.g., `match` → `casez`).
struct ExprScheduler {
    counter: usize,
    extra_elements: Vec<verilog::Element>,
    /// Substitution map: bound pattern-variable names → their Verilog extraction expressions.
    subst: IndexMap<String, verilog::Expr>,
    /// Statements to be prepended into the current sequential always block (collected from
    /// match expressions encountered while `in_sequential` is true).
    sequential_stmts: Vec<verilog::Stmt>,
    /// Whether we are currently scheduling within a sequential (clocked) context.
    in_sequential: bool,
}

impl ExprScheduler {
    fn new() -> Self {
        ExprScheduler {
            counter: 0,
            extra_elements: vec![],
            subst: IndexMap::new(),
            sequential_stmts: vec![],
            in_sequential: false,
        }
    }

    /// Allocate a fresh escaped Verilog identifier for a temporary.
    fn fresh_temp_name(&mut self, kind: &str) -> String {
        let raw = format!("temp${kind}${}", self.counter);
        self.counter += 1;
        valid_verilog_name(&raw)
    }

    /// Schedule a Verilog element to be emitted alongside the module.
    fn push(&mut self, element: verilog::Element) {
        self.extra_elements.push(element);
    }

    /// Drain all scheduled elements for insertion into the module.
    fn drain(&mut self) -> Vec<verilog::Element> {
        std::mem::take(&mut self.extra_elements)
    }

    /// Drain all statements scheduled for the current sequential always block.
    fn drain_sequential_stmts(&mut self) -> Vec<verilog::Stmt> {
        std::mem::take(&mut self.sequential_stmts)
    }
}

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
    module_ports: IndexMap<String, Vec<(String, Width)>>,
    /// "package::Module" -> emitted Verilog name
    emitted_module_names: IndexMap<String, String>,
}

impl<'d> Converter<'d> {
    fn new(db: &'d Db) -> Self {
        Converter {
            db,
            module_ports: IndexMap::new(),
            emitted_module_names: IndexMap::new(),
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
                let moddef_name = parsing.string(moddef.name.clone()).to_str_lossy().into_owned();
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
                    let name = parsing.string(component.name.clone()).to_str_lossy().into_owned();
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
        let moddef_name = parsing.string(moddef.name.clone()).to_str_lossy().into_owned();
        let module_path = qualified_module_name(&package.to_string(), &moddef_name);
        let module_name = self.emitted_module_names[&module_path].clone();

        let symboltable = self.db.get_symboltable();
        let moddef_symbol = symboltable
            .resolve_item_in_package(parsing.string(moddef.name), package.clone())
            .unwrap();
        let component_analysis = self.db.get_component_analysis(moddef_symbol.id());

        let mut scheduler = ExprScheduler::new();
        let mut ports: Vec<verilog::Port> = vec![];
        let mut elements: Vec<verilog::Element> = vec![];
        // reg name -> AstNodeId of its explicit `on clock` expression (if present)
        let mut sequential_regs: IndexMap<String, AstNodeId> = IndexMap::new();
        let mut on_block: Option<verilog::Element> = None;

        // First pass: ports, wires, regs, instances, on
        for stmt in item_ast.children() {
            match stmt.payload() {
                AstNodePayload::Component(component) => {
                    let name = parsing.string(component.name.clone()).to_str_lossy().into_owned();
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
                        ComponentKind::Wire => {
                            let children = stmt.children();
                            if children.len() >= 2 {
                                // Wire with explicit `on clock` — treat exactly like a Reg.
                                sequential_regs.insert(name.clone(), children[1].id());
                                elements.push(verilog::Element::Reg(verilog::Reg {
                                    name: exact_verilog_name(&name),
                                    width,
                                    expr: None,
                                }));
                            } else {
                                elements.push(verilog::Element::Wire(verilog::Wire {
                                    name: exact_verilog_name(&name),
                                    width,
                                    expr: None,
                                }));
                            }
                        }
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
                AstNodePayload::Submodule(module) => {
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
                    let clock_child = stmt.child(0);
                    let clock_type = self.node_type(package, &clock_child).unwrap();
                    let clock_expr = self.convert_expr(package, clock_child, &clock_type, self.db, &mut scheduler);
                    let stmts = stmt.children().into_iter().skip(1)
                        .map(|cmd| self.convert_command(package, cmd, &mut scheduler))
                        .collect();
                    on_block = Some(verilog::Element::Always(verilog::Always {
                        clock: Some(clock_expr),
                        stmts,
                    }));
                }
                _ => {}
            }
        }

        // Second pass: drivers via DriverAnalysis
        let mut hole_displays: Vec<verilog::Element> = vec![];
        let mut combinational: Vec<verilog::Element> = vec![];
        let mut sequential: Vec<verilog::Element> = vec![];

        let driver_analysis = self.db.get_driver_analysis(moddef_symbol.id());
        for (comp_id, comp_drivers) in driver_analysis.drivers() {
            let Some(component) = component_analysis.component(*comp_id) else { continue };
            let path = component.path().to_str_lossy().into_owned();
            let Some(typ) = component.typ() else { continue };

            for driver in comp_drivers {
                // Collect holes from this driver tree
                for region in self.collect_driver_holes(driver) {
                    let message = format!("\"HOLE: ? at {region}\"");
                    hole_displays.push(verilog::Element::Initial(verilog::Initial {
                        stmts: vec![verilog::Stmt::Display(verilog::Display {
                            message: message.into(),
                            exprs: vec![],
                        })],
                    }));
                }

                // For latched-driver wires without an explicit `on clock`, infer the clock
                // from any sequential reg already found in this module.
                let latched = driver.driver_type() == DriverType::Latched;
                let clock_id_opt = sequential_regs.get(&path).copied().or_else(|| {
                    if latched { sequential_regs.values().copied().next() } else { None }
                });

                if let Some(clock_id) = clock_id_opt {
                    // If this is a latched-driver wire not yet in sequential_regs, upgrade
                    // its Wire element to a Reg element so the Verilog declaration is correct.
                    if !sequential_regs.contains_key(&path) {
                        let reg_name = exact_verilog_name(&path);
                        let reg_width = type_width(&typ, self.db);
                        for elem in elements.iter_mut() {
                            if let verilog::Element::Wire(w) = elem {
                                if w.name == reg_name {
                                    *elem = verilog::Element::Reg(verilog::Reg {
                                        name: reg_name.clone(),
                                        width: reg_width,
                                        expr: None,
                                    });
                                    break;
                                }
                            }
                        }
                    }
                    let clock_node = parsing.ast_node(clock_id);
                    let clock_type = self.node_type(package, &clock_node).unwrap();
                    let clock_expr = self.convert_expr(package, clock_node, &clock_type, self.db, &mut scheduler);
                    scheduler.in_sequential = true;
                    let stmts = match driver {
                        Driver::If(driver_if) if driver_if.driver_type == DriverType::Latched => {
                            self.convert_latched_driver_if_to_stmts(package, driver_if, &typ, &path, &mut scheduler)
                        }
                        _ => {
                            let expr = self.convert_driver_to_expr(package, driver, &typ, &mut scheduler);
                            vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                                name: valid_verilog_name(&path),
                                expr,
                            })]
                        }
                    };
                    scheduler.in_sequential = false;
                    let mut all_stmts = scheduler.drain_sequential_stmts();
                    all_stmts.extend(stmts);
                    sequential.push(verilog::Element::Always(verilog::Always {
                        clock: Some(clock_expr),
                        stmts: all_stmts,
                    }));
                } else {
                    let expr = self.convert_driver_to_expr(package, driver, &typ, &mut scheduler);
                    combinational.push(verilog::Element::Assign(verilog::Assign {
                        name: valid_verilog_name(&path),
                        expr,
                    }));
                }
            }
        }

        elements.extend(combinational);
        elements.extend(sequential);
        if let Some(on) = on_block {
            elements.push(on);
        }
        elements.extend(hole_displays);
        // Collect any temp regs/always blocks generated by match expressions
        elements.extend(scheduler.drain());

        let init_stmts: Vec<verilog::Stmt> = elements.iter()
            .filter_map(|e| {
                let verilog::Element::Reg(reg) = e else { return None };
                Some(verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: reg.name.clone(),
                    expr: verilog::Expr::WordLit(verilog::expr::WordLit {
                        value: 0,
                        width: reg.width,
                        radix: Radix::Dec,
                    }),
                }))
            })
            .collect();
        if !init_stmts.is_empty() {
            elements.push(verilog::Element::Initial(verilog::Initial { stmts: init_stmts }));
        }

        verilog::Module { name: module_name, is_ext: moddef.is_ext, ports, elements }
    }

    /// Recursively collects the region strings of all `?` (hole) nodes in a driver tree.
    fn collect_driver_holes(&self, driver: &Driver) -> Vec<String> {
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let expr_node = parsing.ast_node(loc.ast_node_id());
                collect_ast_holes(expr_node)
            }
            Driver::If(driver_if) => {
                let mut holes = vec![];
                for (_, sub_driver) in &driver_if.clauses {
                    holes.extend(self.collect_driver_holes(sub_driver));
                }
                if let Some(else_driver) = &driver_if.else_clause {
                    holes.extend(self.collect_driver_holes(else_driver));
                }
                holes
            }
        }
    }

    /// Convert a `Driver` tree to a single `verilog::Expr` (for combinational / Continuous drivers).
    /// `Driver::If` becomes a right-associative ternary chain (`cond ? then : else`).
    fn convert_driver_to_expr(
        &self,
        package: &PackageFqn,
        driver: &Driver,
        typ: &Type,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let expr_node = parsing.ast_node(loc.ast_node_id());
                self.convert_expr(package, expr_node, typ, self.db, scheduler)
            }
            Driver::If(driver_if) => self.convert_driver_if_to_expr(package, driver_if, typ, scheduler),
        }
    }

    fn convert_driver_if_to_expr(
        &self,
        package: &PackageFqn,
        driver_if: &DriverIf,
        typ: &Type,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        // Base: else clause, or X-bits when there is no else.
        let else_expr = match &driver_if.else_clause {
            Some(else_driver) => self.convert_driver_to_expr(package, else_driver, typ, scheduler),
            None => verilog::Expr::XLit(verilog::expr::XLit { width: type_width(typ, self.db) }),
        };

        // Build right-associative ternary chain from last clause to first.
        driver_if.clauses.iter().rev().fold(else_expr, |acc, (cond_loc, sub_driver)| {
            let cond_parsing = self.db.get_parsing(cond_loc.package());
            let cond_node = cond_parsing.ast_node(cond_loc.ast_node_id());
            let cond_type = self.node_type(package, &cond_node).unwrap_or(Type::Bit);
            let cond_expr = self.convert_expr(package, cond_node, &cond_type, self.db, scheduler);
            let then_expr = self.convert_driver_to_expr(package, sub_driver, typ, scheduler);
            verilog::Expr::If(verilog::expr::If {
                cond: Box::new(cond_expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(acc),
            })
        })
    }

    /// Convert a `Latched Driver::If` tree into a chain of procedural `Stmt::If` blocks
    /// with non-blocking assignments.  When there is no else clause, no else branch is emitted.
    fn convert_latched_driver_if_to_stmts(
        &self,
        package: &PackageFqn,
        driver_if: &DriverIf,
        typ: &Type,
        path: &str,
        scheduler: &mut ExprScheduler,
    ) -> Vec<verilog::Stmt> {
        // Else stmts (empty when there is no else clause).
        let else_stmts = match &driver_if.else_clause {
            Some(else_driver) => self.convert_latched_driver_to_stmts(package, else_driver, typ, path, scheduler),
            None => vec![],
        };

        // Build right-associative Stmt::If chain from last clause to first.
        driver_if.clauses.iter().rev().fold(else_stmts, |acc_else, (cond_loc, sub_driver)| {
            let cond_parsing = self.db.get_parsing(cond_loc.package());
            let cond_node = cond_parsing.ast_node(cond_loc.ast_node_id());
            let cond_type = self.node_type(package, &cond_node).unwrap_or(Type::Bit);
            let cond_expr = self.convert_expr(package, cond_node, &cond_type, self.db, scheduler);
            let then_stmts = self.convert_latched_driver_to_stmts(package, sub_driver, typ, path, scheduler);
            vec![verilog::Stmt::If(verilog::If {
                cond: cond_expr,
                stmts: then_stmts,
                else_stmts: acc_else,
            })]
        })
    }

    fn convert_latched_driver_to_stmts(
        &self,
        package: &PackageFqn,
        driver: &Driver,
        typ: &Type,
        path: &str,
        scheduler: &mut ExprScheduler,
    ) -> Vec<verilog::Stmt> {
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let expr_node = parsing.ast_node(loc.ast_node_id());
                // Inline match expressions as casez statements directly — no temp reg needed
                // inside a sequential (always @(posedge clock)) context.
                if matches!(expr_node.payload(), AstNodePayload::ExprMatch) {
                    return self.convert_match_expr_to_blocking_stmts(
                        package, expr_node, typ, path, scheduler,
                    );
                }
                let expr = self.convert_expr(package, expr_node, typ, self.db, scheduler);
                vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: valid_verilog_name(path),
                    expr,
                })]
            }
            Driver::If(driver_if) => {
                self.convert_latched_driver_if_to_stmts(package, driver_if, typ, path, scheduler)
            }
        }
    }

    /// Converts an `ExprMatch` node directly into a `casez` statement that blockingly
    /// assigns each arm's result to `target_name`.  This avoids creating a temporary reg +
    /// `always @(*)` block, which would be incorrect inside a sequential
    /// `always @(posedge clock)` context.
    fn convert_match_expr_to_blocking_stmts(
        &self,
        package: &PackageFqn,
        node: AstNode,
        typ: &Type,
        target_name: &str,
        scheduler: &mut ExprScheduler,
    ) -> Vec<verilog::Stmt> {
        let db = self.db;
        let children = node.children();
        let subject = children[0].clone();
        let subject_typ = self.node_type(package, &subject).unwrap();
        let subject_expr = self.convert_expr(package, subject, &subject_typ, db, scheduler);
        let result_width = type_width(typ, db);
        let num_arms = (children.len() - 1) / 2;
        let assigned_name = valid_verilog_name(target_name);
        let mut case_items: Vec<verilog::CaseItem> = vec![];
        let mut has_else = false;
        for i in 0..num_arms {
            let pat = children[2 * i + 1].clone();
            let body = children[2 * i + 2].clone();
            let mut bound_keys: Vec<String> = vec![];
            let pattern = match pat.payload() {
                AstNodePayload::PatElse => { has_else = true; verilog::CasePattern::Default },
                AstNodePayload::PatEnumerant(pat_enumerant) => {
                    let enumerant_name = pat.parsing.string(pat_enumerant.name);
                    let Type::Usual(typedef_symbol_id) = &subject_typ else { unreachable!() };
                    let symboltable = db.get_symboltable();
                    let typedef = db.get_typedef(*typedef_symbol_id);
                    let enumerant_sym = symboltable.slot(*typedef_symbol_id, enumerant_name).unwrap();
                    let value = *typedef.enumerant_values.get(&enumerant_sym.id()).unwrap();
                    let width = type_width(&subject_typ, db);
                    let pat_str = format!("{:0>width$b}", value, width = width as usize);
                    verilog::CasePattern::PatternLit(verilog::PatternLit { width, radix: Radix::Bin, pattern: pat_str })
                }
                AstNodePayload::PatIdent(pat_ident) => {
                    let ctor_name = pat.parsing.string(pat_ident.name);
                    let Type::Usual(typedef_symbol_id) = &subject_typ else { unreachable!() };
                    let symboltable = db.get_symboltable();
                    let slots = symboltable.slots(*typedef_symbol_id);
                    let ctor_slot = symboltable.slot(*typedef_symbol_id, ctor_name).unwrap();
                    let ctor_sym_id = ctor_slot.id();
                    let slot_index = slots.iter().position(|s| s.id() == ctor_sym_id).unwrap();
                    let tag_width: Width = slots.len().try_into().unwrap();
                    let tag_value: u64 = 1u64 << slot_index;
                    let max_payload_width: Width = slots.iter().map(|slot| {
                        let sig = db.get_ctor_signature(slot.id());
                        sig.parameters.iter().map(|(_n, pt)| type_width(pt, db)).sum::<Width>()
                    }).max().unwrap_or(0);
                    let this_sig = db.get_ctor_signature(ctor_sym_id);
                    let arg_widths: Vec<Width> = this_sig.parameters.iter()
                        .map(|(_n, pt)| type_width(pt, db)).collect();
                    let mut bit_ranges = vec![(0u16, 0u16); arg_widths.len()];
                    let mut current_lo = tag_width;
                    for j in (0..arg_widths.len()).rev() {
                        let w = arg_widths[j];
                        bit_ranges[j] = (current_lo, current_lo + w - 1);
                        current_lo += w;
                    }
                    for (j, bound_var) in pat.children().iter().enumerate() {
                        let var_name = bound_var.parsing.string(bound_var.path().unwrap())
                            .to_str_lossy().into_owned();
                        let (lo, hi) = bit_ranges[j];
                        let slice_expr = verilog::Expr::IndexRange(verilog::expr::IndexRange {
                            subject: Box::new(subject_expr.clone()),
                            index_hi: Box::new(constant_index_expr(hi)),
                            index_lo: Box::new(constant_index_expr(lo)),
                        });
                        scheduler.subst.insert(var_name.clone(), slice_expr);
                        bound_keys.push(var_name);
                    }
                    let tag_bin = format!("{:0>width$b}", tag_value, width = tag_width as usize);
                    let question_marks = "?".repeat(max_payload_width as usize);
                    let pattern_str = format!("{}{}", question_marks, tag_bin);
                    verilog::CasePattern::PatternLit(verilog::PatternLit {
                        width: tag_width + max_payload_width,
                        radix: Radix::Bin,
                        pattern: pattern_str,
                    })
                }
                _ => unreachable!("expected pattern node"),
            };
            let body_expr = self.convert_expr(package, body, typ, db, scheduler);
            for key in &bound_keys { scheduler.subst.remove(key); }
            case_items.push(verilog::CaseItem {
                pattern,
                stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: assigned_name.clone(),
                    expr: body_expr,
                })],
            });
        }
        // Default arm: drive X — only when the match has no `else` arm of its own.
        if !has_else {
            case_items.push(verilog::CaseItem {
                pattern: verilog::CasePattern::Default,
                stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: assigned_name.clone(),
                    expr: verilog::Expr::XLit(verilog::expr::XLit { width: result_width }),
                })],
            });
        }
        vec![verilog::Stmt::CaseZ(verilog::CaseZ { subject: subject_expr, items: case_items })]
    }

    fn convert_expr(
        &self,
        package: &PackageFqn,
        node: AstNode,
        typ: &Type,
        db: &Db,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let typing = self.typing_for(&node);
        match node.payload() {
            AstNodePayload::ExprReference => {
                let path = node.parsing.string(node.path().unwrap()).to_str_lossy().into_owned();
                // Check if this reference is a pattern-bound variable substitution.
                if let Some(subst_expr) = scheduler.subst.get(&path) {
                    return subst_expr.clone();
                }
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
                    .unwrap_or_else(|| type_width(typ, self.db));
                verilog::Expr::WordLit(verilog::expr::WordLit { value: value.into(), width, radix: Radix::Dec })
            }
            AstNodePayload::ExprWord => {
                let width = self.node_or_expected_width(package, &node, typ);
                let exprs = node.children().into_iter()
                    .map(|child| {
                        let child_type = self.node_type(package, &child).unwrap();
                        self.convert_expr(package, child, &child_type, self.db, scheduler)
                    })
                    .collect();
                verilog::Expr::Concat(verilog::expr::Concat { exprs, width })
            }
            AstNodePayload::ExprBinOp(expr_bin_op) => {
                let lhs = node.child(0);
                let rhs = node.child(1);
                let lhs_type = self.node_type(package, &lhs).unwrap();
                let rhs_type = self.node_type(package, &rhs).unwrap();
                verilog::Expr::BinOp(verilog::expr::BinOp {
                    op: convert_binop(expr_bin_op.op),
                    lhs: Box::new(self.convert_expr(package, lhs, &lhs_type, self.db, scheduler)),
                    rhs: Box::new(self.convert_expr(package, rhs, &rhs_type, self.db, scheduler)),
                })
            }
            AstNodePayload::ExprUnOp(expr_un_op) => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Expr::UnOp(verilog::expr::UnOp {
                    op: convert_unop(expr_un_op.op),
                    expr: Box::new(self.convert_expr(package, child, &child_type, self.db, scheduler)),
                })
            }
            AstNodePayload::ExprZext => self.convert_zext(package, node, typ, scheduler),
            AstNodePayload::ExprSext => self.convert_sext(package, node, typ, scheduler),
            AstNodePayload::ExprIf => {
                let children = node.children();
                self.convert_if_expr(package, &children, typ, scheduler)
            }
            AstNodePayload::ExprIndex(expr_index) => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Expr::Index(verilog::expr::Index {
                    subject: Box::new(self.convert_expr(package, child, &child_type, self.db, scheduler)),
                    index: Box::new(constant_index_expr(expr_index.index)),
                })
            }
            AstNodePayload::ExprIndexRange(expr_index_range) => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Expr::IndexRange(verilog::expr::IndexRange {
                    subject: Box::new(self.convert_expr(package, child, &child_type, self.db, scheduler)),
                    index_hi: Box::new(constant_index_expr(expr_index_range.index_hi - 1)),
                    index_lo: Box::new(constant_index_expr(expr_index_range.index_lo)),
                })
            }
            AstNodePayload::ExprAs | AstNodePayload::ExprParen => {
                self.convert_expr(package, node.child(0), typ, db, scheduler)
            }
            AstNodePayload::ExprHole => {
                let width = self.node_or_expected_width(package, &node, typ);
                verilog::Expr::XLit(verilog::expr::XLit { width })
            }
            AstNodePayload::ExprCtor(_ctor) => {
                let symboltable = db.get_symboltable();
                let Type::Usual(typedef_symbol_id) = typ else {
                    unreachable!()
                };
                let slots = symboltable.slots(*typedef_symbol_id);
                let ctor_symbol_id = typing.tag(node.location()).symbol_id().unwrap();
                let tag_width: Width = TryInto::<u16>::try_into(slots.len()).unwrap();
                let slot_index = slots.iter().position(|slot_id| slot_id.id() == ctor_symbol_id).unwrap();
                let tag_value: u128 = 1 << slot_index;

                // Compute the max payload width across all ctor variants.
                let max_payload_width: Width = slots.iter().map(|slot| {
                    let sig = db.get_ctor_signature(slot.id());
                    sig.parameters.iter().map(|(_name, param_typ)| type_width(param_typ, db)).sum::<Width>()
                }).max().unwrap_or(0);

                let tag_expr = verilog::Expr::WordLit(verilog::expr::WordLit {
                    value: tag_value,
                    width: tag_width,
                    radix: Radix::Bin,
                });

                if max_payload_width == 0 {
                    // No payload at all: just return the tag word.
                    tag_expr
                } else {
                    // Build the full value: {padding_x, arg0, ..., argN, tag}
                    // Layout (LSB to MSB): tag | argN | ... | arg0 | padding
                    // This ensures the tag is always at consistent LSB bit positions
                    // regardless of which ctor variant is used, enabling casez matching.
                    let this_sig = db.get_ctor_signature(ctor_symbol_id);
                    let arguments = node.children();

                    let this_payload_width: Width = this_sig.parameters.iter()
                        .map(|(_name, param_typ)| type_width(param_typ, db))
                        .sum();

                    let padding_width = max_payload_width - this_payload_width;
                    let mut exprs = vec![];
                    // Padding at MSB (most significant, leftmost in concat).
                    // `padding_fill` controls what value fills unused payload bits.
                    // Supported: "x" (don't-care), "z" (high-impedance), "0" (zeros), "1" (ones).
                    if padding_width > 0 {
                        let padding_fill = "1";
                        let padding_expr = match padding_fill {
                            "x" => verilog::Expr::XLit(verilog::expr::XLit { width: padding_width }),
                            "z" => verilog::Expr::ZLit(verilog::expr::ZLit { width: padding_width }),
                            "0" => verilog::Expr::Repeat(verilog::expr::Repeat {
                                count: Box::new(constant_index_expr(padding_width)),
                                exprs: vec![verilog::Expr::BitLit(verilog::expr::BitLit { value: false })],
                                width: padding_width,
                            }),
                            "1" => verilog::Expr::Repeat(verilog::expr::Repeat {
                                count: Box::new(constant_index_expr(padding_width)),
                                exprs: vec![verilog::Expr::BitLit(verilog::expr::BitLit { value: true })],
                                width: padding_width,
                            }),
                            _ => unreachable!("padding_fill must be one of: \"x\", \"z\", \"0\", \"1\""),
                        };
                        exprs.push(padding_expr);
                    }
                    // Arguments in order (first arg at higher bits than last arg)
                    for (arg, (_param_name, param_typ)) in arguments.iter().zip(this_sig.parameters.iter()) {
                        exprs.push(self.convert_expr(package, arg.clone(), param_typ, db, scheduler));
                    }
                    // Tag at LSB (least significant, rightmost in concat)
                    exprs.push(tag_expr);

                    let total_width = tag_width + max_payload_width;
                    verilog::Expr::Concat(verilog::expr::Concat { exprs, width: total_width })
                }
            }
            AstNodePayload::ExprEnumerant(_enumerant) => {
                let symboltable = db.get_symboltable();
                let Type::Usual(typedef_symbol_id) = typ else {
                    unreachable!("{:?} type is {:?}", node.region(), typ)
                };
                let typedef = db.get_typedef(*typedef_symbol_id);
                let slots = symboltable.slots(*typedef_symbol_id);
                let enumerant_symbol_id = typing.tag(node.location()).symbol_id().unwrap();
                let value = *typedef.enumerant_values.get(&enumerant_symbol_id).unwrap();
                let width: u16 = TryInto::<u16>::try_into(slots.len()).unwrap();
                verilog::Expr::WordLit(verilog::expr::WordLit { value: value, width, radix: Radix::Dec })
            }
            AstNodePayload::ExprFn => {
                let tag = typing.tag(node.location());
                let arg = node.child(1);
                match tag {
                    crate::types::typing::Tag::SymbolResolution(_symbol_id) => todo!(),
                    crate::types::typing::Tag::PrimitiveResolution(primitive) => {
                        let arg_type = self.node_type(package, &arg).unwrap();
                        let arg_expr = self.convert_expr(package, arg, &arg_type, self.db, scheduler);
                        match primitive.as_bytes() {
                            b"any" => {
                                verilog::Expr::UnOp(verilog::expr::UnOp {
                                    op: verilog::UnOp::RedOr,
                                    expr: Box::new(arg_expr),
                                })
                            }
                            b"all" => {
                                verilog::Expr::UnOp(verilog::expr::UnOp {
                                    op: verilog::UnOp::RedAnd,
                                    expr: Box::new(arg_expr),
                                })
                            }
                            _ => unreachable!(),
                        }
                    }
                    crate::types::typing::Tag::ComponentResolution(_component_id) => unreachable!(),
                    crate::types::typing::Tag::None => unreachable!(),
                }
            }
            AstNodePayload::ExprMatch => {
                let children = node.children();
                let subject = children[0].clone();
                let subject_typ = self.node_type(package, &subject).unwrap();
                let subject_expr = self.convert_expr(package, subject, &subject_typ, db, scheduler);
                let temp_name = scheduler.fresh_temp_name("match");
                let result_width = type_width(typ, db);
                let num_arms = (children.len() - 1) / 2;
                let mut case_items: Vec<verilog::CaseItem> = vec![];
                let mut has_else = false;
                for i in 0..num_arms {
                    let pat = children[2 * i + 1].clone();
                    let body = children[2 * i + 2].clone();
                    let mut bound_keys: Vec<String> = vec![];
                    let pattern = match pat.payload() {
                        AstNodePayload::PatElse => { has_else = true; verilog::CasePattern::Default },
                        AstNodePayload::PatEnumerant(pat_enumerant) => {
                            let enumerant_name = pat.parsing.string(pat_enumerant.name);
                            let Type::Usual(typedef_symbol_id) = &subject_typ else { unreachable!() };
                            let symboltable = db.get_symboltable();
                            let typedef = db.get_typedef(*typedef_symbol_id);
                            let enumerant_sym = symboltable.slot(*typedef_symbol_id, enumerant_name).unwrap();
                            let value = *typedef.enumerant_values.get(&enumerant_sym.id()).unwrap();
                            let width = type_width(&subject_typ, db);
                            let pat_str = format!("{:0>width$b}", value, width = width as usize);
                            verilog::CasePattern::PatternLit(verilog::PatternLit {
                                width,
                                radix: Radix::Bin,
                                pattern: pat_str,
                            })
                        }
                        AstNodePayload::PatIdent(pat_ident) => {
                            let ctor_name = pat.parsing.string(pat_ident.name);
                            let Type::Usual(typedef_symbol_id) = &subject_typ else { unreachable!() };
                            let symboltable = db.get_symboltable();
                            let slots = symboltable.slots(*typedef_symbol_id);
                            // Look up the ctor slot by name (avoids needing typing tags on pattern nodes)
                            let ctor_slot = symboltable.slot(*typedef_symbol_id, ctor_name).unwrap();
                            let ctor_sym_id = ctor_slot.id();
                            let slot_index = slots.iter().position(|s| s.id() == ctor_sym_id).unwrap();
                            let tag_width: Width = slots.len().try_into().unwrap();
                            let tag_value: u64 = 1u64 << slot_index;
                            let max_payload_width: Width = slots.iter().map(|slot| {
                                let sig = db.get_ctor_signature(slot.id());
                                sig.parameters.iter().map(|(_n, pt)| type_width(pt, db)).sum::<Width>()
                            }).max().unwrap_or(0);
                            let this_sig = db.get_ctor_signature(ctor_sym_id);
                            let arg_widths: Vec<Width> = this_sig.parameters.iter()
                                .map(|(_n, pt)| type_width(pt, db))
                                .collect();
                            // Compute bit ranges: tag at [tag_width-1:0], then args from last to first.
                            // Layout (LSB→MSB): tag | argN | arg(N-1) | ... | arg0 | padding
                            let mut bit_ranges = vec![(0u16, 0u16); arg_widths.len()];
                            let mut current_lo = tag_width;
                            for j in (0..arg_widths.len()).rev() {
                                let w = arg_widths[j];
                                bit_ranges[j] = (current_lo, current_lo + w - 1);
                                current_lo += w;
                            }
                            // Register subst for each bound variable as a direct bit-slice.
                            for (j, bound_var) in pat.children().iter().enumerate() {
                                let var_name = bound_var.parsing.string(bound_var.path().unwrap())
                                    .to_str_lossy().into_owned();
                                let (lo, hi) = bit_ranges[j];
                                let slice_expr = verilog::Expr::IndexRange(verilog::expr::IndexRange {
                                    subject: Box::new(subject_expr.clone()),
                                    index_hi: Box::new(constant_index_expr(hi)),
                                    index_lo: Box::new(constant_index_expr(lo)),
                                });
                                scheduler.subst.insert(var_name.clone(), slice_expr);
                                bound_keys.push(var_name);
                            }
                            // Build casez pattern: ?'s for payload, exact binary tag at LSB
                            let tag_bin = format!("{:0>width$b}", tag_value, width = tag_width as usize);
                            let question_marks = "?".repeat(max_payload_width as usize);
                            let pattern_str = format!("{}{}", question_marks, tag_bin);
                            verilog::CasePattern::PatternLit(verilog::PatternLit {
                                width: tag_width + max_payload_width,
                                radix: Radix::Bin,
                                pattern: pattern_str,
                            })
                        }
                        _ => unreachable!("expected pattern node"),
                    };
                    // Convert body with pattern-variable substitutions in scope
                    let body_expr = self.convert_expr(package, body, typ, db, scheduler);
                    // Remove pattern-variable substitutions
                    for key in &bound_keys {
                        scheduler.subst.remove(key);
                    }
                    case_items.push(verilog::CaseItem {
                        pattern,
                        stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                            name: temp_name.clone(),
                            expr: body_expr,
                        })],
                    });
                }
                // Default case: drive X — only when the match has no `else` arm of its own.
                if !has_else {
                    case_items.push(verilog::CaseItem {
                        pattern: verilog::CasePattern::Default,
                        stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                            name: temp_name.clone(),
                            expr: verilog::Expr::XLit(verilog::expr::XLit { width: result_width }),
                        })],
                    });
                }
                // Schedule: always push the temp reg; in a sequential context, inline the casez
                // into the parent always block instead of emitting a separate always @(*).
                scheduler.push(verilog::Element::Reg(verilog::Reg {
                    name: temp_name.clone(),
                    width: result_width,
                    expr: None,
                }));
                let casez_stmt = verilog::Stmt::CaseZ(verilog::CaseZ {
                    subject: subject_expr,
                    items: case_items,
                });
                if scheduler.in_sequential {
                    scheduler.sequential_stmts.push(casez_stmt);
                } else {
                    scheduler.push(verilog::Element::Always(verilog::Always {
                        clock: None,
                        stmts: vec![casez_stmt],
                    }));
                }
                verilog::Expr::Reference(verilog::expr::Reference { name: temp_name })
            }
            _ => panic!("unsupported expr in conversion: {}", node.summary()),
        }
    }

    fn convert_if_expr(
        &self,
        package: &PackageFqn,
        children: &[AstNode],
        typ: &Type,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let cond_type = self.node_type(package, &children[0]).unwrap();
        let cond = self.convert_expr(package, children[0].clone(), &cond_type, self.db, scheduler);
        let then_expr = self.convert_expr(package, children[1].clone(), typ, self.db, scheduler);
        let else_expr = if children.len() == 3 {
            self.convert_expr(package, children[2].clone(), typ, self.db, scheduler)
        } else {
            self.convert_if_expr(package, &children[2..], typ, scheduler)
        };
        verilog::Expr::If(verilog::expr::If {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    fn convert_zext(&self, package: &PackageFqn, node: AstNode, typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        let inner = node.child(0);
        let inner_type = self.node_type(package, &inner).unwrap();
        let target_width = self.node_or_expected_width(package, &node, typ);
        let source_width = self.node_width(package, &inner);
        let extend_by = target_width.checked_sub(source_width)
            .unwrap_or_else(|| panic!("cannot zext from {source_width} to {target_width}"));
        if extend_by == 0 {
            return self.convert_expr(package, inner, &inner_type, self.db, scheduler);
        }
        verilog::Expr::Concat(verilog::expr::Concat {
            exprs: vec![
                verilog::Expr::Repeat(verilog::expr::Repeat {
                    count: Box::new(constant_index_expr(extend_by)),
                    exprs: vec![constant_word_expr(1, 0)],
                    width: extend_by,
                }),
                self.convert_expr(package, inner, &inner_type, self.db, scheduler),
            ],
            width: target_width,
        })
    }

    fn convert_sext(&self, package: &PackageFqn, node: AstNode, typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        let inner = node.child(0);
        let inner_type = self.node_type(package, &inner).unwrap();
        let target_width = self.node_or_expected_width(package, &node, typ);
        let source_width = self.node_width(package, &inner);
        let extend_by = target_width.checked_sub(source_width)
            .unwrap_or_else(|| panic!("cannot sext from {source_width} to {target_width}"));
        if extend_by == 0 {
            return self.convert_expr(package, inner, &inner_type, self.db, scheduler);
        }
        let fill = if source_width == 1 {
            self.convert_expr(package, inner.clone(), &inner_type, self.db, scheduler)
        } else {
            verilog::Expr::Index(verilog::expr::Index {
                subject: Box::new(self.convert_expr(package, inner.clone(), &inner_type, self.db, scheduler)),
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
                self.convert_expr(package, inner, &inner_type, self.db, scheduler),
            ],
            width: target_width,
        })
    }

    fn convert_command(&self, package: &PackageFqn, node: AstNode, scheduler: &mut ExprScheduler) -> verilog::Stmt {
        match node.payload() {
            AstNodePayload::CommandAssert => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Stmt::Assert(verilog::Assert {
                    exprs: vec![self.convert_expr(package, child, &child_type, self.db, scheduler)],
                })
            }
            AstNodePayload::CommandDisplay(s) => {
                let message = node.parsing.string(s).to_owned();
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                let expr = self.convert_expr(package, child, &child_type, self.db, scheduler);
                verilog::Stmt::Display(verilog::Display { message, exprs: vec![expr] })
            }
            AstNodePayload::CommandFinish => verilog::Stmt::Finish,
            AstNodePayload::CommandFatal => verilog::Stmt::Fatal,
            AstNodePayload::CommandIf => {
                let children = node.children();
                let cond_type = self.node_type(package, &children[0]).unwrap();
                let cond = self.convert_expr(package, children[0].clone(), &cond_type, self.db, scheduler);
                let stmts = children[1..].iter()
                    .map(|cmd| self.convert_command(package, cmd.clone(), scheduler))
                    .collect();
                verilog::Stmt::If(verilog::If { cond, stmts, else_stmts: vec![] })
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

    fn node_or_expected_width(&self, package: &PackageFqn, node: &AstNode, expected: &Type) -> Width {
        self.node_type(package, node)
            .map(|t| type_width(&t, self.db))
            .unwrap_or_else(|| type_width(expected, self.db))
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
            let typedef = db.get_typedef(*typedef_symbol_id);
            let symboltable = db.get_symboltable();
            let slots = symboltable.slots(*typedef_symbol_id);
            if typedef.kind == TypeScheme::UnionDef {
                // For union types: tag_width + max_payload_width across all variants.
                // tag_width = number of ctor variants (1-hot encoding)
                let tag_width: Width = slots.len().try_into().unwrap();
                let max_payload_width: Width = slots.iter().map(|slot| {
                    let sig = db.get_ctor_signature(slot.id());
                    sig.parameters.iter().map(|(_n, pt)| type_width(pt, db)).sum::<Width>()
                }).max().unwrap_or(0);
                tag_width + max_payload_width
            } else {
                // For enum and other types: use number of slots (existing behavior).
                slots.len().try_into().unwrap()
            }
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
fn collect_ast_holes(node: AstNode<'_>) -> Vec<String> {
    let mut holes = vec![];
    match node.payload() {
        AstNodePayload::ExprHole => holes.push(format!("{}", node.region())),
        _ => {
            for child in node.children() {
                holes.extend(collect_ast_holes(child).into_iter());
            }
        }
    }
    holes
}
