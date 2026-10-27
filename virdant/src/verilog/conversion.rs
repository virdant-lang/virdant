//! Converts the checked analysis database into a Verilog AST,
//! translating modules, ports, drivers, match expressions, and
//! primitive operations into Verilog elements using an `ExprScheduler`
//! to hoist non-inline expressions into temporaries.

use indexmap::IndexMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::drivers::{Driver, DriverWhen, DriverMatch};
use crate::analysis::location::Location;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolTable};
use crate::common::{self, ComponentKind, DriverType, Radix, TypeScheme, Width, WordValue};
use crate::db::Db;
use crate::package::PackageId;
use crate::syntax::ast::{AnnotationValue, AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::types::substitute::substitute;
use crate::types::typing::Primitive;
use crate::types::typing::Tag;
use crate::types::context::Referent;
use crate::types::{Type, Typing};
use crate::verilog::{self, valid_verilog_name};
use crate::util::parse_word_literal;

/// Must be one of: "x", "z", "0", "1".
const PADDING_FILL: &str = "1";

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
///
/// Caller must have verified that the database is free of errors (e.g. via
/// `virdant::util::check_db`); this function performs no validation and assumes
/// the input is well-formed.
pub fn convert_db_to_verilog(db: &Db) -> verilog::Verilog {
    let mut converter = Converter::new(db);
    let mut verilog = converter.run();
    verilog.normalize();
    verilog
}

/// One monomorphized instantiation of a generic mod def: the def
/// together with concrete type arguments.  Emitted as one Verilog
/// module with a hash-mangled name and a `//` origin comment.
#[derive(Clone)]
struct MonoModule {
    /// The type arguments of this instantiation.
    args: Vec<Type>,
    /// The emitted Verilog module name (hash-mangled).
    name: String,
    /// Human-readable origin, e.g. `design::Fifo[Word[8]]`.
    origin: String,
    /// `(port_name, width)` for the parent's wire declarations.
    ports: Vec<(String, Width)>,
    /// `@parameter` annotations declared on the def, in order.
    declared_params: Vec<(String, Option<AnnotationValue>)>,
}

struct Converter<'d> {
    db: &'d Db,
    /// "package::Module" -> list of (port_name, width)
    module_ports: IndexMap<String, Vec<(String, Width)>>,
    /// "package::Module" -> emitted Verilog name
    emitted_module_names: IndexMap<String, String>,
    /// "package::Module" -> [(param name, declared default, in @parameter order)]
    module_declared_params: IndexMap<String, Vec<(String, Option<AnnotationValue>)>>,
    /// Discovered instantiations of generic mod defs, keyed by
    /// `(def, args)`.  Insertion order is deterministic (packages, then
    /// item order, then depth-first through nested instances), so the
    /// emitted mono modules are stable.
    mono_modules: IndexMap<(SymbolId, Vec<Type>), MonoModule>,
    /// The type arguments of the mono module currently being converted
    /// (`[]` outside a generic def's instantiation).  Every type
    /// fetched from the (generic) analysis is substituted with these
    /// before use.
    mono_env: Vec<Type>,
}

impl<'d> Converter<'d> {
    fn new(db: &'d Db) -> Self {
        Converter {
            db,
            module_ports: IndexMap::new(),
            emitted_module_names: IndexMap::new(),
            module_declared_params: IndexMap::new(),
            mono_modules: IndexMap::new(),
            mono_env: vec![],
        }
    }

    fn run(&mut self) -> verilog::Verilog {
        self.build_module_info();
        let mut files = vec![];
        let packages = self.db.get_packages();
        for package in packages.ids() {
            if let Some(file) = self.convert_package(package) {
                files.push(file);
            }
        }
        verilog::Verilog { files }
    }

    fn build_module_info(&mut self) {
        let symboltable = self.db.get_symboltable();
        let packages = self.db.get_packages();
        for package in packages.ids() {
            let parsing = self.db.get_parsing(package);
            for item_ast in parsing.root().children() {
                let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                    continue;
                };

                // A generic mod def is never emitted at def level: it
                // has no concrete port widths.  Its instantiations are
                // discovered below and emitted as mono modules.
                if item_generics(&item_ast).is_some() {
                    continue;
                }

                let moddef_name = parsing.string(moddef.name.clone()).to_str_lossy().into_owned();
                let module_path = qualified_module_name(&self.db.get_packages().name(package).to_str_lossy(), &moddef_name);
                let emitted_name = if let Some(cell) = cell_annotation(&self.db, package, &item_ast) {
                    // Yosys matches ext-mod instantiations against its
                    // native cell names (e.g. `SB_RAM40_4K`), so the
                    // `@cell` name is the emitted name.
                    cell
                } else {
                    valid_verilog_name(&module_path)
                };
                self.emitted_module_names.insert(module_path.clone(), emitted_name);

                let moddef_symbol = symboltable
                    .resolve_item_in_package(parsing.string(moddef.name), package)
                    .unwrap();
                let ports_of = self.db.get_ports_of(moddef_symbol.id());
                let mut ports = vec![];
                for port in ports_of.iter() {
                    use bstr::ByteSlice;
                    let name = port.path.to_str_lossy().into_owned();
                    let width = port.clone().typ.map(|t| type_width(&t, self.db)).unwrap_or(0);
                    ports.push((name, width));
                }
                self.module_ports.insert(module_path.clone(), ports);

                let params = item_ast.parameter_annotations(&parsing);
                if !params.is_empty() {
                    let declared: Vec<(String, Option<AnnotationValue>)> = params.into_iter()
                        .map(|(name, value)| (name.to_str_lossy().into_owned(), value))
                        .collect();
                    self.module_declared_params.insert(module_path, declared);
                }
            }
        }

        // Discover every instantiation of a generic mod def reachable
        // from a non-generic module, recursively.  Instantiations are
        // keyed by `(def, args)`; identical arguments share one mono
        // module.
        let packages = self.db.get_packages();
        for package in packages.ids() {
            let parsing = self.db.get_parsing(package);
            for item_ast in parsing.root().children() {
                if !matches!(item_ast.payload(), AstNodePayload::ModDef(_)) {
                    continue;
                }
                if item_generics(&item_ast).is_some() {
                    continue;
                }
                for stmt in item_ast.children() {
                    if !matches!(stmt.payload(), AstNodePayload::Submodule(_)) {
                        continue;
                    }
                    let Some((target_symbol, args)) = instantiation_target(self.db, &stmt) else {
                        continue;
                    };
                    if !target_is_generic(self.db, &target_symbol) {
                        continue;
                    }
                    self.register_mono_module(target_symbol.id(), args);
                }
            }
        }
    }

    /// Register the mono module for instantiating the generic def
    /// `def` at `args` (concrete in this context), computing its ports
    /// with the arguments substituted, then recursing into the def's
    /// own instantiations with the arguments composed.
    fn register_mono_module(&mut self, def: SymbolId, args: Vec<Type>) {
        let key = (def, args.clone());
        if self.mono_modules.contains_key(&key) {
            return;
        }

        let db = self.db;
        let symbol = db.get_symboltable().symbol(def);
        let def_location = symbol.location();
        let def_parsing = db.get_parsing(def_location.package());
        let def_node = def_parsing.ast_node(def_location.ast_node_id());
        let AstNodePayload::ModDef(moddef) = def_node.payload() else {
            return;
        };
        let moddef_name = def_parsing.string(moddef.name.clone()).to_str_lossy().into_owned();
        let module_path = qualified_module_name(
            &db.get_packages().name(def_location.package()).to_str_lossy(),
            &moddef_name,
        );

        let def_params = target_params(db, &symbol);
        let origin = crate::types::render::render_mono_item(db, def, &def_params, &args);
        let name = mono_module_name(&module_path, &args);

        // Ports with the instance's type arguments substituted.
        let ports_of = db.get_ports_of(def);
        let mut ports = vec![];
        for port in ports_of.iter() {
            let port_name = port.path.to_str_lossy().into_owned();
            let width = port
                .typ
                .clone()
                .map(|t| type_width(&substitute(&t, &args), db))
                .unwrap_or(0);
            ports.push((port_name, width));
        }

        // `@parameter` annotations declared on the def.
        let declared_params: Vec<(String, Option<AnnotationValue>)> = def_node
            .parameter_annotations(&def_parsing)
            .into_iter()
            .map(|(name, value)| (name.to_str_lossy().into_owned(), value))
            .collect();

        self.mono_modules.insert(
            key.clone(),
            MonoModule {
                args,
                name,
                origin,
                ports,
                declared_params,
            },
        );

        // Recurse into this def's own instantiations, composing the
        // written arguments (which may mention this def's parameters)
        // with this instance's arguments.
        for stmt in def_node.children() {
            if !matches!(stmt.payload(), AstNodePayload::Submodule(_)) {
                continue;
            }
            let Some((target_symbol, written_args)) = instantiation_target(db, &stmt) else {
                continue;
            };
            if !target_is_generic(db, &target_symbol) {
                continue;
            }
            let mono_args: Vec<Type> = written_args
                .iter()
                .map(|arg| substitute(arg, &key.1))
                .collect();
            self.register_mono_module(target_symbol.id(), mono_args);
        }
    }

    /// Build the merged parameter list for a submodule instantiation.
    /// Walks the declared parameters first (using instance-supplied override
    /// or declared default), then appends any instance-supplied names not
    /// in the declared list.  `declared` is the target's `@parameter`
    /// declaration list (def-level or mono-module-level).
    fn build_submodule_parameters(
        &self,
        stmt: &AstNode,
        declared: Option<&[(String, Option<AnnotationValue>)]>,
    ) -> Vec<(String, String)> {
        use crate::syntax::ast::AnnotationValue;
        // Instance-supplied @parameter annotations.
        let supplied: Vec<(String, Option<AnnotationValue>)> = stmt
            .parameter_annotations(&self.db.get_parsing(stmt.package()))
            .into_iter()
            .map(|(name, value)| (name.to_str_lossy().into_owned(), value))
            .collect();

        let Some(declared) = declared else {
            // Target has no declared parameters; emit only instance-supplied values
            // that have a value (not just a name).
            return supplied.into_iter()
                .filter_map(|(name, value)| value.map(|v| (name, render_param_value(&v))))
                .collect();
        };

        // Build a lookup from instance-supplied names to their values.
        let supplied_map: IndexMap<String, Option<AnnotationValue>> =
            supplied.into_iter().collect();

        let mut result: Vec<(String, String)> = Vec::with_capacity(declared.len());

        // Walk declared params: use instance override if present, else declared default.
        for (decl_name, decl_default) in declared {
            let rendered = supplied_map.get(decl_name.as_str())
                .and_then(|v| v.as_ref())
                .or(decl_default.as_ref())
                .map(render_param_value);
            if let Some(rendered) = rendered {
                result.push((decl_name.clone(), rendered));
            }
        }

        // Append any instance-supplied names not in the declared list.
        for (name, value) in supplied_map.iter() {
            if !declared.iter().any(|(d, _)| d == name) {
                if let Some(v) = value {
                    result.push((name.clone(), render_param_value(v)));
                }
            }
        }

        result
    }

    fn convert_package(&mut self, package: PackageId) -> Option<verilog::VerilogFile> {
        let parsing = self.db.get_parsing(package);
        let package_name = self.db.get_packages().name(package).to_str_lossy().into_owned();
        let symboltable = self.db.get_symboltable();
        let mut modules = vec![];
        for item_ast in parsing.root().children() {
            let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
                continue;
            };
            if item_generics(&item_ast).is_some() {
                // A generic mod def: emit one module per discovered
                // instantiation of it (in discovery order), each with
                // its own arguments in scope.  An uninstantiated
                // generic def emits nothing.
                let moddef_symbol = symboltable
                    .resolve_item_in_package(parsing.string(moddef.name), package)
                    .unwrap();
                let keys: Vec<(SymbolId, Vec<Type>)> = self
                    .mono_modules
                    .keys()
                    .filter(|(def, _)| *def == moddef_symbol.id())
                    .cloned()
                    .collect();
                for key in keys {
                    let Some(mono) = self.mono_modules.get(&key) else {
                        continue;
                    };
                    self.mono_env = mono.args.clone();
                    let item_ast = item_ast.clone();
                    modules.push(self.convert_moddef(package, item_ast, Some(mono)));
                }
                self.mono_env = vec![];
            } else {
                self.mono_env = vec![];
                modules.push(self.convert_moddef(package, item_ast, None));
            }
        }
        if modules.is_empty() {
            return None;
        }
        Some(verilog::VerilogFile {
            name: format!("{package_name}.sv"),
            modules,
        })
    }

    /// Convert one mod def to a Verilog module.  `mono` names the
    /// mono-module instantiation being converted (`None` for a plain
    /// non-generic def); when present, every type fetched from the
    /// (generic) analysis is substituted with `self.mono_env` — the
    /// instance's type arguments — before use.
    fn convert_moddef(
        &self,
        package: PackageId,
        item_ast: AstNode,
        mono: Option<&MonoModule>,
    ) -> verilog::Module {
        let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
            panic!("expected ModDef");
        };
        let parsing = self.db.get_parsing(package);
        let moddef_name = parsing.string(moddef.name.clone()).to_str_lossy().into_owned();
        let module_path = qualified_module_name(&self.db.get_packages().name(package).to_str_lossy(), &moddef_name);
        let (module_name, module_origin) = match mono {
            Some(mono) => (mono.name.clone(), Some(mono.origin.clone())),
            None => (self.emitted_module_names[&module_path].clone(), None),
        };

        let symboltable = self.db.get_symboltable();
        let moddef_symbol = symboltable
            .resolve_item_in_package(parsing.string(moddef.name), package)
            .unwrap();
        let component_analysis = self.db.get_component_analysis(moddef_symbol.id());

        let mut scheduler = ExprScheduler::new();
        let mut elements: Vec<verilog::Element> = vec![];
        // reg name -> AstNodeId of its explicit `on clock` expression (if present)
        let mut sequential_regs: IndexMap<String, AstNodeId> = IndexMap::new();

        // Gather ports using get_ports_of, substituting the mono
        // instantiation's type arguments when converting a generic def.
        let ports_of = self.db.get_ports_of(moddef_symbol.id());
        let mut ports: Vec<verilog::Port> = vec![];
        for port in ports_of.iter() {
            use bstr::ByteSlice;
            let name = port.path.to_str_lossy().into_owned();
            let width = port.typ.clone().map(|t| type_width(&self.mono_substitute(t), self.db)).unwrap_or(0);
            let port_name = valid_verilog_name(&name);
            // OutgoingReg is an output reg port in Verilog.
            // All other ports (Incoming, Outgoing, OutgoingWire) are wire ports.
            let is_outgoing_reg = component_analysis.resolve(port.path.as_bstr())
                .and_then(|c| c.kind())
                .map_or(false, |k| k == ComponentKind::OutgoingReg);
            let port_kind = if is_outgoing_reg {
                verilog::PortKind::Reg
            } else {
                verilog::PortKind::Wire
            };
            ports.push(verilog::Port {
                name: port_name,
                kind: port_kind,
                dir: port.dir,
                width,
            });
        }

        // First pass: wires, regs, instances, on
        for stmt in item_ast.children() {
            match stmt.payload() {
                AstNodePayload::Component(component) => {
                    let name = parsing.string(component.name.clone()).to_str_lossy().into_owned();
                    let typ = component_analysis.type_of(parsing.string(component.name)).unwrap();
                    let typ = self.mono_substitute(typ);
                    let width = type_width(&typ, self.db);
                    match component.kind {
                        ComponentKind::Incoming | ComponentKind::Outgoing
                        | ComponentKind::OutgoingWire => {
                            // Ports are now handled by get_ports_of above.
                        }
                        ComponentKind::OutgoingReg => {
                            // Port declaration is handled by get_ports_of above (output reg).
                            // Check for an explicit `on clock` clause.
                            let children = stmt.children();
                            let clock_child = children.iter().skip(2).find(|c| !matches!(c.payload(), AstNodePayload::It));
                            if let Some(clock_node) = clock_child {
                                sequential_regs.insert(name.clone(), clock_node.id());
                            }
                            // No internal reg element — the port declaration suffices.
                        }
                        ComponentKind::Wire => {
                            let children = stmt.children();
                            let clock_child = children.iter().skip(2).find(|c| !matches!(c.payload(), AstNodePayload::It));
                            if let Some(clock_node) = clock_child {
                                // Wire with explicit `on clock` — treat exactly like a Reg.
                                sequential_regs.insert(name.clone(), clock_node.id());
                                elements.push(verilog::Element::Reg(verilog::Reg {
                                    name: valid_verilog_name(&name),
                                    width,
                                    expr: None,
                                }));
                            } else {
                                elements.push(verilog::Element::Wire(verilog::Wire {
                                    name: valid_verilog_name(&name),
                                    width,
                                    expr: None,
                                }));
                            }
                        }
                        ComponentKind::Reg => {
                            // Regs with `on clock` have a non-ItBlock child after the type.
                            let children = stmt.children();
                            let clock_child = children.iter().skip(2).find(|c| !matches!(c.payload(), AstNodePayload::It));
                            if let Some(clock_node) = clock_child {
                                sequential_regs.insert(name.clone(), clock_node.id());
                            }
                            elements.push(verilog::Element::Reg(verilog::Reg {
                                name: valid_verilog_name(&name),
                                width,
                                expr: None,
                            }));
                        }
                        ComponentKind::Net => {
                            elements.push(verilog::Element::Wire(verilog::Wire {
                                name: valid_verilog_name(&name),
                                width,
                                expr: None,
                            }));
                        }
                        ComponentKind::Bus => {}
                    }
                }
                AstNodePayload::Submodule(module) => {
                    let raw_name = parsing.string(module.name).to_str_lossy().into_owned();
                    // Resolve the target and this instance's type
                    // arguments.  A generic target refers to its
                    // discovered mono module, with the written arguments
                    // composed with the current mono instantiation's
                    // arguments; a non-generic target uses the def-level
                    // registration.
                    let (ports_info, sub_name, declared_params) =
                        match instantiation_target(self.db, &stmt) {
                            Some((target_symbol, written_args))
                                if target_is_generic(self.db, &target_symbol) =>
                            {
                                let mono_args: Vec<Type> = written_args
                                    .iter()
                                    .map(|arg| self.mono_substitute(arg.clone()))
                                    .collect();
                                let mono =
                                    self.mono_modules.get(&(target_symbol.id(), mono_args))
                                        .expect("mono module of an instantiation must have been discovered");
                                (
                                    mono.ports.clone(),
                                    mono.name.clone(),
                                    Some(mono.declared_params.as_slice()),
                                )
                            }
                            _ => {
                                // The Ofness child may sit at different indices depending on
                                // whether an Annotations child is present, so locate it by
                                // payload rather than a hard-coded index.
                                let ofness_node = stmt.children().iter()
                                    .find(|c| matches!(c.payload(), AstNodePayload::Ofness(_)))
                                    .cloned()
                                    .expect("Submodule node must have an Ofness child");
                                let mod_path = render_ofness_path(ofness_node, self.db, package, &symboltable);
                                let ports_info = self.module_ports.get(&mod_path).cloned().unwrap_or_default();
                                let sub_name = self.emitted_module_names.get(&mod_path)
                                    .cloned()
                                    .unwrap_or_else(|| valid_verilog_name(&mod_path));
                                let declared_params = self.module_declared_params.get(&mod_path).map(|v| v.as_slice());
                                (ports_info, sub_name, declared_params)
                            }
                        };
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
                        parameters: self.build_submodule_parameters(&stmt, declared_params),
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
            let Some(typ) = component.typ().map(|t| self.mono_substitute(t)) else { continue };
            let net = matches!(
                component.kind(),
                Some(ComponentKind::Bus) | Some(ComponentKind::Net)
            );

            for driver in comp_drivers {
                // Bidirectional drivers are emitted by the dedicated pass below.
                if matches!(driver, Driver::Bidirectional(_)) {
                    continue;
                }

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
                        let reg_name = valid_verilog_name(&path);
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
                        Driver::When(driver_when) if driver_when.driver_type == DriverType::Latched => {
                            self.convert_latched_driver_when_to_stmts(package, driver_when, &typ, &path, &mut scheduler)
                        }
                        Driver::Match(driver_match) if driver_match.driver_type == DriverType::Latched => {
                            self.convert_driver_match_to_stmts(package, driver_match, &typ, &path, &mut scheduler)
                        }
                        _ => {
                            let expr = self.convert_driver_to_expr(package, driver, &typ, net, &mut scheduler);
                            vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
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
                    let strength = self.driver_strength(driver);
                    let expr = self.convert_driver_to_expr(package, driver, &typ, net, &mut scheduler);
                    combinational.push(verilog::Element::Assign(verilog::Assign {
                        name: valid_verilog_name(&path),
                        expr,
                        strength,
                    }));
                }
            }
        }

        // Handle bidirectional drivers (socket connections).
        // Recurse into Submodule It-blocks to find nested :=: statements,
        // resolving `it.*` paths to the actual submodule instance name.
        collect_bidirectional_assigns(
            item_ast.children(),
            self.db,
            &component_analysis,
            None,
            &mut elements,
        );

        // Explicit `initial` values, keyed by Verilog name. These are built from
        // DriverAnalysis (not from `Element::Reg`) so that `outgoing reg`s, which
        // are emitted as `output reg` ports with no `Element::Reg`, are included.
        let mut initial_exprs: IndexMap<String, verilog::Expr> = IndexMap::new();
        for (comp_id, locs) in driver_analysis.initials() {
            let Some(component) = component_analysis.component(*comp_id) else { continue };
            let Some(typ) = component.typ().map(|t| self.mono_substitute(t)) else { continue };
            let Some(loc) = locs.first() else { continue };
            let expr_parsing = self.db.get_parsing(loc.package());
            let expr_node = expr_parsing.ast_node(loc.ast_node_id());
            let expr = self.convert_expr(package, expr_node, &typ, self.db, &mut scheduler);
            let name = valid_verilog_name(&component.path().to_str_lossy());
            initial_exprs.insert(name, expr);
        }

        elements.extend(combinational);
        elements.extend(sequential);
        elements.extend(hole_displays);
        // Collect any temp regs/always blocks generated by match expressions
        elements.extend(scheduler.drain());

        // Every reg gets an initial value: its explicit `initial` if present,
        // otherwise 0.
        let mut init_stmts: Vec<verilog::Stmt> = elements.iter()
            .filter_map(|e| {
                let verilog::Element::Reg(reg) = e else { return None };
                let expr = initial_exprs.shift_remove(&reg.name).unwrap_or_else(|| {
                    verilog::Expr::WordLit(verilog::expr::WordLit {
                        value: 0,
                        width: reg.width,
                        radix: Radix::Dec,
                    })
                });
                Some(verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: reg.name.clone(),
                    expr,
                }))
            })
            .collect();
        // Remaining explicit initials (e.g. `outgoing reg`s).
        for (name, expr) in initial_exprs {
            init_stmts.push(verilog::Stmt::AssignBlocking(verilog::AssignBlocking { name, expr }));
        }
        if !init_stmts.is_empty() {
            elements.push(verilog::Element::Initial(verilog::Initial { stmts: init_stmts }));
        }

        verilog::Module { name: module_name, is_ext: mono.is_none() && moddef.is_ext, ports, elements, origin: module_origin }
    }

    /// Recursively collects the region strings of all `?` (hole) nodes in a driver tree.
    fn collect_driver_holes(&self, driver: &Driver) -> Vec<String> {
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let expr_node = parsing.ast_node(loc.ast_node_id());
                collect_ast_holes(expr_node, self.db)
            }
            Driver::Bidirectional(_) => vec![],
            Driver::Absent => vec![],
            Driver::When(driver_when) => {
                let mut holes = vec![];
                for (_, sub_driver) in &driver_when.clauses {
                    holes.extend(self.collect_driver_holes(sub_driver));
                }
                if let Some(else_driver) = &driver_when.else_clause {
                    holes.extend(self.collect_driver_holes(else_driver));
                }
                holes
            }
            Driver::Match(driver_match) => {
                let mut holes = vec![];
                for (_, sub_driver) in &driver_match.arms {
                    holes.extend(self.collect_driver_holes(sub_driver));
                }
                if let Some(else_driver) = &driver_match.else_clause {
                    holes.extend(self.collect_driver_holes(else_driver));
                }
                holes
            }
        }
    }

    /// Convert a `Driver` tree to a single `verilog::Expr` (for combinational / Continuous drivers).
    /// `Driver::When` becomes a right-associative ternary chain (`cond ? then : else`).
    fn convert_driver_to_expr(
        &self,
        package: PackageId,
        driver: &Driver,
        typ: &Type,
        net: bool,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let expr_node = parsing.ast_node(loc.ast_node_id());
                self.convert_expr(package, expr_node, typ, self.db, scheduler)
            }
            Driver::Bidirectional(_) => unreachable!("Bidirectional drivers are emitted directly, not via convert_driver_to_expr"),
            Driver::Absent => self.driver_default_expr(typ, net),
            Driver::When(driver_when) => self.convert_driver_when_to_expr(package, driver_when, typ, net, scheduler),
            Driver::Match(driver_match) => self.convert_driver_match_to_expr(package, driver_match, typ, net, scheduler),
        }
    }

    fn driver_default_expr(&self, typ: &Type, net: bool) -> verilog::Expr {
        let width = type_width(typ, self.db);
        if net {
            verilog::Expr::ZLit(verilog::expr::ZLit { width })
        } else {
            verilog::Expr::XLit(verilog::expr::XLit { width })
        }
    }

    fn driver_strength(&self, driver: &Driver) -> crate::common::Strength {
        use crate::common::Strength;
        match driver {
            Driver::Expr(_, loc) => {
                let parsing = self.db.get_parsing(loc.package());
                let node = parsing.ast_node(loc.ast_node_id());
                node.parent()
                    .and_then(|parent| parent.driver_strength())
                    .unwrap_or(Strength::Strong)
            }
            Driver::Bidirectional(_) => Strength::Strong,
            Driver::Absent => Strength::Weak,
            Driver::When(driver_when) => {
                let mut strength = Strength::Weak;
                for (_, sub_driver) in &driver_when.clauses {
                    if self.driver_strength(sub_driver) == Strength::Strong {
                        strength = Strength::Strong;
                    }
                }
                if let Some(else_driver) = &driver_when.else_clause {
                    if self.driver_strength(else_driver) == Strength::Strong {
                        strength = Strength::Strong;
                    }
                }
                strength
            }
            Driver::Match(driver_match) => {
                let mut strength = Strength::Weak;
                for (_, sub_driver) in &driver_match.arms {
                    if self.driver_strength(sub_driver) == Strength::Strong {
                        strength = Strength::Strong;
                    }
                }
                if let Some(else_driver) = &driver_match.else_clause {
                    if self.driver_strength(else_driver) == Strength::Strong {
                        strength = Strength::Strong;
                    }
                }
                strength
            }
        }
    }

    fn convert_driver_when_to_expr(
        &self,
        package: PackageId,
        driver_when: &DriverWhen,
        typ: &Type,
        net: bool,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let else_expr = match &driver_when.else_clause {
            Some(else_driver) => self.convert_driver_to_expr(package, else_driver, typ, net, scheduler),
            None => self.driver_default_expr(typ, net),
        };

        driver_when.clauses.iter().rev().fold(else_expr, |acc, (cond_loc, sub_driver)| {
            let cond_parsing = self.db.get_parsing(cond_loc.package());
            let cond_node = cond_parsing.ast_node(cond_loc.ast_node_id());
            let cond_type = self.node_type(package, &cond_node).unwrap_or(Type::Bit);
            let cond_expr = self.convert_expr(package, cond_node, &cond_type, self.db, scheduler);
            let then_expr = self.convert_driver_to_expr(package, sub_driver, typ, net, scheduler);
            verilog::Expr::If(verilog::expr::If {
                cond: Box::new(cond_expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(acc),
                width: type_width(typ, self.db),
            })
        })
    }

    /// Convert a `Latched Driver::When` tree into a chain of procedural `Stmt::If` blocks
    /// with non-blocking assignments.  When there is no else clause, no else branch is emitted.
    fn convert_latched_driver_when_to_stmts(
        &self,
        package: PackageId,
        driver_when: &DriverWhen,
        typ: &Type,
        path: &str,
        scheduler: &mut ExprScheduler,
    ) -> Vec<verilog::Stmt> {
        // Else stmts (empty when there is no else clause).
        let else_stmts = match &driver_when.else_clause {
            Some(else_driver) => self.convert_latched_driver_to_stmts(package, else_driver, typ, path, scheduler),
            None => vec![],
        };

        // Build right-associative Stmt::If chain from last clause to first.
        driver_when.clauses.iter().rev().fold(else_stmts, |acc_else, (cond_loc, sub_driver)| {
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

    /// Convert a `Driver::Match` into a `casez` statement that assigns each arm's result
    /// to `path`.  Used in both combinational (via temp-reg) and sequential contexts.
    fn convert_driver_match_to_stmts(
        &self,
        package: PackageId,
        driver_match: &DriverMatch,
        typ: &Type,
        path: &str,
        scheduler: &mut ExprScheduler,
    ) -> Vec<verilog::Stmt> {
        let db = self.db;
        let subject_parsing = db.get_parsing(driver_match.subject.package());
        let subject_node = subject_parsing.ast_node(driver_match.subject.ast_node_id());
        let subject_typ = self.node_type(package, &subject_node).unwrap();
        let subject_expr = self.convert_expr(package, subject_node, &subject_typ, db, scheduler);
        let result_width = type_width(typ, db);
        let assigned_name = valid_verilog_name(path);
        let mut case_items: Vec<verilog::CaseItem> = vec![];
        let mut has_else = false;

        for (pat_loc, sub_driver) in &driver_match.arms {
            let pat_parsing = db.get_parsing(pat_loc.package());
            let pat_node = pat_parsing.ast_node(pat_loc.ast_node_id());
            let mut bound_keys: Vec<String> = vec![];
            let pattern = self.build_case_pattern(&pat_node, &subject_expr, &subject_typ, scheduler, &mut bound_keys);
            let arm_stmts = self.convert_latched_driver_to_stmts(package, sub_driver, typ, path, scheduler);
            for key in &bound_keys { scheduler.subst.shift_remove(key); }
            case_items.push(verilog::CaseItem { pattern, stmts: arm_stmts });
        }

        if let Some(else_driver) = &driver_match.else_clause {
            let else_stmts = self.convert_latched_driver_to_stmts(package, else_driver, typ, path, scheduler);
            case_items.push(verilog::CaseItem { pattern: verilog::CasePattern::Default, stmts: else_stmts });
            has_else = true;
        }

        // In the latched case, an absent else clause emits an empty default arm so that the
        // register holds its current value (no assignment = hold in a clocked always block).
        if !has_else {
            let _ = (result_width, assigned_name);
            case_items.push(verilog::CaseItem {
                pattern: verilog::CasePattern::Default,
                stmts: vec![],
            });
        }

        vec![verilog::Stmt::CaseZ(verilog::CaseZ { subject: subject_expr, items: case_items })]
    }

    /// Convert a `Driver::Match` into a Verilog expression using a temp-reg + always @(*) casez.
    fn convert_driver_match_to_expr(
        &self,
        package: PackageId,
        driver_match: &DriverMatch,
        typ: &Type,
        net: bool,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let db = self.db;
        let subject_parsing = db.get_parsing(driver_match.subject.package());
        let subject_node = subject_parsing.ast_node(driver_match.subject.ast_node_id());
        let subject_typ = self.node_type(package, &subject_node).unwrap();
        let subject_expr = self.convert_expr(package, subject_node, &subject_typ, db, scheduler);
        let temp_name = scheduler.fresh_temp_name("match");
        let result_width = type_width(typ, db);
        let mut case_items: Vec<verilog::CaseItem> = vec![];
        let mut has_else = false;

        for (pat_loc, sub_driver) in &driver_match.arms {
            let pat_parsing = db.get_parsing(pat_loc.package());
            let pat_node = pat_parsing.ast_node(pat_loc.ast_node_id());
            let mut bound_keys: Vec<String> = vec![];
            let pattern = self.build_case_pattern(&pat_node, &subject_expr, &subject_typ, scheduler, &mut bound_keys);
            let arm_expr = self.convert_driver_to_expr(package, sub_driver, typ, net, scheduler);
            for key in &bound_keys { scheduler.subst.shift_remove(key); }
            case_items.push(verilog::CaseItem {
                pattern,
                stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: temp_name.clone(),
                    expr: arm_expr,
                })],
            });
        }

        if let Some(else_driver) = &driver_match.else_clause {
            let else_expr = self.convert_driver_to_expr(package, else_driver, typ, net, scheduler);
            case_items.push(verilog::CaseItem {
                pattern: verilog::CasePattern::Default,
                stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: temp_name.clone(),
                    expr: else_expr,
                })],
            });
            has_else = true;
        }

        if !has_else {
            case_items.push(verilog::CaseItem {
                pattern: verilog::CasePattern::Default,
                stmts: vec![verilog::Stmt::AssignBlocking(verilog::AssignBlocking {
                    name: temp_name.clone(),
                    expr: self.driver_default_expr(typ, net),
                })],
            });
        }

        scheduler.push(verilog::Element::Reg(verilog::Reg { name: temp_name.clone(), width: result_width, expr: None }));
        let casez_stmt = verilog::Stmt::CaseZ(verilog::CaseZ { subject: subject_expr, items: case_items });
        if scheduler.in_sequential {
            scheduler.sequential_stmts.push(casez_stmt);
        } else {
            scheduler.push(verilog::Element::Always(verilog::Always { clock: None, stmts: vec![casez_stmt] }));
        }
        verilog::Expr::Reference(verilog::expr::Reference { name: temp_name })
    }

    fn convert_latched_driver_to_stmts(
        &self,
        package: PackageId,
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
                if matches!(expr_node.payload(), AstNodePayload::Match) {
                    return self.convert_match_expr_to_nonblocking_stmts(
                        package, expr_node, typ, path, scheduler,
                    );
                }
                let expr = self.convert_expr(package, expr_node, typ, self.db, scheduler);
                vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
                    name: valid_verilog_name(path),
                    expr,
                })]
            }
            Driver::Bidirectional(_) => unreachable!("Bidirectional drivers are emitted directly, not via convert_latched_driver_to_stmts"),
            // No assignment here: inside a clocked always block, emitting
            // nothing means the register holds its value.
            Driver::Absent => vec![],
            Driver::When(driver_when) => {
                self.convert_latched_driver_when_to_stmts(package, driver_when, typ, path, scheduler)
            }
            Driver::Match(driver_match) => {
                self.convert_driver_match_to_stmts(package, driver_match, typ, path, scheduler)
            }
        }
    }

    /// Converts an `ExprMatch` node directly into a `casez` statement that non-blockingly
    /// assigns each arm's result to `target_name`.  This avoids creating a temporary reg +
    /// `always @(*)` block, which would be incorrect inside a sequential
    /// `always @(posedge clock)` context.
    fn convert_match_expr_to_nonblocking_stmts(
        &self,
        package: PackageId,
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
        let assigned_name = valid_verilog_name(target_name);
        let mut case_items: Vec<verilog::CaseItem> = vec![];
        let mut has_else = false;
        let mut idx = 1;
        while idx < children.len() {
            let (pat, body) = if children[idx].is_pat() {
                let pat = children[idx].clone();
                let body = children[idx + 1].clone();
                idx += 2;
                (Some(pat), body)
            } else {
                let body = children[idx].clone();
                idx += 1;
                (None, body)
            };

            let mut bound_keys: Vec<String> = vec![];
            let pattern = match pat.as_ref().map(|p| p.payload()) {
                None => { has_else = true; verilog::CasePattern::Default },
                Some(AstNodePayload::PatEnumerant(_)) => {
                    let pat = pat.unwrap();
                    self.build_enumerant_case_pattern(&pat, &subject_typ)
                }
                Some(AstNodePayload::PatCtor(_)) => {
                    let pat = pat.unwrap();
                    self.build_ctor_case_pattern(&pat, &subject_expr, &subject_typ, scheduler, &mut bound_keys)
                }
                Some(AstNodePayload::PatWordLit(pat_word_lit)) => {
                    let pat = pat.unwrap();
                    let literal = pat.parsing.string(pat_word_lit.literal).to_str_lossy().into_owned();
                    let (value, _lit_width) = parse_word_literal(&literal);
                    let width = type_width(&subject_typ, db);
                    let pat_str = format!("{:0>width$b}", value, width = width as usize);
                    verilog::CasePattern::PatternLit(verilog::PatternLit { width, radix: Radix::Bin, pattern: pat_str })
                }
                Some(AstNodePayload::PatBitLit(pat_bit_lit)) => {
                    let pat_str = if pat_bit_lit.literal { "1".to_string() } else { "0".to_string() };
                    verilog::CasePattern::PatternLit(verilog::PatternLit { width: 1, radix: Radix::Bin, pattern: pat_str })
                }
                _ => unreachable!("expected pattern node"),
            };
            let body_expr = self.convert_expr(package, body, typ, db, scheduler);
            for key in &bound_keys { scheduler.subst.shift_remove(key); }
            case_items.push(verilog::CaseItem {
                pattern,
                stmts: vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
                    name: assigned_name.clone(),
                    expr: body_expr,
                })],
            });
        }
        // Default arm: drive X — only when the match has no `else` arm of its own.
        if !has_else {
            case_items.push(verilog::CaseItem {
                pattern: verilog::CasePattern::Default,
                stmts: vec![verilog::Stmt::AssignNonBlocking(verilog::AssignNonBlocking {
                    name: assigned_name.clone(),
                    expr: verilog::Expr::XLit(verilog::expr::XLit { width: result_width }),
                })],
            });
        }
        vec![verilog::Stmt::CaseZ(verilog::CaseZ { subject: subject_expr, items: case_items })]
    }

    /// Convert a pattern AST node into a `verilog::CasePattern`, registering any bound
    /// variable substitutions into `scheduler.subst` and recording their keys in `bound_keys`.
    fn build_case_pattern(
        &self,
        pat_node: &AstNode,
        subject_expr: &verilog::Expr,
        subject_typ: &Type,
        scheduler: &mut ExprScheduler,
        bound_keys: &mut Vec<String>,
    ) -> verilog::CasePattern {
        match pat_node.payload() {
            AstNodePayload::PatEnumerant(_) => {
                self.build_enumerant_case_pattern(pat_node, subject_typ)
            }
            AstNodePayload::PatCtor(_) => {
                self.build_ctor_case_pattern(pat_node, subject_expr, subject_typ, scheduler, bound_keys)
            }
            AstNodePayload::PatWordLit(pat_word_lit) => {
                let literal = pat_node.parsing.string(pat_word_lit.literal).to_str_lossy().into_owned();
                let (value, _lit_width) = parse_word_literal(&literal);
                let width = type_width(subject_typ, self.db);
                let pat_str = format!("{:0>width$b}", value, width = width as usize);
                verilog::CasePattern::PatternLit(verilog::PatternLit { width, radix: Radix::Bin, pattern: pat_str })
            }
            AstNodePayload::PatBitLit(pat_bit_lit) => {
                let pat_str = if pat_bit_lit.literal { "1".to_string() } else { "0".to_string() };
                verilog::CasePattern::PatternLit(verilog::PatternLit { width: 1, radix: Radix::Bin, pattern: pat_str })
            }
            AstNodePayload::PatIdent(pat_ident) => {
                // Ident patterns match everything and bind the entire subject to the variable.
                let var_name = pat_node.parsing.string(pat_ident.name).to_str_lossy().into_owned();
                scheduler.subst.insert(var_name.clone(), subject_expr.clone());
                bound_keys.push(var_name);
                let width = type_width(subject_typ, self.db);
                verilog::CasePattern::PatternLit(verilog::PatternLit {
                    width,
                    radix: Radix::Bin,
                    pattern: "?".repeat(width as usize),
                })
            }
            AstNodePayload::PatDontcare => {
                // Dontcare patterns match everything without binding.
                let width = type_width(subject_typ, self.db);
                verilog::CasePattern::PatternLit(verilog::PatternLit {
                    width,
                    radix: Radix::Bin,
                    pattern: "?".repeat(width as usize),
                })
            }
            _ => unreachable!("expected pattern node"),
        }
    }

    /// Build a `casez` pattern for a `@Name(...)` ctor (or payload
    /// enumerant) pattern, registering bound-variable bit-slices into
    /// `scheduler.subst`.
    ///
    /// Layout (LSB→MSB): tag | argN | ... | arg0 | padding.
    fn build_ctor_case_pattern(
        &self,
        pat_node: &AstNode,
        subject_expr: &verilog::Expr,
        subject_typ: &Type,
        scheduler: &mut ExprScheduler,
        bound_keys: &mut Vec<String>,
    ) -> verilog::CasePattern {
        let Type::Usual(typedef_sym, args) = subject_typ else {
            unreachable!("ctor pattern subject must be a typedef type, got {subject_typ:?}")
        };
        let symboltable = self.db.get_symboltable();
        let AstNodePayload::PatCtor(pat_ctor) = pat_node.payload() else {
            unreachable!("expected PatCtor node")
        };
        let ctor_name = pat_node.parsing().string(pat_ctor.name);
        let ctor_slot = symboltable
            .slot(*typedef_sym, ctor_name)
            .unwrap_or_else(|| panic!("unknown ctor name {ctor_name} in pattern"));
        let layout = tagged_layout(self.db, *typedef_sym, args);
        let (tag_value, arg_widths) = layout.slot(ctor_slot.id());

        // Compute bit ranges: tag at [tag_width-1:0], then args from last to first.
        let mut bit_ranges = vec![(0u16, 0u16); arg_widths.len()];
        let mut current_lo = layout.tag_width;
        for j in (0..arg_widths.len()).rev() {
            let w = arg_widths[j];
            bit_ranges[j] = (current_lo, current_lo + w.saturating_sub(1));
            current_lo += w;
        }
        // Register subst for each bound variable as a direct bit-slice.
        for (j, bound_var) in pat_node.children().iter().enumerate() {
            let Some(var_path) = bound_var.path() else { continue };
            let var_name = bound_var.parsing.string(var_path)
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
        let tag_bin = format!("{:0>width$b}", tag_value, width = layout.tag_width as usize);
        let question_marks = "?".repeat(layout.max_payload as usize);
        verilog::CasePattern::PatternLit(verilog::PatternLit {
            width: layout.tag_width + layout.max_payload,
            radix: Radix::Bin,
            pattern: format!("{}{}", question_marks, tag_bin),
        })
    }

    /// Build a `casez` pattern for a `#Name` enumerant pattern.
    ///
    /// For enums whose enumerants carry payloads, the payload bits are
    /// matched as `?`s above the tag, mirroring how the ctor value is
    /// packed.
    fn build_enumerant_case_pattern(
        &self,
        pat_node: &AstNode,
        subject_typ: &Type,
    ) -> verilog::CasePattern {
        let Type::Usual(typedef_sym, args) = subject_typ else {
            unreachable!("enumerant pattern subject must be an enum type, got {subject_typ:?}")
        };
        let symboltable = self.db.get_symboltable();
        let AstNodePayload::PatEnumerant(pat_enumerant) = pat_node.payload() else {
            unreachable!("expected PatEnumerant node")
        };
        let enumerant_name = pat_node.parsing().string(pat_enumerant.name);
        let enumerant_sym = symboltable
            .slot(*typedef_sym, enumerant_name)
            .unwrap_or_else(|| panic!("unknown enumerant name {enumerant_name} in pattern"));
        let layout = tagged_layout(self.db, *typedef_sym, args);
        let (value, _) = layout.slot(enumerant_sym.id());

        if layout.max_payload > 0 {
            let tag_bin = format!("{:0>width$b}", value, width = layout.tag_width as usize);
            let question_marks = "?".repeat(layout.max_payload as usize);
            verilog::CasePattern::PatternLit(verilog::PatternLit {
                width: layout.tag_width + layout.max_payload,
                radix: Radix::Bin,
                pattern: format!("{}{}", question_marks, tag_bin),
            })
        } else {
            let width = type_width(subject_typ, self.db);
            let pat_str = format!("{:0>width$b}", value, width = width as usize);
            verilog::CasePattern::PatternLit(verilog::PatternLit { width, radix: Radix::Bin, pattern: pat_str })
        }
    }

    /// Convert a constructor expression (`@Name(args)`) of a union ctor
    /// or an enum payload enumerant into its packed Verilog value.
    ///
    /// Layout (LSB→MSB): tag | argN | ... | arg0 | padding.
    fn convert_ctor_expr(
        &self,
        package: PackageId,
        node: AstNode,
        typ: &Type,
        typing: &Arc<Typing>,
        db: &Db,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let Type::Usual(typedef_sym, args) = typ else {
            unreachable!("ctor expression must have a typedef type, got {typ:?}")
        };
        let ctor_symbol_id = typing.tag(node.location()).symbol_id().unwrap();
        let layout = tagged_layout(db, *typedef_sym, args);
        let (tag_value, arg_widths) = layout.slot(ctor_symbol_id);

        let tag_expr = verilog::Expr::WordLit(verilog::expr::WordLit {
            value: tag_value,
            width: layout.tag_width,
            radix: Radix::Bin,
        });

        if layout.max_payload == 0 {
            // No payload at all: just return the tag word.
            return tag_expr;
        }

        // Build the full value: {padding, arg0, ..., argN, tag}
        // This ensures the tag is always at consistent LSB bit positions
        // regardless of which ctor variant is used, enabling casez matching.
        let this_sig = db.get_ctor_signature(ctor_symbol_id);
        let arguments = node.children();
        let this_payload_width: Width = arg_widths.iter().sum();
        let padding_width = layout.max_payload - this_payload_width;

        let mut exprs = vec![];
        if padding_width > 0 {
            exprs.push(padding_expr(padding_width));
        }
        // Arguments in order (first arg at higher bits than last arg),
        // substituting the def's generic parameters with the type args.
        for (arg, (_param_name, param_typ)) in arguments.iter().zip(this_sig.parameters.iter()) {
            let param_typ = crate::types::substitute::substitute(param_typ, args);
            exprs.push(self.convert_expr(package, arg.clone(), &param_typ, db, scheduler));
        }
        // Tag at LSB (least significant, rightmost in concat)
        exprs.push(tag_expr);

        let total_width = layout.tag_width + layout.max_payload;
        verilog::Expr::Concat(verilog::expr::Concat { exprs, width: total_width })
    }

    /// Convert an enumerant expression (`#Name`) into its packed
    /// Verilog value: for enums whose enumerants carry payloads, the
    /// payload bits are filled with padding (mirroring `@Name(...)`);
    /// otherwise the value is the bare tag word.
    fn convert_enumerant_expr(
        &self,
        node: AstNode,
        typing: &Arc<Typing>,
        typ: &Type,
    ) -> verilog::Expr {
        let Type::Usual(typedef_sym, args) = typ else {
            unreachable!("{:?} type is {:?}", node.region(), typ)
        };
        let enumerant_symbol_id = typing.tag(node.location()).symbol_id().unwrap();
        let layout = tagged_layout(self.db, *typedef_sym, args);
        let (value, _) = layout.slot(enumerant_symbol_id);

        if layout.max_payload > 0 {
            let mut exprs = vec![padding_expr(layout.max_payload)];
            exprs.push(verilog::Expr::WordLit(verilog::expr::WordLit {
                value,
                width: layout.tag_width,
                radix: Radix::Bin,
            }));
            let total_width = layout.tag_width + layout.max_payload;
            verilog::Expr::Concat(verilog::expr::Concat { exprs, width: total_width })
        } else {
            let width: Width = type_width(typ, self.db);
            verilog::Expr::WordLit(verilog::expr::WordLit { value, width, radix: Radix::Dec })
        }
    }

    fn convert_expr(
        &self,
        package: PackageId,
        node: AstNode,
        typ: &Type,
        db: &Db,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        let typing = self.typing_for(&node);
        match node.payload() {
            AstNodePayload::ExprTap => {
                let path_node = node.child(0);
                let path_typing = self.typing_for(&path_node);
                let referent = path_typing.tag(path_node.location());
                match referent {
                    Tag::ReferentResolution(Referent::Component(component_id)) => {
                        let component_analysis = self.db.get_component_analysis(component_id.item_id());
                        let component = component_analysis
                            .component(component_id)
                            .expect("tap referent component");
                        let name = component.path().to_str_lossy().into_owned();
                        verilog::Expr::Reference(verilog::expr::Reference {
                            name: valid_verilog_name(&name),
                        })
                    }
                    _ => unreachable!("tap operand must resolve to a component"),
                }
            }
            AstNodePayload::ExprReference => {
                let path = node.parsing.string(node.path().unwrap()).to_str_lossy().into_owned();
                let mut path_str = path.clone();
                if path_str.starts_with("it.") || path_str == "it" {
                    let mut current_id = node.id();
                    while let Some(parent_id) = node.parsing.ast_node(current_id).parent {
                        let parent = node.parsing.ast_node(parent_id);
                        let mut resolved_name = None;
                        if let AstNodePayload::Submodule(module) = parent.payload() {
                            resolved_name = Some(node.parsing.string(module.name).to_str_lossy().into_owned());
                        } else if let AstNodePayload::Component(component) = parent.payload() {
                            resolved_name = Some(node.parsing.string(component.name).to_str_lossy().into_owned());
                        } else if let AstNodePayload::Socket(socket) = parent.payload() {
                            resolved_name = Some(node.parsing.string(socket.name).to_str_lossy().into_owned());
                        }

                        if let Some(name) = resolved_name {
                            if path_str.starts_with("it.") {
                                path_str = format!("{}.{}", name, &path_str[3..]);
                            } else {
                                path_str = name;
                            }
                            break;
                        }
                        current_id = parent_id;
                    }
                }
                // Check if this reference is a pattern-bound variable substitution.
                if let Some(subst_expr) = scheduler.subst.get(&path_str) {
                    return subst_expr.clone();
                }
                verilog::Expr::Reference(verilog::expr::Reference {
                    name: valid_verilog_name(&path_str),
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
            AstNodePayload::ExprBinOp(expr_bin_op) => {
                let lhs = node.child(0);
                let rhs = node.child(1);
                let lhs_type = self.node_type(package, &lhs).unwrap();
                let rhs_type = self.node_type(package, &rhs).unwrap();
                verilog::Expr::BinOp(verilog::expr::BinOp {
                    op: convert_binop(expr_bin_op.op),
                    lhs: Box::new(self.convert_expr(package, lhs, &lhs_type, self.db, scheduler)),
                    rhs: Box::new(self.convert_expr(package, rhs, &rhs_type, self.db, scheduler)),
                    width: self.node_width(package, &node),
                })
            }
            AstNodePayload::ExprUnOp(expr_un_op) => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Expr::UnOp(verilog::expr::UnOp {
                    op: convert_unop(expr_un_op.op),
                    expr: Box::new(self.convert_expr(package, child, &child_type, self.db, scheduler)),
                    width: self.node_width(package, &node),
                })
            }
            AstNodePayload::When => {
                let children = node.children();
                self.convert_when_expr(package, &children, typ, scheduler)
            }
            AstNodePayload::ExprIndex(expr_index) => {
                let child = node.child(0);
                let child_type = self.node_type(package, &child).unwrap();
                verilog::Expr::Index(verilog::expr::Index {
                    subject: Box::new(self.convert_expr(package, child, &child_type, self.db, scheduler)),
                    index: Box::new(constant_index_expr(expr_index.index)),
                })
            }
            AstNodePayload::ExprIndexDyn => {
                let subject = node.child(0);
                let index_expr = node.child(1);
                let subject_type = self.node_type(package, &subject).unwrap();
                let index_type = self.node_type(package, &index_expr).unwrap();
                verilog::Expr::Index(verilog::expr::Index {
                    subject: Box::new(self.convert_expr(package, subject, &subject_type, self.db, scheduler)),
                    index: Box::new(self.convert_expr(package, index_expr, &index_type, self.db, scheduler)),
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
            AstNodePayload::ExprDontcare => {
                let width = self.node_or_expected_width(package, &node, typ);
                verilog::Expr::XLit(verilog::expr::XLit { width })
            }
            AstNodePayload::ExprCtor(_ctor) => {
                self.convert_ctor_expr(package, node, typ, &typing, db, scheduler)
            }
            AstNodePayload::ExprEnumerant(_enumerant) => {
                self.convert_enumerant_expr(node, &typing, typ)
            }
            AstNodePayload::ExprFn => {
                let tag = typing.tag(node.location());
                let arg = node.child(1);
                match tag {
                    crate::types::typing::Tag::SymbolResolution(_symbol_id) => todo!(),
                    crate::types::typing::Tag::PrimitiveResolution(primitive) => {
                        let arg_type = self.node_type(package, &arg).unwrap();
                        let arg_expr = self.convert_expr(package, arg, &arg_type, self.db, scheduler);
                        match primitive {
                            Primitive::Any => {
                                verilog::Expr::UnOp(verilog::expr::UnOp {
                                    op: verilog::UnOp::RedOr,
                                    expr: Box::new(arg_expr),
                                    width: 1,
                                })
                            }
                            Primitive::All => {
                                verilog::Expr::UnOp(verilog::expr::UnOp {
                                    op: verilog::UnOp::RedAnd,
                                    expr: Box::new(arg_expr),
                                    width: 1,
                                })
                            }
                            Primitive::Cast => arg_expr,
                            Primitive::Zext => self.convert_zext(package, node, typ, scheduler),
                            Primitive::Sext => self.convert_sext(package, node, typ, scheduler),
                            Primitive::AsClock => self.convert_asclock(package, node, typ, scheduler),
                            Primitive::Trunc => self.convert_trunc(package, node, typ, scheduler),
                            Primitive::Word => {
                                let width = self.node_or_expected_width(package, &node, typ);
                                // children[0] is the function name, children[1..] are the arguments
                                let children = node.children();
                                let args = &children[1..];
                                let mut exprs = Vec::new();
                                for i in 0..args.len() {
                                    let child = node.child(1 + i as u16);
                                    let child_type = self.node_type(package, &child).unwrap();
                                    exprs.push(self.convert_expr(package, child, &child_type, self.db, scheduler));
                                }
                                verilog::Expr::Concat(verilog::expr::Concat { exprs, width })
                            }
                            Primitive::Mux => {
                                // mux(cond, a, b) -> cond ? a : b
                                let cond_node = node.child(1);
                                let a_node = node.child(2);
                                let b_node = node.child(3);
                                let cond_type = self.node_type(package, &cond_node).unwrap();
                                let a_type = self.node_type(package, &a_node).unwrap();
                                let b_type = self.node_type(package, &b_node).unwrap();
                                let cond_expr = self.convert_expr(package, cond_node, &cond_type, self.db, scheduler);
                                let a_expr = self.convert_expr(package, a_node, &a_type, self.db, scheduler);
                                let b_expr = self.convert_expr(package, b_node, &b_type, self.db, scheduler);
                                verilog::Expr::If(verilog::expr::If {
                                    cond: Box::new(cond_expr),
                                    then_expr: Box::new(a_expr),
                                    else_expr: Box::new(b_expr),
                                    width: self.node_width(package, &node),
                                })
                            }
                        }
                    }
                    crate::types::typing::Tag::ReferentResolution(_) => unreachable!(),
                    crate::types::typing::Tag::None => unreachable!(),
                }
            }
            AstNodePayload::Match => {
                let children = node.children();
                let subject = children[0].clone();
                let subject_typ = self.node_type(package, &subject).unwrap();
                let subject_expr = self.convert_expr(package, subject, &subject_typ, db, scheduler);
                let temp_name = scheduler.fresh_temp_name("match");
                let result_width = type_width(typ, db);
                let mut case_items: Vec<verilog::CaseItem> = vec![];
                let mut has_else = false;
                let mut idx = 1;
                while idx < children.len() {
                    let (pat, body) = if children[idx].is_pat() {
                        let pat = children[idx].clone();
                        let body = children[idx + 1].clone();
                        idx += 2;
                        (Some(pat), body)
                    } else {
                        let body = children[idx].clone();
                        idx += 1;
                        (None, body)
                    };

                    let mut bound_keys: Vec<String> = vec![];
                    let pattern = match pat.as_ref().map(|p| p.payload()) {
                        None => { has_else = true; verilog::CasePattern::Default },
                        Some(AstNodePayload::PatEnumerant(_)) => {
                            let pat = pat.unwrap();
                            self.build_enumerant_case_pattern(&pat, &subject_typ)
                        }
                        Some(AstNodePayload::PatCtor(_)) => {
                            let pat = pat.unwrap();
                            self.build_ctor_case_pattern(&pat, &subject_expr, &subject_typ, scheduler, &mut bound_keys)
                        }
                        Some(AstNodePayload::PatWordLit(pat_word_lit)) => {
                            let pat = pat.unwrap();
                            let literal = pat.parsing.string(pat_word_lit.literal).to_str_lossy().into_owned();
                            let (value, _lit_width) = parse_word_literal(&literal);
                            let width = type_width(&subject_typ, db);
                            let pat_str = format!("{:0>width$b}", value, width = width as usize);
                            verilog::CasePattern::PatternLit(verilog::PatternLit { width, radix: Radix::Bin, pattern: pat_str })
                        }
                        Some(AstNodePayload::PatBitLit(pat_bit_lit)) => {
                            let pat_str = if pat_bit_lit.literal { "1".to_string() } else { "0".to_string() };
                            verilog::CasePattern::PatternLit(verilog::PatternLit { width: 1, radix: Radix::Bin, pattern: pat_str })
                        }
                        _ => unreachable!("expected pattern node"),
                    };
                    // Convert body with pattern-variable substitutions in scope
                    let body_expr = self.convert_expr(package, body, typ, db, scheduler);
                    // Remove pattern-variable substitutions
                    for key in &bound_keys {
                        scheduler.subst.shift_remove(key);
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
            AstNodePayload::ExprStruct => {
                // Get the struct type to determine field order
                let Type::Usual(typedef_symbol_id, type_args) = typ else {
                    panic!("ExprStruct must have a struct type, got {:?}", typ);
                };

                // Get the struct fields in their definition order
                let struct_fields = self.db.get_struct_fields(*typedef_symbol_id);

                // Build a map from field name to expression value
                use std::collections::HashMap;
                use bstr::ByteSlice;
                let mut field_exprs: HashMap<String, verilog::Expr> = HashMap::new();
                let parsing = self.db.get_parsing(package);

                for assign_node in node.children() {
                    let AstNodePayload::Assign(assign) = assign_node.payload() else {
                        continue;
                    };
                    let field_name = parsing.string(assign.name).to_str_lossy().into_owned();
                    let expr_node = assign_node.child(0);

                    // Get the field type to convert the expression correctly,
                    // substituting the struct def's generic parameters with the
                    // type arguments of this instantiation.
                    let field_type = struct_fields.iter()
                        .find(|f| f.name.to_str_lossy() == field_name)
                        .and_then(|f| f.typ.clone())
                        .map(|typ| crate::types::substitute::substitute(&typ, type_args))
                        .unwrap_or_else(|| Type::Word(8)); // fallback

                    let field_expr = self.convert_expr(package, expr_node, &field_type, self.db, scheduler);
                    field_exprs.insert(field_name, field_expr);
                }

                // Pack fields so the FIRST field occupies the LOW bits and the
                // LAST field the high bits, matching `ExprField`'s bit-offset
                // arithmetic (`->field` slices with bit_offset accumulated from
                // 0 upward through the field list).
                //
                // A Verilog concatenation `{e0, e1, ...}` places `e0` in the
                // MSBs, so we emit the fields in REVERSE definition order:
                // `{last, ..., second, first}` puts `first` in the LSBs.
                let mut packed_exprs = vec![];
                for field in struct_fields.iter().rev() {
                    let field_name = field.name.to_str_lossy().into_owned();
                    if let Some(field_expr) = field_exprs.get(&field_name) {
                        packed_exprs.push(field_expr.clone());
                    } else {
                        // Field not provided - this should have been caught by type checking
                        // Use a zero value as fallback
                        let field_width = field.typ.as_ref()
                            .map(|t| type_width(t, self.db))
                            .unwrap_or(0);
                        packed_exprs.push(verilog::Expr::WordLit(verilog::expr::WordLit {
                            value: 0,
                            width: field_width,
                            radix: Radix::Dec,
                        }));
                    }
                }

                let total_width = type_width(typ, self.db);
                verilog::Expr::Concat(verilog::expr::Concat {
                    exprs: packed_exprs,
                    width: total_width,
                })
            }
            AstNodePayload::ExprField(field) => {
                // Field projection: extract a field from a packed struct
                // The subject expression should have a struct type
                let subject_node = node.child(0);
                let subject_typ = typing.type_of_node(subject_node.id())
                    .map(|t| self.mono_substitute(t.clone()))
                    .unwrap_or_else(|| Type::Word(8));

                let Type::Usual(typedef_symbol_id, type_args) = &subject_typ else {
                    panic!("ExprField subject must have a struct type, got {:?}", subject_typ);
                };

                // Get the struct fields to determine bit positions,
                // substituting the def's generic parameters with the
                // subject's type arguments before measuring widths.
                let struct_fields = self.db.get_struct_fields(*typedef_symbol_id);
                let parsing = self.db.get_parsing(package);
                let field_name = parsing.string(field.field);

                // Find the field and calculate its bit range
                // Fields are packed: first field in lower bits, last field in higher bits
                use bstr::ByteSlice;
                let mut bit_offset = 0;
                let mut field_width = 0;
                let mut found = false;

                for struct_field in &struct_fields {
                    let current_field_width = struct_field.typ.as_ref()
                        .map(|t| type_width(&crate::types::substitute::substitute(t, type_args), self.db))
                        .unwrap_or(0);

                    if struct_field.name == field_name {
                        field_width = current_field_width;
                        found = true;
                        break;
                    }

                    bit_offset += current_field_width;
                }

                if !found {
                    panic!("Field {} not found in struct", field_name.to_str_lossy());
                }

                // Convert the subject expression
                let subject_expr = self.convert_expr(package, subject_node, &subject_typ, self.db, scheduler);

                // Extract the field using bit slicing
                if field_width == 0 {
                    // Zero-width field (shouldn't happen in practice)
                    verilog::Expr::WordLit(verilog::expr::WordLit {
                        value: 0,
                        width: 0,
                        radix: Radix::Dec,
                    })
                } else if bit_offset == 0 && field_width == type_width(&subject_typ, self.db) {
                    // Field is the entire struct (only field)
                    subject_expr
                } else {
                    // Extract bits [bit_offset + field_width - 1 : bit_offset]
                    let hi = bit_offset + field_width - 1;
                    let lo = bit_offset;
                    verilog::Expr::IndexRange(verilog::expr::IndexRange {
                        subject: Box::new(subject_expr),
                        index_hi: Box::new(verilog::Expr::WordLit(verilog::expr::WordLit {
                            value: hi as WordValue,
                            width: 32,
                            radix: Radix::Dec,
                        })),
                        index_lo: Box::new(verilog::Expr::WordLit(verilog::expr::WordLit {
                            value: lo as WordValue,
                            width: 32,
                            radix: Radix::Dec,
                        })),
                    })
                }
            }
            _ => panic!("unsupported expr in conversion: {}", node.summary()),
        }
    }

    fn convert_when_expr(
        &self,
        package: PackageId,
        children: &[AstNode],
        typ: &Type,
        scheduler: &mut ExprScheduler,
    ) -> verilog::Expr {
        // Children are encoded by the parser as flat sequences:
        //   case arm  => 2 children (guard, body)
        //   else arm  => 1 child  (body)
        // We process in pairs for case arms; a single remaining child is the else body.
        //
        // Build a right-associative ternary chain
        let mut clauses: Vec<(verilog::Expr, verilog::Expr)> = vec![];
        let mut else_expr: Option<verilog::Expr> = None;

        // Process children in pairs: [guard, body, guard, body, ..., else_body?]
        let mut idx = 0;
        while idx + 1 < children.len() {
            let guard = &children[idx];
            let body = &children[idx + 1];
            let guard_type = self.node_type(package, guard).unwrap();
            let guard_expr = self.convert_expr(package, guard.clone(), &guard_type, self.db, scheduler);

            let body_expr = if matches!(body.payload(), AstNodePayload::ModDefStmtBlock) {
                // Extract single expr from block
                let block_children = body.children();
                if block_children.len() == 1 && block_children[0].is_expr() {
                    self.convert_expr(package, block_children[0].clone(), typ, self.db, scheduler)
                } else {
                    panic!("when arm body block with multiple statements in expr context")
                }
            } else if matches!(body.payload(), AstNodePayload::When) {
                self.convert_when_expr(package, &body.children(), typ, scheduler)
            } else if matches!(body.payload(), AstNodePayload::Match) {
                let body_type = self.node_type(package, body).unwrap();
                self.convert_expr(package, body.clone(), &body_type, self.db, scheduler)
            } else if body.is_expr() {
                self.convert_expr(package, body.clone(), typ, self.db, scheduler)
            } else {
                panic!("unexpected when arm body: {:?}", body.payload())
            };
            clauses.push((guard_expr, body_expr));
            idx += 2;
        }

        // If one child remains, it is the else arm body.
        if let Some(remainder) = children.get(idx) {
            else_expr = Some(if matches!(remainder.payload(), AstNodePayload::ModDefStmtBlock) {
                let block_children = remainder.children();
                if block_children.len() == 1 && block_children[0].is_expr() {
                    self.convert_expr(package, block_children[0].clone(), typ, self.db, scheduler)
                } else {
                    panic!("when arm else body block with multiple statements in expr context")
                }
            } else if matches!(remainder.payload(), AstNodePayload::When) {
                self.convert_when_expr(package, &remainder.children(), typ, scheduler)
            } else if matches!(remainder.payload(), AstNodePayload::Match) {
                let body_type = self.node_type(package, remainder).unwrap();
                self.convert_expr(package, remainder.clone(), &body_type, self.db, scheduler)
            } else if remainder.is_expr() {
                self.convert_expr(package, remainder.clone(), typ, self.db, scheduler)
            } else {
                panic!("unexpected when else arm body: {:?}", remainder.payload())
            });
        }

        // Build right-associative ternary chain.
        // When there is no else arm, emit X-bits (consistent with convert_driver_when_to_expr).
        let result = else_expr.unwrap_or_else(|| {
            verilog::Expr::XLit(verilog::expr::XLit { width: type_width(typ, self.db) })
        });
        clauses.into_iter().rev().fold(result, |acc, (guard_expr, body_expr)| {
            verilog::Expr::If(verilog::expr::If {
                cond: Box::new(guard_expr),
                then_expr: Box::new(body_expr),
                else_expr: Box::new(acc),
                width: type_width(typ, self.db),
            })
        })
    }

    fn convert_zext(&self, package: PackageId, node: AstNode, typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        // child(0) is function name, child(1) is the argument
        let inner = node.child(1);
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

    fn convert_trunc(&self, package: PackageId, node: AstNode, typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        // child(0) is function name, child(1) is the argument
        let inner = node.child(1);
        let inner_type = self.node_type(package, &inner).unwrap();
        let target_width = self.node_or_expected_width(package, &node, typ);
        let source_width = self.node_width(package, &inner);
        if source_width == target_width {
            return self.convert_expr(package, inner, &inner_type, self.db, scheduler);
        }
        // Truncate: take the lower `target_width` bits of the source.
        verilog::Expr::IndexRange(verilog::expr::IndexRange {
            subject: Box::new(self.convert_expr(package, inner, &inner_type, self.db, scheduler)),
            index_hi: Box::new(constant_index_expr(target_width - 1)),
            index_lo: Box::new(constant_index_expr(0)),
        })
    }

    fn convert_sext(&self, package: PackageId, node: AstNode, typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        // child(0) is function name, child(1) is the argument
        let inner = node.child(1);
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

    fn convert_asclock(&self, package: PackageId, node: AstNode, _typ: &Type, scheduler: &mut ExprScheduler) -> verilog::Expr {
        let inner = node.child(1);
        let inner_type = self.node_type(package, &inner).unwrap();
        self.convert_expr(package, inner, &inner_type, self.db, scheduler)
    }

    fn typing_for(&self, node: &AstNode) -> Arc<Typing> {
        let exprroot = self.db.get_exprroot_for(node.location());
        self.db.get_typing(exprroot)
    }

    /// Substitute the current mono instantiation's type arguments into
    /// `typ`.  A no-op outside a generic def's instantiation (mono
    /// types substitute to themselves).
    fn mono_substitute(&self, typ: Type) -> Type {
        if self.mono_env.is_empty() {
            typ
        } else {
            substitute(&typ, &self.mono_env)
        }
    }

    fn node_type(&self, package: PackageId, node: &AstNode) -> Option<Type> {
        let typ = self.db.get_typeof(Location::new(package, node.id())).ok();
        typ.map(|t| self.mono_substitute(t))
    }

    fn node_width(&self, package: PackageId, node: &AstNode) -> Width {
        self.node_type(package, node).map(|t| type_width(&t, self.db)).unwrap_or(0)
    }

    fn node_or_expected_width(&self, package: PackageId, node: &AstNode, expected: &Type) -> Width {
        self.node_type(package, node)
            .map(|t| type_width(&t, self.db))
            .unwrap_or_else(|| type_width(expected, self.db))
    }
}


/// Walk `stmts` recursively and emit `assign` elements for every `:=:` bidirectional
/// socket connection, including those nested inside Submodule It-blocks.
/// `it_context` holds the resolved prefix for `it.*` references
/// (the submodule instance name, dotted for nested contexts).
fn collect_bidirectional_assigns<'a>(
    stmts: Vec<AstNode<'a>>,
    db: &Db,
    component_analysis: &crate::analysis::component::ComponentAnalysis,
    it_context: Option<&bstr::BStr>,
    elements: &mut Vec<verilog::Element>,
) {
    use crate::common::Flow;
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);
                let Some(lhs_path) = lhs_node.path() else { continue };
                let Some(rhs_path) = rhs_node.path() else { continue };
                let mut lhs_str = parsing.string(lhs_path).to_owned();
                let mut rhs_str = parsing.string(rhs_path).to_owned();

                // Resolve `it.*` to the enclosing submodule instance name.
                if let Some(ctx) = it_context {
                    if lhs_str.starts_with(b"it.") {
                        let suffix = lhs_str[3..].to_owned();
                        lhs_str.clear();
                        lhs_str.extend_from_slice(ctx);
                        lhs_str.push(b'.');
                        lhs_str.extend_from_slice(&suffix);
                    } else if lhs_str == b"it" {
                        lhs_str = ctx.to_owned();
                    }
                    if rhs_str.starts_with(b"it.") {
                        let suffix = rhs_str[3..].to_owned();
                        rhs_str.clear();
                        rhs_str.extend_from_slice(ctx);
                        rhs_str.push(b'.');
                        rhs_str.extend_from_slice(&suffix);
                    } else if rhs_str == b"it" {
                        rhs_str = ctx.to_owned();
                    }
                }

                let lhs_prefix = format!("{}.", lhs_str.to_str_lossy());
                let rhs_prefix = format!("{}.", rhs_str.to_str_lossy());

                if let (Some(lhs_component), Some(rhs_component)) = (
                    component_analysis.resolve(lhs_str.as_bstr()),
                    component_analysis.resolve(rhs_str.as_bstr()),
                ) {
                    let is_net = |component: &crate::analysis::component::Component| {
                        matches!(
                            component.kind(),
                            Some(ComponentKind::Bus) | Some(ComponentKind::Net)
                        )
                    };
                    if is_net(&lhs_component) && is_net(&rhs_component) {
                        elements.push(verilog::Element::Tran(verilog::Tran {
                            a: valid_verilog_name(&lhs_component.path().to_str_lossy()),
                            b: valid_verilog_name(&rhs_component.path().to_str_lossy()),
                            width: lhs_component
                                .typ()
                                .map(|t| type_width(&t, db))
                                .unwrap_or(1),
                        }));
                        continue;
                    }
                }

                for (lhs_full_path, lhs_component) in component_analysis.components() {
                    let lhs_path_str = lhs_full_path.to_str_lossy();
                    if !lhs_path_str.starts_with(&lhs_prefix) {
                        continue;
                    }
                    let suffix = &lhs_path_str[lhs_prefix.len()..];
                    let rhs_full_path_str = format!("{}{}", rhs_prefix, suffix);
                    let rhs_full_path = bstr::BString::from(rhs_full_path_str.as_bytes());
                    let Some(rhs_component) = component_analysis.resolve(rhs_full_path.as_bstr())
                    else {
                        continue;
                    };
                    let (target_path, source_path) =
                        match (lhs_component.flow(), rhs_component.flow()) {
                            (Flow::Source, Flow::Sink) => (
                                rhs_full_path.to_str_lossy().into_owned(),
                                lhs_full_path.to_str_lossy().into_owned(),
                            ),
                            (Flow::Sink, Flow::Source) => (
                                lhs_full_path.to_str_lossy().into_owned(),
                                rhs_full_path.to_str_lossy().into_owned(),
                            ),
                            _ => continue,
                        };
                    elements.push(verilog::Element::Assign(verilog::Assign {
                        name: valid_verilog_name(&target_path),
                        expr: verilog::Expr::Reference(verilog::expr::Reference {
                            name: valid_verilog_name(&source_path),
                        }),
                        strength: crate::common::Strength::Strong,
                    }));
                }
            }
            AstNodePayload::Submodule(submodule) => {
                // Recurse into the It-block body, using the submodule instance name
                // as the new `it` context so that nested `it.*` references resolve
                // to the correct wire prefix (e.g. `it.instr_mem` -> `cpu.instr_mem`).
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                // A Submodule with a body has an Ofness child, an optional
                // Annotations child, and an It child.  Find the It-block by
                // payload rather than by fixed index so that the optional
                // Annotations child (added by the annotations feature) does
                // not shift the It-block out from under us.
                let it_block = children.iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It));
                if let Some(it_block) = it_block {
                    let block = &it_block.children()[0];
                    let name_bstr = if let Some(ctx) = it_context {
                        let mut new_name = bstr::BString::from(ctx);
                        new_name.push(b'.');
                        new_name.extend_from_slice(name);
                        new_name
                    } else {
                        bstr::BString::from(name)
                    };
                    collect_bidirectional_assigns(
                        block.children(),
                        db,
                        component_analysis,
                        Some(name_bstr.as_bstr()),
                        elements,
                    );
                }
            }
            _ => {}
        }
    }
}

fn qualified_module_name(package_name: &str, module_name: &str) -> String {
    format!("{package_name}::{module_name}")
}

/// The `Generics` declaration of a mod def item node, if it declares
/// one.
fn item_generics<'p>(item_ast: &'p AstNode<'p>) -> Option<AstNode<'p>> {
    crate::types::typedef::generics_decl_of(item_ast)
}

/// The generic parameter list of the item owning `symbol`: empty when
/// the item is not generic.
fn target_params(db: &Db, symbol: &Symbol) -> Vec<crate::types::typedef::TypeParam> {
    let location = symbol.location();
    let parsing = db.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    match crate::types::typedef::generics_decl_of(&node) {
        Some(decl) => crate::types::typedef::parse_generics_decl(&decl),
        None => Vec::new(),
    }
}

/// Whether the item owning `symbol` declares generic parameters.
fn target_is_generic(db: &Db, symbol: &Symbol) -> bool {
    !target_params(db, symbol).is_empty()
}

/// Resolve the target def of a socket or submodule instantiation node
/// and its type arguments, as written.  The component analysis has
/// already kind-checked the arguments (conversion only runs on checked
/// designs); returns `None` if the target or arguments do not resolve.
fn instantiation_target(db: &Db, stmt: &AstNode<'_>) -> Option<(Symbol, Vec<Type>)> {
    let ofness_node = stmt
        .children()
        .iter()
        .find(|c| matches!(c.payload(), AstNodePayload::Ofness(_)))?
        .clone();
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        return None;
    };
    let parsing = db.get_parsing(stmt.package());
    let packages = db.get_packages();
    let target_package = match ofness.package {
        Some(pkg) => packages.id(parsing.string(pkg)),
        None => Some(stmt.package()),
    };
    let name = parsing.string(ofness.name);
    let symboltable = db.get_symboltable();
    let symbol = target_package
        .and_then(|pkg| symboltable.resolve_item(name, pkg))?
        .clone();
    let args = instance_args_of(db, stmt, &symbol)?;
    Some((symbol, args))
}

/// Resolve the type arguments written on an instantiation node against
/// the target def's parameter list.  Returns the arguments *as
/// written*: inside a generic def they may mention that def's
/// parameters (`Param`), to be composed by the caller.
fn instance_args_of(db: &Db, stmt: &AstNode<'_>, def_symbol: &Symbol) -> Option<Vec<Type>> {
    let args_node = stmt
        .children()
        .iter()
        .find(|c| matches!(c.payload(), AstNodePayload::GenericsParams(_)))
        .cloned();
    let params = target_params(db, def_symbol);
    let arg_nodes: Vec<AstNode<'_>> = match &args_node {
        Some(node) => node.children(),
        None => vec![],
    };
    if arg_nodes.len() != params.len() {
        return None;
    }
    let type_index = db.get_type_index();
    let mut args: Vec<Type> = Vec::with_capacity(arg_nodes.len());
    for arg in &arg_nodes {
        match arg.payload() {
            AstNodePayload::Type(_) => {
                args.push(type_index.type_at(arg.location())?.clone());
            }
            AstNodePayload::GenericsNat(nat) => {
                let spelling = arg.parsing().string(nat.value).to_str_lossy().into_owned();
                args.push(Type::Word(spelling.parse().ok()?));
            }
            _ => return None,
        }
    }
    Some(args)
}

/// The Verilog name of a mono module: the def's qualified module path,
/// a `#`, then a hash of the type arguments
/// (`top::Reg#bde2499a`).  Hashing the `Type` values (rather than
/// encoding their string rendering) keeps names valid even if the
/// rendering is not unique in the future; a `//` comment above the
/// emitted declaration records the readable origin.
fn mono_module_name(module_path: &str, args: &[Type]) -> String {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    args.hash(&mut hasher);
    let hash = (hasher.finish() & 0xFFFF_FFFF) as u32;
    valid_verilog_name(&format!("{module_path}#{hash:08x}"))
}

/// The `@cell("...")` annotation on a mod def, if any. Only meaningful
/// for ext mods, whose instantiations must use the toolchain's native
/// cell name (e.g. `SB_RAM40_4K`) for yosys to recognize the primitive.
fn cell_annotation(db: &Db, package: PackageId, item_ast: &AstNode<'_>) -> Option<String> {
    let AstNodePayload::ModDef(moddef) = item_ast.payload() else {
        return None;
    };
    if !moddef.is_ext {
        return None;
    }
    let parsing = db.get_parsing(package);
    use crate::syntax::ast::AnnotationValue;
    item_ast
        .annotation_values(&parsing, "cell")
        .into_iter()
        .next()
        .map(|value| match value {
            AnnotationValue::Str(s) => s,
            AnnotationValue::Nat(n) => n.to_string().into(),
            AnnotationValue::Bool(b) => b.to_string().into(),
        })
        .map(|s| s.to_str_lossy().into_owned())
}

/// Tag+payload packing info for a union or enum type instantiation:
/// the width of the tag, the widest payload across all ctors /
/// enumerants, and per-slot tag values and argument widths.
struct TaggedLayout {
    tag_width: Width,
    max_payload: Width,
    /// `(slot_symbol_id, tag_value, arg_widths)` in slot order.
    slots: Vec<(SymbolId, WordValue, Vec<Width>)>,
}

impl TaggedLayout {
    fn slot(&self, symbol_id: SymbolId) -> (WordValue, &[Width]) {
        for (slot_id, tag_value, arg_widths) in &self.slots {
            if *slot_id == symbol_id {
                return (*tag_value, arg_widths);
            }
        }
        panic!("slot {symbol_id:?} not found in layout");
    }
}

/// Compute the `TaggedLayout` of the union or enum instantiation
/// `Type::Usual(sym, args)`, substituting generic members with `args`.
///
/// Unions use a one-hot tag (bit `i` per ctor `i`); enums use their
/// enumerant values as tags, at the enum's declared (tag) width.
fn tagged_layout(db: &Db, sym: SymbolId, args: &[Type]) -> TaggedLayout {
    let symboltable = db.get_symboltable();
    let typedef = db.get_typedef(sym);
    let slot_symbols: Vec<SymbolId> = symboltable
        .slots(sym)
        .iter()
        .map(|slot| slot.id())
        .collect();

    let tag_width: Width = match typedef.kind {
        TypeScheme::UnionDef => slot_symbols.len().try_into().unwrap(),
        TypeScheme::EnumDef => typedef.width.unwrap_or_else(|| {
            slot_symbols.len().try_into().unwrap()
        }),
        _ => panic!("tagged_layout on a non-tagged type"),
    };

    let mut slots = Vec::with_capacity(slot_symbols.len());
    for (index, slot_id) in slot_symbols.iter().enumerate() {
        let tag_value: WordValue = match typedef.kind {
            TypeScheme::UnionDef => 1u64 << index,
            _ => *typedef
                .enumerant_values
                .get(slot_id)
                .unwrap_or(&0),
        };
        let sig = db.get_ctor_signature(*slot_id);
        let arg_widths: Vec<Width> = sig
            .parameters
            .iter()
            .map(|(_name, typ)| type_width(&crate::types::substitute::substitute(typ, args), db))
            .collect();
        slots.push((*slot_id, tag_value, arg_widths));
    }

    let max_payload: Width = slots
        .iter()
        .map(|(_, _, arg_widths)| arg_widths.iter().sum::<Width>())
        .max()
        .unwrap_or(0);

    TaggedLayout { tag_width, max_payload, slots }
}

/// The bit width of the mono type `typ`, delegating to the shared
/// `types::width::type_width` (which recurses through `Usual`
/// argument lists and substitutes generic members before measuring).
fn type_width(typ: &Type, db: &Db) -> Width {
    let mut ctx = db;
    crate::types::width::type_width(&mut ctx, typ)
}


fn render_param_value(value: &AnnotationValue) -> String {
    match value {
        AnnotationValue::Nat(n) => n.to_string(),
        AnnotationValue::Str(s) => format!("\"{}\"", s.to_str_lossy()),
        AnnotationValue::Bool(b) => (if *b { "1'b1" } else { "1'b0" }).to_string(),
    }
}

fn render_ofness_path(ofness_node: AstNode, db: &Db, package: PackageId, symboltable: &SymbolTable) -> String {
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        panic!("expected Ofness node");
    };
    let module_name = ofness_node.parsing.string(ofness.name).to_str_lossy();
    let module_package = match ofness.package {
        Some(pkg) => ofness_node.parsing.string(pkg).to_str_lossy().into_owned(),
        None => {
            // Resolve unqualified names through the symbol table, so that
            // builtin items (e.g. Sync in the "builtin" package) are found
            // even when no package prefix is written.
            if symboltable.builtin_names.contains(module_name.as_bytes()) {
                "builtin".to_string()
            } else {
                db.get_packages().name(package).to_str_lossy().into_owned()
            }
        }
    };
    format!("{module_package}::{module_name}")
}

fn constant_word_expr(width: Width, value: WordValue) -> verilog::Expr {
    verilog::Expr::WordLit(verilog::expr::WordLit { value, width, radix: Radix::Dec })
}

fn constant_index_expr(index: Width) -> verilog::Expr {
    constant_word_expr(32, u64::from(index))
}

/// The expression filling `width` bits of unused payload above a ctor's
/// arguments.  `PADDING_FILL` controls the value: "x" (don't-care),
/// "z" (high-impedance), "0" (zeros), "1" (ones).
fn padding_expr(width: Width) -> verilog::Expr {
    match PADDING_FILL {
        "x" => verilog::Expr::XLit(verilog::expr::XLit { width }),
        "z" => verilog::Expr::ZLit(verilog::expr::ZLit { width }),
        "0" => verilog::Expr::Repeat(verilog::expr::Repeat {
            count: Box::new(constant_index_expr(width)),
            exprs: vec![verilog::Expr::BitLit(verilog::expr::BitLit { value: false })],
            width,
        }),
        "1" => verilog::Expr::Repeat(verilog::expr::Repeat {
            count: Box::new(constant_index_expr(width)),
            exprs: vec![verilog::Expr::BitLit(verilog::expr::BitLit { value: true })],
            width,
        }),
        _ => unreachable!("PADDING_FILL must be one of: \"x\", \"z\", \"0\", \"1\""),
    }
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
fn collect_ast_holes(node: AstNode<'_>, db: &crate::db::Db) -> Vec<String> {
    let mut holes = vec![];
    match node.payload() {
        AstNodePayload::ExprHole => holes.push(node.region().display(db)),
        _ => {
            for child in node.children() {
                holes.extend(collect_ast_holes(child, db).into_iter());
            }
        }
    }
    holes
}

#[cfg(test)]
mod tests {
    use bstr::BString;

    use crate::common::source::Source;
    use crate::db::Db;
    use crate::package::PackageTable;
    use crate::verilog::Element;

    fn db_from_pairs(files: &[(&str, String)]) -> Db {
        let mut db = Db::new();
        let names: Vec<BString> = files.iter().map(|(name, _)| BString::from(*name)).collect();
        db.set_packages(PackageTable::new(names));
        let packages = db.get_packages();
        for (name, text) in files {
            let package = packages.id(name.as_bytes().into()).unwrap();
            db.set_source(package, Source::new(package, text.clone().into()));
        }
        db
    }

    #[test]
    fn cell_annotation_names_ext_mod_instantiation() {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let builtin = std::fs::read_to_string(root.join("../lib/builtin.vir")).unwrap();
        let ice40 = std::fs::read_to_string(root.join("../lib/ice40.vir")).unwrap();
        let design = r#"pub mod Top {
    incoming clock : Clock
    outgoing rdata : Word[16]
    mod ram of ice40::SbRam40_4k {
        it.RCLK := clock
        it.RCLKE := true
        it.RE := true
        it.RADDR := 0
        it.WCLK := clock
        it.WCLKE := true
        it.WE := false
        it.WADDR := 0
        it.MASK := 0
        it.WDATA := 0
        rdata := it.RDATA
    }
}
"#.to_string();

        let db = db_from_pairs(&[
            ("builtin", builtin),
            ("ice40", ice40),
            ("design", design),
        ]);
        if let Err(diagnostics) = crate::util::check_db(&db) {
            panic!("Design should check clean: {diagnostics:?}");
        }

        let verilog = super::convert_db_to_verilog(&db);

        // The instantiation of the ram primitive is emitted under the
        // `@cell` name, not the mangled Virdant name.
        let top_file = verilog.files.iter().find(|f| f.name == "design.sv").unwrap();
        let top_module = top_file.modules.iter().find(|m| m.name == r"\design::Top ").unwrap();
        let submodule = top_module.elements.iter().find_map(|element| match element {
            Element::Submodule(submodule) if submodule.submodule_name == "SB_RAM40_4K" => Some(submodule),
            _ => None,
        }).expect("expected an SB_RAM40_4K instantiation in Top");
        assert_eq!(submodule.name, "ram");

        // Ext mod definitions are not emitted; the ice40 package's
        // ext mod carries the cell name with `is_ext` set.
        let ice40_file = verilog.files.iter().find(|f| f.name == "ice40.sv").unwrap();
        assert!(ice40_file.modules.iter().any(|m| m.is_ext && m.name == "SB_RAM40_4K"));
    }
}
