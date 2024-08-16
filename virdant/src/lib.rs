pub mod parse;
pub mod error;
pub mod id;
pub mod types;
pub mod ast;
pub mod location;
pub mod design;
pub mod context;
pub mod verilog;

mod common;
mod table;
mod ready;
mod cycle;
mod info;
mod typecheck;

#[cfg(test)]
mod tests;

pub use common::*;
use cycle::detect_cycle;
use indexmap::IndexMap;
use indexmap::IndexSet;
use parse::QualIdent;
use ready::Ready;
use error::VirErr;
use error::VirErrs;
use id::*;
use ast::Ast;
use typecheck::TypingContext;
use types::CtorSig;
use types::Nat;
use std::cell::OnceCell;
use std::hash::Hash;
use std::sync::Arc;
use table::Table;
use types::Type;

use crate::info::*;


////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

/// A [`Virdant`] is a context type for manipulating Virdant designs.
/// Call [`check()`](Virdant::check) to get a list of errors in a design.
#[derive(Default)]
pub struct Virdant {
    pub(crate) errors: VirErrs,

    pub(crate) packages: Table<Package, PackageInfo>,
    pub(crate) items: Table<Item, ItemInfo>,
    pub(crate) fields: Table<Field, FieldInfo>,
    pub(crate) ctors: Table<Ctor, CtorInfo>,
    pub(crate) channels: Table<Channel, ChannelInfo>,
    pub(crate) components: Table<Component, ComponentInfo>,
    pub(crate) exprroots: Table<ExprRoot, ExprRootInfo>,
    pub(crate) submodules: Table<Submodule, SubmoduleInfo>,
    pub(crate) ports: Table<Port, PortInfo>,
}


////////////////////////////////////////////////////////////////////////////////
// Public Virdant API
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    pub fn new<S, P>(sources: &[(S, P)]) -> Virdant
        where
            S: AsRef<str>,
            P: AsRef<std::path::Path> {
        let mut virdant = Virdant::default();

        let sources: IndexMap<String, std::path::PathBuf> = sources
            .into_iter()
            .map(|(s, p)| {
                let s: String = s.as_ref().to_owned();
                let p: std::path::PathBuf = p.as_ref().to_owned();
                (s, p)
            })
            .collect();

        virdant.register_packages(sources);
        virdant
    }

    pub fn check(&mut self) -> Result<design::Design, VirErrs> {
        self.init_package_asts();
        self.register_items();

        let packages: Vec<_> = self.packages.keys().cloned().collect();
        for package in packages {
            if let Err(errs) = self.check_all_imported_packages_exist(package) {
                self.errors.extend(errs)
            }

            if let Err(errs) = self.check_no_duplicate_imports(package) {
                self.errors.extend(errs)
            }
        }

        let items: Vec<_> = self.items.keys().cloned().collect();
        for item in items {
            let item_deps = self.item_deps(item).clone();
            let item_info = self.items.get_mut(item).unwrap();
            item_info.deps.set(item_deps);
        }

        self.check_no_item_dep_cycles();

        self.register_fields();
        self.register_ctors();
        self.register_channels();
        self.register_components();

        self.register_exprroots();
        self.register_ports();
        self.register_submodules();

        self.typecheck();

        self.errors.clone().check()?;
        Ok(self.design())
    }
}


////////////////////////////////////////////////////////////////////////////////
// Packages and Items
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_packages(&mut self, sources: IndexMap<String, std::path::PathBuf>) {
        let package: Id<Package> = Id::new("builtin");
        let package_info = self.packages.register(package);
        package_info.name = "builtin".to_string();
        package_info.source = PackageSource::Builtin;

        for (package_name, package_path) in sources {
            let package: Id<Package> = Id::new(package_name.clone());
            let package_info = self.packages.register(package);
            package_info.name = package_name;
            package_info.source = PackageSource::File(package_path);
        }
    }

    fn init_package_asts(&mut self) {
        let packages: Vec<_> = self.packages.keys().cloned().collect();
        for package in packages {
            match self.package_text(package) {
                Err(err) => self.errors.add(err),
                Ok(text) => {
                    let result: Result<Ast, _> = parse::parse_package(&text);
                    match result {
                        Ok(package_ast) => {
                            let package_info = &mut self.packages[package];
                            package_info.ast.set(package_ast.clone());
                        },
                        Err(err) => self.errors.add(VirErr::Parse(err)),
                    }
                }
            }
        }
    }

    fn package_text(&self, package: Id<Package>) -> Result<String, VirErr> {
        let source = &self.packages[package].source;
        match source {
            PackageSource::Builtin => Ok(include_str!("../../lib/builtin.vir").to_string()),
            PackageSource::File(path) => {
                match std::fs::read_to_string(path) {
                    Ok(source) => Ok(source),
                    Err(err) => Err(VirErr::Io(format!("Could not read {path:?}: {err:?}"))),
                }
            },
        }
    }

    fn register_items(&mut self) {
        let packages: Vec<_> = self.packages.keys().cloned().collect();
        for package in packages {
            if let Ok(package_ast) = &self.packages[package].ast.get() {
                for node in package_ast.children() {
                    if node.is_item() {
                        self.register_item(node, package);
                    }
                }
            }
        }
    }

    fn register_item(&mut self, item_ast: Ast, package: Id<Package>) {
        let item_name = item_ast.name().unwrap();
        let qualified_item_name = format!("{package}::{item_name}");
        let item: Id<Item> = Id::new(qualified_item_name.clone());

        if self.items.is_registered(item) {
            self.errors.add(VirErr::DupItem(qualified_item_name));
        }

        let item_info = self.items.register(item);
        let kind = item_ast.item_kind().unwrap();

        item_info.name = item_name.to_string();
        item_info.kind.set(kind);
        item_info.package.set(package);
        if kind == ItemKind::ModDef {
            item_info.is_ext.set(item_ast.is_ext());
        }
        item_info.ast.set(item_ast);
    }
}


////////////////////////////////////////////////////////////////////////////////
// Item dependencies
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn item_deps(&mut self, item: Id<Item>) -> Vec<Id<Item>> {
        if let Ok(item_ast) = self.items[item].ast.get() {
            let (deps, errors) =
                if item_ast.child(0).is_moddef() {
                    self.item_deps_moddef(item, item_ast.child(0))
                } else if item_ast.child(0).is_uniondef() {
                    self.item_deps_uniondef(item, item_ast.child(0))
                } else if item_ast.child(0).is_structdef() {
                    self.item_deps_structdef(item, item_ast.child(0))
                } else if item_ast.child(0).is_builtindef() {
                    (vec![], VirErrs::new())
                } else if item_ast.child(0).is_portdef() {
                    self.item_deps_moddef(item, item_ast.child(0))
                } else {
                    unreachable!()
                };

            self.errors.extend(errors);
            deps
        } else {
            vec![]
        }
    }

    fn item_deps_moddef(&self, item: Id<Item>, moddef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        let mut results = IndexSet::new();
        for node in moddef_ast.children() {
            if let Some(type_node) = node.typ() {
                let (deps, errs) = self.item_deps_type(type_node, item);
                errors.extend(errs);
                results.extend(deps);
            }

            if let Some(qualident) = node.of() {
                match self.resolve_item(&qualident, item) {
                    Ok(dep_item) => {
                        results.insert(dep_item);
                    },
                    Err(err) => {
                        errors.add(err);
                    },
                }
            }
        }
        (results.into_iter().collect(), errors)
    }

    fn item_deps_uniondef(&self, item: Id<Item>, uniondef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        let mut results = IndexSet::new();
        for node in uniondef_ast.children() {
            if node.is_statement() {
                let args = node.args().unwrap();
                for arg in args {
                    let arg_type = arg.typ().unwrap();
                    let (deps, errs) = self.item_deps_type(arg_type, item);
                    results.extend(deps);
                    errors.extend(errs);
                }
            }
        }
        (results.into_iter().collect(), errors)
    }

    fn item_deps_structdef(&self, item: Id<Item>, structdef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        let mut results = IndexSet::new();
        for node in structdef_ast.children() {
            if node.is_statement() {
                let typ = node.typ().unwrap();
                let (deps, errs) = self.item_deps_type(typ, item);
                results.extend(deps);
                errors.extend(errs);
            }
        }
        (results.into_iter().collect(), errors)
    }

    fn item_deps_type(&self, type_ast: Ast, in_item: Id<Item>) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        match self.resolve_item(type_ast.name().unwrap(), in_item) {
            Ok(item) => (vec![item], errors),
            Err(err) => {
                errors.add(err);
                (vec![], errors)
            }
        }
    }

    fn check_no_item_dep_cycles(&mut self) {
        let mut dep_graph = IndexMap::new();

        for (item, item_info) in self.items.iter() {
            let deps = item_info.deps.unwrap();
            dep_graph.insert(item.clone(), deps.to_owned());
        }

        if let Err(cycle) = detect_cycle(&dep_graph) {
            let cycle_names: Vec<String> = cycle
                .into_iter()
                .map(|item| item.to_string())
                .collect();
            self.errors.add(VirErr::ItemDepCycle(cycle_names));
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Resolution
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn resolve_package(&self, package_name: &str) -> Option<Id<Package>> {
        for (package, package_info) in self.packages.iter() {
            if package_name == package_info.name {
                return Some(*package);
            }
        }
        None
    }

    fn resolve_item(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<Item>, VirErr> {
        let qi = QualIdent::new(qualident);
        let in_package = self.items[in_item].package.unwrap().clone();
        let resolved_package_name = qi.in_package(&in_package.to_string()).to_string();
        let builtin_resolved_package_name = qi.in_package("builtin").to_string();
        self.items
            .resolve(&resolved_package_name)
            .or_else(|| self.items.resolve(&builtin_resolved_package_name))
            .ok_or_else(|| VirErr::UnresolvedIdent(format!("{qualident}")))
    }

    fn resolve_moddef(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<ModDef>, VirErr> {
        let item = self.resolve_item(qualident, in_item)?;
        let item_info = &self.items[item];
        if let Ok(ItemKind::ModDef) = item_info.kind.get() {
            Ok(item.cast())
        } else {
            Err(VirErr::Other(format!("Unable to resolve moddef: {qualident} in {in_item}")))
        }
    }

    fn resolve_portdef(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<PortDef>, VirErr> {
        let item = self.resolve_item(qualident, in_item)?;
        let item_info = &self.items[item];
        if let Ok(ItemKind::PortDef) = item_info.kind.get() {
            Ok(item.cast())
        } else {
            Err(VirErr::Other(format!("Unable to resolve portdef: {qualident} in {in_item}")))
        }
    }

    fn resolve_component(&self, path: &str, in_moddef: Id<Item>) -> Result<Id<Component>, VirErr> {
        if let Some(component) = self.components.resolve(&format!("{in_moddef}::{path}")) {
            Ok(component)
        } else {
            Err(VirErr::Other(format!("Unable to resolve component: {path} in {in_moddef}")))
        }
    }

    fn resolve_structdef(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<StructDef>, VirErr> {
        let item = self.resolve_item(qualident, in_item)?;
        let item_info = &self.items[item];
        if let Ok(ItemKind::StructDef) = item_info.kind.get() {
            Ok(item.cast())
        } else {
            Err(VirErr::Other(format!("Unable to resolve structdef: {qualident} in {in_item}")))
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Import Checks
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn check_all_imported_packages_exist(&mut self, package: Id<Package>) -> Result<(), VirErrs> {
        let mut errors = VirErrs::new();
        for imported_package_name in self.package_imports(package) {
            let imported_package = self.resolve_package(&imported_package_name);
            if imported_package.is_none() {
                errors.add(VirErr::CantImport(imported_package_name));
            }
        }
        errors.check()
    }

    fn check_no_duplicate_imports(&mut self, package: Id<Package>) -> Result<(), VirErrs> {
        let mut errors = VirErrs::new();
        let mut imports: IndexSet<String> = IndexSet::new();

        for import in self.package_imports(package) {
            if !imports.insert(import.clone()) {
                errors.add(VirErr::DupImport(import));
            }
        }

        errors.check()
    }

    fn package_imports(&self, package: Id<Package>) -> Vec<String> {
        let mut packages = vec![];
        if let Ok(ast) = &self.packages[package].ast.get() {
            for node in ast.children() {
                if node.is_import() {
                    packages.push(node.package().unwrap().to_string());
                }
            }
        }

        packages
    }
}


////////////////////////////////////////////////////////////////////////////////
// Register types
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_fields(&mut self) {
        let structdefs = self.items_by_kind(ItemKind::StructDef);
        for item in structdefs {
            let structdef: Id<StructDef> = item.cast();
            let mut fields = vec![];

            let item_info = &self.items[item];

            let item_ast = if let Ok(item_ast) = item_info.ast.get() {
                item_ast
            } else {
                continue;
            };

            let structdef_ast = item_ast.child(0);
            for node in structdef_ast.children() {
                if node.is_statement() {
                    let field_name = node.name().unwrap();
                    let field_type_ast = node.typ().unwrap();

                    let field_type = match self.resolve_type(field_type_ast, item) {
                        Ok(field_type) => field_type,
                        Err(err) => {
                            self.errors.add(err);
                            continue;
                        },
                    };

                    let field: Id<Field> = Id::new(format!("{item}::{field_name}"));
                    let field_info = self.fields.register(field);
                    field_info.structdef.set(structdef);
                    field_info.name = field_name.to_string();
                    field_info.typ.set(field_type);

                    fields.push(field);
                }
            }

            let structdef_info = &mut self.items[item];
            structdef_info.fields.set(fields);
        }
    }

    fn register_ctors(&mut self) {
        let uniondefs = self.items_by_kind(ItemKind::UnionDef);
        for item in uniondefs {
            let uniondef: Id<UnionDef> = item.cast();
            let mut ctors = vec![];

            let item_info = &self.items[item];
            let uniondef_ast = item_info.ast.unwrap().child(0);
            for node in uniondef_ast.children() {
                if node.is_statement() {
                    let ctor_name = node.name().unwrap();
                    let ctor_param_asts = node.args().unwrap();

                    let ctor: Id<Ctor> = Id::new(format!("{item}::{ctor_name}"));
                    let mut params: Vec<(String, Type)> = vec![];

                    for ctor_param_ast in ctor_param_asts {
                        let ctor_param_name = ctor_param_ast.name().unwrap().to_string();
                        let type_ast = ctor_param_ast.typ().unwrap();
                        let ctor_param_type = match self.resolve_type(type_ast, item) {
                            Ok(ctor_type) => ctor_type,
                            Err(err) => {
                                self.errors.add(err);
                                continue;
                            },
                        };
                        params.push((ctor_param_name, ctor_param_type));
                    }

                    let ret_typ = Type::uniondef(uniondef);
                    let ctor_sig = CtorSig::new(ctor, params, ret_typ);

                    let ctor_info = self.ctors.register(ctor);
                    ctor_info.uniondef.set(uniondef);
                    ctor_info.name = ctor_name.to_string();
                    ctor_info.sig.set(ctor_sig);

                    ctors.push(ctor);
                }
            }

            let uniondef_info = &mut self.items[item];
            uniondef_info.ctors.set(ctors);
        }
    }

    fn items_by_kind(&self, kind: ItemKind) -> Vec<Id<Item>> {
        let mut items = vec![];
        for (item, item_info) in self.items.iter() {
            if *item_info.kind.unwrap() == kind {
                items.push(item.clone());
            }
        }
        items
    }

    fn resolve_type(&self, type_ast: Ast, in_item: Id<Item>) -> Result<Type, VirErr> {
        let item = self.resolve_item(type_ast.name().unwrap(), in_item)?;

        let mut width: Option<types::Nat> = None;

        if let Some(args) = type_ast.args() {
            assert_eq!(args.len(), 1);

            let arg_ast = args[0].child(0);
            if arg_ast.is_nat() {
                width = Some(str::parse::<u64>(arg_ast.as_str()).unwrap());
            } else {
                return Err(VirErr::KindError("Only widths are allowed here".to_string()));
            }
        }

        let item_kind = self.items[item].kind.unwrap();
        match item_kind {
            ItemKind::UnionDef => {
                if width.is_some() {
                    return Err(VirErr::KindError(format!("Union definition {item} does not take a generic")));
                }
                Ok(Type::uniondef(item.cast()))
            },
            ItemKind::StructDef => {
                if width.is_some() {
                    return Err(VirErr::KindError(format!("Struct definition {item} does not take a generic")));
                }
                Ok(Type::structdef(item.cast()))
            },
            ItemKind::BuiltinDef => {
                let word_builitindef = self.resolve_item("builtin::Word", in_item).unwrap();

                if item == word_builitindef && width.is_none() {
                    return Err(VirErr::KindError("Word requires a length".to_string()));
                } else if item != word_builitindef && width.is_some() {
                    return Err(VirErr::KindError(format!("Type definition {item} does not take a generic")));
                }

                Ok(Type::builtindef(item.cast(), width))
            },
            _ => unreachable!(),
        }
    }

    fn clock_type(&self) -> Type {
        Type::builtindef(self.items.resolve("builtin::Clock").unwrap().cast(), None)
    }

    fn word_type(&self, width: Nat) -> Type {
        Type::builtindef(self.items.resolve("builtin::Word").unwrap().cast(), Some(width))
    }

    fn bit_type(&self) -> Type {
        Type::builtindef(self.items.resolve("builtin::Bit").unwrap().cast(), None)
    }
}


////////////////////////////////////////////////////////////////////////////////
// PortDefs
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_channels(&mut self) {
        let portdefs = self.items_by_kind(ItemKind::PortDef);
        for item in portdefs {
            let portdef: Id<PortDef> = item.cast();
            let mut channels = vec![];

            let item_info = &self.items[item];
            let portdef_ast = item_info.ast.unwrap().child(0);
            for node in portdef_ast.children() {
                if node.is_statement() {
                    let channel_name = node.name().unwrap();
                    let channel_type_ast = node.typ().unwrap();
                    let channel_dir = if node.dir().unwrap().is_mosi() {
                        ChannelDir::Mosi
                    } else {
                        ChannelDir::Miso
                    };

                    let channel_type = match self.resolve_type(channel_type_ast, item) {
                        Ok(channel_type) => channel_type,
                        Err(err) => {
                            self.errors.add(err);
                            continue;
                        },
                    };

                    let channel: Id<Channel> = Id::new(format!("{item}::{channel_name}"));
                    let channel_info = self.channels.register(channel);
                    channel_info.portdef.set(portdef);
                    channel_info.name = channel_name.to_string();
                    channel_info.typ.set(channel_type);
                    channel_info.dir.set(channel_dir);

                    channels.push(channel);
                }
            }

            let portdef_info = &mut self.items[item];
            portdef_info.channels.set(channels);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Components and Expressions
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_components(&mut self) {
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<ModDef> = item.cast();
            let moddef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };
            let mut components = vec![];
            for node in moddef_ast.children() {
                if node.is_statement() {
                    if node.child(0).is_component() {
                        let component_name = node.name().unwrap();
                        let component_typ_ast = node.typ().unwrap();

                        let component: Id<Component> = Id::new(format!("{item}::{component_name}"));
                        let component_info = self.components.register(component);
                        components.push(component);

                        if node.child(0).is_outgoing() {
                            component_info.class.set(ComponentClass::Port);
                            component_info.flow.set(Flow::Sink);
                        } else if node.child(0).is_incoming() {
                            component_info.class.set(ComponentClass::Port);
                            component_info.flow.set(Flow::Source);
                        } else if node.child(0).is_node() {
                            component_info.class.set(ComponentClass::Node);
                            component_info.flow.set(Flow::Duplex);
                        } else if node.child(0).is_reg() {
                            component_info.class.set(ComponentClass::Reg);
                            component_info.flow.set(Flow::Duplex);
                        } else {
                            unreachable!()
                        }

                        component_info.moddef.set(moddef);
                        component_info.path = vec![component_name.to_string()];
                        component_info.is_reg.set(node.child(0).is_reg());
                        if let Ok(typ) = self.resolve_type(component_typ_ast.clone(), item) {
                            let component_info = &mut self.components[component];
                            component_info.typ.set(typ);
                        } else {
                            self.errors.add(VirErr::Other(format!("Could not resolve type: {:?}", component_typ_ast.summary())));
                        }
                    } else if node.child(0).is_submodule() {
                        let submodule_name = node.name().unwrap();
                        let submodule_moddef_name = node.of().unwrap();
                        match self.resolve_moddef(submodule_moddef_name, item) {
                            Ok(submodule_moddef) => {
                                components.extend(self.register_submodule_components(submodule_name, submodule_moddef, moddef));
                            },
                            Err(err) => {
                                self.errors.add(err);
                            },
                        }
                    } else if node.child(0).is_port() {
                        let port_name = node.name().unwrap();
                        let port_portdef_name = node.of().unwrap();
                        match self.resolve_portdef(port_portdef_name, item) {
                            Ok(portdef) => {
                                let path = vec![port_name.to_string()];
                                let role = if node.role().unwrap() == "master" {
                                    PortRole::Master
                                } else {
                                    PortRole::Slave
                                };
                                components.extend(self.register_port_components(path, portdef, moddef, false, role));
                            },
                            Err(err) => self.errors.add(err),
                        }
                    } else if node.child(0).is_driver() {
                        ()
                    } else {
                        unreachable!()
                    }
                }
            }
            let moddef_info = &mut self.items[item];
            moddef_info.components.set(components);
        }
    }

    fn register_submodule_components(&mut self, name: &str, submodule_moddef: Id<ModDef>, in_moddef: Id<ModDef>) -> Vec<Id<Component>> {
        let mut components = vec![];

        let moddef_ast = if let Ok(item_ast) = self.items[submodule_moddef.as_item()].ast.get() {
            item_ast.child(0)
        } else {
            return vec![];
        };

        for node in moddef_ast.children() {
            if node.is_statement() {
                let statement = node.child(0);
                if statement.is_component() {
                    if statement.is_implicit() || statement.is_incoming() || statement.is_outgoing() {
                        let component_name = statement.name().unwrap();
                        let component_typ_ast = statement.typ().unwrap();

                        let component: Id<Component> = Id::new(format!("{in_moddef}::{name}.{component_name}"));

                        components.push(component);

                        let component_info = self.components.register(component);
                        component_info.class.set(ComponentClass::SubPort);
                        if statement.is_implicit() || statement.is_incoming() {
                            component_info.flow.set(Flow::Sink);
                        } else {
                            component_info.flow.set(Flow::Source);
                        }
                        components.push(component);
                        component_info.moddef.set(in_moddef);
                        component_info.path = vec![name.to_string(), component_name.to_string()];
                        component_info.is_reg.set(node.is_reg());
                        if let Ok(typ) = self.resolve_type(component_typ_ast, submodule_moddef.as_item()) {
                            let component_info = &mut self.components[component];
                            component_info.typ.set(typ);
                        }
                    }
                } else if statement.is_port() {
                    let port_name = statement.name().unwrap();
                    let port_portdef_name = statement.of().unwrap();
                    match self.resolve_portdef(port_portdef_name, submodule_moddef.as_item()) {
                        Ok(portdef) => {
                            let path = vec![name.to_string(), port_name.to_string()];
                            let role = if statement.role().unwrap() == "master" {
                                PortRole::Master
                            } else {
                                PortRole::Slave
                            };
                            components.extend(self.register_port_components(path, portdef, in_moddef, true, role));
                        },
                        Err(err) => self.errors.add(err),
                    }
                }
            }
        }
        components
    }

    fn register_port_components(
        &mut self,
        path: Vec<String>,
        portdef: Id<PortDef>,
        in_moddef: Id<ModDef>,
        is_submodule: bool,
        role: PortRole,
    ) -> Vec<Id<Component>> {
        let mut components = vec![];
        let portdef_info = &self.items[portdef.as_item()];
        if let Ok(channels) = portdef_info.channels.get() {
            for channel in channels {
                let channel_info = &self.channels[*channel];

                let channel_name = channel_info.name.clone();
                let channel_typ = channel_info.typ.unwrap();

                let name = format!("{}.{}", path.join("."), channel_name);
                let component: Id<Component> = Id::new(format!("{in_moddef}::{name}"));

                let component_info = self.components.register(component);
                if is_submodule {
                    component_info.class.set(ComponentClass::SubPort);
                } else {
                    component_info.class.set(ComponentClass::Port);
                }

                let channel_dir = *channel_info.dir.unwrap();
                let flow = match (channel_dir, role) {
                    (ChannelDir::Mosi, PortRole::Slave) => Flow::Source,
                    (ChannelDir::Mosi, PortRole::Master) => Flow::Sink,
                    (ChannelDir::Miso, PortRole::Slave) => Flow::Sink,
                    (ChannelDir::Miso, PortRole::Master) => Flow::Source,
                };

                component_info.flow.set(flow);
                components.push(component);
                component_info.moddef.set(in_moddef);

                let mut path = path.clone();
                path.push(channel_name);
                component_info.path = path;

                component_info.is_reg.set(false);
                component_info.typ.set(*channel_typ);
            }
        }
        components
    }

    fn register_exprroots(&mut self) {
        let clock_type = self.clock_type();
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<ModDef> = item.cast();
            let mut expr_i = 0;
            let moddef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };
            for node in moddef_ast.children() {
                if node.is_statement() {
                    if node.child(0).is_driver() {
                        let driver_ast = node.child(0);
                        let target_path = driver_ast.target().unwrap();
                        let drivertype = if driver_ast.drivertype().unwrap() == "<=" {
                            DriverType::Latched
                        } else {
                            DriverType::Continuous
                        };

                        let expr_ast = driver_ast.clone().expr().unwrap();
                        let span = driver_ast.span();
                        let component = self.resolve_component(target_path, item).unwrap();
                        let component_info = &mut self.components[component];
                        let typ = *component_info.typ.unwrap();

                        match drivertype {
                            DriverType::Continuous if *component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {item}"))),
                            DriverType::Latched if !*component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {item}"))),
                            _ => (),
                        }

                        let exprroot = self.register_exprroots_for(ast::Expr::from_ast(expr_ast.clone()), moddef, &vec![expr_i]);
                        expr_i += 1;

                        let exprroot_info = &mut self.exprroots[exprroot];
                        exprroot_info.span = Some(span);
                        exprroot_info.expected_typ = Some(typ);

                        let component_info = &mut self.components[component];
                        component_info.driver = Some(exprroot);
                    } else if node.child(0).is_reg() {
                        let reg_ast = node.child(0);
                        let expr_ast = reg_ast.clone().expr().unwrap();

                        let exprroot = self.register_exprroots_for(ast::Expr::from_ast(expr_ast.clone()), moddef, &vec![expr_i]);
                        expr_i += 1;

                        let exprroot_info = &mut self.exprroots[exprroot];
                        exprroot_info.expected_typ = Some(clock_type);
                        let span = reg_ast.span();
                        exprroot_info.span = Some(span);
                    }
                }
            }
        }
    }

    fn exprroot_id(moddef: Id<ModDef>, path: &[usize]) -> Id<ExprRoot> {
        let mut parts = vec![format!("{moddef}::expr")];
        for p in path {
            parts.push(format!("[{p}]"));
        }
        Id::new(parts.join(""))
    }

    fn register_exprroots_for(&mut self, expr: Arc<ast::Expr>, moddef: Id<ModDef>, path: &[usize]) -> Id<ExprRoot> {
        let exprroot_id: Id<ExprRoot> = Self::exprroot_id(moddef, path);
        let mut subexprs = vec![];

        for (i, e) in expr.subexprs().iter().enumerate() {
            let subexpr = self.register_exprroots_for(e.clone(), moddef, &Self::extend(path, i));
            let info = &mut self.exprroots[subexpr];
            info.parent = Some(exprroot_id);
            subexprs.push(subexpr);
        }

        let exprroot_info = self.exprroots.register(exprroot_id);


        exprroot_info.ast.set(expr.clone());
        exprroot_info.span = Some(expr.span());
        exprroot_info.children = subexprs;
        exprroot_info.moddef.set(moddef);

        exprroot_id
    }

    fn extend(path: &[usize], p: usize) -> Vec<usize> {
        let mut result = path.to_vec();
        result.push(p);
        result
    }

    fn register_ports(&mut self) {
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<ModDef> = item.cast();
            let moddef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };

            let mut ports = vec![];

            for node in moddef_ast.children() {
                if node.is_statement() && node.child(0).is_port() {
                    let port_ast = node.child(0);
                    let port_name = port_ast.name().unwrap();
                    let port_of = port_ast.of().unwrap();

                    let portdef: Id<PortDef> = if let Ok(portdef) = self.resolve_item(port_of, item) {
                        portdef.cast()
                    } else {
                        continue;
                    };

                    let port_role = if node.get_as_str("role").unwrap() == "master" {
                        PortRole::Master
                    } else {
                        PortRole::Slave
                    };

                    let port_id: Id<Port> = Id::new(format!("{moddef}::{port_name}"));
                    let port_info = self.ports.register(port_id);
                    port_info.moddef.set(item.cast());
                    port_info.name = port_name.to_string();
                    port_info.role.set(port_role);
                    port_info.portdef.set(portdef);
                    ports.push(port_id);
                }
            }
            self.items[item].ports.set(ports);
        }
    }

    fn register_submodules(&mut self) {
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<ModDef> = item.cast();
            let moddef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };

            let mut submodules = vec![];

            for node in moddef_ast.children() {
                if node.is_statement() && node.child(0).is_submodule() {
                    let submodule_ast = node.child(0);
                    let submodule_name = submodule_ast.name().unwrap();
                    let submodule_of = submodule_ast.of().unwrap();

                    let submodule_moddef: Id<ModDef> = self.resolve_item(submodule_of, item).unwrap().cast();

                    let submodule_id: Id<Submodule> = Id::new(format!("{moddef}::{submodule_name}"));
                    let submodule_info = self.submodules.register(submodule_id);
                    submodule_info.moddef.set(item.cast());
                    submodule_info.name = submodule_name.to_string();
                    submodule_info.submodule_moddef.set(submodule_moddef);
                    submodules.push(submodule_id);
                }
            }
            self.items[item].submodules.set(submodules);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Typechecking
////////////////////////////////////////////////////////////////////////////////
///
impl Virdant {
    fn typecheck(&mut self) {
        let roots: Vec<(_, _)> = self.exprroots.iter().map(|(id, info)| (id.clone(), info.clone())).collect();
        for (exprroot, exprroot_info) in roots {
            if exprroot_info.parent.is_none() {
                let moddef = exprroot_info.moddef.unwrap();
                let mut typing_context = TypingContext::new(self, *moddef);
                if let Err(err) = typing_context.check(exprroot, exprroot_info.expected_typ.unwrap()) {
                    self.errors.add(err);
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// For testing
////////////////////////////////////////////////////////////////////////////////

impl std::fmt::Debug for Virdant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "PACKAGES:")?;
        for (package, package_info) in self.packages.iter() {
            writeln!(f, "    {package}")?;
            writeln!(f, "        name: {:?}", package_info.name)?;
            writeln!(f, "        source: {:?}", package_info.source)?;
        }

        writeln!(f, "ITEMS:")?;
        for (item, item_info) in self.items.iter() {
            writeln!(f, "    {item}")?;
            writeln!(f, "        name: {:?}", item_info.name)?;
            writeln!(f, "        package: {:?}", item_info.package)?;
            writeln!(f, "        kind: {:?}", item_info.kind)?;
            writeln!(f, "        deps: {:?}", item_info.deps)?;
            writeln!(f, "        submodules: {:?}", item_info.submodules)?;
            writeln!(f, "        components: {:?}", item_info.components)?;
            writeln!(f, "        is_ext: {:?}", item_info.is_ext)?;
            writeln!(f, "        fields: {:?}", item_info.fields)?;
            writeln!(f, "        ctors: {:?}", item_info.ctors)?;
            writeln!(f, "        channels: {:?}", item_info.channels)?;
        }

        writeln!(f, "FIELDS:")?;
        for (field, field_info) in self.fields.iter() {
            writeln!(f, "    {field}")?;
            writeln!(f, "        structdef: {:?}", field_info.structdef)?;
            writeln!(f, "        name: {:?}", field_info.name)?;
            writeln!(f, "        typ: {:?}", field_info.typ)?;
        }

        writeln!(f, "CTORS:")?;
        for (ctor, ctor_info) in self.ctors.iter() {
            writeln!(f, "    {ctor}")?;
            writeln!(f, "        uniondef: {:?}", ctor_info.uniondef)?;
            writeln!(f, "        name: {:?}", ctor_info.name)?;
            writeln!(f, "        sig: {:?}", ctor_info.sig)?;
        }

        writeln!(f, "CHANNELS:")?;
        for (channel, channel_info) in self.channels.iter() {
            writeln!(f, "    {channel}")?;
            writeln!(f, "        portdef: {:?}", channel_info.portdef)?;
            writeln!(f, "        name: {:?}", channel_info.name)?;
            writeln!(f, "        type: {:?}", channel_info.typ)?;
            writeln!(f, "        dir: {:?}", channel_info.dir)?;
        }

        writeln!(f, "COMPONENTS:")?;
        for (component, component_info) in self.components.iter() {
            writeln!(f, "    {component}")?;
            writeln!(f, "        moddef: {:?}", component_info.moddef)?;
            writeln!(f, "        path: {}", component_info.path.join("."))?;
            writeln!(f, "        flow: {:?}", component_info.flow)?;
            writeln!(f, "        class: {:?}", component_info.class)?;
            writeln!(f, "        typ: {:?}", component_info.typ)?;
            writeln!(f, "        is_reg: {:?}", component_info.is_reg)?;
            writeln!(f, "        driver: {:?}", component_info.driver)?;
        }

        writeln!(f, "EXPRROOTS:")?;
        for (exprroot, exprroot_info) in self.exprroots.iter() {
            writeln!(f, "    {exprroot}")?;
            writeln!(f, "        ast: {:?}", exprroot_info.ast.get().map(|ast| ast.summary()))?;
            writeln!(f, "        span: {:?}", exprroot_info.span)?;
            if let Some(expected_typ) = exprroot_info.expected_typ {
                writeln!(f, "        expected_typ: {:?}", expected_typ)?;
            }
            writeln!(f, "        typ: {:?}", exprroot_info.typ)?;
            if let Some(parent) = exprroot_info.parent {
                writeln!(f, "        parent: {:?}", parent)?;
            }
            if exprroot_info.children.len() > 0 {
                writeln!(f, "        children: {:?}", exprroot_info.children)?;
            }
        }

        writeln!(f, "SUBMODULES:")?;
        for (submodule, submodule_info) in self.submodules.iter() {
            writeln!(f, "    {submodule}")?;
            writeln!(f, "        name: {}", submodule_info.name)?;
            writeln!(f, "        submodule_moddef: {:?}", submodule_info.submodule_moddef)?;
            writeln!(f, "        moddef: {:?}", submodule_info.moddef)?;
        }

        writeln!(f, "PORTS:")?;
        for (port, port_info) in self.ports.iter() {
            writeln!(f, "    {port}")?;
            writeln!(f, "        name: {}", port_info.name)?;
            writeln!(f, "        portdef: {:?}", port_info.portdef)?;
            writeln!(f, "        moddef: {:?}", port_info.moddef)?;
        }

        Ok(())
    }
}


////////////////////////////////////////////////////////////////////////////////
// Extra Types
////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ItemKind {
    ModDef,
    UnionDef,
    StructDef,
    BuiltinDef,
    PortDef,
}

impl ItemKind {
    pub fn is_typedef(&self) -> bool {
        match self {
            ItemKind::ModDef => false,
            ItemKind::UnionDef => true,
            ItemKind::StructDef => true,
            ItemKind::BuiltinDef => true,
            ItemKind::PortDef => false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub enum PackageSource {
    #[default]
    Builtin,
    File(std::path::PathBuf),
}


////////////////////////////////////////////////////////////////////////////////
// Design
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    pub fn design(&self) -> design::Design {
        let mut packages: IndexMap<Id<Package>, design::Package> = IndexMap::new();
        for package in self.packages.keys() {
            packages.insert(*package, self.make_design_package(*package));
        }

        let mut items: IndexMap<Id<Item>, design::Item> = IndexMap::new();
        for item in self.items.keys() {
            items.insert(*item, self.make_design_items(*item));
        }

        let mut components: IndexMap<Id<Component>, design::Component> = IndexMap::new();
        for component in self.components.keys() {
            components.insert(*component, self.make_design_components(*component));
        }

        let mut submodules: IndexMap<Id<Submodule>, design::Submodule> = IndexMap::new();
        for submodule in self.submodules.keys() {
            let info = self.submodules[*submodule].clone();
            let design_submodule = design::Submodule { root: OnceCell::new(), info };
            submodules.insert(*submodule, design_submodule);
        }

        let mut ports: IndexMap<Id<Port>, design::Port> = IndexMap::new();
        for port in self.ports.keys() {
            let info = self.ports[*port].clone();
            let design_port = design::Port { root: OnceCell::new(), info };
            ports.insert(*port, design_port);
        }

        let mut exprroots: IndexMap<Id<ExprRoot>, design::ExprRoot> = IndexMap::new();
        for exprroot in self.exprroots.keys() {
            let id = *exprroot;
            let info = self.exprroots[*exprroot].clone();
            let design_exprroot = design::ExprRoot { root: OnceCell::new(), id, info };
            exprroots.insert(*exprroot, design_exprroot);
        }

        let mut fields: IndexMap<Id<Field>, design::Field> = IndexMap::new();
        for field in self.fields.keys() {
            fields.insert(*field, self.make_design_fields(*field));
        }

        let mut ctors: IndexMap<Id<Ctor>, design::Ctor> = IndexMap::new();
        for ctor in self.ctors.keys() {
            ctors.insert(*ctor, self.make_design_ctors(*ctor));
        }

        let root = Arc::new(design::DesignRoot {
            packages,
            items,
            components,
            submodules,
            ports,
            exprroots,
            fields,
            ctors,
        });

        for package in root.packages.values() {
            package.root.set(Arc::downgrade(&root)).unwrap();
        }

        for item in root.items.values() {
            item.root.set(Arc::downgrade(&root)).unwrap();
        }

        for component in root.components.values() {
            component.root.set(Arc::downgrade(&root)).unwrap();
        }

        for submodule in root.submodules.values() {
            submodule.root.set(Arc::downgrade(&root)).unwrap();
        }

        for port in root.ports.values() {
            port.root.set(Arc::downgrade(&root)).unwrap();
        }

        for field in root.fields.values() {
            field.root.set(Arc::downgrade(&root)).unwrap();
        }

        for ctor in root.ctors.values() {
            ctor.root.set(Arc::downgrade(&root)).unwrap();
        }

        for exprroot in root.exprroots.values() {
            exprroot.root.set(Arc::downgrade(&root)).unwrap();
        }

        design::Design(root)
    }

    fn make_design_package(&self, package: Id<Package>) -> design::Package {
        let info = self.packages[package].clone();
        design::Package {
            root: OnceCell::new(),
            id: package,
            info,
        }
    }

    fn make_design_items(&self, item: Id<Item>) -> design::Item {
        let info = self.items[item].clone();
        design::Item {
            root: OnceCell::new(),
            id: item,
            info,
        }
    }

    fn make_design_components(&self, component: Id<Component>) -> design::Component {
        let info = self.components[component].clone();
        design::Component {
            root: OnceCell::new(),
            info,
        }
    }

    fn make_design_fields(&self, field: Id<Field>) -> design::Field {
        let info = self.fields[field].clone();
        design::Field {
            root: OnceCell::new(),
            info,
        }
    }

    fn make_design_ctors(&self, ctor: Id<Ctor>) -> design::Ctor {
        let info = self.ctors[ctor].clone();
        design::Ctor {
            root: OnceCell::new(),
            info,
        }
    }
}
