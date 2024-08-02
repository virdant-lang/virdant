pub mod parse;
pub mod error;
pub mod id;
pub mod types;
pub mod expr;
pub mod ast;
pub mod location;

mod table;
mod ready;
mod cycle;

#[cfg(test)]
mod tests;

use cycle::detect_cycle;
use expr::Expr;
use indexmap::IndexMap;
use indexmap::IndexSet;
use parse::QualIdent;
use ready::Ready;
use error::VirErr;
use error::VirErrs;
use id::*;
use ast::Ast;
use types::CtorSig;
use std::hash::Hash;
use std::sync::Arc;
use table::Table;
use types::Type;

use crate::expr::DriverType;


////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

/// A [`Virdant`] is a context type for manipulating Virdant designs.
/// Call [`check()`](Virdant::check) to get a list of errors in a design.
#[derive(Default)]
pub struct Virdant {
    errors: VirErrs,

    packages: Table<Package, PackageInfo>,
    items: Table<Item, ItemInfo>,
    builtindefs: Table<BuiltinDef, BuiltinDefInfo>,
    structdefs: Table<StructDef, StructDefInfo>,
    fields: Table<Field, FieldInfo>,
    uniondefs: Table<UnionDef, UnionDefInfo>,
    ctors: Table<Ctor, CtorInfo>,
    portdefs: Table<PortDef, PortDefInfo>,
    channels: Table<Channel, ChannelInfo>,
    components: Table<Component, ComponentInfo>,
    exprroots: Table<ExprRoot, ExprRootInfo>,
}

#[derive(Default, Clone, Debug)]
struct PackageInfo {
    name: String,
    source: PackageSource,
    ast: Ready<Ast>,
}

#[derive(Default, Clone, Debug)]
struct ItemInfo {
    name: String,
    package: Ready<Id<Package>>,
    ast: Ready<Ast>,
    kind: Ready<ItemKind>,
    deps: Ready<Vec<Id<Item>>>,
}

#[derive(Default, Clone, Debug)]
struct BuiltinDefInfo {
    item: Ready<Id<Item>>,
}

#[derive(Default, Clone, Debug)]
struct StructDefInfo {
    item: Ready<Id<Item>>,
    fields: Ready<Vec<Id<Field>>>,
}

#[derive(Default, Clone, Debug)]
struct FieldInfo {
    structdef: Ready<Id<StructDef>>,
    name: String,
    typ: Ready<Type>,
}

#[derive(Default, Clone, Debug)]
struct UnionDefInfo {
    item: Ready<Id<Item>>,
    ctors: Ready<Vec<Id<Ctor>>>,
}

#[derive(Default, Clone, Debug)]
struct CtorInfo {
    uniondef: Ready<Id<UnionDef>>,
    name: String,
    sig: Ready<CtorSig>,
}

#[derive(Default, Clone, Debug)]
struct PortDefInfo {
    item: Ready<Id<Item>>,
    channels: Ready<Vec<Id<Channel>>>,
}

#[derive(Default, Clone, Debug)]
struct ChannelInfo {
    portdef: Ready<Id<PortDef>>,
    name: String,
    typ: Ready<Type>,
    dir: Ready<ChannelDir>,
}

#[derive(Default, Clone, Debug)]
struct ComponentInfo {
    moddef: Ready<Id<ModDef>>,
    path: Vec<String>,
    typ: Ready<Type>,
    is_reg: Ready<bool>,
}

#[derive(Default, Clone, Debug)]
struct ExprRootInfo {
    moddef: Ready<Id<ModDef>>,
    ast: Ready<Ast>,
    expr: Ready<Arc<Expr>>,
    typ: Ready<Type>,
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

    pub fn check(&mut self) -> Result<(), VirErrs> {
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
        self.register_builtindefs();
        self.register_structdefs();
        self.register_uniondefs();

        self.register_portdefs();
        self.register_components();

        self.register_exprroots();

        self.errors.clone().check()
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

    fn resolve_component(&self, path: &str, in_moddef: Id<ModDef>) -> Result<Id<Component>, VirErr> {
        if let Some(component) = self.components.resolve(&format!("{in_moddef}::{path}")) {
            Ok(component)
        } else {
            Err(VirErr::Other(format!("Unable to resolve component: {path} in {in_moddef}")))
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
    fn register_builtindefs(&mut self) {
        let builtindefs = self.items_by_kind(ItemKind::BuiltinDef);
        for item in builtindefs {
            let builtindef: Id<BuiltinDef> = item.cast();
            let builtindef_info = self.builtindefs.register(builtindef);
            builtindef_info.item.set(item);
        }
    }

    fn register_structdefs(&mut self) {
        let structdefs = self.items_by_kind(ItemKind::StructDef);
        for item in structdefs {
            let structdef: Id<StructDef> = item.cast();
            let structdef_info = self.structdefs.register(structdef);
            structdef_info.item.set(item);

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

            let structdef_info = &mut self.structdefs[structdef];
            structdef_info.fields.set(fields);
        }
    }

    fn register_uniondefs(&mut self) {
        let uniondefs = self.items_by_kind(ItemKind::UnionDef);
        for item in uniondefs {
            let uniondef: Id<UnionDef> = item.cast();
            let uniondef_info = self.uniondefs.register(uniondef);
            uniondef_info.item.set(item);

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

            let uniondef_info = &mut self.uniondefs[uniondef];
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
        Type::builtindef(self.builtindefs.resolve("builtin::Clock").unwrap(), None)
    }
}


////////////////////////////////////////////////////////////////////////////////
// PortDefs
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_portdefs(&mut self) {
        let portdefs = self.items_by_kind(ItemKind::PortDef);
        for item in portdefs {
            let portdef: Id<PortDef> = item.cast();
            let portdef_info = self.portdefs.register(portdef);
            portdef_info.item.set(item);

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

            let portdef_info = &mut self.portdefs[portdef];
            portdef_info.channels.set(channels);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Components and Expressions
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_components(&mut self) {
        let moddefs = self.moddefs();

        for moddef in moddefs {
            let moddef_ast = self.items[moddef.as_item()].ast.unwrap().child(0);
            for node in moddef_ast.children() {
                if node.is_statement() {
                    if node.child(0).is_component() {
                        let component_name = node.name().unwrap();
                        let component_typ_ast = node.typ().unwrap();
                        eprintln!("  COMPONENT: {component_name:?} : {:?}", component_typ_ast.summary());

                        let component: Id<Component> = Id::new(format!("{moddef}::{component_name}"));
                        let component_info = self.components.register(component);

                        component_info.moddef.set(moddef);
                        component_info.path = vec![component_name.to_string()];
                        component_info.is_reg.set(node.child(0).is_reg());
                        if let Ok(typ) = self.resolve_type(component_typ_ast, moddef.as_item()) {
                            let component_info = &mut self.components[component];
                            component_info.typ.set(typ);
                        }
                    } else if node.child(0).is_submodule() {
                        let submodule_name = node.name().unwrap();
                        let submodule_moddef_name = node.of().unwrap();
                        match self.resolve_moddef(submodule_moddef_name, moddef.as_item()) {
                            Ok(submodule_moddef) => {
                                self.register_submodule_components(submodule_name, submodule_moddef, moddef);
                            },
                            Err(err) => {
                                self.errors.add(err);
                            },
                        }
                    } else if node.child(0).is_port() {
                        let port_name = node.name().unwrap();
                        let port_portdef_name = node.of().unwrap();
                        match self.resolve_portdef(port_portdef_name, moddef.as_item()) {
                            Ok(portdef) => {
                            let path = vec![port_name.to_string()];
                                self.register_port_components(path, portdef, moddef);
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
        }
    }

    fn register_submodule_components(&mut self, name: &str, submodule_moddef: Id<ModDef>, in_moddef: Id<ModDef>) {
        eprintln!("  mod {name} of {submodule_moddef}");
        let moddef_ast = self.items[submodule_moddef.as_item()].ast.unwrap().child(0);
        for node in moddef_ast.children() {
            if node.is_statement() {
                let statement = node.child(0);
                if statement.is_component() {
                    if statement.is_implicit() || statement.is_incoming() || statement.is_outgoing() {
                        let component_name = statement.name().unwrap();
                        let component_typ_ast = statement.typ().unwrap();

                        let component: Id<Component> = Id::new(format!("{in_moddef}::{name}.{component_name}"));
                        eprintln!("  COMPONENT: {component:?} : {:?}", component_typ_ast.summary());

                        let component_info = self.components.register(component);
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
                            self.register_port_components(path, portdef, in_moddef);
                        },
                        Err(err) => self.errors.add(err),
                    }
                }
            }
        }
    }

    fn register_port_components(&mut self, path: Vec<String>, portdef: Id<PortDef>, in_moddef: Id<ModDef>) {
        let portdef_info = &self.portdefs[portdef];
        if let Ok(channels) = portdef_info.channels.get() {
            for channel in channels {
                let channel_info = &self.channels[*channel];

                let channel_name = channel_info.name.clone();
                let channel_typ = channel_info.typ.unwrap();

                let name = format!("{}.{}", path.join("."), channel_name);
                let component: Id<Component> = Id::new(format!("{in_moddef}::{name}"));
                eprintln!("  COMPONENT: {component:?} : {:?}", channel_typ);

                let component_info = self.components.register(component);
                component_info.moddef.set(in_moddef);

                let mut path = path.clone();
                path.push(channel_name);
                component_info.path = path;

                component_info.is_reg.set(false);
                component_info.typ.set(*channel_typ);
            }
        }
    }

    fn register_exprroots(&mut self) {
        let clock_type = self.clock_type();
        let moddefs = self.moddefs();
        for moddef in moddefs {
            let mut i = 0;
            let moddef_ast = self.items[moddef.as_item()].ast.unwrap().child(0);
            for node in moddef_ast.children() {
                if node.is_statement() {
                    if node.child(0).is_driver() {
                        let expr_id: Id<ExprRoot> = Id::new(format!("{moddef}::expr[{i}]"));
                        i += 1;

                        let driver_ast = node.child(0);
                        let target_path = driver_ast.target().unwrap();
                        let drivertype = if driver_ast.drivertype().unwrap() == "<=" {
                            DriverType::Latched
                        } else {
                            DriverType::Continuous
                        };

                        let expr_ast = driver_ast.clone().expr().unwrap();
                        let exprroot_info = self.exprroots.register(expr_id);
                        exprroot_info.ast.set(expr_ast.clone());
                        exprroot_info.moddef.set(moddef);

                        let expr_ast = driver_ast.clone().expr().unwrap();
                        let component = self.resolve_component(target_path, moddef).unwrap();
                        let component_info = &self.components[component];

                        match drivertype {
                            DriverType::Continuous if *component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {moddef}"))),
                            DriverType::Latched if !*component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {moddef}"))),
                            _ => (),
                        }

                        let expr = Expr::from_ast(expr_ast);
                        let exprroot_info = self.exprroots.register(expr_id);
                        exprroot_info.expr.set(expr);
                        exprroot_info.typ.set(*component_info.typ.unwrap());
                    } else if node.child(0).is_reg() {
                        let reg_ast = node.child(0);
                        let expr_ast = reg_ast.clone().expr().unwrap();

                        let expr_id: Id<ExprRoot> = Id::new(format!("{moddef}::expr[{i}]"));
                        i += 1;

                        let exprroot_info = &mut self.exprroots.register(expr_id);
                        exprroot_info.ast.set(expr_ast.clone());
                        exprroot_info.typ.set(clock_type)
                    }
                }
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// For testing
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    #[cfg(test)]
    fn items(&self) -> Vec<Id<Item>> {
        self.items.keys().cloned().collect()
    }

    fn moddefs(&self) -> Vec<Id<ModDef>> {
        let mut results = vec![];
        for item in self.items.keys() {
            if let Ok(item_ast) = &self.items[*item].ast.get() {
                if let Some(ItemKind::ModDef) = item_ast.item_kind() {
                    results.push(item.cast());
                }
            }
        }
        results
    }
}

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
        }

        writeln!(f, "BUILTINDEFS:")?;
        for (builtindef, _builtindef_info) in self.builtindefs.iter() {
            writeln!(f, "    {builtindef}")?;
        }

        writeln!(f, "STRUCTDEFS:")?;
        for (structdef, structdef_info) in self.structdefs.iter() {
            writeln!(f, "    {structdef}")?;
            writeln!(f, "    fields: {:?}", structdef_info.fields)?;
        }

        writeln!(f, "FIELDS:")?;
        for (field, field_info) in self.fields.iter() {
            writeln!(f, "    {field}")?;
            writeln!(f, "        structdef: {:?}", field_info.structdef)?;
            writeln!(f, "        name: {:?}", field_info.name)?;
            writeln!(f, "        typ: {:?}", field_info.typ)?;
        }

        writeln!(f, "UNIONDEFS:")?;
        for (uniondef, uniondef_info) in self.uniondefs.iter() {
            writeln!(f, "    {uniondef}")?;
            writeln!(f, "    ctors: {:?}", uniondef_info.ctors)?;
        }

        writeln!(f, "CTORS:")?;
        for (ctor, ctor_info) in self.ctors.iter() {
            writeln!(f, "    {ctor}")?;
            writeln!(f, "        uniondef: {:?}", ctor_info.uniondef)?;
            writeln!(f, "        name: {:?}", ctor_info.name)?;
            writeln!(f, "        sig: {:?}", ctor_info.sig)?;
        }

        writeln!(f, "PORTDEFS:")?;
        for (portdef, portdef_info) in self.portdefs.iter() {
            writeln!(f, "    {portdef}")?;
            writeln!(f, "    channels: {:?}", portdef_info.channels)?;
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
            writeln!(f, "        typ: {:?}", component_info.typ)?;
            writeln!(f, "        is_reg: {:?}", component_info.is_reg)?;
        }

        writeln!(f, "EXPRROOTS:")?;
        for (exprroot, exprroot_info) in self.exprroots.iter() {
            writeln!(f, "    {exprroot}")?;
            writeln!(f, "        ast: {:?}", exprroot_info.ast.get().map(|ast| ast.summary()))?;
            writeln!(f, "        expr: (omitted)")?;
            writeln!(f, "        typ: {:?}", exprroot_info.typ)?;
        }

        Ok(())
    }
}

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

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ChannelDir {
    Mosi,
    Miso,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub enum PackageSource {
    #[default]
    Builtin,
    File(std::path::PathBuf),
}
