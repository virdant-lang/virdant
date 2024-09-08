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
mod loader;

#[cfg(test)]
mod tests;

use ast::expr::Path;
pub use common::*;
use cycle::detect_cycle;
use indexmap::IndexMap;
use indexmap::IndexSet;
use internment::Intern;
use loader::Loader;
use location::Span;
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
use crate::types::FnSig;


////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

/// A [`Virdant`] is a context type for manipulating Virdant designs.
/// Call [`check()`](Virdant::check) to get a list of errors in a design.
pub struct Virdant {
    pub(crate) errors: VirErrs,

    pub(crate) loader: Loader,

    pub(crate) packages: Table<Package, PackageInfo>,
    pub(crate) items: Table<Item, ItemInfo>,
    pub(crate) fields: Table<Field, FieldInfo>,
    pub(crate) ctors: Table<Ctor, CtorInfo>,
    pub(crate) enumerants: Table<Enumerant, EnumerantInfo>,
    pub(crate) channels: Table<Channel, ChannelInfo>,
    pub(crate) components: Table<Component, ComponentInfo>,
    pub(crate) exprroots: Table<ExprRoot, ExprRootInfo>,
    pub(crate) submodules: Table<Submodule, SubmoduleInfo>,
    pub(crate) sockets: Table<Socket, SocketInfo>,
}

////////////////////////////////////////////////////////////////////////////////
// Public Virdant API
////////////////////////////////////////////////////////////////////////////////
impl Virdant {
    pub fn new<P>(top_path: P) -> Virdant
        where P: AsRef<std::path::Path> {
        let top_name = top_path.as_ref().file_stem().unwrap().to_string_lossy();
        let parent_dir = top_path.as_ref().parent().unwrap().to_path_buf();
        let mut loader = Loader::new(parent_dir);

        loader.load(&top_name);

        let mut virdant = Virdant {
            errors: Default::default(),
            loader,
            packages: Default::default(),
            items: Default::default(),
            fields: Default::default(),
            ctors: Default::default(),
            enumerants: Default::default(),
            channels: Default::default(),
            components: Default::default(),
            exprroots: Default::default(),
            submodules: Default::default(),
            sockets: Default::default(),
        };

        if let Err(errs) = virdant.loader.errors() {
            virdant.errors.extend(errs);
        }

        virdant.register_packages();
        virdant
    }

    pub fn check(&mut self) -> Result<design::Design, VirErrs> {
        self.init_package_asts();
        self.register_items();

        let packages: Vec<_> = self.packages.keys().cloned().collect();
        for package in packages {
            if let Err(errs) = self.check_no_duplicate_imports(package) {
                self.errors.extend(errs)
            }
        }

        self.errors.check()?;

        let items: Vec<_> = self.items.keys().cloned().collect();
        for item in items {
            let item_deps = self.item_deps(item).clone();
            let item_info = self.items.get_mut(item).unwrap();
            item_info.deps.set(item_deps);
        }

        self.check_no_item_dep_cycles();

        self.register_fields();
        self.register_ctors();
        self.register_enumerants();
        self.register_channels();
        self.register_components();

        self.register_submodules();
        self.register_sockets();

        self.set_fn_sigs();
        self.register_exprroots();
        self.typecheck();
        self.drivercheck();

        self.errors.clone().check()?;
        Ok(self.design())
    }
}


////////////////////////////////////////////////////////////////////////////////
// Packages and Items
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_packages(&mut self) {
        let sources = self.loader.sources();
        let package: Id<Package> = Id::new("builtin");
        let package_info = self.packages.register(package);
        package_info.name = "builtin".to_string();
        package_info.source = PackageSource::Builtin;

        for (package_name, package_path) in sources {
            let package: Id<Package> = Id::new(package_name.clone());
            let package_info = self.packages.register(package);
            package_info.name = package_name;
            package_info.source = package_path;
        }
    }

    fn init_package_asts(&mut self) {
        let packages: Vec<_> = self.packages.keys().cloned().collect();
        for package in packages {
            match self.package_text(package) {
                Err(err) => {
                    self.errors.add(err)
                },
                Ok(text) => {
                    let result: Result<Ast, _> = parse::parse_package(&text);
                    match result {
                        Ok(package_ast) => {
                            let package_info = &mut self.packages[package];
                            package_info.ast.set(package_ast.clone());
                        },
                        Err(err) => {
                            let source = self.packages[package].source.clone();
                            self.errors.add(VirErr::Parse(source, err));
                        },
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
        } else if kind == ItemKind::EnumDef {
            item_info.width.set(item_ast.width().unwrap());
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
                } else if item_ast.child(0).is_enumdef() {
                    self.item_deps_enumdef(item, item_ast.child(0))
                } else if item_ast.child(0).is_builtindef() {
                    (vec![], VirErrs::new())
                } else if item_ast.child(0).is_socketdef() {
                    self.item_deps_socketdef(item, item_ast.child(0))
                } else if item_ast.child(0).is_fndef() {
                    self.item_deps_fndef(item, item_ast.child(0))
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

    fn item_deps_socketdef(&self, item: Id<Item>, socketdef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        let mut results = IndexSet::new();
        for node in socketdef_ast.children() {
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

    fn item_deps_enumdef(&self, _item: Id<Item>, _enumdef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        (vec![], VirErrs::new())
    }

    fn item_deps_fndef(&self, item: Id<Item>, fndef_ast: Ast) -> (Vec<Id<Item>>, VirErrs) {
        let mut errors = VirErrs::new();
        let mut results = IndexSet::new();

        let ret_typ = fndef_ast.get("ret").unwrap();
        let (deps, errs) = self.item_deps_type(ret_typ, item);
        results.extend(deps);
        errors.extend(errs);

        for node in fndef_ast.get("args").unwrap().children() {
            let arg_typ = node.child(1);
            let (deps, errs) = self.item_deps_type(arg_typ, item);
            results.extend(deps);
            errors.extend(errs);
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

    fn resolve_fndef(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<FnDef>, VirErr> {
        let item = self.resolve_item(qualident, in_item)?;
        let item_info = &self.items[item];
        if let Ok(ItemKind::FnDef) = item_info.kind.get() {
            Ok(item.cast())
        } else {
            Err(VirErr::Other(format!("Unable to resolve fndef: {qualident} in {in_item}")))
        }
    }

    fn resolve_socketdef(&self, qualident: &str, in_item: Id<Item>) -> Result<Id<SocketDef>, VirErr> {
        let item = self.resolve_item(qualident, in_item)?;
        let item_info = &self.items[item];
        if let Ok(ItemKind::SocketDef) = item_info.kind.get() {
            Ok(item.cast())
        } else {
            Err(VirErr::Other(format!("Unable to resolve socketdef: {qualident} in {in_item}")))
        }
    }

    fn resolve_component(&self, path: &str, in_item: Id<Item>) -> Result<Id<Component>, VirErr> {
        if let Some(component) = self.components.resolve(&format!("{in_item}::{path}")) {
            Ok(component)
        } else {
            Err(VirErr::Other(format!("Unable to resolve component: {path} in {in_item}")))
        }
    }

    fn resolve_socket(&self, path: &str, in_item: Id<Item>) -> Result<Id<Socket>, VirErr> {
        if let Some(socket) = self.sockets.resolve(&format!("{in_item}::{path}")) {
            Ok(socket)
        } else {
            Err(VirErr::Other(format!("Unable to resolve socket: {path} in {in_item}")))
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

    fn register_enumerants(&mut self) {
        let enumdefs = self.items_by_kind(ItemKind::EnumDef);
        for item in enumdefs {
            let enumdef: Id<EnumDef> = item.cast();
            let mut enumerants = vec![];

            let item_info = &self.items[item];
            let enumdef_ast = item_info.ast.unwrap().child(0);
            for node in enumdef_ast.children() {
                if node.is_statement() {
                    let enumerant_name = node.name().unwrap();
                    let enumerant_value = node.value().unwrap();

                    let enumerant: Id<Enumerant> = Id::new(format!("{item}::{enumerant_name}"));

                    let enumerant_info = self.enumerants.register(enumerant);
                    enumerant_info.enumdef.set(enumdef);
                    enumerant_info.name = enumerant_name.to_string();
                    enumerant_info.value = enumerant_value;
                    enumerant_info.width = *item_info.width.unwrap();

                    enumerants.push(enumerant);
                }
            }

            let enumdef_info = &mut self.items[item];
            enumdef_info.enumerants.set(enumerants);
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
            ItemKind::EnumDef => {
                if width.is_some() {
                    return Err(VirErr::KindError(format!("Struct definition {item} does not take a generic")));
                }
                Ok(Type::enumdef(item.cast()))
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
// SocketDefs
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn register_channels(&mut self) {
        let socketdefs = self.items_by_kind(ItemKind::SocketDef);
        for item in socketdefs {
            let socketdef: Id<SocketDef> = item.cast();
            let mut channels = vec![];

            let item_info = &self.items[item];
            let socketdef_ast = item_info.ast.unwrap().child(0);
            for node in socketdef_ast.children() {
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
                    channel_info.socketdef.set(socketdef);
                    channel_info.name = channel_name.to_string();
                    channel_info.typ.set(channel_type);
                    channel_info.dir.set(channel_dir);

                    channels.push(channel);
                }
            }

            let socketdef_info = &mut self.items[item];
            socketdef_info.channels.set(channels);
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
                        } else if node.child(0).is_wire() {
                            component_info.class.set(ComponentClass::Wire);
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
                    } else if node.child(0).is_socket() {
                        let socket_name = node.name().unwrap();
                        let socket_socketdef_name = node.of().unwrap();
                        match self.resolve_socketdef(socket_socketdef_name, item) {
                            Ok(socketdef) => {
                                let path = vec![socket_name.to_string()];
                                let role = if node.role().unwrap() == "master" {
                                    SocketRole::Master
                                } else {
                                    SocketRole::Slave
                                };
                                components.extend(self.register_socket_components(path, socketdef, moddef, false, role));
                            },
                            Err(err) => self.errors.add(err),
                        }
                    } else if node.child(0).is_driver() {
                        ()
                    } else if node.child(0).is_socket_driver() {
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
                } else if statement.is_socket() {
                    let socket_name = statement.name().unwrap();
                    let socket_socketdef_name = statement.of().unwrap();
                    match self.resolve_socketdef(socket_socketdef_name, submodule_moddef.as_item()) {
                        Ok(socketdef) => {
                            let path = vec![name.to_string(), socket_name.to_string()];
                            let role = if statement.role().unwrap() == "master" {
                                SocketRole::Master
                            } else {
                                SocketRole::Slave
                            };
                            components.extend(self.register_socket_components(path, socketdef, in_moddef, true, role));
                        },
                        Err(err) => self.errors.add(err),
                    }
                }
            }
        }
        components
    }

    fn register_socket_components(
        &mut self,
        path: Vec<String>,
        socketdef: Id<SocketDef>,
        in_moddef: Id<ModDef>,
        is_submodule: bool,
        role: SocketRole,
    ) -> Vec<Id<Component>> {
        let mut components = vec![];
        let socketdef_info = &self.items[socketdef.as_item()];
        if let Ok(channels) = socketdef_info.channels.get() {
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
                let flow = match (channel_dir, role, is_submodule) {
                    (ChannelDir::Mosi, SocketRole::Slave,  true)  => Flow::Sink,
                    (ChannelDir::Mosi, SocketRole::Master, true)  => Flow::Source,
                    (ChannelDir::Miso, SocketRole::Slave,  true)  => Flow::Source,
                    (ChannelDir::Miso, SocketRole::Master, true)  => Flow::Sink,
                    (ChannelDir::Mosi, SocketRole::Slave,  false) => Flow::Source,
                    (ChannelDir::Mosi, SocketRole::Master, false) => Flow::Sink,
                    (ChannelDir::Miso, SocketRole::Slave,  false) => Flow::Sink,
                    (ChannelDir::Miso, SocketRole::Master, false) => Flow::Source,
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

    fn set_fn_sigs(&mut self) {
        let items = self.items_by_kind(ItemKind::FnDef);
        'outer: for item in items {
            let fndef : Id<FnDef> = item.cast();
            let fndef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };

            let ret_typ_ast = fndef_ast.get("ret").unwrap();
            let ret_typ = match self.resolve_type(ret_typ_ast, item) {
                Err(err) => {
                    self.errors.add(err);
                    continue;
                },
                Ok(ret_typ) => ret_typ,
            };

            let mut arg_typs = vec![];
            for node in fndef_ast.get("args").unwrap().children() {
                let arg_name = node.get_as_str("name").unwrap().to_string();
                let arg_typ_ast = node.child(1);
                let arg_typ = match self.resolve_type(arg_typ_ast, item) {
                    Err(err) => {
                        self.errors.add(err);
                        continue 'outer;
                    },
                    Ok(arg_typ) => arg_typ,
                };
                arg_typs.push((arg_name, arg_typ));
            }

            let fndef_info = &mut self.items[item];
            fndef_info.sig.set(FnSig::new(fndef, arg_typs, ret_typ));
        }
    }

    fn register_exprroots(&mut self) {
        self.register_exprroots_moddefs();
        self.register_exprroots_fndefs();
    }

    fn register_exprroots_moddefs(&mut self) {
        let clock_type = self.clock_type();
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<Item> = item.cast();
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
                        let component = if let Ok(component) = self.resolve_component(target_path, item) {
                            component
                        } else {
                            continue
                        };

                        let component_info = &mut self.components[component];
                        let typ = if let Ok(typ) = component_info.typ.get() {
                            *typ
                        } else {
                            continue
                        };

                        match drivertype {
                            DriverType::Continuous if *component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {item}"))),
                            DriverType::Latched if !*component_info.is_reg.get().unwrap() => self.errors.add(VirErr::WrongDriverType(format!("{target_path} in {item}"))),
                            _ => (),
                        }

                        let exprroot = self.register_exprroots_for(ast::Expr::from_ast(expr_ast.clone()), item, &vec![expr_i]);
                        expr_i += 1;

                        let exprroot_info = &mut self.exprroots[exprroot];
                        exprroot_info.span = Some(span);
                        exprroot_info.expected_typ = Some(typ);

                        let component_info = &mut self.components[component];
                        if component_info.driver.is_some() {
                            self.errors.add(VirErr::MultipleDrivers(format!("{component:?}")));
                        } else {
                            component_info.driver = Some(exprroot);
                        }
                    } else if node.child(0).is_socket_driver() {
                        self.register_exprroots_for_socket_driver(node, moddef, &mut expr_i);
                    } else if node.child(0).is_reg() {
                        let component = if let Ok(component) = self.resolve_component(node.name().unwrap(), item) {
                            component
                        } else {
                            continue
                        };

                        let reg_ast = node.child(0);
                        let expr_ast = if let Some(expr_ast) = reg_ast.clone().expr() {
                            expr_ast
                        } else {
                            self.errors.add(VirErr::NoClock(format!("{component}")));
                            continue;
                        };

                        let exprroot = self.register_exprroots_for(ast::Expr::from_ast(expr_ast.clone()), item, &vec![expr_i]);
                        expr_i += 1;

                        let exprroot_info = &mut self.exprroots[exprroot];
                        exprroot_info.expected_typ = Some(clock_type);
                        let span = reg_ast.span();
                        exprroot_info.span = Some(span);

                        let component = if let Ok(component) = self.resolve_component(node.name().unwrap(), item) {
                            component
                        } else {
                            continue
                        };
                        let component_info = &mut self.components[component];
                        component_info.clock.set(exprroot);
                    }
                }
            }
        }
    }

    fn register_exprroots_fndefs(&mut self) {
        let items = self.items_by_kind(ItemKind::FnDef);
        for item in items {
            let fndef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };
            let expr_ast = fndef_ast.get("body").unwrap();
            let exprroot = self.register_exprroots_for(ast::Expr::from_ast(expr_ast.clone()), item, &vec![0]);
            let exprroot_info = &mut self.exprroots[exprroot];
            exprroot_info.span = Some(expr_ast.span());
            let fnsig = self.items[item].sig.unwrap().clone();
            exprroot_info.expected_typ = Some(fnsig.ret());
            let fndef_info = &mut self.items[item];
            fndef_info.body.set(exprroot);
        }
    }

    fn register_exprroots_for_socket_driver(&mut self, node: Ast, moddef: Id<Item>, expr_i: &mut usize) {
        let send_socket_name = node.socket_send().unwrap();
        let recv_socket_name = node.socket_recv().unwrap();

        let send_socket = match self.resolve_socket(send_socket_name, moddef) {
            Ok(socket) => socket,
            Err(e) => {
                self.errors.add(e);
                return;
            },
        };

        let recv_socket = match self.resolve_socket(recv_socket_name, moddef) {
            Ok(socket) => socket,
            Err(e) => {
                self.errors.add(e);
                return;
            },
        };

        let send_socket_info = self.sockets[send_socket].clone();
        let recv_socket_info = self.sockets[recv_socket].clone();

        let send_socketdef = send_socket_info.socketdef.unwrap();
        let recv_socketdef = recv_socket_info.socketdef.unwrap();

        if send_socketdef != recv_socketdef {
            self.errors.add(VirErr::Other(format!("Port defs don't match: {send_socket_name} is {send_socketdef} while {recv_socket_name} is {recv_socketdef}.")));
            return;
        }

        match (recv_socket_info.role, recv_socket_info.perspective, send_socket_info.role, send_socket_info.perspective) {
            // GOOD
            (SocketRole::Master, Perspective::Interior, SocketRole::Master, Perspective::Exterior) |
            (SocketRole::Master, Perspective::Interior, SocketRole::Slave,  Perspective::Interior) |
            (SocketRole::Slave,  Perspective::Exterior, SocketRole::Slave,  Perspective::Interior) |
            (SocketRole::Slave,  Perspective::Exterior, SocketRole::Master, Perspective::Exterior) |
            (SocketRole::Slave,  Perspective::Exterior, SocketRole::Master, Perspective::Interior) => (),

            // WRONG ORDER
            (SocketRole::Master, Perspective::Exterior, SocketRole::Slave,  Perspective::Exterior) |
            (SocketRole::Slave,  Perspective::Interior, SocketRole::Master, Perspective::Exterior) |
            (SocketRole::Slave,  Perspective::Interior, SocketRole::Master, Perspective::Interior) |
            (SocketRole::Slave,  Perspective::Interior, SocketRole::Slave,  Perspective::Exterior) |
            (SocketRole::Master, Perspective::Exterior, SocketRole::Master, Perspective::Interior) => {
                self.errors.add(VirErr::Other(format!("Ports are in the wrong order")));
                return;
            }

            // BAD
            (SocketRole::Master, Perspective::Exterior, SocketRole::Master, Perspective::Exterior) => todo!(),
            (SocketRole::Master, Perspective::Interior, SocketRole::Master, Perspective::Interior) => todo!(),
            (SocketRole::Slave,  Perspective::Exterior, SocketRole::Slave,  Perspective::Exterior) => todo!(),
            (SocketRole::Slave,  Perspective::Interior, SocketRole::Slave,  Perspective::Interior) => todo!(),
            (SocketRole::Master, Perspective::Exterior, SocketRole::Slave,  Perspective::Interior) => todo!(),
            (SocketRole::Master, Perspective::Interior, SocketRole::Slave,  Perspective::Exterior) => {
                self.errors.add(VirErr::Other(format!("Can't connect these two ports together")));
                return;
            }
        }

        let socketdef_info = self.items[send_socketdef.as_item()].clone();

        let channels = socketdef_info.channels.unwrap();
        for channel in channels {
            let channel_info = &self.channels[*channel];
            let send_component: Id<Component> = Id::new(format!("{moddef}::{}.{}", send_socket_info.path.join("."), &channel_info.name));
            let recv_component: Id<Component> = Id::new(format!("{moddef}::{}.{}", recv_socket_info.path.join("."), &channel_info.name));

            let typ = channel_info.typ.unwrap().clone();
            match channel_info.dir.unwrap() {
                ChannelDir::Mosi => {
                    let reference_path = format!("{}.{}", send_socket_info.path.join("."), &channel_info.name);
                    let exprroot = self.register_exprroots_synthetic_reference(node.span(), &reference_path, moddef, typ, expr_i);
                    let recv_component_info = &mut self.components[recv_component];
                    if recv_component_info.driver.is_some() {
                        self.errors.add(VirErr::MultipleDrivers(format!("{recv_component:?}")));
                    } else {
                        recv_component_info.driver = Some(exprroot);
                    }
                },
                ChannelDir::Miso => {
                    let reference_path = format!("{}.{}", recv_socket_info.path.join("."), &channel_info.name);
                    let exprroot = self.register_exprroots_synthetic_reference(node.span(), &reference_path, moddef, typ, expr_i);
                    let send_component_info = &mut self.components[send_component];
                    if send_component_info.driver.is_some() {
                        self.errors.add(VirErr::MultipleDrivers(format!("{send_component:?}")));
                    } else {
                        send_component_info.driver = Some(exprroot);
                    }
                },
            }
        }
    }

    fn register_exprroots_synthetic_reference(&mut self, span: Span, path: &str, item: Id<Item>, typ: Type, expr_i: &mut usize) -> Id<ExprRoot> {
        let ref_path: Path = path.split(".").map(|part| Intern::new(part.to_string())).collect::<Vec<_>>();
        let expr = Arc::new(ast::Expr::Reference(span, ref_path));
        let exprroot_id: Id<ExprRoot> = Self::exprroot_id(item, &[*expr_i]);
        *expr_i += 1;

        let exprroot_info = self.exprroots.register(exprroot_id);
        exprroot_info.ast.set(expr.clone());
        exprroot_info.span = Some(expr.span());
        exprroot_info.children = vec![];
        exprroot_info.item.set(item);
        exprroot_info.expected_typ = Some(typ);
        exprroot_info.synthetic = true;

        exprroot_id
    }

    fn exprroot_id(moddef: Id<Item>, path: &[usize]) -> Id<ExprRoot> {
        let mut parts = vec![format!("{moddef}::expr")];
        for p in path {
            parts.push(format!("[{p}]"));
        }
        Id::new(parts.join(""))
    }

    fn register_exprroots_for(&mut self, expr: Arc<ast::Expr>, item: Id<Item>, path: &[usize]) -> Id<ExprRoot> {
        let exprroot_id: Id<ExprRoot> = Self::exprroot_id(item, path);
        let mut subexprs = vec![];

        for (i, e) in expr.subexprs().iter().enumerate() {
            let subexpr = self.register_exprroots_for(e.clone(), item, &Self::extend(path, i));
            let info = &mut self.exprroots[subexpr];
            info.parent = Some(exprroot_id);
            subexprs.push(subexpr);
        }

        let exprroot_info = self.exprroots.register(exprroot_id);


        exprroot_info.ast.set(expr.clone());
        exprroot_info.span = Some(expr.span());
        exprroot_info.children = subexprs;
        exprroot_info.item.set(item);

        exprroot_id
    }

    fn extend(path: &[usize], p: usize) -> Vec<usize> {
        let mut result = path.to_vec();
        result.push(p);
        result
    }

    fn register_submodules(&mut self) {
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<Item> = item.cast();
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

                    let submodule_moddef: Id<ModDef> = self.resolve_moddef(submodule_of, item).unwrap().cast();

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

    fn register_sockets(&mut self) {
        let items = self.items_by_kind(ItemKind::ModDef);
        for item in items {
            let moddef: Id<ModDef> = item.cast();
            let moddef_ast = if let Ok(item_ast) = self.items[item].ast.get() {
                item_ast.child(0)
            } else {
                continue;
            };

            let mut sockets = vec![];

            for node in moddef_ast.children() {
                if node.is_statement() && node.child(0).is_socket() {
                    let socket_ast = node.child(0);
                    let socket_name = socket_ast.name().unwrap();
                    let socket_of = socket_ast.of().unwrap();

                    let socketdef: Id<SocketDef> = if let Ok(socketdef) = self.resolve_item(socket_of, item) {
                        socketdef.cast()
                    } else {
                        continue;
                    };

                    let socket_role = if node.get_as_str("role").unwrap() == "master" {
                        SocketRole::Master
                    } else {
                        SocketRole::Slave
                    };

                    let socket_id: Id<Socket> = Id::new(format!("{moddef}::{socket_name}"));
                    let socket_info = self.sockets.register(socket_id);
                    socket_info.moddef.set(item.cast());
                    socket_info.path = vec![socket_name.to_string()];
                    socket_info.role = socket_role;
                    socket_info.perspective = Perspective::Interior;
                    socket_info.socketdef.set(socketdef);
                    sockets.push(socket_id);
                } else if node.is_statement() && node.child(0).is_submodule() {
                    let submodule_name = node.name().unwrap();
                    let submodule_moddef_name = node.of().unwrap();
                    match self.resolve_moddef(submodule_moddef_name, item) {
                        Ok(submodule_moddef) => {
                            sockets.extend(self.register_submodule_sockets(submodule_name, submodule_moddef, moddef));
                        },
                        Err(err) => {
                            self.errors.add(err);
                        },
                    }
                }
            }
            self.items[item].sockets.set(sockets);
        }
    }

    fn register_submodule_sockets(&mut self, submodule_name: &str, submodule_moddef: Id<ModDef>, moddef: Id<ModDef>) -> Vec<Id<Socket>> {
        let mut sockets: Vec<Id<Socket>> = vec![];

        let moddef_ast = if let Ok(item_ast) = self.items[submodule_moddef.as_item()].ast.get() {
            item_ast.child(0)
        } else {
            return vec![];
        };

        for node in moddef_ast.children() {
            if node.is_statement() {
                let statement = node.child(0);
                if statement.is_socket() {
                    let socket_name = statement.name().unwrap();
                    let socket_socketdef_name = statement.of().unwrap();
                    let socketdef_result = self.resolve_socketdef(socket_socketdef_name, submodule_moddef.as_item());
                    match socketdef_result {
                        Ok(socketdef) => {
                            let socket_id: Id<Socket> = Id::new(format!("{moddef}::{submodule_name}.{socket_name}"));
                            let socket_info = self.sockets.register(socket_id);

                            let socket_role = if statement.role().unwrap() == "master" {
                                SocketRole::Master
                            } else {
                                SocketRole::Slave
                            };

                            socket_info.moddef.set(moddef);
                            socket_info.path = vec![submodule_name.to_string(), socket_name.to_string()];
                            socket_info.role = socket_role;
                            socket_info.perspective = Perspective::Exterior;
                            socket_info.socketdef.set(socketdef);

                            sockets.push(socket_id);
                        },
                        Err(err) => self.errors.add(err),
                    }
                }
            }
        }
        sockets
    }
}


////////////////////////////////////////////////////////////////////////////////
// Typechecking
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn typecheck(&mut self) {
        let roots: Vec<(_, _)> = self.exprroots.iter().map(|(id, info)| (id.clone(), info.clone())).collect();
        for (exprroot, exprroot_info) in roots {
            if exprroot_info.parent.is_none() {
                let item = *exprroot_info.item.unwrap();
                let mut typing_context = TypingContext::new(self, item);
                if let Err(err) = typing_context.check(exprroot, exprroot_info.expected_typ.unwrap()) {
                    self.errors.add(err);
                }
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Driver Checking
////////////////////////////////////////////////////////////////////////////////

impl Virdant {
    fn drivercheck(&mut self) {
        let components = self.components.iter();
        for (component, component_info) in components {
            let moddef_info = &self.items[component_info.moddef.unwrap().as_item()];
            if *moddef_info.is_ext.unwrap() {
                continue;
            }

            match *component_info.flow.unwrap() {
                Flow::Source => (),
                Flow::Sink | Flow::Duplex => {
                    if component_info.driver.is_none() {
                        self.errors.add(VirErr::NoDriver(format!("{component:?}")));
                    }
                },
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

        writeln!(f, "ENUMS:")?;
        for (enumerant, enumerant_info) in self.enumerants.iter() {
            writeln!(f, "    {enumerant}")?;
            writeln!(f, "        enumdef: {:?}", enumerant_info.enumdef)?;
            writeln!(f, "        name: {:?}", enumerant_info.name)?;
            writeln!(f, "        value: {:?}", enumerant_info.value)?;
            writeln!(f, "        width: {:?}", enumerant_info.width)?;
        }

        writeln!(f, "CHANNELS:")?;
        for (channel, channel_info) in self.channels.iter() {
            writeln!(f, "    {channel}")?;
            writeln!(f, "        socketdef: {:?}", channel_info.socketdef)?;
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
            writeln!(f, "        clock: {:?}", component_info.clock)?;
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
            writeln!(f, "        synthetic: {}", exprroot_info.synthetic)?;
            if let Some(component) = exprroot_info.reference_component {
                writeln!(f, "        component: {component:?}", )?;
            }
        }

        writeln!(f, "SUBMODULES:")?;
        for (submodule, submodule_info) in self.submodules.iter() {
            writeln!(f, "    {submodule}")?;
            writeln!(f, "        name: {}", submodule_info.name)?;
            writeln!(f, "        submodule_moddef: {:?}", submodule_info.submodule_moddef)?;
            writeln!(f, "        moddef: {:?}", submodule_info.moddef)?;
        }

        writeln!(f, "SOCKETS:")?;
        for (socket, socket_info) in self.sockets.iter() {
            writeln!(f, "    {socket}")?;
            writeln!(f, "        path: {}", socket_info.path.join("."))?;
            writeln!(f, "        socketdef: {:?}", socket_info.socketdef)?;
            writeln!(f, "        moddef: {:?}", socket_info.moddef)?;
            writeln!(f, "        role: {:?}", socket_info.role)?;
            writeln!(f, "        perspective: {:?}", socket_info.perspective)?;
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
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
}

impl ItemKind {
    pub fn is_typedef(&self) -> bool {
        match self {
            ItemKind::ModDef => false,
            ItemKind::UnionDef => true,
            ItemKind::StructDef => true,
            ItemKind::EnumDef => true,
            ItemKind::BuiltinDef => true,
            ItemKind::FnDef => false,
            ItemKind::SocketDef => false,
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

        let mut sockets: IndexMap<Id<Socket>, design::Socket> = IndexMap::new();
        for socket in self.sockets.keys() {
            let info = self.sockets[*socket].clone();
            let design_socket = design::Socket { root: OnceCell::new(), info };
            sockets.insert(*socket, design_socket);
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

        let mut enumerants: IndexMap<Id<Enumerant>, design::Enumerant> = IndexMap::new();
        for enumerant in self.enumerants.keys() {
            enumerants.insert(*enumerant, self.make_design_enumerants(*enumerant));
        }

        let root = Arc::new(design::DesignRoot {
            packages,
            items,
            components,
            submodules,
            sockets,
            exprroots,
            fields,
            ctors,
            enumerants,
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

        for socket in root.sockets.values() {
            socket.root.set(Arc::downgrade(&root)).unwrap();
        }

        for field in root.fields.values() {
            field.root.set(Arc::downgrade(&root)).unwrap();
        }

        for ctor in root.ctors.values() {
            ctor.root.set(Arc::downgrade(&root)).unwrap();
        }

        for enumerant in root.enumerants.values() {
            enumerant.root.set(Arc::downgrade(&root)).unwrap();
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

    fn make_design_enumerants(&self, enumerant: Id<Enumerant>) -> design::Enumerant {
        let info = self.enumerants[enumerant].clone();
        design::Enumerant {
            root: OnceCell::new(),
            info,
        }
    }
}
