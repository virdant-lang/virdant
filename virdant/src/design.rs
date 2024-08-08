use crate::common::*;
use crate::{ComponentClass, Flow, PortRole};
use std::cell::OnceCell;
use std::sync::{Arc, Weak};

use indexmap::IndexMap;

use crate::id::{self, *};
use crate::types::{self, Nat};
use crate::info::*;

/// `Design` is a representation of a given Virdant design.
#[derive(Clone, Debug)]
pub struct Design(pub(crate) Arc<DesignRoot>);

#[derive(Clone, Debug)]
pub(crate) struct DesignRoot {
    pub packages: IndexMap<Id<id::Package>, Package>,
    pub items: IndexMap<Id<id::Item>, Item>,
    pub components: IndexMap<Id<id::Component>, Component>,
    pub submodules: IndexMap<Id<id::Submodule>, Submodule>,
    pub ports: IndexMap<Id<id::Port>, Port>,
    pub exprroots: IndexMap<Id<id::ExprRoot>, ExprRoot>,
    pub fields: IndexMap<Id<id::Field>, Field>,
    pub ctors: IndexMap<Id<id::Ctor>, Ctor>,
}

/// `Package` is a representation of a given Virdant package.
/// It will correspond to a single `.vir` file.
#[derive(Clone)]
pub struct Package {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::Package>,
    pub(crate) info: PackageInfo,
}

/// An `Item` is top-level definition.
#[derive(Clone)]
pub struct Item {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::Item>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct ModDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::ModDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct UnionDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::UnionDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct StructDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct BuiltinDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct PortDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct Component {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: ComponentInfo,
}

#[derive(Clone, Debug)]
pub struct Submodule {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: SubmoduleInfo,
}

#[derive(Clone, Debug)]
pub struct Port {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: PortInfo,
}

#[derive(Clone, Debug)]
pub struct ExprRoot {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: ExprRootInfo,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: FieldInfo,
}

#[derive(Clone, Debug)]
pub struct Ctor {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: CtorInfo,
}

#[derive(Clone)]
pub struct Type {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) typ: types::Type,
}

#[derive(Clone)]
pub enum TypeArg {
    Nat(Nat),
    Type(Type),
}

#[derive(Clone, Debug)]
pub enum TypeScheme {
    StructDef(StructDef),
    UnionDef(UnionDef),
    BuiltinDef(BuiltinDef),
}

impl Design {
    /// Returns a list of the packages in the design.
    pub fn packages(&self) -> Vec<Package> {
        self.root().packages.values().cloned().collect()
    }

    fn root(&self) -> &DesignRoot {
        &self.0
    }
}

impl Package {
    /// The name of the package.
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn items(&self) -> Vec<Item> {
        let mut items: Vec<Item> = vec![];
        for item in self.root().items.values() {
            if *item.info.package.unwrap() == self.id {
                items.push(item.clone());
            }
        }
        items
    }
}

impl Item {
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn package(&self) -> Package {
        self.root().packages[self.info.package.unwrap()].clone()
    }

    pub fn kind(&self) -> ItemKind {
        match self.info.kind.unwrap() {
            crate::ItemKind::ModDef => ItemKind::ModDef(self.as_moddef()),
            crate::ItemKind::UnionDef => ItemKind::UnionDef(self.as_uniondef()),
            crate::ItemKind::StructDef => ItemKind::StructDef(self.as_structdef()),
            crate::ItemKind::BuiltinDef => ItemKind::BuiltinDef(self.as_builtindef()),
            crate::ItemKind::PortDef => ItemKind::PortDef(self.as_portdef()),
        }
    }

    fn as_moddef(&self) -> ModDef {
        ModDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_structdef(&self) -> StructDef {
        StructDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_uniondef(&self) -> UnionDef {
        UnionDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_builtindef(&self) -> BuiltinDef {
        BuiltinDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_portdef(&self) -> PortDef {
        PortDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }
}

impl ModDef {
    pub fn components(&self) -> Vec<Component> {
        let mut components = vec![];
        let component_ids = self.info.components.unwrap();
        for (component_id, component) in self.root().components.iter() {
            if component_ids.contains(component_id) {
                components.push(component.clone());
            }
        }
        components
    }

    pub fn simple_ports(&self) -> Vec<Component> {
        let mut components = vec![];
        let component_ids = self.info.components.unwrap();
        for (component_id, component) in self.root().components.iter() {
            if component_ids.contains(component_id) && component.is_port() {
                components.push(component.clone());
            }
        }
        components
    }

    pub fn submodules(&self) -> Vec<Submodule> {
        let mut submodules = vec![];
        for submodule_id in self.info.submodules.unwrap() {
            let submodule = self.root().submodules[submodule_id].clone();
            submodules.push(submodule);
        }
        submodules
    }

    pub fn ports(&self) -> Vec<Port> {
        let mut ports = vec![];
        for port_id in self.info.ports.unwrap() {
            let port = self.root().ports[port_id].clone();
            ports.push(port);
        }
        ports
    }

    pub fn is_ext(&self) -> bool {
        *self.info.is_ext.unwrap()
    }
}

impl UnionDef {
    pub fn ctors(&self) -> Vec<Ctor> {
        let mut ctors = vec![];
        let ctor_ids = self.info.ctors.unwrap();
        for (ctor_id, ctor) in self.root().ctors.iter() {
            if ctor_ids.contains(ctor_id) {
                ctors.push(ctor.clone());
            }
        }
        ctors
    }
}

impl StructDef {
    pub fn fields(&self) -> Vec<Field> {
        let mut fields = vec![];
        let field_ids = self.info.fields.unwrap();
        for (field_id, field) in self.root().fields.iter() {
            if field_ids.contains(field_id) {
                fields.push(field.clone());
            }
        }
        fields
    }
}

impl Component {
    pub fn path(&self) -> &[String] {
        &self.info.path
    }

    pub fn name(&self) -> String {
        self.info.path.join(".")
    }

    pub fn typ(&self) -> Type {
        let typ = self.info.typ.unwrap();
        Type::new(self.root.clone(), *typ)
    }

    pub fn moddef(&self) -> ModDef {
        self.root().items[&self.info.moddef.unwrap().as_item()].clone().as_moddef()
    }

    pub fn package(&self) -> Package {
        self.moddef().package()
    }

    pub fn is_local(&self) -> bool {
        self.info.path.len() == 1
    }

    pub fn class(&self) -> ComponentClass {
        self.info.class.unwrap().clone()
    }

    pub fn driver(&self) -> Option<Arc<Expr>> {
        todo!()
        /*
        let exprroot_id = self.info.driver.unwrap();
        let exprroot = self.root().exprroots[exprroot_id].clone();
        exprroot.info.typedexpr.get().cloned().ok()
*/
    }

    pub fn flow(&self) -> Flow {
        self.info.flow.unwrap().clone()
    }

    pub fn is_port(&self) -> bool {
        self.class() == ComponentClass::Port
    }
}

impl Submodule {
    pub fn name(&self) -> String {
        self.info.name.clone()
    }

    pub fn of(&self) -> ModDef {
        let submodule_moddef = *self.info.submodule_moddef.unwrap();
        self.root().items[&submodule_moddef.as_item()].as_moddef()
    }

    pub fn moddef(&self) -> ModDef {
        let moddef = *self.info.moddef.unwrap();
        self.root().items[&moddef.as_item()].as_moddef()
    }
}

impl Port {
    pub fn name(&self) -> String {
        self.info.name.clone()
    }

    pub fn of(&self) -> PortDef {
        let portdef = *self.info.portdef.unwrap();
        self.root().items[&portdef.as_item()].as_portdef()
    }

    pub fn moddef(&self) -> ModDef {
        let moddef = *self.info.moddef.unwrap();
        self.root().items[&moddef.as_item()].as_moddef()
    }

    pub fn role(&self) -> PortRole {
        *self.info.role.unwrap()
    }
}

impl ExprRoot {
    pub fn typ(&self) -> Type {
        let typ = self.info.typ.unwrap();
        Type::new(self.root.clone(), *typ)
    }
}

impl Field {
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn structdef(&self) -> StructDef {
        self.root().items[&self.info.structdef.unwrap().as_item()].as_structdef()
    }

    pub fn typ(&self) -> Type {
        let typ = self.info.typ.unwrap();
        Type::new(self.root.clone(), *typ)
    }
}

impl Ctor {
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn uniondef(&self) -> UnionDef {
        self.root().items[&self.info.uniondef.unwrap().as_item()].as_uniondef()
    }
}

impl Method {
    pub fn name(&self) -> &str {
        //&self.info.name
        todo!()
    }
}

impl Type {
    fn new(root: OnceCell<Weak<DesignRoot>>, typ: types::Type) -> Self {
        Type { root, typ }
    }

    pub fn scheme(&self) -> TypeScheme {
        let root = self.root();
        match self.typ.scheme() {
            types::TypeScheme::StructDef(structdef) => {
                let item = root.items[&structdef.as_item()].as_structdef();
                TypeScheme::StructDef(item)
            },
            types::TypeScheme::UnionDef(uniondef) => {
                let item = root.items[&uniondef.as_item()].as_uniondef();
                TypeScheme::UnionDef(item)
            },
            types::TypeScheme::BuiltinDef(builtindef) => {
                let item = root.items[&builtindef.as_item()].as_builtindef();
                TypeScheme::BuiltinDef(item)
            },
        }
    }

    pub fn name(&self) -> String {
        let root = self.root();
        let item = match self.typ.scheme() {
            types::TypeScheme::StructDef(structdef) => &root.items[&structdef.as_item()],
            types::TypeScheme::UnionDef(uniondef) => &root.items[&uniondef.as_item()],
            types::TypeScheme::BuiltinDef(builtindef) => &root.items[&builtindef.as_item()],
        };
        item.name().to_string()
    }

    pub fn args(&self) -> Option<Vec<TypeArg>> {
        if let Some(args) = self.typ.args() {
            Some(
                args.into_iter()
                    .map(|width| TypeArg::Nat(width))
                    .collect()
            )
        } else {
            None
        }
    }
}


trait HasRoot {
    fn root(&self) -> Arc<DesignRoot>;
}

macro_rules! impl_hasroot {
    ($struct:ident) => {
        impl HasRoot for $struct {
            fn root(&self) -> Arc<DesignRoot> {
                self.root.get().unwrap().upgrade().unwrap().clone()
            }
        }
    };
}

impl_hasroot!(Package);

impl_hasroot!(Item);
impl_hasroot!(ModDef);
impl_hasroot!(UnionDef);
impl_hasroot!(StructDef);
impl_hasroot!(PortDef);
impl_hasroot!(BuiltinDef);

impl_hasroot!(Component);
impl_hasroot!(Submodule);
impl_hasroot!(Port);
impl_hasroot!(Field);
impl_hasroot!(Ctor);

impl_hasroot!(Type);

#[derive(Clone, Debug)]
pub enum ItemKind {
    ModDef(ModDef),
    UnionDef(UnionDef),
    StructDef(StructDef),
    BuiltinDef(BuiltinDef),
    PortDef(PortDef),
}

macro_rules! item_fns {
    ($struct:ident) => {
        impl $struct {
            pub fn as_item(&self) -> Item {
                Item { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
            }

            pub fn name(&self) -> String {
                self.as_item().name().to_string()
            }

            pub fn package(&self) -> Package {
                self.as_item().package()
            }
        }
    };
}

item_fns!(ModDef);
item_fns!(UnionDef);
item_fns!(StructDef);
item_fns!(PortDef);
item_fns!(BuiltinDef);

macro_rules! name_as_debug {
    ($struct:ident) => {
        impl std::fmt::Debug for $struct {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.name())
            }
        }

    };
}

name_as_debug!(Package);
name_as_debug!(Item);
name_as_debug!(ModDef);
name_as_debug!(UnionDef);
name_as_debug!(StructDef);
name_as_debug!(BuiltinDef);
name_as_debug!(PortDef);

impl std::fmt::Debug for TypeArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeArg::Nat(n) => write!(f, "{n}"),
            TypeArg::Type(typ) => write!(f, "{typ:?}"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(args) = self.args() {
            write!(f, "{}[", self.name())?;
            let mut first = true;
            for arg in args {
                if !first {
                    write!(f, ", {arg:?}")?;
                } else {
                    write!(f, "{arg:?}")?;
                }
                first = false;
            }
            write!(f, "]")
        } else {
            write!(f, "{}", self.name())
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Reference(expr::Reference),
    Word(expr::Word),
    Bit(expr::Bit),
    MethodCall(expr::MethodCall),
    Struct(expr::Struct),
    Field(expr::Field),
    Ctor(expr::Ctor),
    Idx(expr::Idx),
    IdxRange(expr::IdxRange),
    Cat(expr::Cat),
    If(expr::If),
    Match(expr::Match),
}

#[derive(Clone, Debug)]
pub enum Referent {
    Component(Component),
    Binding(Binding),
}

#[derive(Clone, Debug)]
pub struct Binding();

#[derive(Clone, Debug)]
pub struct MatchArm();

mod expr {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Reference {}

    impl Reference {
        pub fn typ(&self) -> Type { todo!() }
        pub fn referent(&self) -> Referent { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Word {}

    impl Word {
        pub fn typ(&self) -> Type { todo!() }
        pub fn val(&self) -> WordVal { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Bit {}

    impl Bit {
        pub fn typ(&self) -> Type { todo!() }
        pub fn val(&self) -> bool { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct MethodCall {}

    impl MethodCall {
        pub fn typ(&self) -> Type { todo!() }
        pub fn method(&self) -> Method { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn args(&self) -> Vec<Expr> { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Struct {}

    impl Struct {
        pub fn typ(&self) -> Type { todo!() }
        pub fn assigns(&self) -> Vec<(Field, Expr)> { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Field {}

    impl Field {
        pub fn typ(&self) -> Type { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn field(&self) -> Field { todo!() }
    }
    #[derive(Clone, Debug)]
    pub struct Ctor {}

    impl Ctor {
        pub fn typ(&self) -> Type { todo!() }
        pub fn ctor(&self) -> Ctor { todo!() }
        pub fn args(&self) -> Vec<Expr> { todo!() }
    }
    #[derive(Clone, Debug)]
    pub struct Idx {}

    impl Idx {
        pub fn typ(&self) -> Type { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn lo(&self) -> StaticIndex { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct IdxRange {}

    impl IdxRange {
        pub fn typ(&self) -> Type { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn hi(&self) -> StaticIndex { todo!() }
        pub fn lo(&self) -> StaticIndex { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Cat {}

    impl Cat {
        pub fn typ(&self) -> Type { todo!() }
        pub fn args(&self) -> Vec<Expr> { todo!() }
    }
    #[derive(Clone, Debug)]
    pub struct If {}

    impl If {
        pub fn typ(&self) -> Type { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn truebranch(&self) -> Expr { todo!() }
        pub fn falsebranch(&self) -> Expr { todo!() }
    }

    #[derive(Clone, Debug)]
    pub struct Match {}

    impl Match {
        pub fn typ(&self) -> Type { todo!() }
        pub fn subject(&self) -> Expr { todo!() }
        pub fn arms(&self) -> Vec<MatchArm> { todo!() }
    }
}
