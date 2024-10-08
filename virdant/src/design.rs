use crate::common::*;
use crate::{ComponentClass, Flow, SocketRole};
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
    pub sockets: IndexMap<Id<id::Socket>, Socket>,
    pub exprroots: IndexMap<Id<id::ExprRoot>, ExprRoot>,
    pub fields: IndexMap<Id<id::Field>, Field>,
    pub ctors: IndexMap<Id<id::Ctor>, Ctor>,
    pub enumerants: IndexMap<Id<id::Enumerant>, Enumerant>,
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
pub struct EnumDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::EnumDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct BuiltinDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone)]
pub struct SocketDef {
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
pub struct Socket {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: SocketInfo,
}

#[derive(Clone, Debug)]
pub struct ExprRoot {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::ExprRoot>,
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

#[derive(Clone, Debug)]
pub struct Enumerant {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) info: EnumerantInfo,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Method {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) name: String,
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
    EnumDef(EnumDef),
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
            crate::ItemKind::EnumDef => ItemKind::EnumDef(self.as_enumdef()),
            crate::ItemKind::BuiltinDef => ItemKind::BuiltinDef(self.as_builtindef()),
            crate::ItemKind::FnDef => ItemKind::FnDef(self.as_fndef()),
            crate::ItemKind::SocketDef => ItemKind::SocketDef(self.as_socketdef()),
        }
    }

    fn as_moddef(&self) -> ModDef {
        ModDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_structdef(&self) -> StructDef {
        StructDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_enumdef(&self) -> EnumDef {
        EnumDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_uniondef(&self) -> UnionDef {
        UnionDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_builtindef(&self) -> BuiltinDef {
        BuiltinDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
    }

    fn as_fndef(&self) -> FnDef {
        FnDef { root: self.root.clone(), info: self.info.clone() }
    }

    fn as_socketdef(&self) -> SocketDef {
        SocketDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
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

    pub fn ports(&self) -> Vec<Component> {
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

    pub fn sockets(&self) -> Vec<Socket> {
        let mut ports = vec![];
        for port_id in self.info.sockets.unwrap() {
            let port = self.root().sockets[port_id].clone();
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

    fn field(&self, name: &str) -> Field {
        let field_ids = self.info.fields.unwrap();
        for (field_id, field) in self.root().fields.iter() {
            if field_ids.contains(field_id) && field.name() == name {
                return field.clone();
            }
        }
        panic!("No such field")
    }
}

impl FnDef {
    pub fn name(&self) -> String {
        self.info.name.to_string()
    }

    pub fn params(&self) -> Vec<(String, Type)> {
        let mut results = vec![];
        for (name, typ) in self.info.sig.unwrap().params() {
            let typ = Type::new(self.root.clone(), typ.clone());
            results.push((name.clone(), typ));
        }
        results
    }

    pub fn ret(&self) -> Type {
        let sig = self.info.sig.unwrap().clone();
        Type::new(self.root.clone(), sig.ret())
    }

    pub fn body(&self) -> Expr {
        let exprroot_id: Id<_> = self.info.body.unwrap().clone();
        let exprroot = self.root().exprroots[&exprroot_id].clone();
        exprroot.to_expr()
    }
}

impl EnumDef {
    pub fn enumerants(&self) -> Vec<Enumerant> {
        let mut enumerants = vec![];
        let enumerant_ids = self.info.enumerants.unwrap();
        for (enumerant_id, enumerant) in self.root().enumerants.iter() {
            if enumerant_ids.contains(enumerant_id) {
                enumerants.push(enumerant.clone());
            }
        }
        enumerants
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

    pub fn driver(&self) -> Option<Expr> {
        if let Some(exprroot_id) = self.info.driver {
            let exprroot = self.root().exprroots[&exprroot_id].clone();
            Some(exprroot.to_expr())
        } else {
            None
        }
    }

    pub fn clock(&self) -> Option<Expr> {
        if let Ok(exprroot_id) = self.info.clock.get() {
            let exprroot = self.root().exprroots[exprroot_id].clone();
            Some(exprroot.to_expr())
        } else {
            None
        }
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

impl Socket {
    pub fn name(&self) -> String {
        self.info.path.join(".")
    }

    pub fn path(&self) -> Vec<String> {
        self.info.path.clone()
    }

    pub fn of(&self) -> SocketDef {
        let socketdef = *self.info.socketdef.unwrap();
        self.root().items[&socketdef.as_item()].as_socketdef()
    }

    pub fn moddef(&self) -> ModDef {
        let moddef = *self.info.moddef.unwrap();
        self.root().items[&moddef.as_item()].as_moddef()
    }

    pub fn role(&self) -> SocketRole {
        self.info.role
    }

    pub fn perspective(&self) -> Perspective {
        self.info.perspective
    }
}

impl ExprRoot {
    pub fn typ(&self) -> Type {
        let typ = self.info.typ.unwrap();
        Type::new(self.root.clone(), *typ)
    }

    fn to_expr(&self) -> Expr {
        let expr_ast = self.info.ast.unwrap();
        let id = self.id;
        let root = self.root.clone();
        let info = self.info.clone();

        match expr_ast.as_ref() {
            crate::ast::Expr::Reference(_, _) => Expr::Reference(expr::Reference { root, id, info }),
            crate::ast::Expr::Word(_, _) => Expr::Word(expr::Word { root, id, info }),
            crate::ast::Expr::Bit(_, _) => Expr::Bit(expr::Bit { root, id, info }),
            crate::ast::Expr::FnCall(_, _, _) => Expr::FnCall(expr::FnCall { root, id, info }),
            crate::ast::Expr::UnOp(_, _, _,) => Expr::UnOp(expr::UnOp { root, id, info }),
            crate::ast::Expr::BinOp(_, _, _, _) => Expr::BinOp(expr::BinOp { root, id, info }),
            crate::ast::Expr::MethodCall(_, _, _, _) => Expr::MethodCall(expr::MethodCall { root, id, info }),
            crate::ast::Expr::Field(_, _, _) => Expr::Field(expr::Field { root, id, info }),
            crate::ast::Expr::Struct(_, _, _) => Expr::Struct(expr::Struct { root, id, info }),
            crate::ast::Expr::Ctor(_, _, _) => Expr::Ctor(expr::Ctor { root, id, info }),
            crate::ast::Expr::Enumerant(_, _) => Expr::Enumerant(expr::Enumerant { root, id, info }),
            crate::ast::Expr::As(_, _, _) => Expr::As(expr::As { root, id, info }),
            crate::ast::Expr::Idx(_, _, _) => Expr::Idx(expr::Idx { root, id, info }),
            crate::ast::Expr::IdxRange(_, _, _, _) => Expr::IdxRange(expr::IdxRange { root, id, info }),
            crate::ast::Expr::Cat(_, _) => Expr::Cat(expr::Cat { root, id, info }),
            crate::ast::Expr::Sext(_, _) => Expr::Sext(expr::Sext { root, id, info }),
            crate::ast::Expr::Zext(_, _) => Expr::Zext(expr::Zext { root, id, info }),
            crate::ast::Expr::If(_, _, _, _) => Expr::If(expr::If { root, id, info }),
            crate::ast::Expr::Match(_, _, _, _) => Expr::Match(expr::Match { root, id, info }),
        }
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

    pub fn params(&self) -> Vec<(String, Type)> {
        let mut results = vec![];
        for (name, typ) in self.info.sig.unwrap().params() {
            let typ = Type::new(self.root.clone(), typ.clone());
            results.push((name.clone(), typ));
        }
        results
    }
}

impl Enumerant {
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn enumdef(&self) -> EnumDef {
        self.root().items[&self.info.enumdef.unwrap().as_item()].as_enumdef()
    }

    pub fn value(&self) -> WordVal {
        self.info.value
    }
}

impl Method {
    pub fn name(&self) -> &str {
        &self.name
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
            types::TypeScheme::EnumDef(enumdef) => {
                let item = root.items[&enumdef.as_item()].as_enumdef();
                TypeScheme::EnumDef(item)
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
            types::TypeScheme::EnumDef(uniondef) => &root.items[&uniondef.as_item()],
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
impl_hasroot!(EnumDef);
impl_hasroot!(SocketDef);
impl_hasroot!(FnDef);
impl_hasroot!(BuiltinDef);

impl_hasroot!(Component);
impl_hasroot!(Submodule);
impl_hasroot!(Socket);
impl_hasroot!(Field);
impl_hasroot!(Ctor);
impl_hasroot!(Enumerant);

impl_hasroot!(Type);

#[derive(Clone, Debug)]
pub enum ItemKind {
    ModDef(ModDef),
    UnionDef(UnionDef),
    StructDef(StructDef),
    EnumDef(EnumDef),
    BuiltinDef(BuiltinDef),
    FnDef(FnDef),
    SocketDef(SocketDef),
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
item_fns!(EnumDef);
item_fns!(SocketDef);
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
name_as_debug!(EnumDef);
name_as_debug!(BuiltinDef);
name_as_debug!(SocketDef);

impl std::fmt::Debug for TypeArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeArg::Nat(n) => write!(f, "{n}"),
            TypeArg::Type(typ) => write!(f, "{typ:?}"),
        }
    }
}

impl std::fmt::Display for TypeArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Reference(expr::Reference),
    Word(expr::Word),
    Bit(expr::Bit),
    FnCall(expr::FnCall),
    UnOp(expr::UnOp),
    BinOp(expr::BinOp),
    MethodCall(expr::MethodCall),
    Struct(expr::Struct),
    Field(expr::Field),
    Ctor(expr::Ctor),
    Enumerant(expr::Enumerant),
    As(expr::As),
    Idx(expr::Idx),
    IdxRange(expr::IdxRange),
    Cat(expr::Cat),
    Sext(expr::Sext),
    Zext(expr::Zext),
    If(expr::If),
    Match(expr::Match),
}

impl Expr {
    pub fn typ(&self) -> Type {
        match self {
            Expr::Reference(reference) => reference.typ(),
            Expr::Word(word) => word.typ(),
            Expr::Bit(bit) => bit.typ(),
            Expr::UnOp(unop) => unop.typ(),
            Expr::BinOp(binop) => binop.typ(),
            Expr::MethodCall(methodcall) => methodcall.typ(),
            Expr::FnCall(fncall) => fncall.typ(),
            Expr::Struct(struct_) => struct_.typ(),
            Expr::Field(field) => field.typ(),
            Expr::Ctor(ctor) => ctor.typ(),
            Expr::Enumerant(enumerant) => enumerant.typ(),
            Expr::As(as_) => as_.typ(),
            Expr::Idx(idx) => idx.typ(),
            Expr::IdxRange(idxrange) => idxrange.typ(),
            Expr::Cat(cat) => cat.typ(),
            Expr::Sext(sext) => sext.typ(),
            Expr::Zext(zext) => zext.typ(),
            Expr::If(if_) => if_.typ(),
            Expr::Match(match_) => match_.typ(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Referent {
    Component(Component),
    Binding(Binding),
}

#[derive(Debug, Clone)]
pub enum Pat {
    CtorAt(Type, Ctor, Vec<Pat>),
    EnumerantAt(Type, Enumerant),
    Bind(Type, Binding),
    Else(Type),
}

impl Pat {
    fn new(root: &DesignRoot, pat: &crate::ast::expr::Pat, typ: Type) -> Pat {
        match pat {
            crate::ast::expr::Pat::CtorAt(ctor_name, pats) => {
                let uniondef_id = match typ.typ.scheme() {
                    types::TypeScheme::StructDef(_) => unreachable!(),
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(_) => unreachable!(),
                    types::TypeScheme::UnionDef(uniondef) => uniondef,
                };
                let uniondef = &root.items[&uniondef_id.as_item()].as_uniondef();

                for ctor in uniondef.ctors() {
                    if ctor.name() == **ctor_name {
                        let ctor_sig = ctor.info.sig.unwrap();
                        let mut new_pats = vec![];
                        for (pat, (_param_name, param_typ)) in pats.into_iter().zip(ctor_sig.params()) {
                            let param_typ = Type::new(ctor.root.clone(), *param_typ);
                            new_pats.push(Pat::new(root, pat, param_typ.clone()));
                        }
                        return Pat::CtorAt(typ, ctor, new_pats);
                    }
                }
                unreachable!()
            },
            crate::ast::expr::Pat::EnumerantAt(ctor_name) => {
                let enumdef_id = match typ.typ.scheme() {
                    types::TypeScheme::StructDef(_) => unreachable!(),
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(enumdef) => enumdef,
                    types::TypeScheme::UnionDef(_) => unreachable!(),
                };
                let enumdef = &root.items[&enumdef_id.as_item()].as_enumdef();

                for enumerant in enumdef.enumerants() {
                    if enumerant.name() == **ctor_name {
                        return Pat::EnumerantAt(typ, enumerant);
                    }
                }
                unreachable!()
            },
            crate::ast::expr::Pat::Bind(ident) => Pat::Bind(typ, Binding(ident.to_string())),
            crate::ast::expr::Pat::Else => Pat::Else(typ),
        }
    }

    pub fn typ(&self) -> Type {
        match self {
            Pat::CtorAt(typ, _, _) => typ.clone(),
            Pat::EnumerantAt(typ, _) => typ.clone(),
            Pat::Bind(typ, _) => typ.clone(),
            Pat::Else(typ) => typ.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binding(String);

impl Binding {
    pub fn name(&self) -> &str {
        &self.0
    }
}

mod expr {
    use crate::ast::{self, expr::MatchArm};

    use super::*;

    macro_rules! expr_type {
        ($name:ident) => {
            #[allow(dead_code)]
            #[derive(Clone, Debug)]
            pub struct $name {
                pub(crate) root: OnceCell<Weak<DesignRoot>>,
                pub(crate) id: Id<id::ExprRoot>,
                pub(crate) info: ExprRootInfo,
            }

            impl_hasroot!($name);

            impl $name {
                pub fn typ(&self) -> Type {
                    let exprroot = &self.root().exprroots[&self.id];
                    let exprroot_info = &exprroot.info;
                    let typ = exprroot_info.typ.unwrap();
                    Type::new(self.root.clone(), *typ)
                }

                #[allow(dead_code)]
                fn ast(&self) -> Arc<crate::ast::Expr> {
                    let exprroot = &self.root().exprroots[&self.id];
                    let exprroot_info = &exprroot.info;
                    exprroot_info.ast.unwrap().clone()
                }
            }
        };
    }

    expr_type!(Reference);
    expr_type!(Word);
    expr_type!(Bit);
    expr_type!(UnOp);
    expr_type!(BinOp);
    expr_type!(MethodCall);
    expr_type!(FnCall);
    expr_type!(Struct);
    expr_type!(Field);
    expr_type!(Ctor);
    expr_type!(Enumerant);
    expr_type!(As);
    expr_type!(Idx);
    expr_type!(IdxRange);
    expr_type!(Cat);
    expr_type!(Sext);
    expr_type!(Zext);
    expr_type!(If);
    expr_type!(Match);

    impl Reference {
        pub fn referent(&self) -> Referent {
            let path = if let crate::ast::Expr::Reference(_span, path) = self.ast().as_ref() {
                path.clone()
            } else {
                unreachable!()
            };

            if let Some(component) = self.info.reference_component {
                let component = self.root().components[&component].clone();
                Referent::Component(component)
            } else {
                assert_eq!(path.len(), 1);
                let binding = Binding(path[0].to_string());
                Referent::Binding(binding)
            }
        }
    }

    impl Word {
        pub fn value(&self) -> WordVal {
            if let crate::ast::Expr::Word(_span, wordlit) = self.ast().as_ref() {
                wordlit.value
            } else {
                unreachable!()
            }
        }

        pub fn width(&self) -> Width {
            self.typ().typ.width()
        }
    }

    impl Bit {
        pub fn value(&self) -> bool {
            if let crate::ast::Expr::Bit(_span, bitlit) = self.ast().as_ref() {
                *bitlit
            } else {
                unreachable!()
            }
        }
    }

    impl UnOp {
        pub fn op(&self) -> &str {
            if let ast::expr::Expr::UnOp(_, op, _) = self.info.ast.unwrap().as_ref() {
                return &*op;
            } else {
                unreachable!()
            }
        }

        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }
    }

    impl BinOp {
        pub fn op(&self) -> &str {
            if let ast::expr::Expr::BinOp(_, _, op, _) = self.info.ast.unwrap().as_ref() {
                return &*op;
            } else {
                unreachable!()
            }
        }

        pub fn left(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn right(&self) -> Expr {
            let subject: Id<_> = self.info.children[1];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }
    }

    impl MethodCall {
        pub fn method(&self) -> super::Method {
            let name = if let crate::ast::Expr::MethodCall(_span, _, method_name, _) = self.ast().as_ref() {
                method_name.to_string()
            } else {
                unreachable!()
            };

            super::Method {
                root: self.root.clone(),
                name,
            }
        }

        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn args(&self) -> Vec<Expr> {
            let args: Vec<Id<_>> = self.info.children[1..].to_vec();
            let exprroots: Vec<_> = args.iter()
                .map(|id| self.root().exprroots[id].to_expr())
                .collect();
            exprroots
        }
    }

    impl FnCall {
        pub fn fndef(&self) -> super::FnDef {
            let fndef_id = &self.info.fncall_fndef.unwrap();
            let info = self.root().items[&fndef_id.as_item()].info.clone();
            super::FnDef {
                root: self.root.clone(),
                info,
            }
        }

        pub fn name(&self) -> String {
            let fndef_id = &self.info.fncall_fndef.unwrap();
            let fndef = &self.root().items[&fndef_id.as_item()];
            fndef.name().to_owned()
        }

        pub fn args(&self) -> Vec<Expr> {
            let args: Vec<Id<_>> = self.info.children.to_vec();
            let exprroots: Vec<_> = args.iter()
                .map(|id| self.root().exprroots[id].to_expr())
                .collect();
            exprroots
        }
    }

    impl Struct {
        pub fn structdef(&self) -> StructDef {
            let structdef_id = self.info.struct_structdef.unwrap();
            let item = &self.root().items[&structdef_id.as_item()];
            item.as_structdef()
        }

        pub fn assigns(&self) -> Vec<(super::Field, super::Expr)> {
            if let crate::ast::Expr::Struct(_span, _, assigns) = self.ast().as_ref() {
                let field_args: Vec<Id<_>> = self.info.children.clone();

                let typ = self.info.typ.unwrap();
                let structdef_id = match typ.scheme() {
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::UnionDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(_) => unreachable!(),
                    types::TypeScheme::StructDef(structdef) => structdef,
                };
                let structdef = &self.root().items[&structdef_id.as_item()].as_structdef();

                let mut results = vec![];

                for ((field_name, _), field_arg) in assigns.iter().zip(field_args) {
                    let field = structdef.field(field_name);
                    let exprroot = self.root().exprroots[&field_arg].clone();

                    results.push((field, exprroot.to_expr()));
                }

                results
            } else {
                unreachable!()
            }
        }
    }

    impl Field {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn field(&self) -> super::Field {
            if let crate::ast::Expr::Field(_span, _, field_name) = self.ast().as_ref() {
                let typ = self.subject().typ().typ;
                let structdef_id = match typ.scheme() {
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::UnionDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(_) => unreachable!(),
                    types::TypeScheme::StructDef(structdef) => structdef,
                };
                let structdef = &self.root().items[&structdef_id.as_item()].as_structdef();

                for field in structdef.fields() {
                    if field.name() == **field_name {
                        return field;
                    }
                }
                unreachable!()
            } else {
                unreachable!()
            }
        }
    }

    impl Ctor {
        pub fn ctor(&self) -> super::Ctor {
            if let crate::ast::Expr::Ctor(_span, ctor_name, _) = self.ast().as_ref() {
                let typ = self.info.typ.unwrap();
                let uniondef_id = match typ.scheme() {
                    types::TypeScheme::StructDef(_) => unreachable!(),
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(_) => unreachable!(),
                    types::TypeScheme::UnionDef(uniondef) => uniondef,
                };
                let uniondef = &self.root().items[&uniondef_id.as_item()].as_uniondef();

                for ctor in uniondef.ctors() {
                    if ctor.name() == **ctor_name {
                        return ctor;
                    }
                }
                unreachable!()
            } else {
                unreachable!()
            }
        }

        pub fn args(&self) -> Vec<Expr> {
            let args: Vec<Id<_>> = self.info.children.clone();
            let exprroots: Vec<_> = args.iter()
                .map(|id| self.root().exprroots[id].to_expr())
                .collect();
            exprroots
        }
    }

    impl Enumerant {
        pub fn enumerant(&self) -> super::Enumerant {
            if let crate::ast::Expr::Enumerant(_span, enumerant_name) = self.ast().as_ref() {
                let typ = self.info.typ.unwrap();
                let enumdef_id = match typ.scheme() {
                    types::TypeScheme::StructDef(_) => unreachable!(),
                    types::TypeScheme::BuiltinDef(_) => unreachable!(),
                    types::TypeScheme::UnionDef(_) => unreachable!(),
                    types::TypeScheme::EnumDef(enumdef) => enumdef,
                };
                let enumdef = &self.root().items[&enumdef_id.as_item()].as_enumdef();

                for enumerant in enumdef.enumerants() {
                    if enumerant.name() == **enumerant_name {
                        return enumerant;
                    }
                }
                unreachable!()
            } else {
                unreachable!()
            }
        }
    }

    impl As {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }
    }

    impl Idx {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn idx(&self) -> StaticIndex {
            if let crate::ast::Expr::Idx(_span, _, i) = self.ast().as_ref() {
                *i
            } else {
                unreachable!()
            }
        }
    }

    impl IdxRange {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn idx_hi(&self) -> StaticIndex {
            if let crate::ast::Expr::IdxRange(_span, _, j, _i) = self.ast().as_ref() {
                *j
            } else {
                unreachable!()
            }
        }

        pub fn idx_lo(&self) -> StaticIndex {
            if let crate::ast::Expr::IdxRange(_span, _, _j, i) = self.ast().as_ref() {
                *i
            } else {
                unreachable!()
            }
        }
    }

    impl Cat {
        pub fn args(&self) -> Vec<Expr> {
            let args: Vec<Id<_>> = self.info.children.clone();
            let exprroots: Vec<_> = args.iter()
                .map(|id| self.root().exprroots[id].to_expr())
                .collect();
            exprroots
        }
    }

    impl Sext {
        pub fn arg(&self) -> Expr {
            let arg: Id<_> = self.info.children[0];
            self.root().exprroots[&arg].to_expr()
        }
    }

    impl Zext {
        pub fn arg(&self) -> Expr {
            let arg: Id<_> = self.info.children[0];
            self.root().exprroots[&arg].to_expr()
        }
    }

    impl If {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn truebranch(&self) -> Expr {
            let truebranch: Id<_> = self.info.children[1];
            let exprroot = self.root().exprroots[&truebranch].clone();
            exprroot.to_expr()
        }

        pub fn falsebranch(&self) -> Expr {
            let falsebranch: Id<_> = self.info.children[2];
            let exprroot = self.root().exprroots[&falsebranch].clone();
            exprroot.to_expr()
        }
    }

    impl Match {
        pub fn subject(&self) -> Expr {
            let subject: Id<_> = self.info.children[0];
            let exprroot = self.root().exprroots[&subject].clone();
            exprroot.to_expr()
        }

        pub fn arms(&self) -> Vec<(super::Pat, super::Expr)> {
            if let crate::ast::Expr::Match(_span, _subject, _ascription, arms) = self.ast().as_ref() {
                let subject_typ = self.subject().typ();
                let arm_exprroots: Vec<_> = self.info.children.iter()
                    .skip(1)
                    .map(|id| self.root().exprroots[id].to_expr())
                    .collect();
                let mut results = vec![];

                for (MatchArm(pat, _), arm_exprroot) in arms.iter().zip(arm_exprroots) {
                    let new_pat = Pat::new(&self.root(), pat, subject_typ.clone());
                    results.push((new_pat, arm_exprroot))
                }

                results
            } else {
                unreachable!()
            }
        }
    }
}
