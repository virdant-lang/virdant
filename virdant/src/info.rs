use crate::location::Span;
use crate::types::FnSig;

use super::*;

#[derive(Default, Clone, Debug)]
pub struct PackageInfo {
    pub name: String,
    pub source: PackageSource,
    pub ast: Ready<Ast>,
}

#[derive(Default, Clone, Debug)]
pub struct ItemInfo {
    pub name: String,
    pub package: Ready<Id<Package>>,
    pub ast: Ready<Ast>,
    pub kind: Ready<ItemKind>,
    pub deps: Ready<Vec<Id<Item>>>,

    // for moddefs
    pub components: Ready<Vec<Id<Component>>>,
    pub is_ext: Ready<bool>,
    pub submodules: Ready<Vec<Id<Submodule>>>,
    pub sockets: Ready<Vec<Id<Socket>>>,

    // for strucdefs
    pub fields: Ready<Vec<Id<Field>>>,

    // for uniondefs
    pub ctors: Ready<Vec<Id<Ctor>>>,

    // for enumdefs
    pub width: Ready<Width>,
    pub enumerants: Ready<Vec<Id<Enumerant>>>,

    // for fndefs
    pub sig : Ready<FnSig>,
    pub body : Ready<Id<ExprRoot>>,

    // for socketdefs
    pub channels: Ready<Vec<Id<Channel>>>,
}

#[derive(Default, Clone, Debug)]
pub struct FieldInfo {
    pub structdef: Ready<Id<StructDef>>,
    pub name: String,
    pub typ: Ready<Type>,
}

#[derive(Default, Clone, Debug)]
pub struct CtorInfo {
    pub uniondef: Ready<Id<UnionDef>>,
    pub name: String,
    pub sig: Ready<CtorSig>,
}

#[derive(Default, Clone, Debug)]
pub struct EnumerantInfo {
    pub enumdef: Ready<Id<EnumDef>>,
    pub name: String,
    pub width: Width,
    pub value: WordVal,
}

#[derive(Default, Clone, Debug)]
pub struct ChannelInfo {
    pub socketdef: Ready<Id<SocketDef>>,
    pub name: String,
    pub typ: Ready<Type>,
    pub dir: Ready<ChannelDir>,
}

#[derive(Default, Clone, Debug)]
pub struct ComponentInfo {
    pub moddef: Ready<Id<ModDef>>,
    pub path: Vec<String>,
    pub typ: Ready<Type>,
    pub is_reg: Ready<bool>,
    pub clock: Ready<Id<ExprRoot>>,
    pub class: Ready<ComponentClass>,
    pub flow: Ready<Flow>,
    pub driver: Option<Id<ExprRoot>>,
}

#[derive(Default, Clone, Debug)]
pub struct SubmoduleInfo {
    pub moddef: Ready<Id<ModDef>>,
    pub name: String,
    pub submodule_moddef: Ready<Id<ModDef>>,
}

#[derive(Clone, Debug)]
pub struct SocketInfo {
    pub moddef: Ready<Id<ModDef>>,
    pub path: Vec<String>,
    pub role: SocketRole,
    pub perspective: Perspective,
    pub socketdef: Ready<Id<SocketDef>>,
}

#[derive(Default, Clone, Debug)]
pub struct ExprRootInfo {
    pub item: Ready<Id<Item>>,
    pub ast: Ready<Arc<ast::Expr>>,
    pub expected_typ: Option<Type>,
    pub span: Option<Span>,
    pub typ: Ready<Type>,

    pub parent: Option<Id<ExprRoot>>,
    pub children: Vec<Id<ExprRoot>>,

    pub reference_component: Option<Id<Component>>,
    pub struct_structdef: Option<Id<StructDef>>,
    pub fncall_fndef: Option<Id<FnDef>>,

    // debug
    pub synthetic: bool,
}

impl Default for SocketInfo {
    fn default() -> Self {
        SocketInfo {
            moddef: Ready::default(),
            path: Vec::default(),
            role: SocketRole::Master,
            perspective: Perspective::Interior,
            socketdef: Ready::default(),
        }
    }
}
