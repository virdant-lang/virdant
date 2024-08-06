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

    // for strucdefs
    pub fields: Ready<Vec<Id<Field>>>,

    // for uniondefs
    pub ctors: Ready<Vec<Id<Ctor>>>,

    // for portdefs
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
pub struct ChannelInfo {
    pub portdef: Ready<Id<PortDef>>,
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
    pub driver: Ready<Id<ExprRoot>>,
}

#[derive(Default, Clone, Debug)]
pub struct ExprRootInfo {
    pub moddef: Ready<Id<ModDef>>,
    pub ast: Ready<Ast>,
    pub expr: Ready<Arc<Expr>>,
    pub typ: Ready<Type>,
}
