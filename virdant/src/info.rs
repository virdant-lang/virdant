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
}

#[derive(Default, Clone, Debug)]
pub struct ModDefInfo {
    pub item: Ready<Id<Item>>,
    pub components: Ready<Vec<Id<Component>>>,
}

#[derive(Default, Clone, Debug)]
pub struct BuiltinDefInfo {
    pub item: Ready<Id<Item>>,
}

#[derive(Default, Clone, Debug)]
pub struct StructDefInfo {
    pub item: Ready<Id<Item>>,
    pub fields: Ready<Vec<Id<Field>>>,
}

#[derive(Default, Clone, Debug)]
pub struct FieldInfo {
    pub structdef: Ready<Id<StructDef>>,
    pub name: String,
    pub typ: Ready<Type>,
}

#[derive(Default, Clone, Debug)]
pub struct UnionDefInfo {
    pub item: Ready<Id<Item>>,
    pub ctors: Ready<Vec<Id<Ctor>>>,
}

#[derive(Default, Clone, Debug)]
pub struct CtorInfo {
    pub uniondef: Ready<Id<UnionDef>>,
    pub name: String,
    pub sig: Ready<CtorSig>,
}

#[derive(Default, Clone, Debug)]
pub struct PortDefInfo {
    pub item: Ready<Id<Item>>,
    pub channels: Ready<Vec<Id<Channel>>>,
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
}

#[derive(Default, Clone, Debug)]
pub struct ExprRootInfo {
    pub moddef: Ready<Id<ModDef>>,
    pub ast: Ready<Ast>,
    pub expr: Ready<Arc<Expr>>,
    pub typ: Ready<Type>,
}
