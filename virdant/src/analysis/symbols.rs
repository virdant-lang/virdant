use bstr::{BStr, BString};
use indexmap::IndexSet;
use indexmap::IndexMap;
use std::sync::Arc;

use crate::analysis::PackageAnalysis;
use crate::db::Builder;
use crate::diagnostics;
use crate::fqn::PackageFqn;
use crate::common::source::Region;
use crate::syntax::ast::{AstNode, AstNodeId, item_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;
use crate::{analysis::Location, diagnostics::Diagnostic};

#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: IndexMap<BString, Symbol>,
    symbols_by_id: Vec<Symbol>,
    pub diagnostics: Vec<Diagnostic>,
    pub builtin_names: IndexSet<BString>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub fqn: BString,
    pub name: BString,
    pub location: Location,
    pub kind: SymbolKind,
    pub parent_id: Option<SymbolId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    ModDef,
    UnionDef,
    StructDef,
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
    Platform,
    Component,
    Submodule,
    Socket,
    Field,
    Ctor,
    Enumerant,
}

impl SymbolTable {
    pub fn symbol(&self, symbol_id: SymbolId) -> Symbol {
        self.symbols_by_id[symbol_id.0 as usize].clone()
    }

    pub fn resolve(&self, fqn: &BStr) -> Option<&Symbol> {
        for (symbol_fqn, symbol) in &self.symbols {
            if symbol_fqn == fqn {
                return Some(symbol);
            }
        }
        None
    }

    pub fn symbols(&self) -> Vec<Symbol> {
        self.symbols.iter().map(|(_key, val)| val.clone()).collect()
    }

    pub fn items(&self) -> Vec<Symbol> {
        self.symbols
            .values()
            .cloned()
            .filter(|val| val.kind.is_item())
            .collect()
    }

    pub fn typedefs(&self) -> Vec<Symbol> {
        self.symbols
            .values()
            .cloned()
            .filter(|val| val.kind.is_typedef())
            .collect()
    }

    pub fn slots(&self, item_symbol_id: SymbolId) -> Vec<&Symbol> {
        let mut slots = vec![];
        for symbol in self.symbols.values() {
            if symbol.parent_id() == Some(item_symbol_id) {
                slots.push(symbol);
            }
        }
        slots
    }

    pub fn slot(&self, item_symbol_id: SymbolId, slot_name: &BStr) -> Option<&Symbol> {
        for symbol in self.symbols.values() {
            if symbol.parent_id() == Some(item_symbol_id) && slot_name == symbol.name {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_item_fqn(&self, item_fqn: &BStr) -> Option<&Symbol> {
        self.lookup_item_by_fqn(item_fqn)
    }

    pub fn resolve_item_in_package(&self, name: &BStr, package: PackageFqn) -> Option<&Symbol> {
        use bstr::ByteSlice;

        let item_fqn: BString = format!("{package}::{name}").into();
        self.lookup_item_by_fqn(item_fqn.as_bstr())
    }

    pub fn resolve_item(&self, name: &BStr, in_package: PackageFqn) -> Option<&Symbol> {
        if let Some((_package_name, _item_name)) = try_split_qualification(name) {
            self.resolve_item_fqn(name)
        } else if self.builtin_names.contains(name) {
            self.resolve_item_in_package(name, PackageFqn::new("builtin".into()))
        } else {
            self.resolve_item_in_package(name, in_package)
        }
    }

    fn lookup_item_by_fqn(&self, item_fqn: &BStr) -> Option<&Symbol> {
        for (fqn, symbol) in &self.symbols {
            if symbol.kind.is_item() && fqn == item_fqn {
                return Some(symbol);
            }
        }
        None
    }
}

// Tries to split "foo::Bar" into Some(("foo", "bar")).
// Returns None if it can't.
fn try_split_qualification(item_fqn: &BStr) -> Option<(BString, BString)> {
    if let Some(split_at) = item_fqn.windows(2).position(|window| window == b"::") {
        let first = BString::from(item_fqn[..split_at].to_vec());
        let second = BString::from(item_fqn[(split_at + 2)..].to_vec());
        Some((first, second))
    } else {
        None
    }
}

#[cfg(test)]
#[test]
fn test_try_split_qualification() {
    assert_eq!(
        try_split_qualification("foo::Bar".into()),
        Some(("foo".into(), "Bar".into()))
    );
    assert_eq!(try_split_qualification("Bar".into()), None);
}

impl Symbol {
    pub fn id(&self) -> SymbolId {
        self.id
    }

    pub fn name(&self) -> &BStr {
        use bstr::ByteSlice;
        self.name.as_bstr()
    }

    pub fn fqn(&self) -> &BStr {
        use bstr::ByteSlice;
        self.fqn.as_bstr()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn package(&self) -> PackageFqn {
        self.location.package()
    }

    pub fn kind(&self) -> SymbolKind {
        self.kind.clone()
    }

    pub fn parent_id(&self) -> Option<SymbolId> {
        self.parent_id
    }
}

impl SymbolKind {
    pub fn is_item(&self) -> bool {
        match self {
            SymbolKind::ModDef => true,
            SymbolKind::UnionDef => true,
            SymbolKind::StructDef => true,
            SymbolKind::EnumDef => true,
            SymbolKind::BuiltinDef => true,
            SymbolKind::FnDef => true,
            SymbolKind::SocketDef => true,
            SymbolKind::Platform => true,
            _ => false,
        }
    }

    pub fn is_typedef(&self) -> bool {
        match self {
            SymbolKind::UnionDef => true,
            SymbolKind::StructDef => true,
            SymbolKind::EnumDef => true,
            SymbolKind::BuiltinDef => true,
            _ => false,
        }
    }
}

impl std::fmt::Debug for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SymbolId({})", self.0)
    }
}

pub(crate) fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics = vec![];
    let mut symbols = vec![];
    let mut builtin_names = vec![];

    for package in packages.iter() {
        build_symboltable_package(
            builder,
            &mut symbols,
            &mut diagnostics,
            &mut builtin_names,
            package.clone(),
        );
    }

    let symbols_by_id: Vec<Symbol> = symbols.iter().map(|(_key, val)| val.clone()).collect();

    Arc::new(SymbolTable {
        symbols: symbols.into_iter().collect(),
        symbols_by_id,
        diagnostics,
        builtin_names: builtin_names.into_iter().collect(),
    })
}

fn build_symboltable_package(
    builder: &mut Builder,
    symbols: &mut Vec<(BString, Symbol)>,
    diagnostics: &mut Vec<Diagnostic>,
    builtin_names: &mut Vec<BString>,
    package: PackageFqn,
) {
    let analysis = builder.get_package_analysis(package.clone());
    let parsing = builder.get_parsing(package.clone());

    diagnostics.extend(analysis.diagnostics());

    for item_name in analysis.item_names() {
        build_symboltable_item(
            symbols,
            diagnostics,
            builtin_names,
            package.clone(),
            &analysis,
            &parsing,
            item_name,
        );
    }
}

fn build_symboltable_item(
    symbols: &mut Vec<(BString, Symbol)>,
    diagnostics: &mut Vec<Diagnostic>,
    builtin_names: &mut Vec<BString>,
    package: PackageFqn,
    analysis: &PackageAnalysis,
    parsing: &Parsing,
    item_name: &BString,

) {
    let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
    let node = parsing.ast_node(ast_node_id);
    let location = Location::new(package.clone(), ast_node_id);
    let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
    let kind = node_to_symbol_kind(&node);

    if package == PackageFqn::new("builtin".into()) {
        builtin_names.push(item_name.to_owned());
    }

    let id = SymbolId(symbols.len().try_into().unwrap());

    symbols.push((
        fqn.clone(),
        Symbol {
            id,
            fqn: fqn.clone(),
            name: item_name.clone(),
            location,
            kind: kind.clone(),
            parent_id: None,
        },
    ));

    match node.payload() {
        AstNodePayload::ModDef(_) => {
            build_symboltable_moddef_slot(
                symbols,
                diagnostics,
                package,
                parsing,
                item_name,
                &node,
                id,
            );
        }
        AstNodePayload::StructDef(_) |
        AstNodePayload::UnionDef(_) |
        AstNodePayload::EnumDef(_) |
        AstNodePayload::BuiltinDef(_) => {
            build_symboltable_typedef_slot(
                symbols,
                diagnostics,
                package,
                parsing,
                item_name,
                &node,
                id,
            );
        }
        AstNodePayload::FnDef(_) => (),
        AstNodePayload::SocketDef(_) => (),
        AstNodePayload::Platform(_) => {
            build_symboltable_platform_slot(
                symbols,
                diagnostics,
                package,
                parsing,
                item_name,
                &node,
                id,
            );
        }
        _ => unreachable!("Unexpected node: {:?}", node.summary()),
    }
}

fn build_symboltable_moddef_slot(
    symbols: &mut Vec<(BString, Symbol)>,
    diagnostics: &mut Vec<Diagnostic>,
    package: PackageFqn,
    parsing: &Parsing,
    item_name: &BString,
    node: &AstNode<'_>,
    parent_id: SymbolId,
) {
    let mut seen: IndexMap<BString, Region> = IndexMap::new();

    for child in node.children() {
        let (component_name, component_ast_node_id, kind) = match child.payload() {
            AstNodePayload::Component(component) => {
                let name: BString = parsing.string(component.name).to_owned();
                (name, child.id(), SymbolKind::Component)
            }
            AstNodePayload::Submodule(module) => {
                let name: BString = parsing.string(module.name).to_owned();
                (name, child.id(), SymbolKind::Submodule)
            }
            AstNodePayload::Socket(socket) => {
                let name: BString = parsing.string(socket.name).to_owned();
                (name, child.id(), SymbolKind::Socket)
            }
            _ => continue,
        };

        let component_region = Region::new(package.clone(), child.span());

        if seen.contains_key(&component_name) {
            diagnostics.push(
                diagnostics::DuplicateSlot {
                    item: item_name.to_owned().into(),
                    region: component_region,
                    slot: component_name,
                }
                .into(),
            );
            continue;
        }

        seen.insert(component_name.clone(), component_region);

        let component_fqn: BString =
            format!("{}::{}::{}", package, item_name, component_name).into();
        let component_location = Location::new(package.clone(), component_ast_node_id);
        let component_id = SymbolId(symbols.len().try_into().unwrap());

        symbols.push((
            component_fqn.clone(),
            Symbol {
                id: component_id,
                fqn: component_fqn,
                name: component_name,
                location: component_location,
                kind,
                parent_id: Some(parent_id),
            },
        ));
    }
}

fn build_symboltable_typedef_slot(
    symbols: &mut Vec<(BString, Symbol)>,
    diagnostics: &mut Vec<Diagnostic>,
    package: PackageFqn,
    parsing: &Parsing,
    item_name: &BString,
    node: &AstNode<'_>,
    parent_id: SymbolId,
) {
    let mut seen: IndexMap<BString, Region> = IndexMap::new();

    for child in node.children() {
        let (slot_name, slot_ast_node_id, kind) = match child.payload() {
            AstNodePayload::Field(field) => {
                let name: BString = parsing.string(field.name).to_owned();
                (name, child.id(), SymbolKind::Field)
            }
            AstNodePayload::Ctor(ctor) => {
                let name: BString = parsing.string(ctor.name).to_owned();
                (name, child.id(), SymbolKind::Ctor)
            }
            AstNodePayload::Enumerant(enumerant) => {
                let name: BString = parsing.string(enumerant.name).to_owned();
                (name, child.id(), SymbolKind::Enumerant)
            }
            _ => continue,
        };

        let slot_region = Region::new(package.clone(), child.span());

        if seen.contains_key(&slot_name) {
            diagnostics.push(
                diagnostics::DuplicateSlot {
                    item: item_name.to_owned().into(),
                    region: slot_region,
                    slot: slot_name,
                }
                .into(),
            );
            continue;
        }

        seen.insert(slot_name.clone(), slot_region);

        let slot_fqn: BString =
            format!("{}::{}::{}", package, item_name, slot_name).into();
        let slot_location = Location::new(package.clone(), slot_ast_node_id);
        let slot_id = SymbolId(symbols.len().try_into().unwrap());

        symbols.push((
            slot_fqn.clone(),
            Symbol {
                id: slot_id,
                fqn: slot_fqn,
                name: slot_name,
                location: slot_location,
                kind,
                parent_id: Some(parent_id),
            },
        ));
    }
}

fn build_symboltable_platform_slot(
    symbols: &mut Vec<(BString, Symbol)>,
    diagnostics: &mut Vec<Diagnostic>,
    package: PackageFqn,
    parsing: &Parsing,
    item_name: &BString,
    node: &AstNode<'_>,
    parent_id: SymbolId,
) {
    let mut seen: IndexMap<BString, Region> = IndexMap::new();

    for child in item_children(node) {
        let AstNodePayload::Component(component) = child.payload() else { continue };
        if !matches!(component.kind,
            crate::common::ComponentKind::Incoming
            | crate::common::ComponentKind::Outgoing
        ) { continue; }

        let port_name: BString = parsing.string(component.name).to_owned();
        let port_region = Region::new(package.clone(), child.span());

        if seen.contains_key(&port_name) {
            diagnostics.push(
                diagnostics::DuplicateSlot {
                    item: item_name.to_owned().into(),
                    region: port_region,
                    slot: port_name,
                }.into(),
            );
            continue;
        }
        seen.insert(port_name.clone(), port_region);

        let fqn: BString = format!("{}::{}::{}", package, item_name, port_name).into();
        let location = Location::new(package.clone(), child.id());
        let slot_id = SymbolId(symbols.len().try_into().unwrap());
        symbols.push((fqn.clone(), Symbol {
            id: slot_id, fqn, name: port_name, location,
            kind: SymbolKind::Component,
            parent_id: Some(parent_id),
        }));
    }
}

fn node_to_symbol_kind(node: &AstNode<'_>) -> SymbolKind {
    match node.payload() {
        AstNodePayload::ModDef(_) => SymbolKind::ModDef,
        AstNodePayload::StructDef(_) => SymbolKind::StructDef,
        AstNodePayload::UnionDef(_) => SymbolKind::UnionDef,
        AstNodePayload::EnumDef(_) => SymbolKind::EnumDef,
        AstNodePayload::BuiltinDef(_) => SymbolKind::BuiltinDef,
        AstNodePayload::FnDef(_) => SymbolKind::FnDef,
        AstNodePayload::SocketDef(_) => SymbolKind::SocketDef,
        AstNodePayload::Platform(_) => SymbolKind::Platform,
        _ => unreachable!(),
    }
}pub(crate) fn build_symbol_ast(builder: &mut Builder, symbol_id: SymbolId) -> AstNodeId {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    symbol.location().ast_node_id()
}
