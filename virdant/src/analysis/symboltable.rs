use std::sync::Arc;

use bstr::{BStr, BString};
use hashbrown::HashMap;
use indexmap::IndexMap;

use crate::syntax::ast::AstNodeId;
use crate::analysis::db::Builder;
use crate::fqn::PackageFqn;
use crate::diagnostics::Diagnostic;
use crate::common::json::ToJson;

#[derive(Debug)]
pub struct SymbolTable {
    symbols: IndexMap<BString, Symbol>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    fqn: BString,
    location: Location,
    kind: SymbolKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    ModDef,
    UnionDef,
    StructDef,
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
}

// TODO move this to maybe the ast module?
#[derive(Debug, Clone)]
pub struct Location(PackageFqn, AstNodeId);

impl Symbol {
    pub fn fqn(&self) -> &BStr {
        use bstr::ByteSlice;
        self.fqn.as_bstr()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn kind(&self) -> SymbolKind {
        self.kind.clone()
    }
}

pub fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics = vec![];
    let mut symbols = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics.extend(analysis.diagnostics());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let location = Location::new(package.clone(), ast_node_id);
            let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
            symbols.push((
                fqn.clone(),
                Symbol {
                    fqn,
                    location,
                    kind: SymbolKind::ModDef,
                },
            ));
        }
    }

    Arc::new(SymbolTable {
        symbols: symbols.into_iter().collect(),
        diagnostics,
    })
}

impl SymbolTable {
    pub fn symbols(&self) -> Vec<Symbol> {
        self.symbols.iter().map(|(key, val)| val.clone()).collect()
    }
}

impl Location {
    pub fn new(package: PackageFqn, ast_node_id: AstNodeId) -> Location {
        Location(package, ast_node_id)
    }

    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn ast_node_id(&self) -> AstNodeId {
        self.1
    }
}

impl ToJson for SymbolTable {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
