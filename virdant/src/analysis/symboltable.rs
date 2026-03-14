use std::sync::Arc;

use bstr::BString;
use hashbrown::HashMap;

use crate::syntax::ast::AstNodeId;
use crate::analysis::db::Builder;
use crate::fqn::PackageFqn;
use crate::diagnostics::Diagnostic;
use crate::common::json::ToJson;

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<BString, Symbol>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    ModDef(ModDef),
}

#[derive(Debug, Clone)]
pub struct ModDef {
    name: BString,
    location: Location,
}

#[derive(Debug, Clone)]
pub struct Location(PackageFqn, AstNodeId);

pub fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics = vec![];
    let mut symbols = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics.extend(analysis.diagnostics());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref()).unwrap();
            let location = Location::new(package.clone(), ast_node_id);
            symbols.push((
                item_name.clone(),
                Symbol::ModDef(ModDef {
                    name: item_name.clone(),
                    location,
                }),
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
