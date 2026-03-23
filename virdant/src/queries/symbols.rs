use std::sync::Arc;

use bstr::BString;
use hashbrown::{HashMap, HashSet};
use indexmap::IndexMap;

use crate::analysis::location::Location;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolKind, SymbolTable};
use crate::db::Builder;
use crate::diagnostics;
use crate::source::Region;
use crate::syntax::ast::AstNodeId;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics_vec = vec![];
    let mut symbols = vec![];
    let mut builtin_names = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics_vec.extend(analysis.diagnostics());
        let parsing = builder.get_parsing(package.clone());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let location = Location::new(package.clone(), ast_node_id);
            let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
            let kind = match node.payload() {
                AstNodePayload::ModDef(_) => SymbolKind::ModDef,
                AstNodePayload::StructDef(_) => SymbolKind::StructDef,
                AstNodePayload::UnionDef(_) => SymbolKind::UnionDef,
                AstNodePayload::EnumDef(_) => SymbolKind::EnumDef,
                AstNodePayload::BuiltinDef(_) => SymbolKind::BuiltinDef,
                AstNodePayload::FnDef(_) => SymbolKind::FnDef,
                AstNodePayload::SocketDef(_) => SymbolKind::SocketDef,
                _ => unreachable!(),
            };

            if kind == SymbolKind::BuiltinDef {
                builtin_names.push(item_name.to_owned());
            }

            let id = SymbolId(symbols.len().try_into().unwrap());

            symbols.push((
                fqn.clone(),
                Symbol {
                    id,
                    fqn: fqn.clone(),
                    location,
                    kind: kind.clone(),
                },
            ));

            if kind == SymbolKind::ModDef {
                let mut seen: HashMap<BString, Region> = HashMap::new();

                for child in node.children() {
                    let (component_name, component_ast_node_id, kind) = match child.payload() {
                        AstNodePayload::Component(component) => {
                            let name: BString = parsing.string(component.name).to_owned();
                            (name, child.id(), SymbolKind::Component)
                        }
                        AstNodePayload::Module(module) => {
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
                        diagnostics_vec.push(diagnostics::DuplicateSlot {
                            item: item_name.to_owned().into(),
                            region: component_region,
                            slot: component_name,
                        }.into());
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
                            location: component_location,
                            kind,
                        },
                    ));
                }
            }
        }
    }

    Arc::new(SymbolTable {
        symbols: symbols.into_iter().collect(),
        diagnostics: diagnostics_vec,
        builtin_names: builtin_names.into_iter().collect(),
    })
}

pub(crate) fn build_symbol_ast(builder: &mut Builder, symbol_id: SymbolId) -> AstNodeId {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    symbol.location().ast_node_id()
}
