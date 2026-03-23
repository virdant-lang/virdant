use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::component::{Component, ComponentAnalysis};
use crate::analysis::location::Location;
use crate::analysis::symbols::{SymbolId, SymbolTable};
use crate::analysis::types::Type;
use crate::common::{ComponentKind, Flow, Width};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
        diagnostics: vec![],
    };

    let symboltable = builder.get_symboltable();
    let location = find_item_location(builder, moddef);
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());

    // ensure all dependent packages have been created
    // TODO this doesn't recurse though?
    let package_analysis = builder.get_package_analysis(location.package());
    for import in package_analysis.imports() {
        builder.get_package_analysis(import);
    }

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let path = parsing.string(component.name).to_owned();
                let typ_node = stmt.typ().unwrap();
                match node_to_typ(typ_node, &parsing, &symboltable) {
                    Ok(typ) => {
                        let flow = match component.kind {
                            ComponentKind::Incoming => Flow::Source,
                            ComponentKind::Outgoing => Flow::Sink,
                            ComponentKind::Reg => Flow::Duplex,
                            ComponentKind::Wire => Flow::Duplex,
                        };
                        let component = Component {
                            path: path.clone(),
                            typ: Some(typ),
                            flow,
                        };
                        component_analysis.components.push((path.clone(), component));
                    }
                    Err(diag) => component_analysis.diagnostics.push(diag),
                }
            }
            AstNodePayload::Module(module) => {
                let instance_name = parsing.string(module.name);
                let ofness_node = stmt.child(0);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let submodule_package = ofness
                    .package
                    .map(|pkg| PackageFqn::new(bstr::BString::from(parsing.string(pkg).to_vec())))
                    .unwrap_or_else(|| location.package());
                let submodule_name = parsing.string(ofness.name);
                let submodule_symbol = match symboltable.resolve_item_in_package(submodule_name, submodule_package) {
                    Some(symbol) => symbol.clone(),
                    None => continue,
                };
                let submodule_location = submodule_symbol.location();
                let submodule_parsing = builder.get_parsing(submodule_location.package());
                let submodule_ast = submodule_parsing.ast_node(submodule_location.ast_node_id());
                for submodule_stmt in submodule_ast.children() {
                    let AstNodePayload::Component(component) = submodule_stmt.payload() else {
                        continue;
                    };
                    if !matches!(component.kind, ComponentKind::Incoming | ComponentKind::Outgoing) {
                        continue;
                    }
                    let port_name = submodule_parsing.string(component.name);
                    let qualified_name = bstr::BString::from(
                        format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes()
                    );
                    let typ_node = submodule_stmt.typ().unwrap();
                    match node_to_typ(typ_node, &submodule_parsing, &symboltable) {
                        Ok(typ) => {
                            let path = qualified_name.to_owned();
                            let flow = match component.kind {
                                ComponentKind::Incoming => Flow::Source,
                                ComponentKind::Outgoing => Flow::Sink,
                                ComponentKind::Reg => Flow::Duplex,
                                ComponentKind::Wire => Flow::Duplex,
                            };
                            let component = Component {
                                path,
                                typ: Some(typ),
                                flow,
                            };
                            component_analysis.components.push((qualified_name, component))
                        }
                        Err(diag) => component_analysis.diagnostics.push(diag),
                    }
                }
            }
            _ => (),
        }
    }

    Arc::new(component_analysis)
}

// TODO shouldn't this be private
pub(crate) fn find_item_location(builder: &mut Builder, item: SymbolId) -> Location {
    let symboltable = builder.get_symboltable();
    symboltable.symbol(item).location()
}

pub(crate) fn node_to_typ(typ_node: AstNode<'_>, parsing: &Parsing, symboltable: &SymbolTable) -> Result<Type, Diagnostic> {
    match typ_node.payload() {
        AstNodePayload::Type(_typ) => {
            let type_name = match typ_node.child(0).payload() {
                AstNodePayload::Ofness(ofness) => {
                    let _package = ofness
                        .package
                        .map(|package| parsing.string(package).to_owned())
                        .unwrap_or_else(|| bstr::BString::from(b"builtin".to_vec()));
                    parsing.string(ofness.name)
                }
                _ => panic!(),
            };

            if let Some(symbol) = symboltable.resolve_item(type_name.as_bstr(), parsing.package()) {
                let bit_symbol = symboltable.resolve(b"builtin::Bit".into()).unwrap();
                let word_symbol = symboltable.resolve(b"builtin::Word".into()).unwrap();
                let clock_symbol = symboltable.resolve(b"builtin::Clock".into()).unwrap();

                let typ = if symbol.id() == bit_symbol.id() {
                    Type::Bit
                } else if symbol.id() == clock_symbol.id() {
                    Type::Clock
                } else if symbol.id() == word_symbol.id() {
                    let generics_node = typ_node.child(1);
                    let AstNodePayload::GenericsParams(generics_params) = generics_node.payload() else {
                        unreachable!()
                    };
                    let spelling = parsing.string(generics_params.value).to_str_lossy().into_owned();
                    let width = spelling.parse::<Width>().unwrap();
                    Type::Word(width)
                } else {
                    Type::Usual(symbol.id())
                };
                Ok(typ)
            } else {
                Err(diagnostics::UnresolvedType {
                    region: typ_node.region(),
                    typ: type_name.into(),
                }.into())
            }
        }
        _ => panic!(),
    }
}
