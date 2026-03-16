use std::sync::Arc;

use bstr::{BStr, BString};

use crate::analysis::Location;
use crate::analysis::symboltable::{SymbolId, SymbolTable};
use crate::analysis::typecheck::Type;
use crate::common::Width;
use crate::common::json::ToJson;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct ComponentAnalysis {
    // TODO reference to ModDef this is created for
    moddef: SymbolId,
    components: Vec<(BString, Option<Type>)>,
    diagnostics: Vec<Diagnostic>,
}

impl ComponentAnalysis {
    pub fn type_of(&self, path: &BStr) -> Option<Type> {
        for (path_, typ) in &self.components {
            if path == path_ {
                return typ.clone();
            }
        }
        None
    }

    pub fn components(&self) -> Vec<(BString, Option<Type>)> {
        self.components.clone()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}

pub fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
        diagnostics: vec![],
    };

    let symboltable = builder.get_symboltable();
    let location = find_item_location(builder, moddef);
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let path = parsing.string(component.name).to_owned();
                let typ_node = stmt.typ().unwrap();
                match node_to_typ(typ_node, parsing.clone(), symboltable.clone()) {
                    Ok(typ) => component_analysis.components.push((path, Some(typ))),
                    Err(diag) => component_analysis.diagnostics.push(diag),
                }
            }
//            AstNodePayload::Module(module) => todo!(), // TODO
//            AstNodePayload::ModDefStmtBlock(mod_def_stmt_block) => todo!(),
//            AstNodePayload::ModDefStmtIf => todo!(),
//            AstNodePayload::ModDefStmtMatch => todo!(),
//            AstNodePayload::Socket(socket) => todo!(),
            _ => (),
        }
    }

    Arc::new(component_analysis)
}

fn node_to_typ(typ_node: AstNode<'_>, parsing: Arc<Parsing>, symboltable: Arc<SymbolTable>) -> Result<Type, Diagnostic> {
    use bstr::ByteSlice;

    match typ_node.payload() {
        AstNodePayload::Type(_typ) => {
            // TODO Look at the internned string behind the "Ofness" node in the AST.
            // Turn it into a fully-qualified type name if it's Bit, Clock, or Word[n].
            // Then convert it to a Type::Bit, Type::Clock, or Type::Word[n], respectively.
            let type_name = match typ_node.child(0).payload() {
                AstNodePayload::Ofness(ofness) => {
                    let package = ofness
                        .package
                        .map(|package| parsing.string(package).to_owned())
                        .unwrap_or_else(|| BString::from(b"builtin".to_vec()));
                    parsing.string(ofness.name)
//                    let name = parsing.string(ofness.name);
//                    BString::from(format!("{}::{name}", package.to_str_lossy()).into_bytes())
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
                    let spelling = typ_node.spelling().to_str_lossy().into_owned();
                    let width = spelling
                        .strip_prefix("Word[")
                        .or_else(|| spelling.strip_prefix("builtin::Word["))
                        .and_then(|rest| rest.strip_suffix(']'))
                        .and_then(|width| width.parse::<Width>().ok())
                        .unwrap();
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

pub fn find_item_location(builder: &mut Builder, item: SymbolId) -> Location {
    let symboltable = builder.get_symboltable();
    symboltable.symbol(item).location()
}

impl ToJson for ComponentAnalysis {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
