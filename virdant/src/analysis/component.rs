use std::sync::Arc;

use bstr::{BStr, BString};

use crate::analysis::Location;
use crate::analysis::db::Builder;
use crate::analysis::symboltable::SymbolId;
use crate::analysis::typecheck::Type;
use crate::common::json::ToJson;
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct ComponentAnalysis {
    // TODO reference to ModDef this is created for
    moddef: SymbolId,
    components: Vec<(BString, Type)>,
}

impl ComponentAnalysis {
    pub fn type_of(&self, path: &BStr) -> Type {
        for (path_, typ) in &self.components {
            if path == path_ {
                return typ.clone();
            }
        }
        panic!()
    }

    pub fn components(&self) -> Vec<(BString, Type)> {
        self.components.clone()
    }
}

pub fn build_component_analysis(builder: &mut Builder, moddef: SymbolId) -> Arc<ComponentAnalysis> {
    let mut component_analysis = ComponentAnalysis {
        moddef,
        components: vec![],
    };

    let location = find_item_location(builder, moddef);
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let path = parsing.string(component.name).to_owned();
                let typ_node = stmt.typ().unwrap();
                let typ = node_to_typ(typ_node, parsing.clone());
                component_analysis.components.push((path, typ));
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

fn node_to_typ(typ_node: AstNode<'_>, parsing: Arc<Parsing>) -> Type {
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
                    let name = parsing.string(ofness.name);
                    BString::from(format!("{}::{name}", package.to_str_lossy()).into_bytes())
                }
                _ => panic!(),
            };

            match type_name.as_slice() {
                b"builtin::Bit" => Type::Bit,
                b"builtin::Clock" => Type::Clock,
                b"builtin::Word" => {
                    let spelling = typ_node.spelling().to_str_lossy().into_owned();
                    let width = spelling
                        .strip_prefix("Word[")
                        .or_else(|| spelling.strip_prefix("builtin::Word["))
                        .and_then(|rest| rest.strip_suffix(']'))
                        .and_then(|width| width.parse::<u64>().ok())
                        .unwrap();
                    Type::Word(width)
                }
                _ => panic!("Unsupported type: {}", type_name.to_str_lossy()),
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
