use std::sync::Arc;

use bstr::{BStr, BString};

use crate::analysis::Location;
use crate::analysis::db::Builder;
use crate::analysis::typecheck::Type;
use crate::common::json::ToJson;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct ComponentAnalysis {
    // TODO reference to ModDef this is created for
    moddef_fqn: BString,
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

pub fn build_component_analysis(builder: &mut Builder, moddef_fqn: BString) -> Arc<ComponentAnalysis> {
    use bstr::ByteSlice;
    let mut component_analysis = ComponentAnalysis {
        moddef_fqn: moddef_fqn.clone(),
        components: vec![],
    };

    let location = find_item_location(builder, moddef_fqn);
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

pub fn find_item_location(builder: &mut Builder, item_fqn: BString) -> Location {
    // TODO HACK I really need to not use a BString and instead, use a SymbolId, which can locate
    // the package it's in. Until then, we can just brute force and deal with duplicates.
    let (package_name, moddef_name) = split(item_fqn.clone());
    let package: PackageFqn = PackageFqn::new(package_name.into());
    let parsing = builder.get_parsing(package.clone());
    for item_node in parsing.root().children() {
        if !item_node.is_item() {
            continue;
        }

        let name = parsing.string(item_node.name().unwrap());

        if name == moddef_name {
            return Location::new(package.clone(), item_node.id());
        }
    }
    panic!("Couldn't find Item: {item_fqn}")
}

fn split(moddef_fqn: BString) -> (BString, BString) {
    let bytes = moddef_fqn.as_slice();
    let split_at = bytes
        .windows(2)
        .position(|window| window == b"::")
        .unwrap();

    let first = BString::from(bytes[..split_at].to_vec());
    let second = BString::from(bytes[(split_at + 2)..].to_vec());

    (first, second)
}

impl ToJson for ComponentAnalysis {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
