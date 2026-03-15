use std::sync::Arc;

use bstr::{BStr, BString};

use crate::analysis::Location;
use crate::analysis::db::Builder;
use crate::analysis::typecheck::Type;
use crate::common::json::ToJson;
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNodeId;
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
}

pub fn build_component_analysis(builder: &mut Builder, moddef_fqn: BString) -> Arc<ComponentAnalysis> {
    use bstr::ByteSlice;
    let mut component_analysis = ComponentAnalysis {
        moddef_fqn: moddef_fqn.clone(),
        components: vec![],
    };

    let location = find_component(builder, moddef_fqn);
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let path = parsing.string(component.name).to_owned();
                let typ = Type::Word(8);
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

fn find_component(builder: &mut Builder, moddef_fqn: BString) -> Location {
    // TODO HACK I really need to not use a BString and instead, use a SymbolId, which can locate
    // the package it's in. Until then, we can just brute force and deal with duplicates.
    let (package_name, moddef_name) = split(moddef_fqn.clone());
    let package: PackageFqn = PackageFqn::new(package_name.into());
    let parsing = builder.get_parsing(package.clone());
    for item_node in parsing.root().children() {
        if !matches!(item_node.payload(), AstNodePayload::ModDef(_) | AstNodePayload::BuiltinDef(_)) {
            continue;
        }

        let name = parsing.string(item_node.name().unwrap());

        if name == moddef_name {
            return Location::new(package.clone(), item_node.id());
        }
    }
    panic!("Couldn't find component {moddef_fqn}")
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
