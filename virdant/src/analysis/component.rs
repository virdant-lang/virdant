use std::sync::Arc;

use bstr::BString;

use crate::analysis::db::Builder;
use crate::common::json::ToJson;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct ComponentAnalysis {
}

pub fn build_components(builder: &mut Builder, name: BString) -> Arc<ComponentAnalysis> {
    use bstr::ByteSlice;
    let mut component_analysis = ComponentAnalysis {
    };

    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        for item_node in parsing.root().children() {
            if !matches!(item_node.payload(), AstNodePayload::ModDef(_)) {
                continue;
            }

            for child_node in item_node.children() {
                if let AstNodePayload::Component(component) = child_node.payload() {
                    if parsing.string(component.name) == name.as_bstr() {
                        // TODO
//                        component_analysis.push(component);
                    }
                }
            }
        }
    }

    Arc::new(component_analysis)
}

impl ToJson for ComponentAnalysis {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
