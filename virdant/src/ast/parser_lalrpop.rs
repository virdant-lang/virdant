use crate::ast::{AstData, AstNodeId};
use crate::stringtable::StringTable;
use crate::source::Source;

lalrpop_util::lalrpop_mod!(grammar);

pub fn parse(ast_data: &mut AstData, stringtable: StringTable, source: Source) {
    let parser = grammar::PackageParser::new();
    let text = String::from_utf8_lossy(&*source.text()).to_string();

    parser
        .parse(ast_data, &stringtable, &source, &text)
        .unwrap();

    let package = ast_data.package();

    ast_data.parents = vec![AstNodeId(package.clone(), u16::MAX); ast_data.payloads.len()];
    let mut stack: Vec<AstNodeId> = Vec::with_capacity(ast_data.payloads.len());
    for i in 0..ast_data.payloads.len() {
        let ast_node_id = AstNodeId(package.clone(), i.try_into().unwrap());

        let num_children = ast_data.num_children[i];
        for _ in 0..num_children {
            let child_ast_node_id = stack.pop().unwrap();
            ast_data.parents[child_ast_node_id.index()] = ast_node_id.clone();
        }

        stack.push(ast_node_id);
    }
}
