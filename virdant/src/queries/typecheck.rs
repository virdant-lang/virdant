use bstr::BString;
use hashbrown::{HashMap, HashSet};
use bstr::ByteSlice;

use crate::analysis::location::Location;
use crate::analysis::symbols::{Symbol, SymbolId};
use crate::types::typing::item_for;
use crate::types::{ExprRoot, Type};
use crate::common::Flow;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_exprroot_for(builder: &mut Builder, location: Location) -> ExprRoot {
    let parsing = builder.get_parsing(location.package());

    let exprroot_ids: HashSet<_> = builder
        .get_exprroots()
        .into_iter()
        .filter(|exprroot| exprroot.location.package() == location.package())
        .map(|exprroot| exprroot.location().ast_node_id())
        .collect();

    let mut node = parsing.ast_node(location.ast_node_id());
    let original_node = node.clone();
    loop {
        if let Some(exprroot) = exprroot_ids.get(&node.id()) {
            return ExprRoot::new(node.location());
        }

        if let Some(parent) = node.parent() {
            let parent_id = parent.id();
            node = parsing.ast_node(parent_id);
        } else {
            builder.dump();
            dbg!(&node);
            eprintln!("{:?}", original_node.summary());
            eprintln!("{:?}", parsing.text(original_node.span()));
            eprintln!("{:?}", original_node.region());
            panic!("No ExprRoot found")
        }
    }
}

pub(crate) fn build_typeof(builder: &mut Builder, location: Location) -> Result<Type, Vec<Diagnostic>> {
    let exprroot = builder.get_exprroot_for(location.clone());
    let typing = builder.get_typing(exprroot);
    if let Some(typ) = typing.type_of_node(location.ast_node_id()) {
        Ok(typ.clone())
    } else {
        let region = builder.get_location_region(location.clone());
        Err(vec![diagnostics::Todo {
            region,
            message: "No expr root".into(),
        }.into()])
    }
}

pub(crate) fn build_typeof_all(builder: &mut Builder) -> HashMap<Location, Option<Type>> {
    let mut typeof_all = HashMap::new();

    for location in builder.get_all_exprs() {
        let item = item_for(builder, location.clone());
        let parsing = builder.get_parsing(item.package());
        let item_node = parsing.ast_node(item.location().ast_node_id());
        if !item_node.contains_errors() {
            // TODO Probably need thread the diagnostics through instead of discarding them here.
            if let Ok(typ) = builder.get_typeof(location.clone()) {
                typeof_all.insert(location, Some(typ));
            }
        }
    }

    typeof_all
}
