use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::location::Location;
use crate::types::typing::item_for;
use crate::types::{ExprRoot, Type};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};

fn find_exprroot(builder: &mut Builder, location: Location) -> Option<ExprRoot> {
    let parsing = builder.get_parsing(location.package());

    let exprroot_ids: IndexSet<_> = builder
        .get_exprroots()
        .iter()
        .filter(|exprroot| exprroot.location.package() == location.package())
        .map(|exprroot| exprroot.location().ast_node_id())
        .collect();

    let mut node = parsing.ast_node(location.ast_node_id());
    loop {
        if exprroot_ids.contains(&node.id()) {
            return Some(ExprRoot::new(node.location()));
        }

        if let Some(parent) = node.parent() {
            let parent_id = parent.id();
            node = parsing.ast_node(parent_id);
        } else {
            return None;
        }
    }
}

pub(crate) fn build_exprroot_for(builder: &mut Builder, location: Location) -> ExprRoot {
    if let Some(exprroot) = find_exprroot(builder, location.clone()) {
        return exprroot;
    }

    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    builder.dump();
    dbg!(&node);
    eprintln!("{:?}", node.summary());
    eprintln!("{:?}", parsing.text(node.span()));
    eprintln!("{:?}", node.region());
    panic!("No ExprRoot found")
}

pub(crate) fn build_typeof(builder: &mut Builder, location: Location) -> Result<Type, Vec<Diagnostic>> {
    let Some(exprroot) = find_exprroot(builder, location.clone()) else {
        let region = builder.get_location_region(location.clone());
        return Err(vec![diagnostics::Todo {
            region,
            message: "No expr root".into(),
        }.into()]);
    };
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

pub(crate) fn build_typeof_all(builder: &mut Builder) -> IndexMap<Location, Option<Type>> {
    let mut typeof_all = IndexMap::new();

    let all_exprs = builder.get_all_exprs();
    for location in all_exprs.iter() {
        let item = item_for(builder, location.clone());
        let parsing = builder.get_parsing(item.package());
        let item_node = parsing.ast_node(item.location().ast_node_id());
        if !item_node.contains_errors() {
            // TODO Probably need thread the diagnostics through instead of discarding them here.
            if let Ok(typ) = builder.get_typeof(location.clone()) {
                typeof_all.insert(location.clone(), Some(typ));
            }
        }
    }

    typeof_all
}
