use crate::analysis::location::Location;
use crate::db::Builder;
use crate::common::source::Region;

pub(crate) fn build_location_region(builder: &mut Builder, location: Location) -> Region {
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    node.region()
}
