use crate::db::Builder;
use crate::fqn::ItemFqn;
use crate::ast::node;

pub type Params = ItemFqn;
pub type Response = Result<node::Item, ()>;

pub fn build_itemresolution(builder: &mut Builder, item_fqn: ItemFqn) -> Result<node::Item, ()> {
    let ast = builder.get_ast(item_fqn.package());
    for item in ast.root().items() {
        if item.name() == item_fqn.name() {
            return Ok(item);
        }
    }
    Err(())
}
