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

#[test]
fn test_itemresolution() {
    use crate::Vir;
    let examples_dir: std::path::PathBuf = std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap();


    let mut vir = Vir::new();
    vir.open_dir(&examples_dir).unwrap();
    vir.diagnostics().unwrap();

    assert!(matches!(vir.db().get_itemresolution(ItemFqn::new(b"basic::Top".into())), Ok(_)));
    assert!(matches!(vir.db().get_itemresolution(ItemFqn::new(b"basic::Foo".into())), Ok(_)));
    assert!(matches!(vir.db().get_itemresolution(ItemFqn::new(b"lfsr::Lfsr".into())), Ok(_)));
    assert!(matches!(vir.db().get_itemresolution(ItemFqn::new(b"lfsr::Bar".into())), Err(_)));

    let basic_top = vir.db().get_itemresolution(ItemFqn::new(b"basic::Top".into())).unwrap();
    assert_eq!(basic_top.kind(), crate::common::ItemKind::ModDef);

    let fns_foo = vir.db().get_itemresolution(ItemFqn::new(b"fns::foo".into())).unwrap();
    assert_eq!(fns_foo.kind(), crate::common::ItemKind::FnDef);
}
