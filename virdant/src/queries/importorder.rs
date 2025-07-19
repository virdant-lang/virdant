use hashbrown::HashMap;

use crate::ast::Ast;
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::graph::{CycleError, Graph};
use crate::source::Region;

pub type Params = ();
pub type Response = Result<Vec<PackageFqn>, CycleError>;

pub fn build_importorder(builder: &mut Builder, (): ()) -> Result<Vec<PackageFqn>, CycleError> {
    let mut import_graph: Graph<PackageFqn> = Graph::new();

    let asts = get_asts(builder);

    // import_sites[(from_package, to_package)] points to the first import statement of to_package is in from_package.
    let mut import_sites: HashMap<(PackageFqn, PackageFqn), Region> = HashMap::new();

    for ast in &asts {
        import_graph.add_vert(ast.package());
    }

    for ast in &asts {
        for import in ast.root().imports() {
            let from_package = ast.package();
            let to_package = PackageFqn::new(builder.stringtable().get(&import.imported_package()).into());

            // Skip if either package is unresolved.
            // This gets handled later.
            if let (Some(from_index), Some(to_index)) = (
                import_graph.vertex(&from_package),
                import_graph.vertex(&to_package),
            ) {
                import_graph.add_edge(from_index, to_index);
            }

            // Record import site if this is the first import statement.
            let key = (from_package, to_package);
            if !import_sites.contains_key(&key) {
                import_sites.insert(key, import.as_ast_node().region());
            }
        }
    }

    import_graph.toposort()
}

fn get_asts(builder: &mut Builder) -> Vec<Ast> {
    let mut asts = vec![];
    let packages = builder.get_packages(());

    for package in &packages {
        let ast = builder.get_ast(package.clone());
        asts.push(ast);
    }
    asts
}
