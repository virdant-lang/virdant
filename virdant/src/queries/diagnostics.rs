use hashbrown::HashMap;

use crate::ast::{Ast, AstNode};
use crate::db::Builder;
use crate::error;
use crate::fqn::PackageFqn;
use crate::graph::{CycleError, Graph};
use crate::source::{Region, Span};
use crate::union::Union;

pub type Params = ();
pub type Response = Result<(), Vec<error::Diagnostic>>;

pub fn build(builder: &mut Builder, _: ()) -> Result<(), Vec<error::Diagnostic>> {
    let mut errors = vec![];

    let asts = get_asts(builder);

    for ast in &asts {
        errors.extend(diagnostics_parse_errors(ast));
    }

    errors.extend(diagnostics_import_cycles(builder));

    for ast in &asts {
        errors.extend(diagnostics_import_errors(builder, ast));
    }

    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(())
    }
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

fn diagnostics_parse_errors(ast: &Ast) -> Vec<error::Diagnostic> {
    let mut errors = vec![];
    let ast_errors = ast.errors();
    let mut error_union: Union<AstNode> = Union::new();

    let mut indexes = vec![];

    // The parser will sometimes report multiple errors for what should be a single error.
    // We coalesce errors together when they should be reported as just one.
    for error in ast_errors.clone() {
        let index = error_union.insert(error);
        indexes.push(index);
    }

    // Inefficient double loop, but hopefully you don't have billions of syntax errors.
    for i in indexes.iter().cloned() {
        for j in indexes.iter().cloned() {
            let i_region = error_union[i].region();
            let j_region = &error_union[j].region();

            // This condition is fudged.
            // It should be that the regions either overlap
            // or that one region immediately follows another.
            // It should also join when the interspersing source is
            // whitespace or invalid tokens.
            //
            // It does none of these things.
            //
            // Instead, we just look to see if the two errors start on the same line.
            if i_region.end().line() == j_region.start().line() {
                error_union.join(i, j);
            }
        }
    }

    for error_group in error_union.into_iter() {
        let first_error_region = error_group.first().unwrap().region();

        let package = first_error_region.package();
        let mut start = first_error_region.start();
        let mut end = first_error_region.end();

        // take the individual error regions and create a big error region contianing them all.
        for error in error_group {
            start = start.min(error.region().start());
            end = end.max(error.region().end());
        }
        let region = Region::new(package, Span::new(start, end));
        errors.push(
            error::ParseError {
                region,
            }.into(),
        );
    }

    errors
}

fn diagnostics_import_cycles(builder: &mut Builder) -> Vec<error::Diagnostic> {
    let mut errors = vec![];
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

    // Are there any import cycles?
    if let Err(CycleError(cycle)) = import_graph.toposort() {
        // This gives goes from the cycle [a, b, c] to an iterator [(a, b), (b, c), (c, a)].
        // This allows us to add one "copy" of the error for each import site.
        let cyclic_pair_iter = cycle.iter().zip(cycle.iter().cycle().skip(1)).take(cycle.len());

        for (from_package_index, to_package_index) in cyclic_pair_iter {
            let from_package = import_graph[*from_package_index].clone();
            let to_package = import_graph[*to_package_index].clone();
            let key = (from_package, to_package);
            let region = import_sites[&key].clone();

            // This is just the string representation of the cycle.
            let package_cycle = cycle.iter().cloned().map(|index| import_graph[index].to_string()).collect();

            errors.push(error::ImportCycle {
                region,
                package_cycle,
            }.into());
        }
    }
    errors
}

fn diagnostics_import_errors(builder: &mut Builder, ast: &Ast) -> Vec<error::Diagnostic> {
    let mut errors = vec![];
    let mut is_in_header = true;

    let mut import_regions = HashMap::new();

    let packages_that_exist = builder.get_packages(());
    let package = ast.root();

    for stmt in package.stmts() {
        if stmt.as_item().is_some() {
            // this is set once we reach the first item
            // any imports found after this point is an error.
            is_in_header = false;
        } else if !is_in_header {
            if stmt.as_import().is_some() {
                errors.push(
                    error::ImportNotAtTopError {
                        region: stmt.as_ast_node().region(),
                    }.into(),
                );
            }
        }

        // shares the same loop to save a few cycles
        if let Some(import) = stmt.as_import() {
            // track where this import is so we can go back and mark duplicates
            if !import_regions.contains_key(&import.imported_package()) {
                import_regions.insert(import.imported_package(), vec![]);
            }
            let regions = import_regions.get_mut(&import.imported_package()).unwrap();
            regions.push(stmt.as_ast_node().region());

            // see if this package is loaded into the Vir object and error if it's missing.
            let package_name = PackageFqn::new(builder.stringtable().get(&import.imported_package()).to_owned());
            if !packages_that_exist.contains(&package_name) {
                errors.push(
                    error::UnresolvedImportError {
                        region: stmt.as_ast_node().region(),
                        imported_package: package_name,
                    }.into(),
                );
            }
        }
    }

    // find all imports which are duplicated and flag all but the first.
    for (import_name, regions) in import_regions {
        if regions.len() > 1 {
            for region in regions.into_iter().skip(1) {
                errors.push(
                    error::DuplicateImportError {
                        region,
                        imported_package: PackageFqn::new(builder.stringtable().get(&import_name).to_owned()),
                    }.into(),
                );
            }
        }
    }

    errors
}
