#![allow(unused, dead_code, unused_variables)] // TODO

use hashbrown::HashMap;
use std::string::ParseError;

use bstr::BStr;
use bstr::BString;

use crate::graph::*;
use crate::ast::*;
use crate::fqn::*;
use crate::source::*;
use crate::error;

use crate::stringtable::StringTable;

pub struct Vir {
    asts: Vec<Ast>,
    stringtable: StringTable,
}

impl Vir {
    pub fn new() -> Vir {
        Vir {
            asts: vec![],
            stringtable: StringTable::new(),
        }
    }

    pub fn set_source(&mut self, package: PackageFqn, text: BString) -> anyhow::Result<()> {
        tracing::info!("set_source({package}, {:?})", BStr::new(&text[..16.min(text.len())]));
        let stringtable = self.stringtable.clone().clone();
        if let Some(ast) = self.get_ast_mut(package.clone()) {
            let source = Source::new(package.clone(), &text);
            let mut new_ast = Ast::new(source, stringtable);
            std::mem::replace(ast, new_ast);
        } else {
            let source = Source::new(package.clone(), &text);
            self.asts.push(Ast::new(source, self.stringtable.clone()));
        }
        tracing::info!("number ASTs: {}", self.asts.len());
        Ok(())
    }

    pub fn ast(&self, package: PackageFqn) -> Option<&Ast> {
        for ast in self.asts.iter() {
            if ast.package() == package {
                return Some(ast);
            }
        }
        None
    }

    fn get_ast_mut(&mut self, package: PackageFqn) -> Option<&mut Ast> {
        for ast in self.asts.iter_mut() {
            if ast.package() == package {
                return Some(ast);
            }
        }
        None
    }

    pub fn set_packages(&mut self, package: Vec<PackageFqn>) -> anyhow::Result<()> {
        Ok(())
    }

    fn packages(&self) -> Vec<PackageFqn> {
        let mut result = vec![];
        for ast in &self.asts {
            result.push(ast.package());
        }
        result
    }

    pub fn diagnostics(&self) -> Result<(), Vec<error::VirError>> {
        let mut errors = vec![];

        for ast in &self.asts {
            errors.extend(self.diagnostics_parse_errors(ast));
        }

        errors.extend(self.diagnostics_import_cycles());

        for ast in &self.asts {
            errors.extend(self.diagnostics_import_errors(ast));
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn diagnostics_parse_errors(&self, ast: &Ast) -> Vec<error::VirError> {
        let mut errors = vec![];
        for error_node in ast.errors() {
            errors.push(
                error::ParseError {
                    region: error_node.region(),
                }.into(),
            );
        }
        errors
    }

    fn diagnostics_import_cycles(&self) -> Vec<error::VirError> {
        let mut errors = vec![];
        let mut import_graph: Graph<PackageFqn> = Graph::new();

        // import_sites[(from_package, to_package)] points to the first import statement of to_package is in from_package.
        let mut import_sites: HashMap<(PackageFqn, PackageFqn), Region> = HashMap::new();

        for ast in &self.asts {
            import_graph.add_vert(ast.package());
        }

        for ast in &self.asts {
            for import in ast.root().imports() {
                let from_package = ast.package();
                let to_package = PackageFqn::new(self.stringtable.get(&import.imported_package()).into());

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

    fn diagnostics_import_errors(&self, ast: &Ast) -> Vec<error::VirError> {
        let mut errors = vec![];
        let mut is_in_header = true;

        let mut import_regions = HashMap::new();

        let packages_that_exist = self.packages();
        let package = ast.root();

        for stmt in package.stmts() {
            if stmt.as_item().is_some() {
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

            if let Some(import) = stmt.as_import() {
                if !import_regions.contains_key(&import.imported_package()) {
                    import_regions.insert(import.imported_package(), vec![]);
                }
                let regions = import_regions.get_mut(&import.imported_package()).unwrap();
                regions.push(stmt.as_ast_node().region());

                let package_name = PackageFqn::new(self.stringtable.get(&import.imported_package()).to_owned());
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

        for (import_name, regions) in import_regions {
            if regions.len() > 1 {
                for region in regions {
                    errors.push(
                        error::DuplicateImportError {
                            regions: vec![region],
                            imported_package: PackageFqn::new(self.stringtable.get(&import_name).to_owned()),
                        }.into(),
                    );
                }
            }
        }

        errors
    }
}
