#![allow(unused, dead_code, unused_variables)] // TODO

use hashbrown::HashMap;
use std::string::ParseError;

use bstr::BStr;
use bstr::BString;

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
        tracing::info!("set_source({package}, {:?})", BStr::new(&text[..16]));
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
                if !import_regions.contains_key(&import.name()) {
                    import_regions.insert(import.name(), vec![]);
                }
                let regions = import_regions.get_mut(&import.name()).unwrap();
                regions.push(stmt.as_ast_node().region());

                let package_name = PackageFqn::new(self.stringtable.get(&import.name()));
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
                            imported_package: PackageFqn::new(&self.stringtable.get(&import_name)),
                        }.into(),
                    );
                }
            }
        }

        errors
    }
}
