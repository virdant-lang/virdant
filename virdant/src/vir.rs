#![allow(unused, dead_code, unused_variables)] // TODO

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

    pub fn diagnostics(&self) -> Result<(), Vec<error::VirError>> {
        let mut errors = vec![];

        let start = LineCol::new(1, 1);
        let end = LineCol::new(3, 1);
        for ast in &self.asts {
            for error_node in ast.errors() {
                errors.push(
                    error::ParseError {
                        region: error_node.region(),
                    }.into(),
                );
            }
        }
        if errors.len() > 0 {
            tracing::info!("{errors:#?}");
            Err(errors)
        } else {
            Ok(())
        }
    }
}
