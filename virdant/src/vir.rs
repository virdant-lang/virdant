#![allow(unused, dead_code, unused_variables)] // TODO

use hashbrown::HashMap;
use std::os::unix::ffi::OsStrExt;
use std::string::ParseError;

use bstr::BStr;
use bstr::BString;

use crate::db::{Db, DbContext};
use crate::graph::CycleError;
use crate::graph::Graph;
use crate::ast::*;
use crate::fqn::*;
use crate::source::*;
use crate::error;

use crate::stringtable::StringTable;
use crate::union::Union;

pub struct Vir {
    db: Db,
    stringtable: StringTable,
    packages: Vec<PackageFqn>,
}

impl Vir {
    pub fn new() -> Vir {
        let stringtable = StringTable::new();
        let mut db = Db::new(DbContext {
            stringtable: stringtable.clone(),
        });

        let builtin_package = crate::fqn::PackageFqn::new(bstr::BString::new("builtin".as_bytes().to_vec()));
        let builtin_source = bstr::BString::new(include_bytes!("../../lib/builtin.vir").to_vec());

        db.set_source(builtin_package.clone(), builtin_source.clone());
        let packages = vec![builtin_package];
        db.set_packages((), packages.clone());

        Vir {
            db,
            stringtable,
            packages,
        }
    }

    pub fn open_dir(&mut self, dirpath: &std::path::Path) -> std::io::Result<()> {
        use std::io::Read;

        for entry in std::fs::read_dir(dirpath)? {
            let filepath = entry?.path();
            if filepath.extension().map(|s| s.as_bytes()) == Some(b"vir") {
                let package_name = BString::new(filepath.file_stem().unwrap().as_bytes().to_vec());
                let package = PackageFqn::new(package_name);

                let mut text = vec![];
                let mut file = std::fs::File::open(filepath)?;
                file.read_to_end(&mut text).unwrap();

                self.set_source(package.clone(), BString::from(text));
            }
        }
        Ok(())
    }

    pub fn set_source(&mut self, package: PackageFqn, text: BString) {
        self.db.set_source(package.clone(), text);
        if !self.packages.contains(&package.clone()) {
            self.packages.push(package.clone());
            self.db.set_packages((), self.packages.clone());
        }
    }

    pub fn ast(&self, package: PackageFqn) -> Option<Ast> {
        let ast = self.db.get_ast(package);
        Some(ast)
    }

    pub fn packages(&self) -> Vec<PackageFqn> {
        self.db.get_packages(())
    }

    pub fn diagnostics(&self) -> Result<(), Vec<error::Diagnostic>> {
        self.db.diagnostics()
    }

    pub fn db(&self) -> &Db {
        &self.db
    }
}
