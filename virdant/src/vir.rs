use std::sync::Arc;

use bstr::BString;

use crate::LIB_DIR;
use crate::analysis::Location;
use crate::db::Db;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticLevel;
use crate::source::Region;
use crate::syntax::parsing::Parsing;
use crate::source::Source;
use crate::fqn::PackageFqn;

pub struct Vir {
    db: Db,
}

impl Vir {
    pub fn new() -> Vir {
        let mut vir = Vir {
            db: Db::new(),
        };

        vir.db.set_packages(vec![]);
        vir.add_package("builtin");
        vir.set_package_text("builtin", include_bytes!("../../lib/builtin.vir"));

        vir
    }

    pub fn from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Vir {
        let mut vir = Vir::new();
        let db = &mut vir.db;
        let builtin_source = Source::load_file(LIB_DIR.join("builtin.vir"));
        let mut sources = vec![builtin_source.clone()];
        db.set_source(builtin_source.package(), builtin_source.clone());

        for filepath in std::fs::read_dir(source_dir.into()).unwrap() {
            let filepath = match filepath {
                Ok(filepath) => filepath.path(),
                Err(_) => continue,
            };

            match filepath.extension() {
                Some(ext) if ext.to_string_lossy() == "vir" => (),
                _ => continue,
            }
            let source: Source = Source::load_file(filepath);
            db.set_source(source.package(), source.clone());
            sources.push(source);
        }
        db.set_packages(sources.iter().map(|source| source.package()).collect());
        vir
    }

    pub fn packages(&self) -> Vec<String> {
        self.db.get_packages().into_iter().map(|package| package.to_string()).collect()
    }

    // TODO remove this wart
    pub fn is_empty(&self) -> bool {
        self.packages().len() < 2 // just builtin
    }

    pub fn add_package<S: AsRef<str>>(&mut self, package_name: S) {
        let package = PackageFqn::new(package_name.as_ref().into());
        let mut packages = self.db.get_packages();
        if !packages.contains(&package) {
            packages.push(package);
            self.db.set_packages(packages);
        }
    }

    pub fn set_package_text<S: AsRef<str>, T: Into<BString>>(&mut self, package_name: S, source: T) {
        let package = PackageFqn::new(package_name.as_ref().into());
        let new_source = Source::new(package.clone(), source.into());

        self.db.set_source(package, new_source);
    }

    pub fn add_package_from_file<P: AsRef<std::path::Path>>(&mut self, filepath: P) {
        let package_name = filepath.as_ref().file_stem().unwrap().to_str().unwrap().to_string();
        let package = PackageFqn::new(package_name.clone().into());
        let source = Source::load_file(filepath);
        let mut packages = self.db.get_packages();
        packages.push(package.clone());
        self.db.set_source(package, source);
        self.db.set_packages(packages);
    }

    pub fn parsing<S: AsRef<str>>(&self, package_name: S) -> Arc<Parsing> {
        let package = PackageFqn::new(package_name.as_ref().to_string().into());
        self.db.get_parsing(package)
    }

    pub fn check(&self) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
        let diagnostics = self.db.check();

        if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
            Err(diagnostics)
        } else {
            Ok(diagnostics)
        }
    }

    pub fn spelling(&self, location: Location) -> BString {
        let package = location.package();
        let parsing = self.db.get_parsing(package.clone());

        let node = parsing.ast_node(location.ast_node_id());
        node.spelling().to_owned()
    }

    pub fn region(&self, location: Location) -> Region {
        let package = location.package();
        let parsing = self.db.get_parsing(package.clone());
        let node = parsing.ast_node(location.ast_node_id());
        Region::new(package, node.span())
    }
}

impl Vir {
    pub fn dump_diagnostics(&self) {
        let diagnostics = match self.check() {
            Ok(diags) => diags,
            Err(diags) => diags,
        };

        if !diagnostics.is_empty() {
            println!("Diagnostics:");
            for diagnostic in diagnostics {
                println!("  {:?}", diagnostic);
            }
        }
    }

    pub fn db(&self) -> &Db {
        &self.db
    }
}

impl std::fmt::Debug for Vir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Vir")
            .finish()
    }
}

#[cfg(test)]
#[test]
fn test_vir() {
    use crate::tests::EXAMPLES_DIR;
    use crate::source::Source;

    let mut vir = Vir::new();

    vir.add_package_from_file(EXAMPLES_DIR.join("basic.vir"));

    vir.check().unwrap();
    dbg!(&vir);
}
