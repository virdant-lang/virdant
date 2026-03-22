use std::collections::HashMap;
use std::sync::Arc;

use bstr::BStr;
use bstr::BString;

use crate::LIB_DIR;
use crate::analysis::Location;
use crate::db::Db;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticLevel;
use crate::source::Region;
use crate::syntax::parsing::Parsing;
use crate::syntax::parsing::parse;
use crate::source::Source;
use crate::fqn::PackageFqn;

pub struct Vir {
    sources: HashMap<String, Source>,
    parsings: HashMap<PackageFqn, Arc<Parsing>>,
    db: Db,
}

impl Vir {
    pub fn new() -> Vir {
        let mut vir = Vir {
            db: Db::new(),
            sources: HashMap::new(),
            parsings: HashMap::new(),
        };

        vir.db.set_packages(vec![]);
        vir.add_package("builtin");
        vir.set_package_text("builtin", include_bytes!("../../lib/builtin.vir"));

        let builtin_package = PackageFqn::new("builtin".into());
        let new_source = Source::new(builtin_package.clone(), include_bytes!("../../lib/builtin.vir").into());

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
        self.sources.keys().cloned().collect()
    }

    pub fn add_package<S: AsRef<str>>(&mut self, package_name: S) {
        assert!(!self.sources.contains_key(package_name.as_ref()));
        let package = PackageFqn::new(package_name.as_ref().into());
        let source = Source::new(package.clone(), "".into());
        self.sources.insert(package_name.as_ref().to_owned(), source);
        let mut packages = self.db.get_packages();
        if !packages.contains(&package) {
            packages.push(package);
            self.db.set_packages(packages);
        }
    }

    pub fn remove_package<S: AsRef<str>>(&mut self, package_name: S) {
        assert!(self.sources.contains_key(package_name.as_ref()));
        self.sources.remove(package_name.as_ref());
        self.db.set_packages(self.sources.keys().map(|package| package.clone().into()).collect());
    }

    pub fn set_package_text<S: AsRef<str>, T: Into<BString>>(&mut self, package_name: S, source: T) {
        assert!(self.sources.contains_key(package_name.as_ref()));

        let package = PackageFqn::new(package_name.as_ref().into());
        let new_source = Source::new(package.clone(), source.into());

        let mut source = self.sources.get_mut(package_name.as_ref()).unwrap();
        *source = new_source.clone();

        self.parsings.remove(&package);
        self.db.set_source(package, source.clone());
    }

    pub fn add_package_from_file<P: AsRef<std::path::Path>>(&mut self, filepath: P) {
        let package_name = filepath.as_ref().file_stem().unwrap().to_str().unwrap().to_string();
        assert!(!self.sources.contains_key(&package_name));
        let package = PackageFqn::new(package_name.clone().into());
        let source = Source::load_file(filepath);
        self.sources.insert(package_name, source.clone());
        self.db.set_source(package, source);
        self.db.set_packages(self.sources.keys().map(|package| package.clone().into()).collect());
    }

    pub fn parse<S: AsRef<str>>(&mut self, package_name: S) {
        let source = self.sources.get(package_name.as_ref()).unwrap();
        let parsing = Arc::new(parse(source));
        self.parsings.insert(source.package(), parsing);
    }

    pub fn parsing<S: AsRef<str>>(&self, package_name: S) -> Arc<Parsing> {
        let package = self.sources[package_name.as_ref()].package();
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
    pub fn check_all<P: Into<std::path::PathBuf>>(source_dir: P) {
        let mut vir = Vir::from_dir(source_dir);
        vir.dump_diagnostics();
    }

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
        let parsings: HashMap<String, String> = self.parsings
            .iter()
            .map(|(package, parsing)| {
                (package.to_string(), parsing.summary())
            }).collect();
        let sources: HashMap<String, String> = self.sources
            .iter()
            .map(|(package, source)| {
                (package.to_string(), source.summary())
            }).collect();
        f.debug_struct("Vir")
            .field("sources", &sources)
            .field("parsings", &parsings)
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
