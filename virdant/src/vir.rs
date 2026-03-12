use std::collections::HashMap;
use std::sync::Arc;

use bstr::BString;

use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::source::Region;
use crate::syntax::parsing::Parsing;
use crate::syntax::parsing::parse;
use crate::source::Source;
use crate::fqn::PackageFqn;

pub struct Vir {
    sources: HashMap<String, Source>,
    parsings: HashMap<PackageFqn, Arc<Parsing>>,
}

impl Vir {
    pub fn new() -> Vir {
        let mut vir = Vir {
            sources: HashMap::new(),
            parsings: HashMap::new(),
        };

        vir.add_package("builtin");
        vir.set_package_text("builtin", include_bytes!("../../lib/builtin.vir"));

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
    }

    pub fn remove_package<S: AsRef<str>>(&mut self, package_name: S) {
        assert!(self.sources.contains_key(package_name.as_ref()));
        self.sources.remove(package_name.as_ref());
    }

    pub fn set_package_text<S: AsRef<str>, T: Into<BString>>(&mut self, package_name: S, source: T) {
        assert!(self.sources.contains_key(package_name.as_ref()));

        let package = PackageFqn::new(package_name.as_ref().into());
        let new_source = Source::new(package.clone(), source.into());

        let mut source = self.sources.get_mut(package_name.as_ref()).unwrap();
        *source = new_source;

        self.parsings.remove(&package);
    }

    pub fn add_package_from_file<P: AsRef<std::path::Path>>(&mut self, filepath: P) {
        let package_name = filepath.as_ref().file_stem().unwrap().to_str().unwrap().to_string();
        assert!(!self.sources.contains_key(&package_name));
        let package = PackageFqn::new(package_name.clone().into());
        let source = Source::load_file(filepath);
        self.sources.insert(package_name, source);
    }

    pub fn parse<S: AsRef<str>>(&mut self, package_name: S) {
        let source = self.sources.get(package_name.as_ref()).unwrap();
        let parsing = Arc::new(parse(source));
        self.parsings.insert(source.package(), parsing);
    }

    pub fn parse_all(&mut self) {
        for package in self.packages() {
            self.parse(&package);
        }
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        for parsing in self.parsings.values() {
            for error in parsing.errors() {
                let region = Region::new(parsing.package(), error.span());
                let diagnostic = diagnostics::ParseError {
                    region,
                };
                diagnostics.push(diagnostic.into());
            }
        }
        diagnostics
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

    vir.add_package("top");
    vir.add_package_from_file(EXAMPLES_DIR.join("broken.vir"));

    vir.parse_all();

    dbg!(vir.diagnostics());

    dbg!(&vir);
}
