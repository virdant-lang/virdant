use indexmap::IndexMap;

use crate::error::{VirErrs, VirErr};
use crate::PackageSource;

pub struct Loader {
    source_dir: std::path::PathBuf,
    texts: IndexMap<String, String>,
    paths: IndexMap<String, std::path::PathBuf>,
    errors: VirErrs,
}

impl Loader {
    pub fn new(source_dir: std::path::PathBuf) -> Loader {
        Loader {
            source_dir,
            texts: IndexMap::new(),
            paths: IndexMap::new(),
            errors: VirErrs::new(),
        }
    }

    pub fn load(&mut self, package: &str) {
        if self.texts.contains_key(package) {
            return;
        }

        let package_path = self.package_to_path(package);
        match std::fs::read_to_string(&package_path) {
            Ok(package_text) => {
                self.texts.insert(package.to_owned(), package_text.clone());
                self.paths.insert(package.to_owned(), package_path);

                let imports = crate::parse::imports::parse_imports(&package_text);
                for import in imports {
                    self.load(&import);
                }
            },
            Err(_err) => self.errors.add(VirErr::CantImport(package.to_owned())),
        }
    }

    fn package_to_path(&self, package: &str) -> std::path::PathBuf {
        self.source_dir.join(format!("{package}.vir"))
    }

    pub fn errors(&self) -> Result<(), VirErrs> {
        self.errors.check()?;
        Ok(())
    }

    pub fn sources(&self) -> IndexMap<String, PackageSource> {
        let mut sources = IndexMap::new();
        for (package, path) in self.paths.iter() {
            sources.insert(package.to_owned(), PackageSource::File(path.to_path_buf()));
        }

        sources
    }
}
