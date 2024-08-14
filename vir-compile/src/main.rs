use clap::Parser;
use virdant::Virdant;

#[derive(Parser)]
#[command(name = "myapp")]
#[command(about = "A simple example CLI", long_about = None)]
struct Args {
    filename: std::path::PathBuf,
}

mod parse {
    use pest::*;
    use pest_derive::*;

    #[derive(Parser)]
    #[grammar = "import_grammar.pest"]
    struct ImportParser;

    pub fn parse_imports(text: &str) -> Vec<String> {
        let text = text.to_owned().leak(); // TODO
        let results = ImportParser::parse(Rule::package, text).unwrap();

        let imports = results.clone().into_iter()
            .collect::<Vec<_>>()
            .first() .unwrap()
            .clone().into_inner();

        let mut packages = vec![];
        for import_ast in imports {
            let package = import_ast.clone().into_inner().find_first_tagged("package").unwrap().as_str();
            packages.push(package.to_string());
        }
        packages
    }
}



fn main() {
    let args = Args::parse();

    let filepath = &args.filename;
    let parent = filepath.parent().unwrap();

    let first_package = filepath.file_stem().unwrap().to_string_lossy().to_string();

    let mut filepaths = vec![(first_package, filepath.to_owned())];
    let text = std::fs::read_to_string(&args.filename).unwrap();
    for package in parse::parse_imports(&text) {
        filepaths.push((package.clone(), parent.join(format!("{package}.vir"))));
    }

    for (package, filepath) in &filepaths {
        eprintln!("{package}: {filepath:?}");
    }

    let mut virdant = Virdant::new(&filepaths);
    let design = virdant.check().unwrap();
    design.verilog("build").unwrap();
}
