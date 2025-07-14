use std::io::Read;
use std::os::unix::ffi::OsStrExt;

use bstr::io::BufReadExt;
use bstr::BString;
use virdant::ast::{Ast, AstNode};
use virdant::fqn::PackageFqn;
use virdant::Vir;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    let filepaths = args
        .iter()
        .skip(1)
        .map(|arg| std::path::PathBuf::from(arg));

    let mut vir = Vir::new();

    let mut first_package = None;

    for filepath in filepaths {
        let mut file = std::fs::File::open(&filepath).unwrap();
        let mut text = vec![];
        file.read_to_end(&mut text).unwrap();

        let package_name = BString::new(filepath.file_stem().unwrap().as_bytes().to_vec());
        let package = PackageFqn::new(package_name);

        vir.set_source(package.clone(), BString::from(text)).unwrap();

        if first_package.is_none() {
            first_package = Some(package);
        }
    }

    let top_package = first_package.unwrap();
    dbg!(&top_package);

    let ast = vir.ast(top_package).unwrap();
    let node = ast.root().as_ast_node();
    dbg!(&node);
    eprintln!();

    dump(node, 0);
    eprintln!();

    let package = ast.root();
    eprintln!("IMPORTS:");
    for import in package.imports() {
        eprintln!("    {import:?}");
    }

    eprintln!("ITEMS:");
    for item in package.items() {
        eprintln!("    {item:?}");
    }

    eprintln!("MODDEFS:");
    for moddef in package.moddefs() {
        eprintln!("    {moddef:?}");
        eprintln!("    COMPONENTS");
        for component in moddef.components() {
            eprintln!("            {:?} {}", component.kind(), component.name());
        }
        eprintln!("    ITEM REFS");
        for reference in moddef.as_item().item_references() {
            eprintln!("            {:?}", reference);
        }
    }

    if let Err(errors) = vir.diagnostics() {
        eprintln!("DIAGNOSTICS");
        for error in errors {
            eprintln!("    {}: {}", error.region(), error.message());
        }
        eprintln!();
    }

}

fn dump(node: AstNode, level: usize) {
    let padding = " ".repeat(4 * level);
    let spelling = node.spelling();
    if spelling.byte_lines().collect::<Vec<_>>().len() == 1 {
        eprintln!(
            "{padding}{:?} {:?} @ {} {spelling:?}",
            node.payload(),
            node.id(),
            node.region()
        );
    } else {
        eprintln!(
            "{padding}{:?} {:?} @ {}",
            node.payload(),
            node.id(),
            node.region()
        );
    }
    for child in node.children() {
        dump(child, level + 1);
    }
}

fn dump_errors(ast: &Ast) {
    for ast_node in ast.errors() {
        eprintln!("error: {} {:?}", ast_node.region(), ast_node.spelling());
    }
}
