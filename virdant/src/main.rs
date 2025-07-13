use std::io::Read;

use bstr::io::BufReadExt;
use virdant::ast::{Ast, AstNode};
use virdant::fqn::PackageFqn;
use virdant::source::Source;
use virdant::stringtable::StringTable;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let filepath = args.into_iter().skip(1).next().unwrap();
    let mut file = std::fs::File::open(filepath).unwrap();
    let mut text = vec![];
    file.read_to_end(&mut text).unwrap();

    let stringtable = StringTable::new();
    let source = Source::new(PackageFqn::new(b"top"), &text);
    let ast = Ast::new(source, stringtable);
    eprintln!();

    let node = ast.root().as_ast_node();
    dbg!(&node);
    eprintln!();

    dump(node, 0);
    eprintln!();

    dump_errors(&ast);
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
            eprintln!("        {:?} {}", component.kind(), component.name());
        }
        eprintln!("    ITEM REFS");
        for reference in moddef.as_item().item_references() {
            eprintln!("        {:?}", reference);
        }
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
