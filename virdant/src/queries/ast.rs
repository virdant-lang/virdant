use crate::ast::Ast;
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::source::Source;

pub type Params = PackageFqn;
pub type Response = Ast;

pub fn build_ast(builder: &mut Builder, package: PackageFqn) -> Ast {
    let source = builder.get_source(package.clone());
    let stringtable = builder.stringtable();
    let text: &[u8] = source.as_ref();
    let source = Source::new(package.clone(), &text);
    Ast::new(source, stringtable)
}
