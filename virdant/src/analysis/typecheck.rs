use std::sync::Arc;

use crate::analysis::db::Builder;
use crate::common::json::ToJson;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNodeId;
use crate::analysis::Location;
use crate::syntax::parsing::Parsing;

pub type Width = u64;

#[derive(Debug)]
pub struct TypeCheck {
    expr_root: Location,
    context: TypingContext, // TODO
    types: Vec<Type>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
    Clock,
    Word(Width),
}

#[derive(Debug, Clone)]
pub struct TypingContext {
}

pub fn build_exprroots(builder: &mut Builder) -> Vec<Location> {
    let mut exprroots = vec![];
    for package in builder.get_packages() {
        let analysis = builder.get_package_analysis(package);
        exprroots.extend(
            analysis
                .expr_roots()
                .into_iter()
                .map(|ast_node_id| Location::new(analysis.package(), ast_node_id))
        );
    }
    exprroots
}

pub fn typecheck(builder: &mut Builder, location: Location) -> Arc<TypeCheck> {
    let parsing = builder.get_parsing(location.package());

    let context = TypingContext {}; // TODO
    let diagnostics = vec![];
    let mut typing = TypeCheck {
        expr_root: location.clone(),
        context,
        types: vec![],
        diagnostics,
    };

    typing.check(parsing, location);

    Arc::new(typing)
}

impl TypeCheck {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    fn check(&mut self, parsing: Arc<Parsing>, location: Location) {
        // TODO

        self.diagnostics.push(diagnostics::Todo {
            region:  parsing.ast_node(location.ast_node_id()).region(),
            message: "Typechecking not ready".into(),
        }.into());
    }
}

impl ToJson for TypeCheck {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
