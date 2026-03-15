use std::sync::Arc;

use hashbrown::HashMap;

use crate::analysis::db::Builder;
use crate::common::json::ToJson;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::analysis::Location;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub type Width = u64;

#[derive(Debug)]
pub struct TypeCheck {
    expr_root: Location,
    expected_typ: Type,
    context: TypingContext, // TODO
    types: HashMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    let parsing_noborrow = parsing.clone();

    let node = parsing.ast_node(location.ast_node_id());
    // TODO get rid of this
    let parent_node = node.parent().unwrap();

    match parent_node.payload() {
        _ => {
            // TODO
        }
    }

    let expected_typ = Type::Word(8);

    let context = TypingContext {}; // TODO
    let diagnostics = vec![];
    let mut typing = TypeCheck {
        expr_root: location.clone(),
        context,
        types: HashMap::new(),
        diagnostics,
        expected_typ,
    };

    let expected_typ = Type::Word(8);

    typing.check(parsing_noborrow, &node, &expected_typ);

    Arc::new(typing)
}

impl TypeCheck {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    // TODO Check should check *against* something*
    fn check<'p>(&mut self, parsing: Arc<Parsing>, node: &AstNode<'p>, expected_typ: &Type) {
        if let Some(actual_typ) = self.infer(parsing, node) {
            if actual_typ == *expected_typ {
                self.types.insert(node.id(), Type::Bit);
            } else {
                let diag: Diagnostic = diagnostics::WrongType {
                    region:  node.region(),
                    expected: expected_typ.to_string().into(),
                    actual: actual_typ.to_string().into(),
                }.into();

                self.diagnostics.push(diag.to_warning());
            }
            return;
        }

        /*
        // TODO
        match node.payload() {
            AstNodePayload::ExprParen => todo!(),
            AstNodePayload::ExprIf => todo!(),
            AstNodePayload::ExprMatch => todo!(),
            AstNodePayload::ExprWordLit(expr_word_lit) => todo!(),
            AstNodePayload::ExprBinOp(expr_bin_op) => todo!(),
            AstNodePayload::ExprUnOp(expr_un_op) => todo!(),
            AstNodePayload::ExprMethod(expr_method) => todo!(),
            AstNodePayload::ExprFn => todo!(),
            AstNodePayload::ExprCtor(expr_ctor) => todo!(),
            AstNodePayload::ExprEnumerant(expr_enumerant) => todo!(),
            AstNodePayload::ExprStruct => todo!(),
            AstNodePayload::ExprIndex(expr_index) => todo!(),
            AstNodePayload::ExprIndexRange(expr_index_range) => todo!(),
            AstNodePayload::ExprWord => todo!(),
            AstNodePayload::ExprZext => todo!(),
            AstNodePayload::ExprSext => todo!(),
            _ => unreachable!(),
        }
        */
    }

    fn infer<'p>(&mut self, parsing: Arc<Parsing>, node: &AstNode<'p>) -> Option<Type> {
        match node.payload() {
            AstNodePayload::ExprReference => {
                // TODO HACK
                let path = parsing.string(node.path().unwrap());
                let typ = if path == b"clock" {
                    // TODO HACK all "clock" signals are Clock
                    Type::Clock
                } else {
                    // TODO HACK and the rest are Word[8]
                    Type::Word(8)
                };
                Some(typ)
            }
            AstNodePayload::ExprBitLit(expr_bit_lit) => {
                Some(Type::Bit)
            }
            AstNodePayload::ExprWordLit(expr_word_lit) => {
                // TODO HACK This ignores width and just gives Word[8]
                Some(Type::Word(8))
            }
            _ => None,
        }
    }
}

impl ToJson for TypeCheck {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Word(n) => write!(f, "Word[{n}]"),
        }
    }
}
