use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use hashbrown::HashMap;

use crate::analysis::db::Builder;
use crate::analysis::Location;
use crate::common::ComponentKind;
use crate::common::json::ToJson;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub type Width = u64;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    location: Location,
    expected_typ: Type,
}

#[derive(Debug)]
pub struct Typecheck {
    expr_root: ExprRoot,
    expected_typ: Type,
    context: TypingContext, // TODO
    types: HashMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Word(Width),
}

#[derive(Debug, Clone)]
pub struct TypingContext {
}

impl ExprRoot {
    pub fn new(location: Location, expected_typ: Type) -> ExprRoot {
        ExprRoot { location, expected_typ }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn expected_typ(&self) -> Type {
        self.expected_typ.clone()
    }
}

pub fn build_exprroots(builder: &mut Builder) -> Vec<ExprRoot> {
    let mut exprroots = vec![];
    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package.clone());
        let analysis = builder.get_package_analysis(package);

        for ast_node_id in analysis.expr_roots_node_ids() {
            let location = Location::new(analysis.package(), ast_node_id);
            let node = parsing.ast_node(ast_node_id);
            let parent_node = node.parent().unwrap();

            let expected_typ = match parent_node.payload() {
                AstNodePayload::Component(component) if component.kind == ComponentKind::Reg => Type::Clock,
                AstNodePayload::Driver(_) => {
                    let lhs_path = parsing.string(parent_node.child(0).path().unwrap());

                    let moddef_node = parent_node.parent().unwrap();
                    let moddef_name = parsing.string(moddef_node.name().unwrap());
                    let moddef_fqn = BString::from(
                        format!("{}::{moddef_name}", location.package()).into_bytes()
                    );
                    let component_analysis = builder.get_component_analysis(moddef_fqn);

                    if let Some(dot_index) = lhs_path.iter().position(|ch| *ch == b'.') {
                        let instance_name = BStr::new(&lhs_path[..dot_index]);
                        let port_name = BStr::new(&lhs_path[(dot_index + 1)..]);

                        let instance_node = moddef_node
                            .children()
                            .into_iter()
                            .find(|child| {
                                matches!(child.payload(), AstNodePayload::Module(module)
                                    if parsing.string(module.name) == instance_name)
                            })
                            .unwrap_or_else(|| panic!("Couldn't find instance {lhs_path}"));

                        let module_fqn = match instance_node.child(0).payload() {
                            AstNodePayload::Ofness(ofness) => {
                                let module_package = ofness
                                    .package
                                    .map(|package| BString::from(parsing.string(package).to_vec()))
                                    .unwrap_or_else(|| location.package().to_string().into());
                                let module_name = parsing.string(ofness.name);
                                BString::from(format!("{}::{module_name}", module_package.to_str_lossy()).into_bytes())
                            }
                            _ => todo!(),
                        };

                        let module_component_analysis = builder.get_component_analysis(module_fqn);
                        if port_name == b"clock" {
                            Type::Clock
                        } else {
                            module_component_analysis.type_of(port_name)
                        }
                    } else {
                        if lhs_path == b"clock" {
                            Type::Clock
                        } else {
                            component_analysis.type_of(lhs_path)
                        }
                    }
                }
                _ => todo!(),
            };

            exprroots.push(ExprRoot::new(location, expected_typ));
        }
    }
    exprroots
}

pub fn typecheck(builder: &mut Builder, expr_root: ExprRoot) -> Arc<Typecheck> {
    let location = expr_root.location();
    let parsing = builder.get_parsing(location.package());
    let parsing_noborrow = parsing.clone();

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = expr_root.expected_typ();

    let context = TypingContext {}; // TODO
    let diagnostics = vec![];
    let mut typing = Typecheck {
        expr_root: expr_root.clone(),
        context,
        types: HashMap::new(),
        diagnostics,
        expected_typ: expected_typ.clone(),
    };

    typing.check(parsing_noborrow, &node, &expected_typ);

    Arc::new(typing)
}

impl Typecheck {
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

impl ToJson for Typecheck {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}

impl ToJson for ExprRoot {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.location.to_json(), self.expected_typ.to_json())
    }
}

impl ToJson for Type {
    fn to_json(&self) -> json::JsonValue {
        self.to_string().into()
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
