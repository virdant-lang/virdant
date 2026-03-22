use bstr::ByteSlice;
use hashbrown::HashMap;

use crate::common::{BinOp as CommonBinOp, UnOp as CommonUnOp, Width};
use crate::common::json::ToJson;
use crate::diagnostics::{self, Diagnostic};
use crate::analysis::location::Location;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;

use super::context::TypingContext;
use super::typ::Type;

macro_rules! error {
    ($self_:expr, $node:expr, $fmt:literal) => {{
        error!($self_, $node, $fmt,)
    }};
    ($self_:expr, $node:expr, $fmt:literal, $($arg:expr),*) => {{
        $self_.diagnostics.push(diagnostics::Unknown {
            region: $node.region(),
            message: format!($fmt, $($arg)*).into(),
        }.into());
    }};
}

macro_rules! info {
    ($self_:expr, $node:expr, $fmt:literal) => {{
        error!($self_, $node, $fmt,)
    }};
    ($self_:expr, $node:expr, $fmt:literal, $($arg:expr),*) => {{
        $self_.diagnostics.push(diagnostics::Todo {
            region: $node.region(),
            message: format!($fmt, $($arg)*).into(),
        }.into());
    }};
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}

impl ExprRoot {
    pub fn new(location: Location) -> ExprRoot {
        ExprRoot { location }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }
}

impl ToJson for ExprRoot {
    fn to_json(&self) -> json::JsonValue {
        self.location.to_json()
    }
}

#[derive(Debug)]
pub struct Typing {
    pub(crate) exprroot: ExprRoot,
    pub(crate) expected_typ: Type,
    pub(crate) context: TypingContext,
    pub(crate) typs: HashMap<AstNodeId, Type>,
    pub(crate) diagnostics: Vec<Diagnostic>,
}

impl Typing {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub(crate) fn type_of_node(&self, id: AstNodeId) -> Option<&Type> {
        self.typs.get(&id)
    }

    fn flag_wrong_type<'p>(&mut self, node: &AstNode<'p>, expected: &Type, actual: &Type) {
        let diag: Diagnostic = diagnostics::WrongType {
            region: node.region(),
            expected: expected.to_string().into(),
            actual: actual.to_string().into(),
        }.into();
        self.diagnostics.push(diag.to_warning());
    }

    fn flag_not_word_type<'p>(&mut self, node: &AstNode<'p>, typ: &Type) {
        self.diagnostics.push(diagnostics::NotWordType {
            region: node.region(),
            typ: typ.to_string().into(),
        }.into());
    }

    fn flag_cant_infer<'p>(&mut self, node: &AstNode<'p>) {
        self.diagnostics.push(diagnostics::CantInfer {
            region: node.region(),
        }.into());
    }

    fn flag_unresolved_component<'p>(&mut self, node: &AstNode<'p>, path: &bstr::BStr) {
        self.diagnostics.push(diagnostics::UnresolvedComponent {
            region: node.region(),
            path: path.into(),
        }.into());
    }

    pub(crate) fn check<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        match self.infer(node) {
            Err(diagnostics) => {
                self.diagnostics.extend(diagnostics);
                return;
            }
            Ok(Some(actual_typ)) => {
                if actual_typ == *expected_typ {
                    self.typs.insert(node.id(), actual_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &actual_typ);
                }
                return;
            }
            _ => (),
        }

        match node.payload() {
            AstNodePayload::ExprParen => self.check_paren(node, expected_typ),
            AstNodePayload::ExprIf => self.check_if(node, expected_typ),
            AstNodePayload::ExprMatch => (), // TODO
            AstNodePayload::ExprWordLit(_) => self.check_word_lit(node, expected_typ),
            AstNodePayload::ExprBinOp(expr_bin_op) => self.check_binop(node, expr_bin_op.op, expected_typ),
            AstNodePayload::ExprUnOp(expr_un_op) => self.check_unop(node, expr_un_op.op, expected_typ),
            AstNodePayload::ExprMethod(_expr_method) => (), // TODO
            AstNodePayload::ExprFn => (), // TODO
            AstNodePayload::ExprCtor(_expr_ctor) => (), // TODO
            AstNodePayload::ExprEnumerant(_expr_enumerant) => (), // TODO
            AstNodePayload::ExprStruct => (), // TODO
            AstNodePayload::ExprIndex(expr_index) => self.check_index(node, expr_index.index, expected_typ),
            AstNodePayload::ExprIndexRange(_expr_index_range) => (), // TODO
            AstNodePayload::ExprZext | AstNodePayload::ExprSext => self.check_ext(node, expected_typ),
            _ => unreachable!("Can't typecheck {:?}", node.summary()),
        }
    }



    fn check_paren<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let child = node.child(0);
        self.check(&child, expected_typ);
        if self.typs.contains_key(&child.id()) {
            self.typs.insert(node.id(), expected_typ.clone());
        }
    }

    fn check_if<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let children = node.children();
        self.check(&children[0], &Type::Bit);
        self.check(&children[1], expected_typ);
        let mut i = 2;
        while i + 1 < children.len() - 1 {
            self.check(&children[i], &Type::Bit);
            self.check(&children[i + 1], expected_typ);
            i += 2;
        }

        // TODO this all needs helpers in AstNode
        self.check(&children[children.len() - 1], expected_typ);

        if children.iter().all(|child| self.typs.contains_key(&child.id())) {
            self.typs.insert(node.id(), expected_typ.clone());
        }
    }

    fn check_word_lit<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let Type::Word(width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return;
        };

        let (value, explicit_width) = parse_word_literal(node.spelling().to_str_lossy().as_ref());
        if let Some(explicit_width) = explicit_width {
            let actual = Type::Word(explicit_width);
            if actual == *expected_typ {
                self.typs.insert(node.id(), actual);
            } else {
                self.flag_wrong_type(node, expected_typ, &actual);
            }
            return;
        }

        let minwidth = min_word_width(value);
        if minwidth > u64::from(*width) {
            self.diagnostics.push(diagnostics::DoesntFit {
                region: node.region(),
                value,
                width: u64::from(*width),
                minwidth,
            }.into());
            return;
        }

        self.typs.insert(node.id(), Type::Word(*width));
    }

    fn check_binop<'p>(&mut self, node: &AstNode<'p>, op: CommonBinOp, expected_typ: &Type) {
        let lhs = node.child(0);
        let rhs = node.child(1);
        let lhs_typ = self.infer(&lhs).unwrap();
        let rhs_typ = self.infer(&rhs).unwrap();

        // TODO this is sus
        let (lhs_typ, rhs_typ) = match (&lhs_typ, &rhs_typ) {
            (&None, &None) => {
                self.flag_cant_infer(node);
                return;
            }
            (&None, Some(rhs_typ)) => {
                self.typs.insert(rhs.id(), rhs_typ.clone());
                self.check(&lhs, rhs_typ);
                (rhs_typ.clone(), rhs_typ.clone())
            }
            (Some(lhs_typ), &None) => {
                self.typs.insert(lhs.id(), lhs_typ.clone());
                self.check(&rhs, lhs_typ);
                (lhs_typ.clone(), lhs_typ.clone())
            }
            (Some(lhs_typ), Some(rhs_typ)) => {
                self.typs.insert(lhs.id(), lhs_typ.clone());
                self.typs.insert(rhs.id(), rhs_typ.clone());
                (lhs_typ.clone(), rhs_typ.clone())
            }
        };

        match op {
            CommonBinOp::Add | CommonBinOp::Sub => {
                if lhs_typ != rhs_typ {
                    self.flag_wrong_type(&rhs, &lhs_typ, &rhs_typ);
                    return;
                }
                if lhs_typ == *expected_typ {
                    self.typs.insert(node.id(), lhs_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &lhs_typ);
                }
            }
            CommonBinOp::Lt
            | CommonBinOp::Lte
            | CommonBinOp::Gt
            | CommonBinOp::Gte
            | CommonBinOp::Eq
            | CommonBinOp::Neq => {
                if lhs_typ != rhs_typ {
                    self.flag_wrong_type(&rhs, &lhs_typ, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
            CommonBinOp::And | CommonBinOp::Or | CommonBinOp::Xor => {
                if lhs_typ != Type::Bit {
                    self.flag_wrong_type(&lhs, &Type::Bit, &lhs_typ);
                    return;
                }
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
        }
    }


    fn check_unop<'p>(&mut self, node: &AstNode<'p>, op: CommonUnOp, expected_typ: &Type) {
        let rhs = node.child(0);
        let Ok(Some(rhs_typ)) = self.infer(&rhs) else {
            self.flag_cant_infer(node);
            return;
        };
        self.typs.insert(rhs.id(), rhs_typ.clone());

        match op {
            CommonUnOp::Neg | CommonUnOp::Inv => {
                if rhs_typ == *expected_typ {
                    self.typs.insert(node.id(), rhs_typ);
                } else {
                    self.flag_wrong_type(node, expected_typ, &rhs_typ);
                }
            }
            CommonUnOp::Not => {
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
        }
    }

    fn check_index<'p>(&mut self, node: &AstNode<'p>, index: Width, expected_typ: &Type) {
        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return;
        };

        self.typs.insert(subject.id(), subject_typ.clone());

        match subject_typ {
            Type::Word(width) => {
                if index >= width {
                    self.diagnostics.push(diagnostics::Unknown {
                        region: node.region(),
                        message: format!("Index {index} out of bounds for Word[{width}]").into(),
                    }.into());
                    return;
                }
                if Type::Bit == *expected_typ {
                    self.typs.insert(node.id(), Type::Bit);
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                }
            }
            other => self.flag_not_word_type(&subject, &other),
        }
    }

    fn check_ext<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) {
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return;
        };

        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return;
        };

        self.typs.insert(subject.id(), subject_typ.clone());

        let subject_width = match subject_typ {
            Type::Bit | Type::Clock => 1,
            Type::Word(width) => width,
            other => {
                self.flag_not_word_type(&subject, &other);
                return;
            }
        };

        if subject_width > *target_width {
            self.flag_wrong_type(node, &Type::Word(subject_width), expected_typ);
            return;
        }

        self.typs.insert(node.id(), expected_typ.clone());
    }
}

fn parse_word_literal(literal: &str) -> (u64, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

fn parse_nat_literal(literal: &str) -> u64 {
    let literal = literal.replace('_', "");
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

pub(super) fn min_word_width(value: u64) -> u64 {
    if value == 0 {
        0
    } else {
        u64::BITS as u64 - u64::leading_zeros(value) as u64
    }
}

impl ToJson for Typing {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
