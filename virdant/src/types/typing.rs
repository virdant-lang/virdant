use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use hashbrown::{HashMap, HashSet};

use crate::analysis::symbols::{Symbol, SymbolId, SymbolTable};
use crate::common::{BinOp as CommonBinOp, Flow, UnOp as CommonUnOp, Width};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::analysis::location::Location;
use crate::fqn::PackageFqn;
use crate::analysis::component::node_to_typ;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::{self, AstNodePayload};

use super::context::TypingContext;
use super::typ::Type;

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

    pub fn package(&self) -> PackageFqn {
        self.location.package()
    }
}

#[derive(Debug)]
pub struct Typing {
    pub exprroot: ExprRoot,
    #[allow(dead_code)]
    pub expected_typ: Type,
    pub context: TypingContext,
    pub typs: HashMap<AstNodeId, Type>,
    pub diagnostics: Vec<Diagnostic>,
    // TODO Remove the symbol table and instead resolve type expressions beforehand.
    pub symboltable: Arc<SymbolTable>,
    pub use_locations: HashMap<BString, Vec<Location>>,
}

impl Typing {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn reference_use_locations(&self) -> &HashMap<BString, Vec<Location>> {
        &self.use_locations
    }

    pub fn use_component(&mut self, path: &BStr, location: Location) {
        if !self.use_locations.contains_key(path) {
            self.use_locations.insert(path.to_owned(), vec![]);
        }
        self.use_locations.get_mut(path).unwrap().push(location);
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
        self.diagnostics.push(diag);
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

    fn annotate(&mut self, node: &AstNode<'_>, typ: &Type) {
        self.typs.insert(node.id(), typ.clone());
    }

    #[rustfmt::skip]
    pub(crate) fn check<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        if let Some(actual_typ) = self.infer(node)? {
            if actual_typ != *expected_typ {
                self.flag_wrong_type(node, expected_typ, &actual_typ);
                return Err(());
            } else {
                return Ok(());
            }
        }

        match node.payload() {
            AstNodePayload::ExprParen                => self.check_paren(node, expected_typ),
            AstNodePayload::ExprIf                   => self.check_if(node, expected_typ),
            AstNodePayload::ExprWordLit(_)           => self.check_word_lit(node, expected_typ),
            AstNodePayload::ExprBinOp(expr_bin_op)   => self.check_binop(node, expr_bin_op.op, expected_typ),
            AstNodePayload::ExprUnOp(expr_un_op)     => self.check_unop(node, expr_un_op.op, expected_typ),
            AstNodePayload::ExprIndex(expr_index)    => self.check_index(node, expr_index.index, expected_typ),
            AstNodePayload::ExprZext                 => self.check_ext(node, expected_typ),
            AstNodePayload::ExprSext                 => self.check_ext(node, expected_typ),
            AstNodePayload::ExprHole                 => self.check_hole(node, expected_typ),
            AstNodePayload::ExprMatch                => Ok(()), // TODO
            AstNodePayload::ExprMethod(_expr_method) => Ok(()), // TODO
            AstNodePayload::ExprFn                   => Ok(()), // TODO
            AstNodePayload::ExprCtor(_expr_ctor)     => Ok(()), // TODO
            AstNodePayload::ExprStruct               => Ok(()), // TODO
            AstNodePayload::ExprEnumerant(_expr_enumerant)    => Ok(()), // TODO
            AstNodePayload::ExprIndexRange(_expr_index_range) => Ok(()), // TODO
            _ => unreachable!("Can't typecheck {:?}", node.summary()),
        }
    }

    fn check_paren<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let child = node.child(0);
        self.check(&child, expected_typ)?;
        if self.typs.contains_key(&child.id()) {
            self.annotate(node, expected_typ);
            Ok(())
        } else {
            Err(())
        }
    }

    fn check_if<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let mut err = false;
        let children = node.children();
        err |= self.check(&children[0], &Type::Bit).is_err();
        err |= self.check(&children[1], expected_typ).is_err();
        let mut i = 2;
        while i + 1 < children.len() - 1 {
            err |= self.check(&children[i], &Type::Bit).is_err();
            err |= self.check(&children[i + 1], expected_typ).is_err();
            i += 2;
        }

        // TODO this all needs helpers in AstNode
        err |= self.check(&children[children.len() - 1], expected_typ).is_err();

        if children.iter().all(|child| self.typs.contains_key(&child.id())) {
            self.annotate(node, expected_typ);
        }

        if err {
            Err(())
        } else {
            Ok(())
        }
    }

    fn check_word_lit<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let Type::Word(width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return Err(());
        };

        let (value, explicit_width) = parse_word_literal(node.spelling().to_str_lossy().as_ref());
        if let Some(explicit_width) = explicit_width {
            let actual = Type::Word(explicit_width);
            if actual == *expected_typ {
                self.annotate(node, &actual);
                return Ok(());
            } else {
                self.flag_wrong_type(node, expected_typ, &actual);
                return Err(());
            }
        }

        let minwidth = min_word_width(value);
        if minwidth > u64::from(*width) {
            self.diagnostics.push(diagnostics::DoesntFit {
                region: node.region(),
                value,
                width: u64::from(*width),
                minwidth,
            }.into());
            return Err(());
        }

        self.annotate(node, &Type::Word(*width));
        Ok(())
    }

    fn check_binop<'p>(&mut self, node: &AstNode<'p>, op: CommonBinOp, expected_typ: &Type) -> Result<(), ()> {
        let lhs = node.child(0);
        let rhs = node.child(1);
        let lhs_typ = self.infer(&lhs).unwrap();
        let rhs_typ = self.infer(&rhs).unwrap();

        // TODO this is sus
        let (lhs_typ, rhs_typ) = match (&lhs_typ, &rhs_typ) {
            (&None, &None) => {
                self.flag_cant_infer(node);
                return Err(());
            }
            (&None, Some(rhs_typ)) => {
                self.typs.insert(rhs.id(), rhs_typ.clone());
                let _ = self.check(&lhs, rhs_typ);
                (rhs_typ.clone(), rhs_typ.clone())
            }
            (Some(lhs_typ), &None) => {
                self.typs.insert(lhs.id(), lhs_typ.clone());
                let _ = self.check(&rhs, lhs_typ);
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
                    return Err(());
                }
                if lhs_typ == *expected_typ {
                    self.annotate(node, &lhs_typ);
                    return Ok(());
                } else {
                    self.flag_wrong_type(node, expected_typ, &lhs_typ);
                    return Err(());
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
                    return Err(());
                }
                if Type::Bit == *expected_typ {
                    self.annotate(node, &Type::Bit);
                    return Ok(());
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                    return Err(());
                }
            }
            CommonBinOp::LogicalAnd | CommonBinOp::LogicalOr | CommonBinOp::LogicalXor |
            CommonBinOp::And | CommonBinOp::Or | CommonBinOp::Xor => {
                if lhs_typ != Type::Bit {
                    self.flag_wrong_type(&lhs, &Type::Bit, &lhs_typ);
                    return Err(());
                }
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return Err(());
                }
                if Type::Bit == *expected_typ {
                    self.annotate(node, &Type::Bit);
                    Ok(())
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                    Err(())
                }
            }
        }
    }


    fn check_unop<'p>(&mut self, node: &AstNode<'p>, op: CommonUnOp, expected_typ: &Type) -> Result<(), ()> {
        let rhs = node.child(0);
        let Ok(Some(rhs_typ)) = self.infer(&rhs) else {
            self.flag_cant_infer(node);
            return Err(());
        };
        self.typs.insert(rhs.id(), rhs_typ.clone());

        match op {
            CommonUnOp::Neg | CommonUnOp::Inv => {
                if rhs_typ == *expected_typ {
                    self.annotate(node, &rhs_typ);
                    return Ok(());
                } else {
                    self.flag_wrong_type(node, expected_typ, &rhs_typ);
                    return Err(());
                }
            }
            CommonUnOp::Not => {
                if rhs_typ != Type::Bit {
                    self.flag_wrong_type(&rhs, &Type::Bit, &rhs_typ);
                    return Err(());
                }
                if Type::Bit == *expected_typ {
                    self.annotate(node, &Type::Bit);
                    return Ok(());
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                    return Err(());
                }
            }
        }
    }

    fn check_index<'p>(&mut self, node: &AstNode<'p>, index: Width, expected_typ: &Type) -> Result<(), ()> {
        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return Err(());
        };

        self.annotate(&subject, &subject_typ);

        match subject_typ {
            Type::Word(width) => {
                if index >= width {
                    self.diagnostics.push(diagnostics::Unknown {
                        region: node.region(),
                        message: format!("Index {index} out of bounds for Word[{width}]").into(),
                    }.into());
                    return Err(());
                }
                if Type::Bit == *expected_typ {
                    self.annotate(node, &Type::Bit);
                    Ok(())
                } else {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                    Err(())
                }
            }
            other => {
                self.flag_not_word_type(&subject, &other);
                Err(())
            }
        }
    }

    fn check_ext<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return Err(());
        };

        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(&subject) else {
            self.flag_cant_infer(node);
            return Err(());
        };

        self.annotate(&subject, &subject_typ);

        let subject_width = match subject_typ {
            Type::Bit | Type::Clock => 1,
            Type::Word(width) => width,
            other => {
                self.flag_not_word_type(&subject, &other);
                return Err(());
            }
        };

        if subject_width > *target_width {
            self.flag_wrong_type(node, &Type::Word(subject_width), expected_typ);
            return Err(());
        }

        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_hole<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        self.annotate(node, &expected_typ);
        self.diagnostics.push(diagnostics::UnfilledHole {
            region: node.region(),
            name: None,
            typ: Some(format!("{expected_typ:?}").into()),
        }.into());
        Ok(())
    }

    pub(crate) fn infer<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        match node.payload() {
            AstNodePayload::ExprParen => self.infer(&node.child(0)),
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                let typ = self.context.get(path.to_owned()); // TODO need to walk backwards to get it.
                self.use_component(path, node.location());
                if typ.is_none() {
                    self.diagnostics.push(diagnostics::UnresolvedComponent {
                        region: node.region(),
                        path: path.into(),
                    }.into());
                    Err(())
                } else {
                    Ok(typ)
                }
            }
            AstNodePayload::ExprBitLit(_expr_bit_lit) => {
                Ok(Some(Type::Bit))
            }
            AstNodePayload::ExprWordLit(_expr_word_lit) => {
                // TODO I shouldn't be doing string manip here.
                let path = node.spelling();
                if path.contains(&b'w') {
                    let parts: Vec<_> = path.split(|ch| *ch == b'w').collect();
                    let width: Width = std::str::from_utf8(parts[1]).unwrap().parse().unwrap();
                    Ok(Some(Type::Word(width)))
                } else {
                    Ok(None)
                }
            }
            AstNodePayload::ExprStrLit(_expr_str_lit) => Ok(self.infer_str(&node)?),
            AstNodePayload::ExprIndex(index) => Ok(self.infer_index(&node, index)?),
            AstNodePayload::ExprIndexRange(indexrange) => Ok(self.infer_index_range(&node, indexrange)?),
            AstNodePayload::ExprWord => Ok(self.infer_word(&node)?),
            AstNodePayload::ExprAs => {
                let subject = node.child(0);
                let typ_node = node.child(1);
                let parsing = node.parsing();
                match node_to_typ(typ_node, parsing, &self.symboltable) {
                    Ok(typ) => {
                        let _ = self.check(&subject, &typ);
                        Ok(Some(typ))
                    }
                    Err(diag) => {
                        self.diagnostics.push(diag);
                        Err(())
                    }
                }
            }
            AstNodePayload::ExprBinOp(_expr_bin_op) => self.infer_binop(node),
            _ => Ok(None),
        }
    }

    fn infer_word<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        // We can assume we have a width-less WordLit here, since it would have been inferred earlier.
        let mut total_width = 0;
        let mut diagnostics: Vec<Diagnostic> = vec![];
        for child in node.children() {
            match self.infer(&child)? {
                None => {
                    self.diagnostics.push(diagnostics::CantInfer {
                        region: node.region(),
                    }.into());
                }
                Some(typ) => {
                    total_width += match typ {
                        Type::Bit => 1,
                        Type::Word(w) => w,
                        _ => {
                            diagnostics.push(diagnostics::Todo {
                                region: node.region(),
                                message: "Invalid type in word(...): {typ:?}".into(),
                            }.into());
                            0
                        }
                    };
                    self.typs.insert(child.id(), typ);
                }
            }
        }

        if !diagnostics.is_empty() {
            return Err(());
        }

        Ok(Some(Type::Word(total_width)))
    }

    fn infer_str<'p>(&mut self, _node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        // TODO
        Ok(None)
    }

    fn infer_binop<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        let lhs = node.child(0);
        let rhs = node.child(1);
        let lhs_typ = self.infer(&lhs)?; // TODO combine the diagnostics on error for either of these two
        let rhs_typ = self.infer(&rhs)?;

        if let Some(ref typ) = lhs_typ {
            self.typs.insert(lhs.id(), typ.clone());
        }
        if let Some(ref typ) = rhs_typ {
            self.typs.insert(rhs.id(), typ.clone());
        }

        let AstNodePayload::ExprBinOp(binop) = node.payload() else { unreachable!() };
        match binop.op {
            crate::common::BinOp::Lt | crate::common::BinOp::Lte |
            crate::common::BinOp::Gt | crate::common::BinOp::Gte |
            crate::common::BinOp::Eq | crate::common::BinOp::Neq => {
                if let (Some(lhs_typ), Some(rhs_typ)) = (lhs_typ, rhs_typ) {
                    if lhs_typ == rhs_typ {
                        return Ok(Some(Type::Bit));
                    } else {
                        return Ok(None);
                    }
                } else {
                    return Ok(None);
                }
            }
            _ => (),
        }

        if let (Some(lhs_typ), Some(rhs_typ)) = (lhs_typ, rhs_typ) {
            if lhs_typ == rhs_typ {
                Ok(Some(lhs_typ))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }

    }

    fn infer_index<'p>(&mut self, node: &AstNode<'p>, index: payload::ExprIndex) -> Result<Option<Type>, ()> {
        let subject = node.subject().unwrap();
        let region = node.region();
        if let Some(subject_typ) = self.infer(&subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if index.index < *width {
                    Ok(Some(Type::Bit))
                } else {
                    self.diagnostics.push(diagnostics::Todo {
                        region,
                        message: "infer_index out of range".into(),
                    }.into());
                    Err(())
                }
            } else {
                self.diagnostics.push(diagnostics::Todo {
                    region,
                    message: "infer_index subject not a Word type".into(),
                }.into());
                Err(())
            }
        } else {
            self.diagnostics.push(diagnostics::Todo {
                region,
                message: "infer_index can't infer subject".into(),
            }.into());
            Err(())
        }
    }

    fn infer_index_range<'p>(
        &mut self,
        node: &AstNode<'p>,
        indexrange: payload::ExprIndexRange,
    ) -> Result<Option<Type>, ()> {
        let subject = node.subject().unwrap();
        let region = node.region();
        if let Some(subject_typ) = self.infer(&subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if indexrange.index_lo <= indexrange.index_hi && indexrange.index_hi <= *width {
                    Ok(Some(Type::Word(indexrange.index_hi - indexrange.index_lo)))
                } else {
                    self.diagnostics.push(diagnostics::Todo {
                        region,
                        message: "infer_index_range out of range".into(),
                    }.into());
                    Err(())
                }
            } else {
                self.diagnostics.push(diagnostics::Todo {
                    region,
                    message: "infer_index_range subject not a Word type".into(),
                }.into());
                Err(())
            }
        } else {
            self.diagnostics.push(diagnostics::Todo {
                region,
                message: "infer_index_range can't infer subject".into(),
            }.into());
            Err(())
        }
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

pub(crate) fn typecheck(builder: &mut Builder, symbol_id: SymbolId) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let exprroots = builder.get_exprroots();
    let item = symboltable.symbol(symbol_id);

    let mut use_locations: HashMap<BString, HashSet<Location>> = HashMap::new();

    let parsing = builder.get_parsing(item.package());
    let node: AstNode = parsing.ast_node(item.location().ast_node_id());
    if !node.contains_errors() {
        diagnostics.extend(typecheck_item(builder, item, &exprroots, &mut use_locations));
    }

    if let AstNodePayload::ModDef(moddef) = node.payload() && !moddef.is_ext {
        // Unused varibles and read from sink warnings
        let component_analysis = builder.get_component_analysis(symbol_id);
        for (path, component) in component_analysis.components() {
            if component.can_source() && !use_locations.contains_key(&path) {
                let region = builder.get_location_region(component.location());
                diagnostics.push(diagnostics::UnusedSource {
                    region,
                    path: path.into(),
                }.into());
            } else if component.flow() == Flow::Sink && use_locations.contains_key(&path) {
                for location in &use_locations[&path] {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::ReadFromSink {
                        region,
                        path: path.clone(),
                    }.into());
                }
            }
        }
    }

    diagnostics
}

fn typecheck_item(
    builder: &mut Builder,
    item: Symbol,
    exprroots: &[ExprRoot],
    use_locations: &mut HashMap<BString, HashSet<Location>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for exprroot in exprroots {
        if exprroot.package() == item.package() && item_for(builder, exprroot.location()).id() == item.id() {
            let typing = builder.get_typing(exprroot.clone());
            diagnostics.extend(typing.diagnostics());

            let typing_use_locations = typing.reference_use_locations();
            for (path, locations) in typing_use_locations.iter() {
                if !use_locations.contains_key(path.as_bstr()) {
                    use_locations.insert(path.clone(), HashSet::new());
                }
                let t = use_locations.get_mut(path.as_bstr()).unwrap();
                t.extend(locations.clone());
            }
        }
    }
    diagnostics
}

pub(crate) fn item_for(builder: &mut Builder, location: Location) -> Symbol {
    let symboltable = builder.get_symboltable();
    let parsing = builder.get_parsing(location.package());

    let mut node = parsing.ast_node(location.ast_node_id());
    loop {
        if node.is_item() {
            let name = node.name().unwrap();
            let name_str = parsing.string(name);
            return symboltable
                .resolve_item_in_package(name_str, location.package())
                .unwrap()
                .clone();
        }

        if let Some(parent) = node.parent() {
            node = parsing.ast_node(parent.id());
        } else {
            let region = builder.get_location_region(location.clone());
            panic!("No containing item found for location {location:?} at {region:?}");
        }
    }
}
