use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::component::ComponentId;
use crate::analysis::symbols::{Symbol, SymbolId};
use crate::common::{BinOp, Flow, UnOp as UnOp, Width};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticLevel};
use crate::analysis::location::Location;
use crate::fqn::PackageFqn;
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

#[derive(Debug, Clone)]
pub enum Primitive {
    Any,
    All,
    Cast,
}


#[derive(Debug, Clone)]
pub enum Tag {
    None,
    SymbolResolution(SymbolId),
    PrimitiveResolution(Primitive), // TODO add an enum for primitives
    ComponentResolution(ComponentId),
}

impl Tag {
    pub fn symbol_id(&self) -> Option<SymbolId> {
        if let Tag::SymbolResolution(symbol_id) = self {
            Some(symbol_id.clone())
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Typing {
    item: Symbol,
    exprroot: ExprRoot,
    typs: IndexMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
    use_locations: IndexMap<BString, Vec<Location>>, // TODO should be use for Referents
    tags: IndexMap<Location, Tag>,
}

impl Typing {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn tag(&self, location: Location) -> Tag {
        if let Some(tag) = self.tags.get(&location).cloned() {
            tag
        } else {
            Tag::None
        }
    }

    pub fn reference_use_locations(&self) -> &IndexMap<BString, Vec<Location>> {
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

    #[allow(unused)]
    fn flag_todo<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Todo {
            region: node.region(),
            message: message.into(),
        }.into());
    }

    fn flag_unknown<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Unknown {
            region: node.region(),
            message: message.into(),
        }.into());
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
        let previous = self.typs.insert(node.id(), typ.clone());
        if let Some(previous_typ) = previous {
            panic!("Node is already annotated with {previous_typ:?}, can't annotate with {typ:?} at {:?}", node.region());
        }
    }

    #[rustfmt::skip]
    pub(crate) fn check<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        if let Some(actual_typ) = self.infer(builder, context.clone(), node)? {
            if actual_typ != *expected_typ {
                self.flag_wrong_type(node, expected_typ, &actual_typ);
                return Err(());
            } else {
                return Ok(());
            }
        }

        match node.payload() {
            AstNodePayload::ExprParen                => self.check_paren(builder, context, node, expected_typ),
            AstNodePayload::ExprIf                   => self.check_if(builder, context, node, expected_typ),
            AstNodePayload::ExprWordLit(_)           => self.check_word_lit(node, expected_typ),
            AstNodePayload::ExprIndex(expr_index)    => self.check_index(builder, context, node, expr_index.index, expected_typ),
            AstNodePayload::ExprZext                 => self.check_ext(builder, context, node, expected_typ),
            AstNodePayload::ExprSext                 => self.check_ext(builder, context, node, expected_typ),
            AstNodePayload::ExprHole                 => self.check_hole(node, expected_typ),
            AstNodePayload::ExprMatch                => self.check_match(builder, context, node, expected_typ),
            AstNodePayload::ExprMethod(_expr_method) => Ok(()), // TODO
            AstNodePayload::ExprFn                   => self.check_fn(builder, context, node, expected_typ),
            AstNodePayload::ExprCtor(_ctor)          => self.check_ctor(builder, context, node, expected_typ),
            AstNodePayload::ExprEnumerant(_)         => self.check_enumerant(builder, node, expected_typ),
            AstNodePayload::ExprStruct               => Ok(()), // TODO
            AstNodePayload::ExprIndexRange(_expr_index_range) => Ok(()), // TODO
            _ => unreachable!("Can't typecheck {:?}", node.summary()),
        }
    }

    fn check_paren<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let child = node.child(0);
        self.check(builder, context, &child, expected_typ)?;
        if self.typs.contains_key(&child.id()) {
            self.annotate(node, expected_typ);
            Ok(())
        } else {
            Err(())
        }
    }

    fn check_if<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let mut err = false;
        let children = node.children();
        err |= self.check(builder, context.clone(), &children[0], &Type::Bit).is_err();
        err |= self.check(builder, context.clone(), &children[1], expected_typ).is_err();
        let mut i = 2;
        while i + 1 < children.len() - 1 {
            err |= self.check(builder, context.clone(), &children[i], &Type::Bit).is_err();
            err |= self.check(builder, context.clone(), &children[i + 1], expected_typ).is_err();
            i += 2;
        }

        // TODO this all needs helpers in AstNode
        err |= self.check(builder, context, &children[children.len() - 1], expected_typ).is_err();

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

    fn check_fn<'p>(&mut self, builder: &mut Builder<'_>, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let children = node.children();
        let fn_ofness = node.child(0);
        let args = &children[1..];

        let AstNodePayload::Ofness(ofness) = fn_ofness.payload() else { unreachable!() };
        let fn_name = fn_ofness.parsing().string(ofness.name);
//        let fn_item = builder.get_symboltable().resolve_item_in_package(fn_name, node.package());
        let fn_name: &[u8] = fn_name.into();

        match fn_name {
            b"cast" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags.insert(node.location(), Tag::PrimitiveResolution(Primitive::Cast));
                    self.annotate(node, expected_typ);
                    Ok(())
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn infer_unop<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, op: UnOp) -> Result<Option<Type>, ()> {
        let subject = node.subject().unwrap();

        match op {
            UnOp::Inv | UnOp::Neg => {
                let Some(subject_typ) = self.infer(builder, context, &subject)? else {
                    //self.flag_cant_infer(&subject);
                    self.flag_todo(&subject, "OK");
                    return Err(());
                };

                if let Type::Word(width) = subject_typ {
                    self.annotate(&node, &Type::Word(width));
                    Ok(Some(Type::Word(width)))
                } else {
                    self.flag_not_word_type(&subject, &subject_typ);
                    Err(())
                }
            }
            UnOp::Not => {
                let Some(subject_typ) = self.infer(builder, context, &subject)? else {
                    //self.flag_cant_infer(&subject);
                    self.flag_todo(&subject, "OK2");
                    return Err(());
                };

                if let Type::Bit = subject_typ {
                    self.annotate(&node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    self.flag_wrong_type(&subject, &Type::Bit, &subject_typ);
                    Err(())
                }
            }
        }
    }

    fn infer_fn<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        let children = node.children();
        let fn_ofness = node.child(0);
        let args = &children[1..];

        let AstNodePayload::Ofness(ofness) = fn_ofness.payload() else { unreachable!() };
        let fn_name = fn_ofness.parsing().string(ofness.name);
//        let fn_item = builder.get_symboltable().resolve_item_in_package(fn_name, node.package());
        let fn_name: &[u8] = fn_name.into();

        match fn_name {
            b"any" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags.insert(node.location(), Tag::PrimitiveResolution(Primitive::Any));
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    todo!()
                }
            }
            b"all" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags.insert(node.location(), Tag::PrimitiveResolution(Primitive::All));
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    todo!()
                }
            }
            _ => Ok(None),
        }
    }

    fn check_index<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, index: Width, expected_typ: &Type) -> Result<(), ()> {
        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(builder, context, &subject) else {
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

    fn check_ext<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return Err(());
        };

        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(builder, context, &subject) else {
            self.flag_cant_infer(node);
            return Err(());
        };

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

    fn check_match<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let subject = node.subject().unwrap();
        let Some(subject_typ) = self.infer(builder, context.clone(), &subject)? else {
            self.flag_unknown(node, "Could not infer subject");
            return Err(());
        };

        for chunks in node.children().into_iter().skip(1).collect::<Vec<_>>().chunks(2) {
            let [pat, expr] = chunks else { unreachable!() };
            let arm_context = self.check_pat(builder, context.clone(), pat, &subject_typ)?;
            self.check(builder, arm_context, expr, expected_typ)?;
        }

        self.annotate(node, &expected_typ);

//        self.diagnostics.push(diagnostics::UnfilledHole {
//            region: node.region(),
//            name: None,
//            typ: Some(format!("{expected_typ:?}").into()),
//        }.into());
        Ok(())
    }

    fn check_pat<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<TypingContext, ()> {
        match node.payload() {
            AstNodePayload::PatElse => Ok(context),
            AstNodePayload::PatIdent(pat_ident) => {
                let Type::Usual(typedef_id) = expected_typ else {
                    self.flag_unknown(node, "PatIdent expects a union type");
                    return Err(());
                };
                let ctor_name = node.parsing().string(pat_ident.name);
                let symboltable = builder.get_symboltable();
                let Some(ctor_symbol) = symboltable.slot(*typedef_id, ctor_name) else {
                    self.flag_unknown(node, "Unknown ctor name in pattern");
                    return Err(());
                };
                let sig = builder.get_ctor_signature(ctor_symbol.id());
                let children = node.children();
                if sig.parameters.len() != children.len() {
                    self.flag_unknown(
                        node,
                        format!(
                            "Pattern has wrong number of arguments: expected {}, but found {}",
                            sig.parameters.len(),
                            children.len(),
                        ),
                    );
                    return Err(());
                }
                let mut arm_context = context;
                for (child, (_param_name, param_typ)) in children.iter().zip(sig.parameters.iter()) {
                    self.annotate(child, param_typ);
                    if let Some(var_name_interned) = child.path() {
                        let var_name = child.parsing().string(var_name_interned).to_owned();
                        arm_context = arm_context.extend(std::iter::once((var_name, param_typ.clone())));
                    }
                }
                Ok(arm_context)
            }
            AstNodePayload::PatEnumerant(pat_enumerant) => {
                let Type::Usual(typedef_id) = expected_typ else {
                    self.flag_unknown(node, "PatEnumerant expects an enum type");
                    return Err(());
                };
                let enumerant_name = node.parsing().string(pat_enumerant.name);
                let symboltable = builder.get_symboltable();
                let Some(_enumerant_symbol) = symboltable.slot(*typedef_id, enumerant_name) else {
                    self.flag_unknown(node, "Unknown enumerant name in pattern");
                    return Err(());
                };
                Ok(context)
            }
            _ => unreachable!("Expected a pattern node, found: {}", node.summary()),
        }
    }

    fn check_ctor<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        // TODO check that it's an ctor type
        let AstNodePayload::ExprCtor(expr_ctor) = node.payload() else { unreachable!() };
        let Type::Usual(typedef_id) = expected_typ else {
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let ctor_name = node.parsing().string(expr_ctor.ctor);
        let symboltable = builder.get_symboltable();
        let Some(ctor_symbol) = symboltable.slot(*typedef_id, ctor_name) else {
            self.flag_unknown(node, "Unknown ctor name: {ctor_name} in union type {expected_typ:?}");
            return Err(());
        };

        let sig = builder.get_ctor_signature(ctor_symbol.id());
        let arguments = node.children();

        if sig.parameters.len() != arguments.len() {
            self.flag_unknown(
                node,
                format!(
                    "Ctor was provided the wrong number of arguments: expected {}, but found {}",
                    sig.parameters.len(),
                    arguments.len(),
                ),
            );
            return Err(());
        }

        for (arg, (_param_name, param_typ)) in arguments.into_iter().zip(sig.parameters.iter()) {
            self.check(builder, context.clone(), &arg, param_typ)?;
        }

        self.tags.insert(node.location(), Tag::SymbolResolution(ctor_symbol.id()));
        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_enumerant<'p>(&mut self, builder: &mut Builder, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        let AstNodePayload::ExprEnumerant(expr_enumerant) = node.payload() else { unreachable!() };
        let Type::Usual(typedef_id) = expected_typ else {
            // TODO check that it's an enum type
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let enumerant_name = node.parsing().string(expr_enumerant.enumerant);
        let symboltable = builder.get_symboltable();
        let Some(enumerant_symbol) = symboltable.slot(*typedef_id, enumerant_name) else {
            self.flag_unknown(node, "Unknown enumerant name: {enumerant_name} in enum type {expected_typ:?}");
            return Err(());
        };

        self.tags.insert(node.location(), Tag::SymbolResolution(enumerant_symbol.id()));
        self.annotate(node, &expected_typ);
        Ok(())
    }

    pub(crate) fn infer<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        match node.payload() {
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                if let Some(typ) = context.get(path.to_owned()) {
                    let component_analysis = builder.get_component_analysis(self.item.id());

                    if let Some(component) = component_analysis.resolve(path) {
                        self.tags.insert(node.location(), Tag::ComponentResolution(component.id()));
                    } else {
                        // This case is where we are referencing a local.
                    }

                    self.use_component(path, node.location());
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                } else {
                    self.flag_unknown(node, "Unknown component");
                    Err(())
                }
            }
            AstNodePayload::ExprBitLit(_expr_bit_lit) => {
                self.annotate(&node, &Type::Bit);
                Ok(Some(Type::Bit))
            }
            AstNodePayload::ExprWordLit(_expr_word_lit) => {
                // TODO I shouldn't be doing string manip here.
                let path = node.spelling();
                if path.contains(&b'w') {
                    let parts: Vec<_> = path.split(|ch| *ch == b'w').collect();
                    let width: Width = std::str::from_utf8(parts[1]).unwrap().parse().unwrap();
                    self.annotate(&node, &Type::Word(width));
                    Ok(Some(Type::Word(width)))
                } else {
                    Ok(None)
                }
            }
            AstNodePayload::ExprStrLit(_expr_str_lit) => Ok(self.infer_str(&node)?),
            AstNodePayload::ExprIndex(index) => Ok(self.infer_index(builder, context, &node, index)?),
            AstNodePayload::ExprIndexRange(indexrange) => Ok(self.infer_index_range(builder, context, &node, indexrange)?),
            AstNodePayload::ExprWord => Ok(self.infer_word(builder, context, &node)?),
            AstNodePayload::ExprAs => {
                let subject = node.child(0);
                let typ_node = node.child(1);
                let maybe_typ = builder.get_type_index().type_at(typ_node.location()).cloned();
                if let Some(typ) = maybe_typ  {
                    let _ = self.check(builder, context, &subject, &typ);
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                } else {
                    Err(())
                }
            }
            AstNodePayload::ExprBinOp(_expr_bin_op) => self.infer_binop(builder, context, node),
            AstNodePayload::ExprUnOp(expr_un_op)     => self.infer_unop(builder, context, node, expr_un_op.op),
            AstNodePayload::ExprFn     => self.infer_fn(builder, context, node),
            AstNodePayload::ExprParen => {
                if let Some(typ) = self.infer(builder, context, &node.subject().unwrap())? {
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn infer_word<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        // We can assume we have a width-less WordLit here, since it would have been inferred earlier.
        let mut total_width = 0;
        let mut diagnostics: Vec<Diagnostic> = vec![];
        let mut uninferred_child = false;
        for child in node.children() {
            match self.infer(builder, context.clone(), &child) {
                Ok(None) => {
                    self.diagnostics.push(diagnostics::CantInfer {
                        region: node.region(),
                    }.into());
                }
                Ok(Some(typ)) => {
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
                }
                Err(_) => uninferred_child = true,
            }
        }

        if uninferred_child {
            return Err(());
        }

        self.annotate(&node, &Type::Word(total_width));
        Ok(Some(Type::Word(total_width)))
    }

    fn infer_str<'p>(&mut self, _node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        // TODO
        Ok(None)
    }

    fn infer_binop<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>) -> Result<Option<Type>, ()> {
        let lhs = node.child(0);
        let rhs = node.child(1);

        let AstNodePayload::ExprBinOp(binop) = node.payload() else { unreachable!() };
        match binop.op {
            BinOp::Lt | BinOp::Lte |
            BinOp::Gt | BinOp::Gte |
            BinOp::Eq | BinOp::Neq => {
                if let Some(lhs_typ) = self.infer(builder, context.clone(), &lhs)? {
                    self.check(builder, context, &rhs, &lhs_typ)?;
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    self.flag_unknown(&node, "Can't infer LHS");
                    Err(())
                }
            }
            BinOp::Add | BinOp::Sub | BinOp::And |
            BinOp::Or | BinOp::Xor => {
                if let Some(lhs_typ) = self.infer(builder, context.clone(), &lhs)? {
                    self.check(builder, context, &rhs, &lhs_typ)?;
                    self.annotate(node, &lhs_typ);
                    Ok(Some(lhs_typ))
                } else {
                    self.flag_cant_infer(node);
                    Err(())
                }
            }
            BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::LogicalXor => {
                let lhs_ok = self.check(builder, context.clone(), &lhs, &Type::Bit).is_ok();
                let rhs_ok = self.check(builder, context, &rhs, &Type::Bit).is_ok();
                if lhs_ok && rhs_ok {
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    Err(())
                }
            }
        }
    }

    fn infer_index<'p>(&mut self, builder: &mut Builder, context: TypingContext, node: &AstNode<'p>, index: payload::ExprIndex) -> Result<Option<Type>, ()> {
        let subject = node.subject().unwrap();
        let region = node.region();
        if let Some(subject_typ) = self.infer(builder, context, &subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if index.index < *width {
                    self.annotate(&node, &Type::Bit);
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
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        indexrange: payload::ExprIndexRange,
    ) -> Result<Option<Type>, ()> {
        let subject = node.subject().unwrap();
        let region = node.region();
        if let Some(subject_typ) = self.infer(builder, context, &subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if indexrange.index_lo <= indexrange.index_hi && indexrange.index_hi <= *width {
                    let typ = Type::Word(indexrange.index_hi - indexrange.index_lo);
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
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

    // Checks that in a clean typecheck, all expressions in the tree have an annotation.
    pub fn validate(&mut self, builder: &mut Builder) {
        if self.diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
            return;
        }

        let parsing = builder.get_parsing(self.exprroot.package());
        let root_id = self.exprroot.location().ast_node_id();
        let mut queue = vec![root_id];
        while let Some(node_id) = queue.pop() {
            let node = parsing.ast_node(node_id);
            if node.is_expr() && !self.typs.contains_key(&node.id()) {
                self.flag_unknown(&node, "Missing annotation");
            }

            for child in node.children() {
                queue.push(child.id());
            }
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

pub(crate) fn build_type_at(builder: &mut Builder, location: Location) -> Result<Type, Vec<Diagnostic>> {
    let parsing = builder.get_parsing(location.package());
    let symboltable = builder.get_symboltable();
    let typ_node = parsing.ast_node(location.ast_node_id());

    match typ_node.payload() {
        AstNodePayload::Type(_typ) => {
            let type_name: BString = match typ_node.child(0).payload() {
                AstNodePayload::Ofness(ofness) => {
                    if let Some(package) = ofness.package {
                        let package_name = parsing.string(package);
                        let typ_name = parsing.string(ofness.name);
                        format!("{}::{}", package_name, typ_name).into()
                    } else {
                        parsing.string(ofness.name).to_owned()
                    }
                }
                _ => {
                    let region = builder.get_location_region(location);
                    return Err(vec![diagnostics::Unknown {
                        region,
                        message: format!("Expected Ofness, but found {:?}", typ_node.payload()).into()
                    }.into()])
                }
            };

            if let Some(symbol) = symboltable.resolve_item(type_name.as_bstr(), parsing.package()) {
                let bit_symbol = symboltable.resolve(b"builtin::Bit".into()).unwrap();
                let word_symbol = symboltable.resolve(b"builtin::Word".into()).unwrap();
                let clock_symbol = symboltable.resolve(b"builtin::Clock".into()).unwrap();

                let typ = if symbol.id() == bit_symbol.id() {
                    Type::Bit
                } else if symbol.id() == clock_symbol.id() {
                    Type::Clock
                } else if symbol.id() == word_symbol.id() {
                    let generics_node = typ_node.child(1);
                    let AstNodePayload::GenericsParams(generics_params) = generics_node.payload() else {
                        unreachable!()
                    };
                    let spelling = parsing.string(generics_params.value).to_str_lossy().into_owned();
                    let width = spelling.parse::<Width>().unwrap();
                    Type::Word(width)
                } else {
                    Type::Usual(symbol.id())
                };
                Ok(typ)
            } else {
                Err(vec![diagnostics::UnresolvedType {
                    region: typ_node.region(),
                    typ: type_name.into(),
                }.into()])
            }
        }
        _ => panic!(),
    }
}

pub(crate) fn typecheck(builder: &mut Builder, symbol_id: SymbolId) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let exprroots = builder.get_exprroots();
    let item = symboltable.symbol(symbol_id);

    let mut use_locations: IndexMap<BString, IndexSet<Location>> = IndexMap::new();

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
    use_locations: &mut IndexMap<BString, IndexSet<Location>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for exprroot in exprroots {
        if exprroot.package() == item.package() && item_for(builder, exprroot.location()).id() == item.id() {
            let typing = builder.get_typing(exprroot.clone());
            diagnostics.extend(typing.diagnostics());

            let typing_use_locations = typing.reference_use_locations();
            for (path, locations) in typing_use_locations.iter() {
                if !use_locations.contains_key(path.as_bstr()) {
                    use_locations.insert(path.clone(), IndexSet::new());
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

pub(crate) fn build_typing(builder: &mut Builder, exprroot: ExprRoot) -> Arc<Typing> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());

    let mut current_id = location.ast_node_id();
    // REVIEW walk up until you find the containing item ast node.
    let item_name = loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            break parsing
                .string(current.name().expect("expected containing item to have a name"))
                .to_owned();
        }
        current_id = current
            .parent()
            .expect("expected expr root to be contained in an item")
            .id();
    };
    let symboltable = builder.get_symboltable();
    let item = symboltable
        .resolve_item_in_package(item_name.as_bstr(), location.package())
        .unwrap();
    let context = builder.get_typing_context(item.id());

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = builder.get_expected_type(exprroot.clone());

    let diagnostics = vec![];
    let mut typing = Typing {
        item: item.clone(),
        exprroot: exprroot.clone(),
        typs: IndexMap::new(),
        diagnostics,
        use_locations: IndexMap::new(),
        tags: IndexMap::new(),
    };

    // if there is no expected type, you can't type check the expression.
    if let Some(expected_typ) = expected_typ {
        let _ = typing.check(builder, context, &node, &expected_typ);
    } else {
        match typing.infer(builder, context, &node) {
            Ok(None) => {
                typing.diagnostics.push(diagnostics::Todo {
                    region: node.region(),
                    message: format!("Can't typecheck expression because we don't know what type it should have").into(),
                }.into());
            }
            Ok(Some(typ)) => {
                typing.typs.insert(node.id(), typ);
            }
            _ => (),
        }
    }

    typing.validate(builder);

    Arc::new(typing)
}
