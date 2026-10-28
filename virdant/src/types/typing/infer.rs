//! Implements the type-inference pass of the `Typing` engine,
//! synthesizing a type for an expression from its structure and
//! context, handling references, literals, indexing, binary/unary
//! operators, function calls, constructors, and word operations.

use super::*;
use crate::util::log2;

impl Typing {
    pub(super) fn infer<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        match node.payload() {
            AstNodePayload::ExprReference => Ok(self.infer_reference(builder, context, &node)?),
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
            AstNodePayload::ExprIndex(index) => {
                Ok(self.infer_index(builder, context, &node, index)?)
            }
            AstNodePayload::ExprIndexDyn => {
                Ok(self.infer_index_dyn(builder, context, &node)?)
            }
            AstNodePayload::ExprIndexRange(indexrange) => {
                Ok(self.infer_index_range(builder, context, &node, indexrange)?)
            }
            AstNodePayload::ExprWord => Ok(self.infer_word(builder, context, &node)?),
            AstNodePayload::ExprAs => {
                let subject = node.child(0);
                let typ_node = node.child(1);
                let maybe_typ = builder
                    .get_type_index()
                    .type_at(typ_node.location())
                    .cloned();
                if let Some(typ) = maybe_typ {
                    let _ = self.check(builder, context, &subject, &typ);
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                } else {
                    Err(())
                }
            }
            AstNodePayload::ExprBinOp(_expr_bin_op) => self.infer_binop(builder, context, node),
            AstNodePayload::ExprUnOp(expr_un_op) => {
                self.infer_unop(builder, context, node, expr_un_op.op)
            }
            AstNodePayload::ExprTap => self.infer_tap(builder, node),
            AstNodePayload::ExprFn => self.infer_fn(builder, context, node),
            AstNodePayload::ExprField(field) => self.infer_field(builder, context, node, field),
            AstNodePayload::ExprCtor(_expr_ctor) => {
                // Constructors are not inferred from their arguments:
                // they are checked against an expected type, which
                // supplies the type arguments of the enclosing type.
                Ok(None)
            }
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

    fn infer_word<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        // We can assume we have a width-less WordLit here, since it would have been inferred earlier.
        let mut total_width = 0;
        let mut diagnostics: Vec<Diagnostic> = vec![];
        let mut uninferred_child = false;
        let args = node.args().unwrap();

        for child in args {
            match self.infer(builder, context.clone(), &child) {
                Ok(None) => {
                    self.diagnostics.push(Diagnostic::new(
                        node.region(),
                        DiagnosticPayload::CantInfer,
                    ));
                }
                Ok(Some(typ)) => {
                    let width = match &typ {
                        Type::Bit | Type::Reset => 1,
                        Type::Word(w) => *w,
                        Type::Usual(_symbol_id, _args) => {
                            if !typ.is_mono() {
                                diagnostics.push(Diagnostic::new(
                                    node.region(),
                                    diagnostics::Todo {
                                        message: "Generic type in word(...) is not supported yet".into(),
                                    },
                                ));
                                0
                            } else {
                                crate::types::width::type_width(builder, &typ)
                            }
                        }
                        Type::Param(_) | Type::Clock => {
                            diagnostics.push(Diagnostic::new(
                                node.region(),
                                diagnostics::Todo {
                                    message: format!("Invalid type in word(...): {typ:?}").into(),
                                },
                            ));
                            0
                        }
                    };
                    total_width += width;
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

    fn infer_binop<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        let lhs = node.child(0);
        let rhs = node.child(1);

        let AstNodePayload::ExprBinOp(binop) = node.payload() else {
            unreachable!()
        };
        match binop.op {
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::Neq => {
                // A comparison is inferrable whenever either operand
                // is inferrable.  If both are, they must be
                // compatible; if only one is, the other is checked
                // against it.  The result is always a Bit.
                let lhs_typ = self.infer(builder, context.clone(), &lhs)?;
                let rhs_typ = self.infer(builder, context.clone(), &rhs)?;
                match (lhs_typ, rhs_typ) {
                    (Some(lhs_typ), Some(rhs_typ)) => {
                        if !Self::compatible_typs(&lhs_typ, &rhs_typ) {
                            self.flag_wrong_type(node, &lhs_typ, &rhs_typ);
                            return Err(());
                        }
                    }
                    (Some(lhs_typ), None) => {
                        self.check(builder, context.clone(), &rhs, &lhs_typ)?;
                    }
                    (None, Some(rhs_typ)) => {
                        self.check(builder, context.clone(), &lhs, &rhs_typ)?;
                    }
                    (None, None) => {
                        self.flag_cant_infer(node);
                        return Err(());
                    }
                }
                self.annotate(node, &Type::Bit);
                Ok(Some(Type::Bit))
            }
            BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Xor => {
                // An arithmetic/bitwise op is inferrable whenever
                // either operand is inferrable.  If both are, they
                // must be compatible; if only one is, the other is
                // checked against it.  The result takes their common
                // type.
                let lhs_typ = self.infer(builder, context.clone(), &lhs)?;
                let rhs_typ = self.infer(builder, context.clone(), &rhs)?;
                let typ = match (lhs_typ, rhs_typ) {
                    (Some(lhs_typ), Some(rhs_typ)) => {
                        if !Self::compatible_typs(&lhs_typ, &rhs_typ) {
                            self.flag_wrong_type(node, &lhs_typ, &rhs_typ);
                            return Err(());
                        }
                        lhs_typ
                    }
                    (Some(lhs_typ), None) => {
                        self.check(builder, context.clone(), &rhs, &lhs_typ)?;
                        lhs_typ
                    }
                    (None, Some(rhs_typ)) => {
                        self.check(builder, context.clone(), &lhs, &rhs_typ)?;
                        rhs_typ
                    }
                    (None, None) => {
                        self.flag_cant_infer(node);
                        return Err(());
                    }
                };
                self.annotate(node, &typ);
                Ok(Some(typ))
            }
            BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::LogicalXor => {
                let lhs_ok = self
                    .check(builder, context.clone(), &lhs, &Type::Bit)
                    .is_ok();
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

    fn infer_index_dyn<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        // Dynamic index: a[i] where a: Word[n], i: Word[k], and n == 2^k
        let subject = node.subject().unwrap();
        let index = node.index().unwrap();

        // Infer subject type: a must be Word[n]
        let Ok(Some(subject_typ)) = self.infer(builder, context.clone(), &subject) else {
            self.flag_cant_infer(&subject);
            return Err(());
        };

        let Type::Word(n) = subject_typ else {
            self.diagnostics.push(Diagnostic::new(
                subject.region(),
                diagnostics::IndexNotWordType {
                    typ: format!("Subject is not a Word type: {:?}", subject_typ).into(),
                },
            ));
            return Err(());
        };

        let k = match log2(n) {
            Some(k) => k,
            None => {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::InvalidWordIndexWidth {
                        array_width: n,
                        index_width: 0,
                    },
                ));
                return Err(());
            }
        };

        let expected_index_type = Type::Word(k);

        self.check(builder, context, &index, &expected_index_type)?;

        self.annotate(node, &Type::Bit);
        Ok(Some(Type::Bit))
    }

    fn infer_index<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        index: payload::ExprIndex,
    ) -> Result<Option<Type>, ()> {
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
                    self.diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::IndexOutOfBounds {
                            array_width: *width,
                            index: index.index,
                        },
                    ));
                    Err(())
                }
            } else {
                    self.diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::Todo {
                            message: "infer_index subject not a Word type".into(),
                        },
                    ));
                Err(())
            }
        } else {
            self.diagnostics.push(Diagnostic::new(
                region,
                diagnostics::Todo {
                    message: "infer_index can't infer subject".into(),
                },
            ));
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
                if indexrange.index_lo > indexrange.index_hi {
                    self.diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::InvalidIndexRange {
                            array_width: *width,
                            index_hi: indexrange.index_hi,
                            index_lo: indexrange.index_lo,
                        },
                    ));
                    Err(())
                } else if indexrange.index_hi > *width {
                    self.diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::IndexRangeOutOfBounds {
                            array_width: *width,
                            index_hi: indexrange.index_hi,
                            index_lo: indexrange.index_lo,
                        },
                    ));
                    Err(())
                } else {
                    let typ = Type::Word(indexrange.index_hi - indexrange.index_lo);
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                }
            } else {
                self.diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::Todo {
                        message: "infer_index_range subject not a Word type".into(),
                    },
                ));
                Err(())
            }
        } else {
            self.diagnostics.push(Diagnostic::new(
                region,
                diagnostics::Todo {
                    message: "infer_index_range can't infer subject".into(),
                },
            ));
            Err(())
        }
    }

    fn infer_field<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        field: payload::ExprField,
    ) -> Result<Option<Type>, ()> {
        // A field projection `foo->x` infers from its subject: once the
        // subject's type is known (a struct typedef with type arguments),
        // the field's type is fully determined by the struct definition.
        let subject = node.subject().unwrap();
        let Some(subject_typ) = self.infer(builder, context, &subject)? else {
            // Subject couldn't be inferred; leave the projection uninferred.
            // In check mode, check_field produces the diagnostic.
            return Ok(None);
        };

        let Type::Usual(typedef_id, args) = &subject_typ else {
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let struct_fields = builder.get_struct_fields(*typedef_id);
        let field_name = node.parsing().string(field.field);

        let Some(struct_field) = struct_fields
            .into_iter()
            .filter(|field_| field_.name() == field_name)
            .next()
        else {
            self.flag_unknown(node, format!("Unknown field {field_name}"));
            return Err(());
        };

        let Some(field_typ) = struct_field.typ() else {
            self.flag_todo(&subject, "Field missing type");
            return Err(());
        };
        // Substitute the generic parameters of the struct def with the
        // type arguments at this use site.
        let field_typ = substitute(field_typ, args);

        self.annotate(node, &field_typ);
        Ok(Some(field_typ))
    }

    fn infer_fn<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        let children = node.children();
        let fn_ofness = node.child(0);
        let args = &children[1..];

        let AstNodePayload::Ofness(ofness) = fn_ofness.payload() else {
            unreachable!()
        };
        let fn_name = fn_ofness.parsing().string(ofness.name);
        //        let fn_item = builder.get_symboltable().resolve_item_in_package(fn_name, node.package());
        let fn_name: &[u8] = fn_name.into();

        match fn_name {
            b"any" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags
                        .insert(node.location(), Tag::PrimitiveResolution(Primitive::Any));
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    todo!()
                }
            }
            b"all" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags
                        .insert(node.location(), Tag::PrimitiveResolution(Primitive::All));
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    todo!()
                }
            }
            b"word" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::Word));
                self.infer_word(builder, context, &node)
            }
            b"mux" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::Mux));
                self.infer_mux(builder, context, node, args)
            }
            b"asclock" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::AsClock));
                let subject = node.subject().unwrap();

                // Infer the subject directly rather than checking it
                // against Bit: a `check` here would annotate the
                // subject even when this function goes on to return
                // `Ok(None)`, and a later visit would annotate it
                // again.
                match self.infer(builder, context, &subject)? {
                    Some(typ) if matches!(typ, Type::Bit | Type::Reset) => {
                        self.annotate(&node, &Type::Clock);
                        Ok(Some(Type::Clock))
                    }
                    Some(typ) => {
                        self.flag_wrong_type(node, &Type::Bit, &typ);
                        Err(())
                    }
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    /// Infer the type of a `mux(cond, a, b)` expression.
    ///
    /// A mux is inferrable whenever either of its data arms (`a` or
    /// `b`) is inferrable.  If both arms are inferrable, they must
    /// match.  If only one is, the other is checked against it.
    ///
    /// When neither arm is inferrable, the condition is left
    /// untouched for `check_mux`, which is reached exactly when this
    /// function returns `Ok(None)`.
    fn infer_mux<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        args: &[AstNode<'p>],
    ) -> Result<Option<Type>, ()> {
        // mux(cond, a, b) requires exactly 3 arguments
        if args.len() != 3 {
            self.flag_unknown(node, format!("mux requires exactly 3 arguments, got {}", args.len()));
            return Err(());
        }

        let cond = &args[0];
        let a = &args[1];
        let b = &args[2];

        let a_typ = self.infer(builder, context.clone(), a)?;
        let b_typ = self.infer(builder, context.clone(), b)?;

        let typ = match (a_typ, b_typ) {
            (None, None) => return Ok(None),
            (Some(a_typ), Some(b_typ)) => {
                if !Self::compatible_typs(&a_typ, &b_typ) {
                    self.flag_wrong_type(node, &a_typ, &b_typ);
                    return Err(());
                }
                a_typ
            }
            (Some(typ), None) => {
                self.check(builder, context.clone(), b, &typ)?;
                typ
            }
            (None, Some(typ)) => {
                self.check(builder, context.clone(), a, &typ)?;
                typ
            }
        };

        // The mux is inferrable from its arms, so the condition is
        // checked here rather than in `check_mux`.
        self.check(builder, context.clone(), cond, &Type::Bit)?;

        self.annotate(node, &typ);
        Ok(Some(typ))
    }

    fn infer_unop<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        op: UnOp,
    ) -> Result<Option<Type>, ()> {
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

                if matches!(subject_typ, Type::Bit | Type::Reset) {
                    self.annotate(&node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    self.flag_wrong_type(&subject, &Type::Bit, &subject_typ);
                    Err(())
                }
            }
        }
    }

    fn infer_tap<'p>(
        &mut self,
        builder: &mut Builder,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        let path_node = node.child(0);
        let component_analysis = builder.get_component_analysis(self.item.id());
        let Some(component) = component_analysis.component_at(path_node.location()) else {
            self.flag_tap_not_on_net(node);
            return Err(());
        };
        if !matches!(
            component.kind(),
            Some(ComponentKind::Bus) | Some(ComponentKind::Net)
        ) {
            self.flag_tap_not_on_net(node);
            return Err(());
        }
        let referent = Referent::Component(component.id());
        self.tags
            .insert(path_node.location(), Tag::ReferentResolution(referent));
        self.use_component(component.path().as_bstr(), node.location());
        if let Some(typ) = component.typ() {
            self.annotate(node, &typ);
            Ok(Some(typ))
        } else {
            Err(())
        }
    }

    fn flag_tap_not_on_net(&mut self, node: &AstNode<'_>) {
        self.diagnostics.push(Diagnostic::new(
            node.region(),
            diagnostics::TapNotOnNet {
                source: node.spelling().to_owned(),
            },
        ));
    }

    fn infer_reference<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        // Use ComponentAnalysis to resolve the reference by AST location.
        // The component analysis already resolved "it" during reference
        // collection, so no string manipulation is needed here.
        let component_analysis = builder.get_component_analysis(self.item.id());
        if let Some(component) = component_analysis.component_at(node.location()) {
            if matches!(
                component.kind(),
                Some(ComponentKind::Bus) | Some(ComponentKind::Net)
            ) {
                self.diagnostics.push(Diagnostic::new(
                    node.region(),
                    diagnostics::NetReadWithoutTap {
                        source: node.spelling().to_owned(),
                    },
                ));
                return Err(());
            }
            let referent = Referent::Component(component.id());
            self.tags
                .insert(node.location(), Tag::ReferentResolution(referent));
            self.use_component(component.path().as_bstr(), node.location());
            if let Some(typ) = component.typ() {
                self.annotate(&node, &typ);
                Ok(Some(typ))
            } else {
                Err(())
            }
        } else {
            // Fall back to name-based context lookup, for pattern-bound
            // locals and non-moddef items.
            let parsing = node.parsing();
            let path = parsing.string(node.path().unwrap());
            let path_str = path.to_owned();
            if let Some((referent, typ)) = context.get(path_str.clone()) {
                self.tags
                    .insert(node.location(), Tag::ReferentResolution(referent));
                if let Some(typ) = typ {
                    self.annotate(&node, &typ);
                    Ok(Some(typ))
                } else {
                    Err(())
                }
            } else {
                self.flag_unknown(node, format!("Unknown component {}", path));
                Err(())
            }
        }
    }
}
