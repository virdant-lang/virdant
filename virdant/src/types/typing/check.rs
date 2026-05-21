use super::*;

impl Typing {
    #[rustfmt::skip]
    pub(super) fn check<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        if let Some(actual_typ) = self.infer(builder, context.clone(), node)? {
            let compatible = actual_typ == *expected_typ
                || matches!(
                    (&actual_typ, expected_typ),
                    (Type::Bit, Type::Reset) | (Type::Reset, Type::Bit)
                );
            if !compatible {
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
            AstNodePayload::ExprHole                 => self.check_hole(node, expected_typ),
            AstNodePayload::ExprDontcare             => self.check_dontcare(node, expected_typ),
            AstNodePayload::ExprMatch                => self.check_match(builder, context, node, expected_typ),
            AstNodePayload::ExprFn                   => self.check_fn(builder, context, node, expected_typ),
            AstNodePayload::ExprField(field)         => self.check_field(builder, context, node, expected_typ, field),
            AstNodePayload::ExprCtor(_ctor)          => self.check_ctor(builder, context, node, expected_typ),
            AstNodePayload::ExprEnumerant(_)         => self.check_enumerant(builder, node, expected_typ),
            AstNodePayload::ExprStruct               => self.check_struct(builder, context, node, expected_typ),
            AstNodePayload::ExprIndexRange(_expr_index_range) => Ok(()), // TODO
            _ => unreachable!("Can't typecheck {:?}", node.summary()),
        }
    }

    fn check_paren<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let child = node.child(0);
        self.check(builder, context, &child, expected_typ)?;
        if self.typs.contains_key(&child.id()) {
            self.annotate(node, expected_typ);
            Ok(())
        } else {
            Err(())
        }
    }

    fn check_if<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let mut err = false;
        let children = node.children();
        err |= self
            .check(builder, context.clone(), &children[0], &Type::Bit)
            .is_err();
        err |= self
            .check(builder, context.clone(), &children[1], expected_typ)
            .is_err();
        let mut i = 2;
        while i + 1 < children.len() - 1 {
            err |= self
                .check(builder, context.clone(), &children[i], &Type::Bit)
                .is_err();
            err |= self
                .check(builder, context.clone(), &children[i + 1], expected_typ)
                .is_err();
            i += 2;
        }

        // TODO this all needs helpers in AstNode
        err |= self
            .check(
                builder,
                context,
                &children[children.len() - 1],
                expected_typ,
            )
            .is_err();

        if !err {
            self.annotate(node, expected_typ);
            Ok(())
        } else {
            Err(())
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
        if minwidth > *width {
            self.diagnostics.push(
                diagnostics::DoesntFit {
                    region: node.region(),
                    value,
                    width: *width,
                    minwidth,
                }
                .into(),
            );
            return Err(());
        }

        self.annotate(node, &Type::Word(*width));
        Ok(())
    }

    fn check_fn<'p>(
        &mut self,
        builder: &mut Builder<'_>,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
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
            b"cast" => {
                // TODO stricter type checking
                if let Some(_typ) = self.infer(builder, context, &args[0])? {
                    self.tags
                        .insert(node.location(), Tag::PrimitiveResolution(Primitive::Cast));
                    self.annotate(node, expected_typ);
                    Ok(())
                } else {
                    self.flag_cant_infer(&args[0]);
                    Err(())
                }
            }
            b"zext" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::Zext));
                self.check_ext(builder, context, node, expected_typ)
            }
            b"sext" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::Sext));
                self.check_ext(builder, context, node, expected_typ)
            }
            b"trunc" => {
                self.tags
                    .insert(node.location(), Tag::PrimitiveResolution(Primitive::Trunc));
                self.check_trunc(builder, context, node, expected_typ)
            }
            _ => {
                self.flag_unknown(node, format!("Unknown function: {}", BStr::new(fn_name)));
                Err(())
            }
        }
    }

    fn check_index<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        index: Width,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let subject = node.child(0);
        let Ok(Some(subject_typ)) = self.infer(builder, context, &subject) else {
            self.flag_cant_infer(node);
            return Err(());
        };

        self.annotate(&subject, &subject_typ);

        match subject_typ {
            Type::Word(width) => {
                if index >= width {
                    self.diagnostics.push(
                        diagnostics::Unknown {
                            region: node.region(),
                            message: format!("Index {index} out of bounds for Word[{width}]")
                                .into(),
                        }
                        .into(),
                    );
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

    fn check_ext<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return Err(());
        };

        let subject = node.subject().unwrap();

        // Try to infer the subject type first
        let subject_typ = if let Ok(Some(typ)) = self.infer(builder, context.clone(), &subject) {
            typ
        } else {
            self.flag_cant_infer(node);
            return Err(());
        };

        let subject_width = match subject_typ {
            Type::Bit | Type::Clock | Type::Reset => 1,
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

    fn check_trunc<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let Type::Word(target_width) = expected_typ else {
            self.flag_not_word_type(node, expected_typ);
            return Err(());
        };

        let subject = node.subject().unwrap();

        // Try to infer the subject type first
        let subject_typ = if let Ok(Some(typ)) = self.infer(builder, context.clone(), &subject) {
            typ
        } else {
            self.flag_cant_infer(node);
            return Err(());
        };

        let subject_width = match subject_typ {
            Type::Bit | Type::Clock | Type::Reset => 1,
            Type::Word(width) => width,
            other => {
                self.flag_not_word_type(&subject, &other);
                return Err(());
            }
        };

        if *target_width > subject_width {
            self.diagnostics.push(diagnostics::CantTruncate {
                region: node.region(),
                source_width: subject_width,
                target_width: *target_width,
            }.into());
            return Err(());
        }

        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_dontcare<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_hole<'p>(&mut self, node: &AstNode<'p>, expected_typ: &Type) -> Result<(), ()> {
        self.annotate(node, &expected_typ);
        self.diagnostics.push(
            diagnostics::UnfilledHole {
                region: node.region(),
                name: None,
                typ: Some(format!("{expected_typ:?}").into()),
            }
            .into(),
        );
        Ok(())
    }

    fn check_match<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let subject = node.subject().unwrap();
        let Some(subject_typ) = self.infer(builder, context.clone(), &subject)? else {
            self.flag_cant_infer(&subject);
            return Err(());
        };

        let children = node.children();
        let mut idx = 1;
        while idx < children.len() {
            let child = &children[idx];
            if child.is_pat() {
                let pat = child;
                let expr = &children[idx + 1];
                let arm_context = self.check_pat(builder, context.clone(), pat, &subject_typ)?;
                self.check(builder, arm_context, expr, expected_typ)?;
                idx += 2;
            } else {
                let expr = child;
                self.check(builder, context.clone(), expr, expected_typ)?;
                idx += 1;
            }
        }

        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_field<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
        field: payload::ExprField,
    ) -> Result<(), ()> {
        let subject = node.subject().unwrap();
        let Some(subject_typ) = self.infer(builder, context.clone(), &subject)? else {
            self.flag_cant_infer(&subject);
            return Err(());
        };

        let Type::Usual(typedef_id) = subject_typ else {
            // TODO check that it's a struct type
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let struct_fields = builder.get_struct_fields(typedef_id);
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

        if field_typ == expected_typ {
            self.annotate(node, &expected_typ);
        } else {
            self.flag_unknown(node, format!("Unknown field {field_name}"));
            self.flag_wrong_type(node, expected_typ, field_typ);
        }

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
            AstNodePayload::PatCtor(pat_ident) => {
                let ctor_name = node.parsing().string(pat_ident.name);
                let Type::Usual(typedef_id) = expected_typ else {
                    self.diagnostics.push(diagnostics::UnresolvedCtor {
                        region: node.region(),
                        ctor: ctor_name.to_owned(),
                    }.into());
                    return Err(());
                };
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
                for (child, (_param_name, param_typ)) in children.iter().zip(sig.parameters.iter())
                {
                    self.annotate(child, param_typ);
                    if let Some(var_name_interned) = child.path() {
                        let var_name = child.parsing().string(var_name_interned).to_owned();
                        arm_context =
                            arm_context.push_local(var_name, child.location(), param_typ.clone());
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
            AstNodePayload::PatWordLit(_pat_word_lit) => {
                let Type::Word(width) = expected_typ else {
                    self.flag_not_word_type(node, expected_typ);
                    return Err(());
                };

                let (value, explicit_width) =
                    parse_word_literal(node.spelling().to_str_lossy().as_ref());
                if let Some(explicit_width) = explicit_width {
                    let actual = Type::Word(explicit_width);
                    if actual != *expected_typ {
                        self.flag_wrong_type(node, expected_typ, &actual);
                        return Err(());
                    }
                }

                let minwidth = min_word_width(value);
                if minwidth > *width {
                    self.diagnostics.push(
                        diagnostics::DoesntFit {
                            region: node.region(),
                            value,
                            width: *width,
                            minwidth,
                        }
                        .into(),
                    );
                    return Err(());
                }

                self.annotate(node, expected_typ);
                Ok(context)
            }
            AstNodePayload::PatBitLit(_pat_bit_lit) => {
                if !matches!(expected_typ, Type::Bit | Type::Reset) {
                    self.flag_wrong_type(node, expected_typ, &Type::Bit);
                    return Err(());
                }
                self.annotate(node, expected_typ);
                Ok(context)
            }
            _ => unreachable!("Expected a pattern node, found: {}", node.summary()),
        }
    }

    fn check_ctor<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        // TODO check that it's an ctor type
        let AstNodePayload::ExprCtor(expr_ctor) = node.payload() else {
            unreachable!()
        };
        let Type::Usual(typedef_id) = expected_typ else {
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let ctor_name = node.parsing().string(expr_ctor.ctor);
        let symboltable = builder.get_symboltable();
        let Some(ctor_symbol) = symboltable.slot(*typedef_id, ctor_name) else {
            self.flag_unknown(
                node,
                format!("Unknown ctor name: {ctor_name} in union type {expected_typ:?}"),
            );
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

        self.tags
            .insert(node.location(), Tag::SymbolResolution(ctor_symbol.id()));
        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_enumerant<'p>(
        &mut self,
        builder: &mut Builder,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let AstNodePayload::ExprEnumerant(expr_enumerant) = node.payload() else {
            unreachable!()
        };
        let Type::Usual(typedef_id) = expected_typ else {
            // TODO check that it's an enum type
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let enumerant_name = node.parsing().string(expr_enumerant.enumerant);
        let symboltable = builder.get_symboltable();
        let Some(enumerant_symbol) = symboltable.slot(*typedef_id, enumerant_name) else {
            self.flag_unknown(
                node,
                format!("Unknown enumerant name: {enumerant_name} in enum type {expected_typ:?}"),
            );
            return Err(());
        };

        self.tags.insert(
            node.location(),
            Tag::SymbolResolution(enumerant_symbol.id()),
        );
        self.annotate(node, &expected_typ);
        Ok(())
    }

    fn check_struct<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
        expected_typ: &Type,
    ) -> Result<(), ()> {
        let AstNodePayload::ExprStruct = node.payload() else {
            unreachable!()
        };
        let Type::Usual(typedef_id) = expected_typ else {
            // TODO check that it's a struct type
            self.flag_unknown(node, "Should be a typedef type");
            return Err(());
        };

        let symboltable = builder.get_symboltable();
        let struct_fields = builder.get_struct_fields(*typedef_id);

        // Track which fields have been assigned to check for duplicates and completeness
        use bstr::ByteSlice;
        use std::collections::HashSet;
        let mut assigned_fields: HashSet<bstr::BString> = HashSet::new();
        let mut has_errors = false;

        for child in node.children() {
            let AstNodePayload::Assign(assign) = child.payload() else {
                unreachable!()
            };
            let field_name = node.parsing().string(assign.name);
            let Some(field_symbol) = symboltable.slot(*typedef_id, field_name) else {
                self.flag_unknown(node, format!("Unknown field {field_name}"));
                has_errors = true;
                continue;
            };

            // Check for duplicate field assignments
            if assigned_fields.contains(field_name) {
                self.flag_unknown(&child, format!("Duplicate field assignment: {field_name}"));
                has_errors = true;
                continue;
            }
            assigned_fields.insert(field_name.to_owned());

            let expr = child.child(0);

            // Get the actual field type from the struct definition
            let expected_field_typ = struct_fields
                .iter()
                .find(|f| f.field_symbol_id == field_symbol.id())
                .and_then(|f| f.typ.clone());

            let Some(expected_field_typ) = expected_field_typ else {
                self.flag_unknown(
                    &child,
                    format!("Cannot determine type for field {field_name}"),
                );
                has_errors = true;
                continue;
            };

            // Check the field expression, accumulating errors
            if self
                .check(builder, context.clone(), &expr, &expected_field_typ)
                .is_err()
            {
                has_errors = true;
            }
        }

        // Check completeness: all struct fields must be assigned
        for field in &struct_fields {
            if !assigned_fields.contains(&field.name) {
                self.flag_unknown(
                    node,
                    format!("Missing field assignment: {}", field.name.to_str_lossy()),
                );
                has_errors = true;
            }
        }

        // TODO tag the fields
        //        self.tags.insert(node.location(), Tag::SymbolResolution(enumerant_symbol.id()));
        self.annotate(node, &expected_typ);

        if has_errors { Err(()) } else { Ok(()) }
    }
}

fn min_word_width(value: WordValue) -> Width {
    if value == 0 {
        0
    } else {
        u64::BITS as Width - u64::leading_zeros(value) as Width
    }
}

fn parse_word_literal(literal: &str) -> (WordValue, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

fn parse_nat_literal(literal: &str) -> WordValue {
    let literal = literal.replace('_', "");
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}
