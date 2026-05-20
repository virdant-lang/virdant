use super::*;

impl Typing {
    pub(super) fn infer<'p>(
        &mut self,
        builder: &mut Builder,
        context: TypingContext,
        node: &AstNode<'p>,
    ) -> Result<Option<Type>, ()> {
        match node.payload() {
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                let mut path_str = path.to_owned();
                let original_path = path.to_owned();

                let mut original_was_it = false;
                if path_str.starts_with(b"it.") || path_str == b"it" {
                    original_was_it = true;
                    let mut current_id = node.id();
                    let mut saw_it_block = false;
                    let mut found_parent = false;
                    while let Some(parent_id) = parsing.ast_node(current_id).parent {
                        let parent = parsing.ast_node(parent_id);
                        if matches!(parent.payload(), AstNodePayload::It) {
                            saw_it_block = true;
                        }

                        let mut resolved_name = None;
                        if let AstNodePayload::Submodule(module) = parent.payload() {
                            resolved_name = Some(parsing.string(module.name));
                        } else if let AstNodePayload::Component(component) = parent.payload() {
                            resolved_name = Some(parsing.string(component.name));
                        } else if let AstNodePayload::Socket(socket) = parent.payload() {
                            resolved_name = Some(parsing.string(socket.name));
                        }

                        if let Some(name) = resolved_name {
                            found_parent = true;
                            if !saw_it_block {
                                self.diagnostics.push(crate::diagnostics::ItNotInItBlock {
                                    region: node.region(),
                                }.into());
                            }
                            if path_str.starts_with(b"it.") {
                                let suffix = path_str[3..].to_owned();
                                path_str.clear();
                                path_str.extend_from_slice(name);
                                path_str.push(b'.');
                                path_str.extend_from_slice(&suffix);
                            } else {
                                path_str = name.to_owned();
                            }
                            break;
                        }
                        current_id = parent_id;
                    }
                    if !found_parent {
                        self.diagnostics.push(crate::diagnostics::ItNotInItBlock {
                            region: node.region(),
                        }.into());
                    }
                }

                if let Some((referent, typ)) = context.get(path_str.clone()) {
                    self.tags
                        .insert(node.location(), Tag::ReferentResolution(referent));
                    self.use_component(path_str.as_bstr(), node.location());
                    if let Some(typ) = typ {
                        self.annotate(&node, &typ);
                        Ok(Some(typ))
                    } else {
                        Err(())
                    }
                } else {
                    // Use original path (with 'it') for error message if applicable
                    let error_path = if original_was_it {
                        original_path
                    } else {
                        path_str
                    };
                    self.flag_unknown(node, format!("Unknown component {}", error_path));
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
            AstNodePayload::ExprIndex(index) => {
                Ok(self.infer_index(builder, context, &node, index)?)
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
            AstNodePayload::ExprFn => self.infer_fn(builder, context, node),
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
                    self.diagnostics.push(
                        diagnostics::CantInfer {
                            region: node.region(),
                        }
                        .into(),
                    );
                }
                Ok(Some(typ)) => {
                    total_width += match typ {
                        Type::Bit | Type::Reset => 1,
                        Type::Word(w) => w,
                        Type::Usual(symbol_id) => {
                            let typedef = builder.get_typedef(symbol_id);
                            match typedef.width {
                                Some(w) => w,
                                None => {
                                    diagnostics.push(
                                        diagnostics::Todo {
                                            region: node.region(),
                                            message: "Type without width in word(...): {typ:?}".into(),
                                        }
                                        .into(),
                                    );
                                    0
                                }
                            }
                        }
                        _ => {
                            diagnostics.push(
                                diagnostics::Todo {
                                    region: node.region(),
                                    message: "Invalid type in word(...): {typ:?}".into(),
                                }
                                .into(),
                            );
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
                if let Some(lhs_typ) = self.infer(builder, context.clone(), &lhs)? {
                    self.check(builder, context, &rhs, &lhs_typ)?;
                    self.annotate(node, &Type::Bit);
                    Ok(Some(Type::Bit))
                } else {
                    self.flag_unknown(&node, "Can't infer LHS");
                    Err(())
                }
            }
            BinOp::Add | BinOp::Sub | BinOp::And | BinOp::Or | BinOp::Xor => {
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
                    self.diagnostics.push(
                        diagnostics::Todo {
                            region,
                            message: "infer_index out of range".into(),
                        }
                        .into(),
                    );
                    Err(())
                }
            } else {
                self.diagnostics.push(
                    diagnostics::Todo {
                        region,
                        message: "infer_index subject not a Word type".into(),
                    }
                    .into(),
                );
                Err(())
            }
        } else {
            self.diagnostics.push(
                diagnostics::Todo {
                    region,
                    message: "infer_index can't infer subject".into(),
                }
                .into(),
            );
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
                    self.diagnostics.push(
                        diagnostics::Todo {
                            region,
                            message: "infer_index_range out of range".into(),
                        }
                        .into(),
                    );
                    Err(())
                }
            } else {
                self.diagnostics.push(
                    diagnostics::Todo {
                        region,
                        message: "infer_index_range subject not a Word type".into(),
                    }
                    .into(),
                );
                Err(())
            }
        } else {
            self.diagnostics.push(
                diagnostics::Todo {
                    region,
                    message: "infer_index_range can't infer subject".into(),
                }
                .into(),
            );
            Err(())
        }
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
            _ => Ok(None),
        }
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
}
