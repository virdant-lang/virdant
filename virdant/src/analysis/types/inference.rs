use bstr::ByteSlice;

use crate::common::Width;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::{self, AstNodePayload};

use super::typ::Type;
use super::typing::{min_word_width, Typing};
use crate::queries::component::node_to_typ;

impl Typing {
    pub(crate) fn infer<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, Vec<Diagnostic>> {
        match node.payload() {
            AstNodePayload::ExprParen => self.infer(&node.child(0)),
            AstNodePayload::ExprReference => {
                // TODO HACK
                let parsing = node.parsing();
                let path = parsing.string(node.path().unwrap());
                let typ = self.context.get(path.to_owned()); // TODO need to walk backwards to get it.
                if typ.is_none() {
                    Err(vec![diagnostics::UnresolvedComponent {
                        region: node.region(),
                        path: path.into(),
                    }.into()])
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
                        self.check(&subject, &typ);
                        Ok(Some(typ))
                    }
                    Err(diag) => Err(vec![diag]),
                }
            }
            AstNodePayload::ExprBinOp(expr_bin_op) => self.infer_binop(node),
            _ => Ok(None),
        }
    }

    fn infer_word<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, Vec<Diagnostic>> {
        // We can assume we have a width-less WordLit here, since it would have been inferred earlier.
        let mut total_width = 0;
        let mut diagnostics = vec![];
        for child in node.children() {
            match self.infer(&child) {
                Err(diags) => {
                    diagnostics.extend(diags);
                }
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
                    self.typs.insert(child.id(), typ);
                }
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }

        Ok(Some(Type::Word(total_width)))
    }

    fn infer_str<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, Vec<Diagnostic>> {
        // TODO
        Ok(None)
    }

    fn infer_binop<'p>(&mut self, node: &AstNode<'p>) -> Result<Option<Type>, Vec<Diagnostic>> {
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

    fn infer_index<'p>(&mut self, node: &AstNode<'p>, index: payload::ExprIndex) -> Result<Option<Type>, Vec<Diagnostic>> {
        let subject = node.subject().unwrap();
        if let Some(subject_typ) = self.infer(&subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if index.index < *width {
                    Ok(Some(Type::Bit))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn infer_index_range<'p>(
        &mut self,
        node: &AstNode<'p>,
        indexrange: payload::ExprIndexRange,
    ) -> Result<Option<Type>, Vec<Diagnostic>> {
        let subject = node.subject().unwrap();
        if let Some(subject_typ) = self.infer(&subject)? {
            self.typs.insert(subject.id(), subject_typ.clone());
            if let Type::Word(width) = &subject_typ {
                // TODO check
                if indexrange.index_lo <= indexrange.index_hi && indexrange.index_hi < *width {
                    Ok(Some(Type::Word(indexrange.index_hi - indexrange.index_lo)))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }
}

