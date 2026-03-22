use bstr::ByteSlice;

use crate::common::Width;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;

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
}

