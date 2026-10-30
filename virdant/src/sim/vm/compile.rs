//! Compiled-sim lowering: `compile_program(&Circuit) -> Program`.
//!
//! Pipeline (per the approved design):
//!
//! 1. (Stage 1–2, existing) `Circuit::new` builds the dataflow tables
//!    and lowers every driver AST to an `Expr` IR, reused verbatim.
//! 2. (Stage 3, here) each driven component's `Expr` is lowered to a
//!    flat `Ins` stream over fresh slots (one slot per node
//!    occurrence, post-order), with layout geometry pre-decoded via
//!    `TypeLayoutIndex::ctor_geometry`.
//! 3. *[Optimizer passes slot in here, the next unit inserts
//!    constant folding / branch propagation / peephole DICE between
//!    lowering and link.]*
//! 4. (Stage 4, here) link: concatenate the per-driver code ranges
//!    into `Program::code`, fill `entry`/`frames`/`driver_typs`, and
//!    validate in debug builds.
//!
//! Drivers whose `Expr` contains a construct this unit does not lower
//! (`When`/`Match` subtrees) fall back to the interpreter:
//! `Program::entry` is `None` for them and `Sim`'s per-driver
//! execution seam routes them to `Context`-based evaluation.  This
//! fallback is temporary, later units delete it as they land the
//! remaining lowering, so it lives behind exactly one seam
//! (`Sim::eval_driver`) plus the reason counts logged at `Sim::new`.
//!
//! ## Op-to-`eval_*` mapping checklist (synthesis graft 5)
//!
//! Each instruction transcribes one interpreter path exactly:
//!
//! | Ins | Interpreter source |
//! |---|---|
//! | `Load` | `Expr::eval`'s `Reference` arm (`val_value`: re-tag, keep the value's width) over a `Context::context_for_component` binding |
//! | `LoadUnresolved` | `Context::get`'s missing-referent fallback: a width-1 zero value (the once-per-driver `eprintln!` already lives in `circuit.rs::compute_resolved_referents`) |
//! | `Const` | `BitLit` (`val_bool`); `WordLit` with width ≤ 64 (`Value::from_u64`) |
//! | `WideConst` | `WordLit` with width > 64 (`from_u64`'s Big arm) |
//! | `Zero` | `Hole`/`Dontcare` (`val_zeros`) and compile-time-detected `encode_ctor` failure |
//! | `Bin` | `eval_binop`'s Small path, including its quirks: logical ops read bit 0 only, operand value widths are asserted equal (mismatch falls back) |
//! | `BinWide` | `eval_binop`'s Big path (`Eq`/`Neq` via `BitPat: PartialEq`; orderings via `ucmp`) |
//! | `Un` / `UnWide` | `eval_unop` (Small / Big) |
//! | `EncodeCtor` | `eval_ctor`/`eval_enumerant` via `encode_ctor` (narrow): tag in the low bits, payload args concatenated MSB-first above, zero padding to the type width |
//! | `EncodeCtorWide` | same, wide arm (delegates to `TypeLayoutIndex::encode_ctor` on `BitPat`s) |
//! | `Splice` | `eval_word` / `eval_struct`: MSB-first concatenation of the runtime value bits, then `zext_to(node static width)`, which truncates oversized payloads from the MSB end |
//! | `Index` / `IndexWide` / `IndexDyn` / `IndexDynWide` | `eval_index` / `eval_index_dyn` (`Value::get_bit`: false beyond the value's width) |
//! | `IndexRange` / `IndexRangeWide` | `eval_index_range` (`x[hi..lo]`, `hi` exclusive; the interpreter asserts `lo <= hi <= value width`, violations fall back so it panics as before) |
//! | `Zext` / `ZextWide` | `val_zext` (`zext`, `trunc` share it), output width is the node's *static* type width |
//! | `Sext` / `SextWide` | `eval_sext`, subject width is the subject *value*'s width; output width the node's static width |
//! | `Any` / `All` / `AnyWide` / `AllWide` | `eval_any` / `eval_all` (`is_zero` / `is_all_ones`, including the interpreter's 64-bit `is_all_ones` quirk) |
//! | `Trap` | `Method` / `Fn` / `StrLit`'s lazy `todo!()` (synthesis graft 4: a never-reached construct still simulates) |
//! | `Ret` / `RetWide` | the driver's result value, tagged exactly once with the driver's type |
//!
//! The `decode_ctor` padding caveat (judge verdict): a *valid tag*
//! whose payload padding bits are nonzero makes `decode_ctor` return
//! `None`, so today a match arm is NOT taken for such a pattern. The
//! `when`/`match` lowering (next unit) must reproduce that; see
//! `TypeLayoutIndex::ctor_geometry`'s doc comment.

use std::collections::HashMap;
use std::sync::Arc;

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::symbols::SymbolId;
use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::circuit::Circuit;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::sim::layout::{TypeId, TypeLayoutIndex};
use crate::sim::vm::isa::{
    CodeRange, Dst, FALLBACK_REASONS, FallbackReason, FrameLayout, Ins, Ip, Program, SlotN, SlotW,
    Src,
};

/// Mask with the low `width` bits set (assumes `width <= 64`).
fn mask(width: Width) -> u64 {
    if width >= 64 { u64::MAX } else { (1u64 << width) - 1 }
}

/// A compiled value: which slot holds its bits, and how many bits are
/// meaningful.  `width` is the value's *runtime* width, what the
/// interpreter's `Value::width()` would report, which passthrough
/// nodes (`as`/`cast`/`asclock`/paren) carry up from their subject.
#[derive(Clone, Copy, Debug)]
struct VVal {
    dst: Dst,
    width: Width,
}

/// Per-driver lowering context.  `fallback` is set on the first
/// unlowered construct; after that the driver is discarded
/// (`Program::entry` stays `None`) and the interpreter runs it.
struct DriverCompiler<'a> {
    layouts: &'a TypeLayoutIndex,
    elaboration: &'a Elaboration,
    /// Resolved referents for this driver, last-write-wins (the same
    /// shadowing order `Context`'s `IndexMap` inserts produce).
    referents: HashMap<Referent, SignalId>,
    code: Vec<Ins>,
    n_narrow: u16,
    n_wide: u16,
    wide_widths: Vec<Width>,
    fallback: Option<FallbackReason>,
}

impl<'a> DriverCompiler<'a> {
    fn new(circuit: &'a Circuit, owner: SignalId) -> DriverCompiler<'a> {
        let mut referents = HashMap::new();
        for (referent, target) in circuit.referents(owner) {
            // IndexMap-style last insert wins.
            referents.insert(referent.clone(), *target);
        }
        DriverCompiler {
            layouts: &circuit.type_layout_index,
            elaboration: &circuit.elaboration,
            referents,
            code: Vec::new(),
            n_narrow: 0,
            n_wide: 0,
            wide_widths: Vec::new(),
            fallback: None,
        }
    }

    /// The node's static type width.
    fn static_width(&self, expr: &Expr) -> Width {
        self.layouts.type_width(expr.typ())
    }

    /// The runtime width of a resolved referent's stored value: the
    /// target component's type width (`State` stores each signal's
    /// value at its own type's width).
    fn signal_value_width(&self, signal: SignalId) -> Width {
        let typ = self.elaboration.component(signal).typ();
        self.layouts.type_width(&typ)
    }

    /// Record an unlowered construct and return a dummy value handle
    /// (never executed: the whole driver falls back).
    fn fallback(&mut self, reason: FallbackReason) -> VVal {
        if self.fallback.is_none() {
            self.fallback = Some(reason);
        }
        VVal { dst: Dst::Narrow(0), width: 0 }
    }

    fn alloc_narrow(&mut self) -> SlotN {
        let slot = self.n_narrow;
        assert!(slot < u16::MAX, "compile: narrow slot space exhausted");
        self.n_narrow += 1;
        slot
    }

    fn alloc_wide(&mut self, width: Width) -> SlotW {
        let slot = self.n_wide;
        assert!(slot < u16::MAX, "compile: wide slot space exhausted");
        self.n_wide += 1;
        self.wide_widths.push(width);
        slot
    }

    /// Allocate a slot for a value of `width` bits.
    fn slot_for(&mut self, width: Width) -> Dst {
        if width <= 64 {
            Dst::Narrow(self.alloc_narrow())
        } else {
            Dst::Wide(self.alloc_wide(width))
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> VVal {
        // Short-circuit after the first unlowered construct: the
        // driver is discarded, so just stop emitting.
        if self.fallback.is_some() {
            return VVal { dst: Dst::Narrow(0), width: 0 };
        }

        match expr.payload() {
            ExprPayload::Reference(reference) => {
                match self.referents.get(&reference.referent) {
                    Some(&signal) => {
                        let width = self.signal_value_width(signal);
                        let dst = self.slot_for(width);
                        self.code.push(Ins::Load { signal, dst });
                        VVal { dst, width }
                    }
                    None => {
                        // Unresolved referent: `Context::get`'s
                        // fallback is a width-1 zero value.
                        let dst = Dst::Narrow(self.alloc_narrow());
                        self.code.push(Ins::LoadUnresolved {
                            dst: match dst {
                                Dst::Narrow(d) => d,
                                Dst::Wide(_) => unreachable!("unresolved referent value is 1 bit"),
                            },
                        });
                        VVal { dst, width: 1 }
                    }
                }
            }
            // Passthrough nodes (`eval_passthrough`/`Paren`): the
            // value's bits and width pass through; only the type tag
            // changes, which the frame does not carry.
            ExprPayload::Paren(paren) => self.compile_expr(&paren.subject),
            ExprPayload::As(as_) => self.compile_expr(&as_.subject),
            ExprPayload::Cast(cast) => self.compile_expr(&cast.subject),
            ExprPayload::AsClock(asclock) => self.compile_expr(&asclock.subject),
            ExprPayload::When(_) => self.fallback(FallbackReason::When),
            ExprPayload::Match(_) => self.fallback(FallbackReason::Match),
            ExprPayload::BitLit(bit_lit) => {
                let dst = Dst::Narrow(self.alloc_narrow());
                self.code.push(Ins::Const {
                        bits: bit_lit.value as u64,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("BitLit value is 1 bit"),
                        },
                    });
                VVal { dst, width: 1 }
            }
            ExprPayload::WordLit(word_lit) => {
                let width = word_lit.width;
                if width <= 64 {
                    let dst = Dst::Narrow(self.alloc_narrow());
                    self.code.push(Ins::Const {
                        bits: word_lit.value & mask(width),
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("narrow WordLit checked above"),
                        },
                    });
                    VVal { dst, width }
                } else {
                    let slot = self.alloc_wide(width);
                    self.code.push(Ins::WideConst {
                        bits: BitPat::from_u64(width, word_lit.value),
                        dst: slot,
                    });
                    VVal { dst: Dst::Wide(slot), width }
                }
            }
            // `Method`/`Fn`/`StrLit`: synthesis graft 4, `Trap`
            // preserves today's lazy `todo!()` failure timing (the
            // interpreter panics when it reaches the node, not
            // before).  The args are *not* compiled: the interpreter
            // never evaluates them.
            ExprPayload::StrLit(_) | ExprPayload::Method(_) | ExprPayload::Fn(_) => {
                self.code.push(Ins::Trap);
                VVal { dst: Dst::Narrow(0), width: 0 }
            }
            ExprPayload::BinOp(binop) => self.compile_binop(binop.op, &binop.lhs, &binop.rhs),
            ExprPayload::UnOp(unop) => self.compile_unop(unop.op, &unop.subject),
            ExprPayload::Ctor(ctor) => {
                self.compile_ctor(expr.typ_id(), ctor.symbol_id, &ctor.args)
            }
            ExprPayload::Enumerant(enumerant) => {
                self.compile_ctor(expr.typ_id(), enumerant.symbol_id, &[])
            }
            ExprPayload::Struct(struct_) => {
                let fields: Vec<VVal> =
                    struct_.fields.iter().map(|(_, field)| self.compile_expr(field)).collect();
                self.compile_splice(self.static_width(expr), fields)
            }
            ExprPayload::Index(index) => {
                let subj = self.compile_expr(&index.subject);
                let dst = Dst::Narrow(self.alloc_narrow());
                match subj.dst {
                    Dst::Narrow(s) => self.code.push(Ins::Index {
                        subj: s,
                        index: index.index,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("index result is 1 bit"),
                        },
                    }),
                    Dst::Wide(s) => self.code.push(Ins::IndexWide {
                        subj: s,
                        index: index.index,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("index result is 1 bit"),
                        },
                    }),
                }
                VVal { dst, width: 1 }
            }
            ExprPayload::IndexDyn(index_dyn) => {
                let subj = self.compile_expr(&index_dyn.subject);
                let index = self.compile_expr(&index_dyn.index);
                if index.width > 64 {
                    // The interpreter reads `idx_val.to_u64()`, only
                    // its low 64 bits.  A wide index slot cannot be
                    // read into the narrow index operand without
                    // another instruction; fall back.
                    return self.fallback(FallbackReason::Unsupported);
                }
                let dst = Dst::Narrow(self.alloc_narrow());
let index_slot = match index.dst {
                    Dst::Narrow(i) => i,
                    Dst::Wide(_) => unreachable!("index width checked above"),
                };
                match subj.dst {
                    Dst::Narrow(s) => self.code.push(Ins::IndexDyn {
                        subj: s,
                        index: index_slot,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("index result is 1 bit"),
                        },
                    }),
                    Dst::Wide(s) => self.code.push(Ins::IndexDynWide {
                        subj: s,
                        index: index_slot,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("index result is 1 bit"),
                        },
                    }),
                }
                VVal { dst, width: 1 }
            }
            ExprPayload::IndexRange(index_range) => {
                let subj = self.compile_expr(&index_range.subject);
                let lo = index_range.index_lo;
                let hi = index_range.index_hi;
                if lo > hi || hi > subj.width {
                    // The interpreter asserts these (`Value::slice`);
                    // fall back so it panics exactly as before.
                    return self.fallback(FallbackReason::Unsupported);
                }
                let dst = self.slot_for(hi - lo);
                match (subj.dst, dst) {
                    (Dst::Narrow(s), Dst::Narrow(d)) => self.code.push(Ins::IndexRange {
                        subj: s,
                        lo,
                        hi,
                        dst: d,
                    }),
                    (Dst::Wide(s), d) => self.code.push(Ins::IndexRangeWide {
                        subj: s,
                        lo,
                        hi,
                        dst: d,
                    }),
                    _ => unreachable!("subject width bounds the slice width"),
                }
                VVal { dst, width: hi - lo }
            }
            ExprPayload::Word(word) => {
                let args: Vec<VVal> = word.args.iter().map(|a| self.compile_expr(a)).collect();
                self.compile_splice(self.static_width(expr), args)
            }
            ExprPayload::Zext(zext) => self.compile_zext(expr, &zext.subject),
            // `eval_trunc` is `val_zext`, same lowering.
            ExprPayload::Trunc(trunc) => self.compile_zext(expr, &trunc.subject),
            ExprPayload::Sext(sext) => {
                let subj = self.compile_expr(&sext.subject);
                let out_width = self.static_width(expr);
                let dst = self.slot_for(out_width);
                match (subj.dst, dst) {
                    (Dst::Narrow(s), d) => self.code.push(Ins::Sext {
                        subj: s,
                        subj_width: subj.width,
                        out_width,
                        dst: d,
                    }),
                    (Dst::Wide(s), d) => self.code.push(Ins::SextWide {
                        subj: s,
                        out_width,
                        dst: d,
                    }),
                }
                VVal { dst, width: out_width }
            }
            ExprPayload::Any(any) => {
                let subj = self.compile_expr(&any.subject);
                let dst = Dst::Narrow(self.alloc_narrow());
                match subj.dst {
                    Dst::Narrow(s) => self.code.push(Ins::Any {
                        subj: s,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("Any result is 1 bit"),
                        },
                    }),
                    Dst::Wide(s) => self.code.push(Ins::AnyWide {
                        subj: s,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("Any result is 1 bit"),
                        },
                    }),
                }
                VVal { dst, width: 1 }
            }
            ExprPayload::All(all) => {
                let subj = self.compile_expr(&all.subject);
                let dst = Dst::Narrow(self.alloc_narrow());
                match subj.dst {
                    Dst::Narrow(s) => self.code.push(Ins::All {
                        subj: s,
                        width: subj.width,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("All result is 1 bit"),
                        },
                    }),
                    Dst::Wide(s) => self.code.push(Ins::AllWide {
                        subj: s,
                        dst: match dst {
                            Dst::Narrow(d) => d,
                            Dst::Wide(_) => unreachable!("All result is 1 bit"),
                        },
                    }),
                }
                VVal { dst, width: 1 }
            }
            ExprPayload::Hole(_) | ExprPayload::Dontcare(_) => {
                // `val_zeros(node static width)`.
                let width = self.static_width(expr);
                let dst = self.slot_for(width);
                self.code.push(Ins::Zero { dst, width });
                VVal { dst, width }
            }
        }
    }

    fn compile_binop(&mut self, op: BinOp, lhs_expr: &Expr, rhs_expr: &Expr) -> VVal {
        use crate::common::BinOp::*;
        let lhs = self.compile_expr(lhs_expr);
        let rhs = self.compile_expr(rhs_expr);
        if lhs.width != rhs.width {
            // The interpreter asserts operand-width equality on every
            // binop path; fall back so it panics exactly as before.
            return self.fallback(FallbackReason::Unsupported);
        }
        let operand_width = lhs.width;
        let comparison =
            matches!(op, Lt | Lte | Gt | Gte | Eq | Neq | LogicalAnd | LogicalOr | LogicalXor);
        let result_width = if comparison { 1 } else { operand_width };
        let dst = self.slot_for(result_width);
        match (lhs.dst, rhs.dst) {
            (Dst::Narrow(l), Dst::Narrow(r)) => {
                self.code.push(Ins::Bin {
                    op,
                    width: operand_width,
                    lhs: l,
                    rhs: r,
                    dst: match dst {
                        Dst::Narrow(d) => d,
                        Dst::Wide(_) => unreachable!("comparison result is 1 bit"),
                    },
                });
            }
            (Dst::Wide(l), Dst::Wide(r)) => {
                self.code.push(Ins::BinWide { op, lhs: l, rhs: r, dst });
            }
            // Operand widths are equal, so the slot classes must
            // agree; a mixed pair means the width check was bypassed.
            _ => return self.fallback(FallbackReason::Unsupported),
        }
        VVal { dst, width: result_width }
    }

    fn compile_unop(&mut self, op: UnOp, subject: &Expr) -> VVal {
        let subj = self.compile_expr(subject);
        let result_width = match op {
            // `Not` produces a `Bit`.
            UnOp::Not => 1,
            // Neg/Inv keep the subject's value width.
            UnOp::Neg | UnOp::Inv => subj.width,
        };
        let dst = self.slot_for(result_width);
        match (subj.dst, dst) {
            (Dst::Narrow(s), d) => self.code.push(Ins::Un {
                op,
                width: subj.width,
                subj: s,
                dst: match d {
                    Dst::Narrow(d) => d,
                    Dst::Wide(_) => unreachable!("narrow subj width <= 64"),
                },
            }),
            (Dst::Wide(s), d) => self.code.push(Ins::UnWide { op, subj: s, dst: d }),
        }
        VVal { dst, width: result_width }
    }

    /// Lower `Ctor`/`Enumerant`: transcribe `encode_ctor`, the tag
    /// in the low bits, the argument values (their *runtime* widths)
    /// concatenated MSB-first above, zero padding to the type width.
    fn compile_ctor(&mut self, typ: TypeId, symbol: SymbolId, args: &[Arc<Expr>]) -> VVal {
        let arg_vals: Vec<VVal> = args.iter().map(|a| self.compile_expr(a)).collect();
        let Some(geom) = self.layouts.ctor_geometry(typ, symbol) else {
            // `encode_ctor` returns None (symbol is not a constructor
            // of this type): the interpreter yields zeros of the
            // node's static width.
            let width = geom_total_width(self.layouts, typ);
            let dst = self.slot_for(width);
            self.code.push(Ins::Zero { dst, width });
            return VVal { dst, width };
        };
        if geom.tag_width > 64 {
            // `encode_ctor`'s tag arithmetic is a `u64`; a wider tag
            // area panics there.  Fall back so it panics as before.
            return self.fallback(FallbackReason::Unsupported);
        }
        let payload_width: u64 = arg_vals.iter().map(|v| v.width as u64).sum();
        if payload_width + geom.tag_width as u64 > geom.total_width as u64 {
            // `encode_ctor` asserts on `set_bit` out of range; fall
            // back so the interpreter panics exactly as before.
            return self.fallback(FallbackReason::Unsupported);
        }
        // Argument k sits at bit offset tag_width + sum_{j>k} w_j
        // (first argument at the MSB end of the payload area).
        let mut offsets = Vec::with_capacity(arg_vals.len());
        let mut hi = payload_width as u64;
        for v in &arg_vals {
            offsets.push((geom.tag_width as u64 + hi - v.width as u64) as u16);
            hi -= v.width as u64;
        }

        if geom.total_width <= 64 {
            let dst = self.alloc_narrow();
            let parts: Vec<(SlotN, u16)> = arg_vals
                .iter()
                .zip(offsets.iter())
                .map(|(v, lo)| match v.dst {
                    Dst::Narrow(slot) => (slot, *lo),
                    Dst::Wide(_) => unreachable!("arg width <= payload <= total <= 64"),
                })
                .collect();
            self.code.push(Ins::EncodeCtor {
                total_width: geom.total_width,
                tag: geom.tag,
                tag_lo: 0,
                parts: parts.into_boxed_slice(),
                dst,
            });
            VVal { dst: Dst::Narrow(dst), width: geom.total_width }
        } else {
            let dst = self.alloc_wide(geom.total_width);
            let enc_args: Vec<(Src, Width)> = arg_vals
                .iter()
                .zip(offsets.iter())
                .map(|(v, _)| (src_of(v.dst), v.width))
                .collect();
            self.code.push(Ins::EncodeCtorWide {
                typ,
                symbol,
                total_width: geom.total_width,
                args: enc_args.into_boxed_slice(),
                dst,
            });
            VVal { dst: Dst::Wide(dst), width: geom.total_width }
        }
    }

    /// Lower `Word`/`Struct`: transcribe `eval_word`/`eval_struct` —
    /// the runtime value bits concatenated MSB-first, then
    /// `zext_to(node static width)` (zero-fill, truncating from the
    /// MSB end when the payload overflows).
    fn compile_splice(&mut self, total_width: Width, parts: Vec<VVal>) -> VVal {
        let payload_width: u64 = parts.iter().map(|v| v.width as u64).sum();
        let dst = self.slot_for(total_width);
        // Field k sits at bit offset sum_{j>k} w_j (MSB-first).
        let mut offsets = Vec::with_capacity(parts.len());
        let mut hi = payload_width;
        for v in &parts {
            offsets.push((hi - v.width as u64) as u16);
            hi -= v.width as u64;
        }
        let wide_parts = parts.iter().any(|v| matches!(v.dst, Dst::Wide(_)));
        match dst {
            Dst::Narrow(d) => {
                if wide_parts {
                    // A wide value spliced into a narrow result: the
                    // interpreter truncates the whole concat to
                    // `total_width` via `zext_to`; the splice
                    // instruction reads wide slots only via their low
                    // 64 bits.  Fall back (pathological casts only).
                    return self.fallback(FallbackReason::Unsupported);
                }
                let spliced: Vec<(Src, u16, u16)> = parts
                    .iter()
                    .zip(offsets.iter())
                    .map(|(v, lo)| (src_of(v.dst), *lo, v.width))
                    .collect();
                self.code.push(Ins::Splice {
                    total_width,
                    parts: spliced.into_boxed_slice(),
                    dst: Dst::Narrow(d),
                });
            }
            Dst::Wide(d) => {
                let spliced: Vec<(Src, u16, u16)> = parts
                    .iter()
                    .zip(offsets.iter())
                    .map(|(v, lo)| (src_of(v.dst), *lo, v.width))
                    .collect();
                self.code.push(Ins::Splice {
                    total_width,
                    parts: spliced.into_boxed_slice(),
                    dst: Dst::Wide(d),
                });
            }
        }
        VVal { dst, width: total_width }
    }

    /// `zext`/`trunc`: both keep the low bits and (re)tag to the
    /// node's static width (`eval.rs::val_zext`).
    fn compile_zext(&mut self, expr: &Expr, subject: &Expr) -> VVal {
        let subj = self.compile_expr(subject);
        let out_width = self.static_width(expr);
        let dst = self.slot_for(out_width);
        match (subj.dst, dst) {
            (Dst::Narrow(s), d) => self.code.push(Ins::Zext {
                subj: s,
                out_width,
                dst: d,
            }),
            (Dst::Wide(s), d) => self.code.push(Ins::ZextWide {
                subj: s,
                out_width,
                dst: d,
            }),
        }
        VVal { dst, width: out_width }
    }
}

fn geom_total_width(layouts: &TypeLayoutIndex, typ: TypeId) -> Width {
    layouts.type_width(layouts.type_of(typ))
}

fn src_of(dst: Dst) -> Src {
    match dst {
        Dst::Narrow(slot) => Src::Narrow(slot),
        Dst::Wide(slot) => Src::Wide(slot),
    }
}

/// Lower every driven component into one `Program`.
pub(in crate::sim) fn compile_program(circuit: &Circuit) -> Program {
    let n = circuit.elaboration.components().len();
    let mut code: Vec<Ins> = Vec::new();
    let mut entry: Vec<Option<CodeRange>> = vec![None; n];
    let mut frames: Vec<FrameLayout> = vec![FrameLayout::default(); n];
    let mut driver_typs: Vec<Option<TypeId>> = vec![None; n];
    let mut driver_widths: Vec<Width> = vec![0; n];
    let mut fallback_counts = [0usize; FALLBACK_REASONS];

    for comp in circuit.elaboration.components() {
        let id = comp.id();
        let Some(expr) = circuit.exprs[id.index()].clone() else {
            continue;
        };
        let mut compiler = DriverCompiler::new(circuit, id);
        let root = compiler.compile_expr(&expr);
        if let Some(reason) = compiler.fallback {
            fallback_counts[reason as usize] += 1;
            continue;
        }
        match root.dst {
            Dst::Narrow(src) => compiler.code.push(Ins::Ret { src }),
            Dst::Wide(src) => compiler.code.push(Ins::RetWide { src }),
        }
        let start = code.len() as Ip;
        code.extend(compiler.code);
        entry[id.index()] = Some(CodeRange { start, len: code.len() as Ip - start });
        frames[id.index()] = FrameLayout {
            n_narrow: compiler.n_narrow,
            n_wide: compiler.n_wide,
            wide_widths: compiler.wide_widths.into_boxed_slice(),
        };
        driver_typs[id.index()] = Some(expr.typ_id());
        driver_widths[id.index()] = root.width;
    }

    let program = Program {
        code: code.into_boxed_slice(),
        entry,
        frames,
        driver_typs,
        driver_widths,
        layouts: circuit.type_layout_index.clone(),
        fallback_counts,
    };
    if cfg!(debug_assertions) {
        program.validate();
    }
    program
}

#[cfg(test)]
mod tests {
    use super::*;
    use bstr::ByteSlice;
    use crate::sim::eval::{Context, Value};
    use crate::sim::state::State;
    use crate::sim::vm::frame::Vm;
    use crate::sim::sim::InitStrategy;
    use crate::util::{check_db, db_from_dir_with_lib};
    use std::path::Path;

    /// Differential check on a real design: compile every driver,
    /// settle the whole network through the executor (the same
    /// fixpoint `Sim::flow_all` runs), then assert every VM-driven
    /// value equals what the interpreter would compute on the settled
    /// state, i.e. the VM and the interpreter agree exactly on every
    /// driver this unit lowered.
    fn differential_check(project: &str) {
        let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
        let source = manifest.join(format!("../tests/sim/{project}/src"));
        let lib = manifest.join("../lib");
        let db = db_from_dir_with_lib(source, lib);
        if let Err(diagnostics) = check_db(&db) {
            for diag in diagnostics.iter() {
                eprintln!("{diag:?}");
            }
            panic!("differential_check({project}): project failed to build");
        }
        // The projects under test each have exactly one top-level
        // `ModDef` (its fqn is `<file stem>::<mod name>`); resolve it
        // the way the trace harness resolves `top::Top`.
        let symboltable = db.get_symboltable();
        let Some(top) = symboltable
            .items()
            .into_iter()
            .filter(|item| item.kind() == crate::analysis::symbols::SymbolKind::ModDef)
            .find(|item| {
                let fqn = item.fqn();
                fqn.rsplit_str(b"::").count() == 2
                    && fqn != item.name()
            })
        else {
            panic!("differential_check({project}): no top-level ModDef");
        };

        let circuit = Circuit::new(&db, top.id());
        let program = compile_program(&circuit);
        if cfg!(debug_assertions) {
            program.validate();
        }
        let mut vm = Vm::new(&program);
        let mut state = State::new(
            circuit.elaboration.as_ref(),
            &circuit.type_layout_index,
            &InitStrategy::Seeded(42),
        );

        // Settle to a fixpoint through the executor, mirroring
        // `Sim::flow_all` (drivers, then alias copies, until no
        // change).
        loop {
            let mut changed = false;
            for comp in circuit.elaboration.components() {
                let id = comp.id();
                if circuit.exprs[id.index()].is_none() {
                    continue;
                }
                let new_value = if program.has_driver(id) {
                    vm.run(&program, &state, id)
                } else {
                    let expr = circuit.exprs[id.index()].clone().unwrap();
                    let context = Context::context_for_component(&circuit, &state, id);
                    expr.eval(&context)
                };
                if circuit.registers[id.index()] {
                    state.set_reg(id, new_value);
                } else if state.get(id) != new_value {
                    state.set_val(id, new_value);
                    changed = true;
                }
            }
            for comp in circuit.elaboration.components() {
                if let Some(source) = circuit.aliases[comp.id().index()] {
                    let value = state.get(source);
                    if state.get(comp.id()) != value {
                        state.set_val(comp.id(), value);
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
        state.clear_dirty();

        // Differential: for every VM driver, the interpreter must
        // produce the same value on the settled state.
        let mut checked = 0usize;
        for comp in circuit.elaboration.components() {
            let id = comp.id();
            let Some(expr) = circuit.exprs[id.index()].clone() else {
                continue;
            };
            if !program.has_driver(id) {
                continue;
            }
            let context = Context::context_for_component(&circuit, &state, id);
            let expected: Value = expr.eval(&context);
            let actual = state.get(id);
            assert_eq!(
                actual,
                expected,
                "differential_check({}): driver {:?} differs",
                project,
                comp.path()
            );
            checked += 1;
        }
        // Projects whose drivers all fall back (When/Match) simply
        // check nothing here, the fallback counts report covers
        // them; value equivalence is claimed only for lowered
        // drivers.
        if checked == 0 {
            eprintln!("differential_check({project}): no compiled drivers (all fell back)");
        }
    }

    #[test]
    fn differential_basics() {
        differential_check("basics");
    }

    #[test]
    fn differential_gcd() {
        differential_check("gcd");
    }

    #[test]
    fn differential_valid() {
        differential_check("valid");
    }

    #[test]
    fn differential_lfsr() {
        differential_check("lfsr");
    }

    #[test]
    fn differential_literals() {
        differential_check("literals");
    }

    #[test]
    fn differential_typedefs() {
        differential_check("typedefs");
    }

    #[test]
    fn differential_when_guard() {
        differential_check("when_guard");
    }

    #[test]
    fn differential_fifo() {
        differential_check("fifo");
    }

    #[test]
    fn differential_edge() {
        differential_check("edge");
    }

    #[test]
    fn differential_drivers() {
        differential_check("drivers");
    }

    #[test]
    fn differential_dontcare() {
        differential_check("dontcare");
    }

    #[test]
    fn differential_sockets() {
        differential_check("sockets");
    }

    #[test]
    fn differential_submod_socket() {
        differential_check("submod_socket");
    }

    #[test]
    fn differential_sync() {
        differential_check("sync");
    }

    #[test]
    fn differential_statemachine() {
        differential_check("statemachine");
    }

    #[test]
    fn differential_pipe() {
        differential_check("pipe");
    }

    #[test]
    fn differential_generics_mod() {
        differential_check("generics_mod");
    }
}
