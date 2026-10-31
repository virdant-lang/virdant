//! Compiled-sim lowering: `compile_program(&Circuit) -> Program`.
//!
//! Pipeline (per the approved design):
//!
//! 1. (Stage 1–2, existing) `Circuit::new` builds the dataflow tables
//!    and lowers every driver AST to an `Expr` IR, reused verbatim.
//! 2. (Stage 3, here) each driven component's `Expr` is lowered to a
//!    flat `Ins` stream over fresh slots (one slot per node
//!    occurrence, post-order), with layout geometry pre-decoded via
//!    `TypeLayoutIndex::ctor_geometry`.  `When` and `Match` lower to
//!    the control-flow forms (`BrTruthy`/`Jmp` chains, `TestTag`/
//!    `TestBits`/`TestBit` dispatch, `Extract` payload binding) with
//!    an owner-hold fall-through.
//! 3. *[Optimizer passes slot in here, the next unit inserts
//!    constant folding / branch propagation / peephole DICE between
//!    lowering and link.]*
//! 4. (Stage 4, here) link: concatenate the per-driver code ranges
//!    into `Program::code`, fill `entry`/`frames`/`driver_typs`, and
//!    validate in debug builds.
//!
//! Every driven component lowers to bytecode: there is no interpreter
//! fallback and no unlowered construct.  The conditions the lowering
//! checks (operand width equality, range bounds, ctor geometry) are
//! invariants the type checker already enforces; a violation panics at
//! compile time (fail fast, instead of the old interpreter's eval-time
//! panic), and the one shape the ISA could not express (a dynamic index
//! wider than 64 bits) lowers by extracting the index's low 64 bits
//! first, exactly what the interpreter's `to_u64` read.
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
//! | `Jmp` / `BrTruthy` | `eval_when`: conditions evaluated in source order, the first truthy one selects its body; `Truthiness::Bit`/`NonZero` transcribe `Expr::truthy`'s width-1/nonzero split, and a wide condition reduces through `AnyWide` (`!is_zero`) |
//! | `TestTag` / `TestTagWide` | `eval_match`'s `Ctor` arm: `decode_ctor`'s tag read, `bits.slice(0, tag_width) == tag`, one test per arm in source order (first match wins) |
//! | `Extract` / `ExtractWide` | `eval_match`'s payload binding (`Context::extend`'s per-arm bindings): bound variable `k` reads the payload range `ctor_geometry.params[k]` |
//! | `TestBits` / `TestBit` | `eval_match`'s `WordLit`/`BitLit` arms (`bits_eq` on equal widths; unequal widths never match, the arm is dropped) |
//! | `Move` / `MoveWide` | arm convergence: the interpreter returns the taken path's value directly, the shared result slot plus per-arm moves reproduce that with one static `Ret` |
//! | `Ret` / `RetWide` | the driver's result value, tagged exactly once with the driver's type |
//!
//! The `decode_ctor` padding caveat (judge verdict): a *valid tag*
//! whose payload padding bits are nonzero makes `decode_ctor` return
//! `None`, so today a match arm is NOT taken for such a pattern. The
//! `match` lowering reproduces that by extracting the padding bits
//! into a temp slot and requiring them to be zero (`TestBits` against
//! 0) before the arm's body is reachable; see `compile_match`.

use std::collections::HashMap;
use std::sync::Arc;

use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::symbols::SymbolId;
use crate::analysis::Location;
use crate::common::{BinOp, UnOp, Width};
use crate::sim::bits::BitPat;
use crate::sim::circuit::Circuit;
use crate::sim::value::Value;
use crate::sim::expr::{Expr, ExprPayload, Referent};
use crate::sim::layout::{CtorGeometry, TypeId, TypeLayoutIndex};
use crate::sim::payload;
use crate::sim::vm::isa::{
    CodeRange, Dst, FrameLayout, Ins, Ip, Program, SlotN, SlotW,
    Src, Truthiness,
};
use crate::sim::vm::optimize::optimize_driver;
use crate::sim::vm::schedule;

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

/// Per-driver lowering context.
struct DriverCompiler<'a> {
    layouts: &'a TypeLayoutIndex,
    elaboration: &'a Elaboration,
    /// Resolved referents for this driver, last-write-wins (the same
    /// shadowing order `Context`'s `IndexMap` inserts produce).
    referents: HashMap<Referent, SignalId>,
    /// In-scope match pattern bindings: the pattern variable's AST
    /// location to the slot holding its extracted payload.  The
    /// compiled replacement for `Context::extend`'s per-arm map
    /// clone: each match arm pushes its bindings while its body is
    /// compiled and pops them afterwards (nested matches stack).
    bindings: HashMap<Location, VVal>,
    code: Vec<Ins>,
    n_narrow: u16,
    n_wide: u16,
    wide_widths: Vec<Width>,
}

impl<'a> DriverCompiler<'a> {
    fn new(circuit: &'a Circuit, owner: SignalId) -> DriverCompiler<'a> {
        let mut referents = HashMap::new();
        for (referent, target) in circuit.referents(owner) {
            // IndexMap-style last insert wins.
            referents.insert(referent.clone(), *target);
        }
        DriverCompiler::with_referents(circuit, referents)
    }

    /// A compiler with an *empty* referent table: every reference
    /// lowers to `LoadUnresolved` (a width-1 zero), matching the old
    /// empty-`Context` semantics for `initial` expressions.  Used by
    /// `compile_initial_value`.
    fn new_for_constant(circuit: &'a Circuit) -> DriverCompiler<'a> {
        DriverCompiler::with_referents(circuit, HashMap::new())
    }

    fn with_referents(circuit: &'a Circuit, referents: HashMap<Referent, SignalId>) -> DriverCompiler<'a> {
        DriverCompiler {
            layouts: &circuit.type_layout_index,
            elaboration: &circuit.elaboration,
            referents,
            bindings: HashMap::new(),
            code: Vec::new(),
            n_narrow: 0,
            n_wide: 0,
            wide_widths: Vec::new(),
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
        match expr.payload() {
            ExprPayload::Reference(reference) => {
                // A pattern-bound variable (`Referent::Location`):
                // `eval_match` binds it through `Context::extend`, the
                // compiler binds it to the arm's `Extract` slot.
                if let Referent::Location(loc) = &reference.referent {
                    if let Some(bound) = self.bindings.get(loc) {
                        return *bound;
                    }
                }
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
            ExprPayload::When(when) => self.compile_when(when),
            ExprPayload::Match(match_) => self.compile_match(expr, match_),
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
                let dst = Dst::Narrow(self.alloc_narrow());
                // The interpreter reads the index through `to_u64()`:
                // only its low 64 bits.  A wide index slot is reduced
                // to those 64 bits with one `Extract`, so the executed
                // semantics are identical.
                let index_slot = match index.dst {
                    Dst::Narrow(i) => i,
                    Dst::Wide(s) => {
                        let reduced = self.alloc_narrow();
                        self.code.push(Ins::ExtractWide {
                            subj: s,
                            lo: 0,
                            width: 64,
                            dst: Dst::Narrow(reduced),
                        });
                        reduced
                    }
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
                    // Typing rejects out-of-bounds ranges
                    // (`infer_index_range`); reaching here means a
                    // typing bug.  The interpreter used to panic at
                    // eval time (`Value::slice`); panic at compile
                    // time instead.
                    panic!(
                        "compile: out-of-bounds range index {hi}..{lo} on a {width}-bit subject (typing invariant violated)",
                        width = subj.width,
                    );
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
            // Typing requires equal operand types (hence widths,
            // `compatible_typs`); the interpreter used to assert this
            // at eval time.  Reaching here means a typing bug; panic
            // at compile time instead.
            panic!(
                "compile: binop operand width mismatch ({lw} vs {rw}) (typing invariant violated)",
                lw = lhs.width,
                rw = rhs.width,
            );
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
            _ => unreachable!("equal-width binop operands share a slot class"),
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
            // area needs over 2^64 constructors of one type, which the
            // type system cannot express.  The interpreter used to
            // panic there at eval time; panic at compile time instead.
            panic!(
                "compile: constructor tag area of {w} bits exceeds u64 arithmetic (unrepresentable type)",
                w = geom.tag_width,
            );
        }
        let payload_width: u64 = arg_vals.iter().map(|v| v.width as u64).sum();
        if payload_width + geom.tag_width as u64 > geom.total_width as u64 {
            // Typing sizes ctor args to their parameters, so the
            // payload fits by construction; the interpreter used to
            // assert this at eval time (`set_bit` out of range).
            // Reaching here means a typing bug; panic at compile time.
            panic!(
                "compile: constructor payload of {pw} bits plus {tw}-bit tag overflows the {tot}-bit type (typing invariant violated)",
                pw = payload_width,
                tw = geom.tag_width,
                tot = geom.total_width,
            );
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
                    // A wide value spliced into a narrow result: typing
                    // makes the concat node at least as wide as its
                    // widest argument, so this cannot occur in typed
                    // code (the interpreter truncated via `zext_to`).
                    unreachable!("narrow splice with a wide part (typing invariant violated)");
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

    /// Lower `When`: transcribe `eval_when`.  Each condition's code
    /// sits immediately before its branch, in source order: an earlier
    /// truthy condition jumps to its body and later conditions'
    /// instructions never execute, which keeps a `Trap` inside a
    /// later condition lazy exactly as `eval_when`'s loop does.  The
    /// fall-through path is the else branch.  Driver conversion
    /// makes the owner-hold semantics an ordinary arm: an incomplete
    /// `when` clause or missing else becomes a self-reference
    /// (`Referent::Owner`), which lowers to a `Load` of the driver's
    /// own live value, so the fall-through path returns the previous
    /// value exactly as `Context::get(Owner)` did.
    ///
    /// Each path computes into its own slot and then moves into one
    /// shared result slot, the compiled shape of the interpreter
    /// returning the taken path's value directly.  All paths must
    /// produce the same runtime width (a single static `Ret` width
    /// cannot express a per-execution width); a mismatch falls back
    /// so the interpreter runs the driver as before.
    fn compile_when(&mut self, when: &payload::When) -> VVal {
        // Dispatch: one truthiness test per condition, in source
        // order.  Execution reaches condition k only when every
        // earlier condition was untruthy, which is exactly
        // `eval_when`'s loop.
        let mut branch_ins: Vec<(Ip, SlotN, Truthiness)> = Vec::new();
        for (cond, _) in &when.branches {
            let cv = self.compile_expr(cond);
            let (cond_slot, kind) = match cv.dst {
                Dst::Narrow(s) => (
                    s,
                    // `Expr::truthy`: a width-1 value tests bit 0,
                    // anything wider tests nonzero.
                    if cv.width == 1 { Truthiness::Bit } else { Truthiness::NonZero },
                ),
                Dst::Wide(s) => {
                    // A wide condition's truthiness is `!is_zero`,
                    // which is `Any`'s reduction.
                    let d = self.alloc_narrow();
                    self.code.push(Ins::AnyWide { subj: s, dst: d });
                    (d, Truthiness::Bit)
                }
            };
            self.code.push(Ins::BrTruthy { cond: cond_slot, kind, to: 0 });
            branch_ins.push(((self.code.len() - 1) as Ip, cond_slot, kind));
        }
        // No condition matched: jump to the else path.
        self.code.push(Ins::Jmp { to: 0 });
        let dispatch_jmp = (self.code.len() - 1) as Ip;

        let mut temps: Vec<VVal> = Vec::with_capacity(when.branches.len() + 1);
        let mut arm_labels: Vec<Ip> = Vec::with_capacity(when.branches.len());
        let mut move_ins: Vec<(Ip, VVal)> = Vec::new();
        let mut jmp_ins: Vec<Ip> = Vec::new();
        for (_, body) in &when.branches {
            arm_labels.push(self.code.len() as Ip);
            let t = self.compile_expr(body);
            temps.push(t);
            self.emit_convergence(&mut move_ins, &mut jmp_ins, t);
        }

        let else_label = self.code.len() as Ip;
        let t_else = self.compile_expr(&when.else_branch);
        temps.push(t_else);
        self.emit_convergence(&mut move_ins, &mut jmp_ins, t_else);

        self.finish_branching(
            &branch_ins, &arm_labels, dispatch_jmp, else_label, &move_ins, &jmp_ins, &temps,
        )
    }

    /// Emit one path's convergence onto the (yet unallocated) shared
    /// result slot: a move placeholder the finisher patches once the
    /// slot exists, then a jump placeholder to the convergence point.
    fn emit_convergence(&mut self, move_ins: &mut Vec<(Ip, VVal)>, jmp_ins: &mut Vec<Ip>, t: VVal) {
        match t.dst {
            Dst::Narrow(_) => self.code.push(Ins::Move { src: 0, dst: 0 }),
            Dst::Wide(_) => self.code.push(Ins::MoveWide { src: 0, dst: 0 }),
        }
        move_ins.push(((self.code.len() - 1) as Ip, t));
        self.code.push(Ins::Jmp { to: 0 });
        jmp_ins.push((self.code.len() - 1) as Ip);
    }

    /// The common tail of the `when`/`match` lowering: every path is
    /// compiled, so allocate the shared result slot, patch the moves,
    /// jumps, and branch targets, and enforce the shared runtime
    /// width (see `compile_when`).
    fn finish_branching(
        &mut self,
        branch_ins: &[(Ip, SlotN, Truthiness)],
        arm_labels: &[Ip],
        dispatch_jmp: Ip,
        else_label: Ip,
        move_ins: &[(Ip, VVal)],
        jmp_ins: &[Ip],
        temps: &[VVal],
    ) -> VVal {
        if temps.iter().any(|t| t.width != temps[0].width) {
            // Typing makes every `when` arm the node's type, so all
            // widths agree by construction; the interpreter used to
            // run each path at its own width.  Reaching here means a
            // typing bug; panic at compile time.
            panic!(
                "compile: `when` arms have unequal widths (typing invariant violated)"
            );
        }
        let width = temps[0].width;
        let result = self.slot_for(width);
        for (ip, t) in move_ins {
            self.code[*ip as usize] = match (t.dst, result) {
                (Dst::Narrow(src), Dst::Narrow(dst)) => Ins::Move { src, dst },
                (Dst::Wide(src), Dst::Wide(dst)) => Ins::MoveWide { src, dst },
                _ => unreachable!("equal widths share a slot class"),
            };
        }
        let end = self.code.len() as Ip;
        for ip in jmp_ins {
            self.code[*ip as usize] = Ins::Jmp { to: end };
        }
        for ((ip, cond, kind), label) in branch_ins.iter().zip(arm_labels.iter()) {
            self.code[*ip as usize] = Ins::BrTruthy { cond: *cond, kind: *kind, to: *label };
        }
        self.code[dispatch_jmp as usize] = Ins::Jmp { to: else_label };
        VVal { dst: result, width }
    }

    /// Lower `Match`: transcribe `eval_match`.  The subject is loaded
    /// once; arms are tested in source order and the first match wins
    /// (Verilog `casez`).  Each dispatched arm gets its own test
    /// region: when the constructor leaves padding bits above its
    /// payload, the region tests the padding chunks against zero
    /// first and the tag test comes last, reproducing
    /// `decode_ctor`'s padding caveat: a valid tag with nonzero
    /// padding does NOT take the arm (see `ctor_geometry`'s doc
    /// comment).  Bound pattern variables are extracted once into
    /// arm-scoped slots that shadow the referent table while the arm
    /// body compiles, replacing `Context::extend`.
    ///
    /// The fall-through path is the first `Pat::Else` arm; driver
    /// conversion always appends one (the owner-hold self-reference,
    /// lowered as a `Load` of the driver's own live value), and arms
    /// after an `Else` are unreachable (`eval_match` returns there).
    /// A match with no `Else` arm falls through to zero bits of the
    /// match node's static type, `eval_match`'s tail.
    ///
    /// Pattern notes (answering DESIGN.md's open question, checked
    /// against `tests/`): nested patterns do NOT occur.  The `Pat` IR
    /// (`sim/payload.rs`) is flat: `Ctor` (variable bindings only),
    /// `WordLit`, `BitLit`, `Else`.  `convert_pattern` cannot
    /// represent payload constraints inside a pattern, so none exist
    /// in any `Expr`; source that *looks* nested (fifo's
    /// `case @Valid(in_data) match write[2..0] {...}`) parses as a
    /// `Ctor` arm whose body is a nested `match` EXPRESSION, which
    /// this lowering handles by recursion.  A non-variable pattern
    /// argument (`@Pair(true, false)`) binds nothing and matches on
    /// the tag alone, in both engines.  So one `Extract` per bound
    /// variable is the complete binding rule.
    fn compile_match(&mut self, expr: &Expr, match_: &payload::Match) -> VVal {
        let subj = self.compile_expr(&match_.subject);

        // Which arms can ever match (the others are dropped, exactly
        // as `eval_match`'s guards skip them at runtime).  The first
        // `Pat::Else` arm, if any, ends the dispatched prefix: arms
        // after it are unreachable (`eval_match` returns there).
        let mut dispatched: Vec<(usize, &payload::Pat)> = Vec::new();
        let mut geometry: Vec<Option<CtorGeometry>> = Vec::new();
        let mut else_arm: Option<usize> = None;
        for (i, (pattern, _)) in match_.arms.iter().enumerate() {
            match pattern {
                payload::Pat::Else => {
                    else_arm = Some(i);
                    break;
                }
                payload::Pat::Ctor { symbol_id, .. } => {
                    let Some(geom) = self.layouts.ctor_geometry(match_.subject.typ_id(), *symbol_id)
                    else {
                        // `decode_ctor` can never succeed for this
                        // (symbol is not a constructor of the subject's
                        // type): the arm never matches.
                        continue;
                    };
                    if geom.tag_width > 64 {
                        // The tag read (`bits.slice(0, tag_width).
                        // to_u64()`) is a `u64` compare; a wider tag
                        // area needs over 2^64 constructors of one
                        // type, which the type system cannot express.
                        // The interpreter used to fall back; panic at
                        // compile time instead.
                        panic!(
                            "compile: constructor tag area of {w} bits exceeds u64 arithmetic (unrepresentable type)",
                            w = geom.tag_width,
                        );
                    }
                    if subj.width < geom.total_width {
                        // `decode_ctor` slices the subject's bits up to
                        // the type's total width; the subject carries
                        // the match node's type, so its value is at
                        // least that wide by construction.  Reaching
                        // here means a typing bug; panic at compile
                        // time (the interpreter panicked in
                        // `BitPat::slice` at eval time).
                        panic!(
                            "compile: match subject of {w} bits is narrower than its {tot}-bit match type (typing invariant violated)",
                            w = subj.width,
                            tot = geom.total_width,
                        );
                    }
                    dispatched.push((i, pattern));
                    geometry.push(Some(geom));
                }
                payload::Pat::WordLit { width, .. } => {
                    if subj.width == *width {
                        dispatched.push((i, pattern));
                        geometry.push(None);
                    }
                }
                payload::Pat::BitLit { .. } => {
                    if subj.width == 1 {
                        dispatched.push((i, pattern));
                        geometry.push(None);
                    }
                }
            }
        }

        // Dispatch: one test region per dispatched arm, in source
        // order.  A region branches into its arm's body only when
        // every test of the arm passes, and otherwise falls through
        // (or jumps) to the next arm's region, so the first matching
        // arm wins.  A `Ctor` arm's region checks the padding bits
        // FIRST (each chunk tested against zero, a zero chunk
        // chaining to the next chunk and then the tag test, any
        // nonzero chunk skipping the arm) and the tag test LAST:
        // entering the padding chain without a tag match would let a
        // coincidentally-zero padding take an arm whose tag did not
        // match.
        let mut region_starts: Vec<Ip> = Vec::with_capacity(dispatched.len());
        // Test instructions whose match target is their arm's body
        // label, patched once the bodies exist.
        let mut test_ins: Vec<(Ip, usize)> = Vec::new();
        // Padding-chain wiring, known as soon as the arm's region is
        // emitted: (TestBits ip, absolute target ip).
        let mut chunk_links: Vec<(Ip, Ip)> = Vec::new();
        // Arm-region skip jumps: (Jmp ip, arm index); each targets the
        // next arm's region, patched once all regions exist.
        let mut skip_jmps: Vec<(Ip, usize)> = Vec::new();
        for (k, (_, pattern)) in dispatched.iter().enumerate() {
            region_starts.push(self.code.len() as Ip);
            match pattern {
                payload::Pat::Ctor { symbol_id: _, .. } => {
                    let geom = geometry[k].clone().expect("Ctor arm carries geometry");
                    // Padding validity: the bits above the payload
                    // area must be zero for `decode_ctor` to accept
                    // the pattern (see `ctor_geometry`'s caveat:
                    // a valid tag with dirty padding does NOT take
                    // the arm).  Extracted in up-to-64-bit chunks
                    // before the tag test; every chunk's zero-test
                    // chains forward, and a nonzero chunk skips the
                    // arm.
                    let payload_width: u64 =
                        geom.params.iter().map(|(lo, hi)| (hi - lo) as u64).sum();
                    let pad_lo = geom.tag_width as u64 + payload_width;
                    let mut chunk_tests: Vec<Ip> = Vec::new();
                    let mut chunk_extracts: Vec<Ip> = Vec::new();
                    let mut lo = pad_lo;
                    while lo < geom.total_width as u64 {
                        let w = ((geom.total_width as u64 - lo).min(64)) as u16;
                        chunk_extracts.push(self.code.len() as Ip);
                        let d = self.alloc_narrow();
                        match subj.dst {
                            Dst::Narrow(s) => self.code.push(Ins::Extract {
                                subj: s,
                                lo: lo as u16,
                                width: w,
                                dst: d,
                            }),
                            Dst::Wide(s) => self.code.push(Ins::ExtractWide {
                                subj: s,
                                lo: lo as u16,
                                width: w,
                                dst: Dst::Narrow(d),
                            }),
                        }
                        chunk_tests.push(self.code.len() as Ip);
                        self.code.push(Ins::TestBits { subj: d, value: 0, next_if_match: 0 });
                        self.code.push(Ins::Jmp { to: 0 });
                        skip_jmps.push(((self.code.len() - 1) as Ip, k));
                        lo += w as u64;
                    }
                    // Tag test, last in the region: on match the arm's
                    // body runs; on mismatch execution falls through
                    // to the next arm's region.
                    let tag_test_ip = self.code.len() as Ip;
                    match subj.dst {
                        Dst::Narrow(s) => {
                            self.code.push(Ins::TestTag {
                                subj: s,
                                tag_lo: 0,
                                tag_mask: mask(geom.tag_width),
                                tag: geom.tag,
                                next_if_match: 0,
                            });
                        }
                        Dst::Wide(s) => {
                            // `decode_ctor` reads the tag as
                            // `bits.slice(0, tag_width)`: compare the
                            // tag pattern against the subject's low
                            // bits (the slot is at least `tag_width`
                            // wide, checked above).
                            self.code.push(Ins::TestTagWide {
                                subj: s,
                                tag: BitPat::from_u64(geom.tag_width, geom.tag),
                                next_if_match: 0,
                            });
                        }
                    }
                    test_ins.push((tag_test_ip, k));
                    // Chain the padding chunks: a zero chunk continues
                    // to the next chunk's extract, the last to the tag
                    // test.
                    for (j, tb_ip) in chunk_tests.iter().enumerate() {
                        let target =
                            chunk_extracts.get(j + 1).copied().unwrap_or(tag_test_ip);
                        chunk_links.push((*tb_ip, target));
                    }
                }
                payload::Pat::WordLit { width, value } => {
                    let expected = BitPat::from_u64(*width, *value);
                    match subj.dst {
                        Dst::Narrow(s) => {
                            self.code.push(Ins::TestBits {
                                subj: s,
                                value: expected.to_u64(),
                                next_if_match: 0,
                            });
                        }
                        Dst::Wide(s) => {
                            // No wide-equality test form: build the
                            // expected pattern in a temp wide slot and
                            // compare through `BinWide(Eq)`.
                            let wslot = self.alloc_wide(*width);
                            self.code.push(Ins::WideConst { bits: expected, dst: wslot });
                            let t = self.alloc_narrow();
                            self.code.push(Ins::BinWide {
                                op: crate::common::BinOp::Eq,
                                lhs: s,
                                rhs: wslot,
                                dst: Dst::Narrow(t),
                            });
                            self.code
                                .push(Ins::BrTruthy { cond: t, kind: Truthiness::Bit, to: 0 });
                        }
                    }
                    test_ins.push(((self.code.len() - 1) as Ip, k));
                }
                payload::Pat::BitLit { value } => {
                    if let Dst::Narrow(s) = subj.dst {
                        self.code.push(Ins::TestBit { subj: s, bit: *value, next_if_match: 0 });
                        test_ins.push(((self.code.len() - 1) as Ip, k));
                    }
                }
                payload::Pat::Else => unreachable!("dispatched arms exclude the else arm"),
            }
        }
        self.code.push(Ins::Jmp { to: 0 });
        let dispatch_jmp = (self.code.len() - 1) as Ip;
        // Patch the region-internal wiring now that every region
        // exists: a skip jump goes to the next arm's region, the last
        // arm's to the dispatch jump (which forwards to the else
        // path), and each chunk's zero-test to the next chunk's
        // extract.
        for (ip, k) in skip_jmps {
            let target = region_starts.get(k + 1).copied().unwrap_or(dispatch_jmp);
            self.code[ip as usize] = Ins::Jmp { to: target };
        }
        for (ip, target) in chunk_links {
            self.code[ip as usize] = match &self.code[ip as usize] {
                Ins::TestBits { subj, value, .. } => Ins::TestBits {
                    subj: *subj,
                    value: *value,
                    next_if_match: target,
                },
                other => unreachable!("padding chain link was not a TestBits: {other:?}"),
            };
        }

        // Arm bodies, each preceded by its payload extractions.
        let mut arm_labels: Vec<Ip> = Vec::with_capacity(dispatched.len());
        let mut temps: Vec<VVal> = Vec::with_capacity(dispatched.len() + 1);
        let mut move_ins: Vec<(Ip, VVal)> = Vec::new();
        let mut jmp_ins: Vec<Ip> = Vec::new();
        for (k, (orig_i, pattern)) in dispatched.iter().enumerate() {
            arm_labels.push(self.code.len() as Ip);
            let t = match pattern {
                payload::Pat::Ctor { bound_vars, .. } => {
                    let geom = geometry[k].clone().expect("Ctor arm carries geometry");
                    // Bind each pattern variable to its payload range
                    // (`eval_match` zips the bound variables with the
                    // decoded payloads and the slot's parameter types,
                    // stopping at the shortest).
                    let mut scoped: Vec<Location> = Vec::with_capacity(bound_vars.len());
                    for (var_loc, (lo, hi)) in bound_vars
                        .iter()
                        .map(|(_, loc)| loc.clone())
                        .zip(geom.params.iter().map(|(lo, hi)| (*lo, *hi)))
                    {
                        let width = hi - lo;
                        let dst = self.slot_for(width);
                        match (subj.dst, dst) {
                            (Dst::Narrow(s), Dst::Narrow(d)) => self.code.push(Ins::Extract {
                                subj: s,
                                lo,
                                width,
                                dst: d,
                            }),
                            (Dst::Narrow(_), Dst::Wide(_)) => {
                                unreachable!("payload width fits the subject's width class")
                            }
                            (Dst::Wide(s), d) => self.code.push(Ins::ExtractWide {
                                subj: s,
                                lo,
                                width,
                                dst: d,
                            }),
                        }
                        let vv = VVal { dst, width };
                        self.bindings.insert(var_loc.clone(), vv);
                        scoped.push(var_loc.clone());
                    }
                    let t = self.compile_expr(&match_.arms[*orig_i].1);
                    for loc in scoped {
                        self.bindings.remove(&loc);
                    }
                    t
                }
                _ => self.compile_expr(&match_.arms[*orig_i].1),
            };
            temps.push(t);
            self.emit_convergence(&mut move_ins, &mut jmp_ins, t);
        }

        // The fall-through path: the first `Pat::Else` arm, or (when
        // no else arm exists) `eval_match`'s tail, zero bits of the
        // match node's static type.
        let else_label = self.code.len() as Ip;
        let t_else = match else_arm {
            Some(i) => self.compile_expr(&match_.arms[i].1),
            None => {
                let width = self.static_width(expr);
                let dst = self.slot_for(width);
                self.code.push(Ins::Zero { dst, width });
                VVal { dst, width }
            }
        };
        temps.push(t_else);
        self.emit_convergence(&mut move_ins, &mut jmp_ins, t_else);

        self.finish_branching_match(&test_ins, &arm_labels, dispatch_jmp, else_label, &move_ins, &jmp_ins, &temps)
    }

    /// `finish_branching` for `match`: same tail, but the dispatch is
    /// a chain of test instructions (all targeting their arm's body
    /// label) instead of one `BrTruthy` per arm.
    fn finish_branching_match(
        &mut self,
        test_ins: &[(Ip, usize)],
        arm_labels: &[Ip],
        dispatch_jmp: Ip,
        else_label: Ip,
        move_ins: &[(Ip, VVal)],
        jmp_ins: &[Ip],
        temps: &[VVal],
    ) -> VVal {
        if temps.iter().any(|t| t.width != temps[0].width) {
            // Typing makes every match arm the match node's type, so
            // all widths agree by construction; the interpreter used
            // to run each path at its own width.  Reaching here means
            // a typing bug; panic at compile time.
            panic!(
                "compile: `match` arms have unequal widths (typing invariant violated)"
            );
        }
        let width = temps[0].width;
        let result = self.slot_for(width);
        for (ip, t) in move_ins {
            self.code[*ip as usize] = match (t.dst, result) {
                (Dst::Narrow(src), Dst::Narrow(dst)) => Ins::Move { src, dst },
                (Dst::Wide(src), Dst::Wide(dst)) => Ins::MoveWide { src, dst },
                _ => unreachable!("equal widths share a slot class"),
            };
        }
        for ip in jmp_ins {
            self.code[*ip as usize] = Ins::Jmp { to: self.code.len() as Ip };
        }
        for (ip, k) in test_ins {
            let target = arm_labels[*k];
            self.code[*ip as usize] = match &self.code[*ip as usize] {
                Ins::TestTag { subj, tag_lo, tag_mask, tag, .. } => Ins::TestTag {
                    subj: *subj,
                    tag_lo: *tag_lo,
                    tag_mask: *tag_mask,
                    tag: *tag,
                    next_if_match: target,
                },
                Ins::TestTagWide { subj, tag, .. } => Ins::TestTagWide {
                    subj: *subj,
                    tag: tag.clone(),
                    next_if_match: target,
                },
                Ins::TestBits { subj, value, .. } => Ins::TestBits {
                    subj: *subj,
                    value: *value,
                    next_if_match: target,
                },
                Ins::TestBit { subj, bit, .. } => Ins::TestBit {
                    subj: *subj,
                    bit: *bit,
                    next_if_match: target,
                },
                Ins::BrTruthy { cond, kind, .. } => Ins::BrTruthy {
                    cond: *cond,
                    kind: *kind,
                    to: target,
                },
                other => unreachable!("match dispatch test was not a test form: {other:?}"),
            };
        }
        self.code[dispatch_jmp as usize] = Ins::Jmp { to: else_label };
        VVal { dst: result, width }
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

/// Recognize a driver whose optimized stream folds entirely to a
/// constant: the shape `Const/Zero/WideConst` immediately followed by
/// the `Ret` reading exactly that slot.  The optimizer's DCE removes
/// everything between the constant and the `Ret`, so this shape is
/// the canonical "constant driver" after optimization.
fn detect_constant_driver(code: &[Ins], typ: TypeId, ret_width: Width) -> Option<Value> {
    if code.len() != 2 {
        return None;
    }
    match (&code[0], &code[1]) {
        (Ins::Const { bits, dst }, Ins::Ret { src }) if dst == src => {
            Some(Value::from_u64(typ, ret_width, *bits))
        }
        (Ins::Zero { dst, width }, Ins::Ret { src }) if dst == &Dst::Narrow(*src) => {
            Some(Value::from_u64(typ, ret_width, 0))
        }
        // A wide all-zero constant (`Ins::Zero` with a wide dst): no
        // `ZeroWide` form exists, so the pattern is `Zero` + `RetWide`.
        (Ins::Zero { width, .. }, Ins::RetWide { .. }) if *width > 64 => {
            Some(Value::new(typ, BitPat::zeros(*width)))
        }
        (Ins::WideConst { bits, dst }, Ins::RetWide { src }) if dst == src => {
            Some(Value::new(typ, bits.clone()))
        }
        _ => None,
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
    let mut const_values: Vec<Option<Value>> = vec![None; n];
    // Per-driver input-signal lists for the static schedule (stage A):
    // the signals each driver's optimized bytecode actually loads,
    // deduped.  Scanning the optimized stream (not the AST referents)
    // keeps the list exact after constant folding and dead-load
    // elimination, which is what lets the flow-time epoch check skip a
    // driver the AST sensitivity lists would still reach.
    let mut reads: Vec<Vec<u32>> = vec![Vec::new(); n];

    for comp in circuit.elaboration.components() {
        let id = comp.id();
        let Some(expr) = circuit.exprs[id.index()].clone() else {
            continue;
        };
        let mut compiler = DriverCompiler::new(circuit, id);
        let root = compiler.compile_expr(&expr);
        match root.dst {
            Dst::Narrow(src) => compiler.code.push(Ins::Ret { src }),
            Dst::Wide(src) => compiler.code.push(Ins::RetWide { src }),
        }
        // Optimizer passes: constant folding / branch propagation,
        // then peephole DCE + slot fusion + CSE, all per driver,
        // inside one `CodeRange`, never across drivers.  They rewrite
        // `Ins` sequences within the existing opcode set.
        optimize_driver(&mut compiler.code);
        // The static schedule's per-driver input list: every signal the
        // optimized bytecode loads (`Load` is the only instruction that
        // reads engine state), deduped.
        let mut loads: Vec<u32> = compiler
            .code
            .iter()
            .filter_map(|ins| match ins {
                Ins::Load { signal, .. } => Some(signal.index() as u32),
                _ => None,
            })
            .collect();
        loads.sort_unstable();
        loads.dedup();
        reads[id.index()] = loads;
        // Constant-unit skip (synthesis graft 2): a driver whose
        // program folds entirely to a constant value commits that
        // value directly without executing.
        let const_value = detect_constant_driver(
            &compiler.code,
            expr.typ_id(),
            root.width,
        );
        let start = code.len() as Ip;
        // The lowering computes jump targets against the driver's own
        // instruction vector (base 0); the linked program addresses
        // them absolutely, so shift every target by the driver's
        // start offset.
        for ins in &mut compiler.code {
            ins.rebase_targets(start);
        }
        code.extend(compiler.code);
        entry[id.index()] = Some(CodeRange { start, len: code.len() as Ip - start });
        frames[id.index()] = FrameLayout {
            n_narrow: compiler.n_narrow,
            n_wide: compiler.n_wide,
            wide_widths: compiler.wide_widths.into_boxed_slice(),
        };
        driver_typs[id.index()] = Some(expr.typ_id());
        driver_widths[id.index()] = root.width;
        const_values[id.index()] = const_value;
    }

    // Socket-connection aliases are copy drivers: the sink's "bytecode"
    // reads exactly its peer.
    for sink_idx in 0..n {
        if let Some(peer) = circuit.aliases[sink_idx] {
            reads[sink_idx] = vec![peer.index() as u32];
        }
    }

    // The static schedule (stage A): a driver node per component with a
    // driver expression or an alias peer; only combinational drivers
    // propagate (a register's commit writes only the pending
    // next-value, never a live value mid-flow, so a register is a graph
    // source, never a cycle member).  A cyclic graph leaves the
    // schedule off: `flow` keeps the dynamic worklist path.
    let mut is_driver = vec![false; n];
    let mut propagates = vec![false; n];
    for idx in 0..n {
        let driven = entry[idx].is_some() || circuit.aliases[idx].is_some();
        is_driver[idx] = driven;
        propagates[idx] = driven && !circuit.registers[idx];
    }
    let reads: Vec<Box<[u32]>> = reads.into_iter().map(|v| v.into_boxed_slice()).collect();
    let schedule = schedule::TopoSchedule::build(n, &is_driver, &propagates, &reads);
    #[cfg(feature = "sim-profile")]
    match &schedule {
        Some(_) => crate::sim::profile::bump_topo_design(),
        None => crate::sim::profile::bump_topo_fallback(),
    }
    // Debug check of the topological property against the same edge
    // definition the build consumed: every propagating input's producer
    // must sit at a strictly smaller schedule position than its reader.
    if cfg!(debug_assertions) {
        if let Some(schedule) = &schedule {
            for d in 0..n {
                for &s in &*reads[d] {
                    if propagates[s as usize] {
                        assert!(
                            schedule.pos(s as usize) < schedule.pos(d),
                            "schedule: input {s} of driver {d} is not topologically ordered"
                        );
                    }
                }
            }
        }
    }

    let program = Program {
        code: code.into_boxed_slice(),
        entry,
        frames,
        driver_typs,
        driver_widths,
        const_values,
        schedule,
        layouts: circuit.type_layout_index.clone(),
    };
    if cfg!(debug_assertions) {
        program.validate();
    }
    program
}

/// Lower a register's constant `initial` expression to its value.
///
/// `initial` expressions are constant by construction, so they compile
/// through the exact driver pipeline (lowering with an *empty*
/// referent table, i.e. the bindings an empty eval context produced,
/// then the optimizer), and the optimized stream must have folded entirely
/// to one constant (`Const`/`Zero`/`WideConst` + `Ret`).  A constant
/// expression that does not fold (e.g. it reaches a `Trap`) is an
/// optimizer/lowering gap, not a runtime fallback: panic with the
/// signal's path instead.
pub(in crate::sim) fn compile_initial_value(circuit: &Circuit, expr: &Expr) -> Value {
    let mut compiler = DriverCompiler::new_for_constant(circuit);
    let root = compiler.compile_expr(expr);
    match root.dst {
        Dst::Narrow(src) => compiler.code.push(Ins::Ret { src }),
        Dst::Wide(src) => compiler.code.push(Ins::RetWide { src }),
    }
    optimize_driver(&mut compiler.code);
    detect_constant_driver(&compiler.code, expr.typ_id(), root.width).unwrap_or_else(|| {
        panic!(
            "compile_initial_value: constant initial expression did not fold to a constant (optimizer/lowering gap at {loc:?})",
            loc = expr.location(),
        )
    })
}
