//! Cranelift JIT backend for deep-cycle replay phases (JIT v2; the
//! `jit` cargo feature).
//!
//! ## Boundary
//!
//! Evaluation only, per phase.  The JIT compiles one qualifying
//! deep-cycle replay phase (see `vm::schedule::CycleSchedule`) to a
//! single native function that evaluates ALL of the phase's drivers in
//! ONE call.  All commit logic stays in Rust (`sim.rs`); the JIT's
//! output is the same raw per-driver bits the interpreter's `Ret`
//! would produce, and the commit path never sees a difference.
//!
//! ## ABI
//!
//! ```text
//! fn(out: *mut u64) -> ()
//! ```
//!
//! * `out` — one `u64` per phase driver, in the phase's driver order:
//!   driver k's raw `Ret` result, already masked to the driver's
//!   width (the same masking `Value::from_u64` applies, so the Rust
//!   commit path compares identical bits).  The buffer is owned by
//!   [`PhaseJit`] and passed per call.
//!
//! Everything else is compiled-time baked:
//!
//! * *State reads* — `Load` reads input bits DIRECTLY from the live
//!   narrow-value store (`State::vals`): driver bytecode addresses
//!   signal s at `base + s * stride + bits_off` (see
//!   `Value::jit_layout`), with the base pointer baked into the code.
//!   No gather Vec, no per-driver indirect calls, no per-eval checks —
//!   the whole v1 per-eval ceremony is gone; this is the point of v2.
//! * *Scratch* — driver k's narrow frame slots are the interpreter's
//!   own `Frame::narrow` heap buffer; its pointer is baked into the
//!   code.  JIT and interpreter therefore share the exact same scratch
//!   memory with the exact same cross-run slot persistence, so
//!   interleaving the two paths (a phase that falls back between two
//!   replayed ones) cannot diverge.
//!
//! ## Addressing invariants (correctness-critical)
//!
//! 1. `State::vals` is allocated at exactly `n_components` in
//!    `State::new` and afterwards only written in place — it never
//!    reallocates, so the baked base pointer is valid for the whole
//!    engine's lifetime.  (A moving buffer would be silent memory
//!    corruption; this is the load-bearing stability fact.)
//! 2. `Frame::narrow` is allocated at exactly `n_narrow` in
//!    `Frame::from_layout` and afterwards only written in place — same
//!    stability, per driver.
//! 3. A component whose type width is ≤ 64 always stores the `Small`
//!    variant of `Value` (every constructor narrows that way), so its
//!    live bits sit at the fixed `Small.bits` offset
//!    (`Value::jit_layout`).  Wider components are heap-backed
//!    (`Big`): phase eligibility refuses any `Load` of one.
//!
//! All three are one-allocation-at-construction facts about the
//! engine, not about the JIT; the JIT only ever *bakes* the pointers,
//! it never triggers allocation.
//!
//! ## Soundness
//!
//! Only phases the recorder already validated as terminal (see
//! `vm::schedule::CycleSchedule` and stage C's soundness proof)
//! reach this module: for a terminal settlement no visited driver
//! reads another's committed output, so the phase's input bits are
//! frozen for the whole native call, evaluation order is
//! unobservable, and computing all drivers before any commit is
//! exactly what the interleaved interpreter computes.  The replay/
//! abort machinery is untouched: any perturbation falls through to
//! the unchanged dynamic path.
//!
//! ## Coverage / fallback
//!
//! A phase qualifies when every driver has bytecode (no alias copies,
//! not constant-folded) in the narrow instruction subset (≤ 64-bit
//! slots, no `Trap`, no `Sext` past 64) and every `Load`ed signal is
//! narrow.  A phase with any ineligible driver is not compiled; its
//! settlements keep the per-driver interpreter replay.  The split is
//! logged at phase-build time and counted in the profile counters.
//!
//! Raw-pointer work is confined to this module: the baked pointers are
//! derived from engine-owned buffers at build time; the compiled
//! functions touch nothing else and never call back into Rust.

use std::collections::HashMap;

use crate::common::{BinOp, UnOp};
use crate::sim::vm::exec::narrow_mask;
use crate::sim::vm::isa::{CodeRange, Dst, Ins, Ip, Program, Src, Truthiness};
use crate::sim::vm::schedule::Phase;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder, MemFlagsData};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};

/// The compiled phase's native signature (see the module docs).
pub(in crate::sim) type JitPhaseFn = unsafe fn(out: *mut u64);

/// The whole replay schedule's JIT state, built when the schedule
/// activates.  One entry per phase, parallel to the schedule's phase
/// list.
pub(in crate::sim) struct PhaseJit {
    /// `phases[i]`: the compiled native function for the schedule's
    /// i-th phase, or `None` when that phase stays on the interpreter.
    phases: Vec<Option<JitPhaseFn>>,
    /// Per-phase driver counts (parallel to `phases`): the length of
    /// the `out` prefix each phase's call fills.
    driver_counts: Vec<usize>,
    /// The per-call result buffer: one `u64` per phase driver, sized
    /// to the widest phase, allocated once here.  Passed to the
    /// compiled function per call and read back by the Rust commit
    /// loop (`Sim::cycle_settlement`).
    out: Vec<u64>,
    /// Never read directly, but load-bearing: the `JITModule` owns the
    /// executable memory backing the function pointers in `phases`.
    /// Dropping it (while `phases` lived) would dangle every pointer.
    #[allow(dead_code)]
    module: JITModule,
}

impl PhaseJit {
    /// Whether phase `pos` has a compiled batch function.
    pub(in crate::sim) fn is_compiled(&self, pos: usize) -> bool {
        self.phases.get(pos).map_or(false, Option::is_some)
    }

    /// Run the compiled phase: one native call evaluates every driver
    /// in the phase, reading the live state store directly.  Returns
    /// the per-driver raw result bits (masked to each driver's width),
    /// in the phase's driver order — the commit loop commits
    /// `results[k]` for the phase's k-th driver through the exact
    /// commit body the interpreter's `RunOut::Bits` arm uses.
    ///
    /// Panics only on caller-contract violation (an uncompiled phase);
    /// the compiled function itself cannot panic.
    pub(in crate::sim) fn run(&mut self, pos: usize) -> &[u64] {
        let f = self.phases[pos].expect("jit: phase not compiled");
        let n = self.driver_counts[pos];
        // The one raw-pointer site.  `out` is caller-owned (this
        // struct) and sized for every phase; the compiled function
        // writes exactly the first `n` slots and touches nothing else.
        unsafe {
            (f)(self.out.as_mut_ptr());
        }
        &self.out[..n]
    }
}

/// Build the replay schedule's JIT state (called at schedule
/// activation).  Each qualifying phase compiles to one native
/// function; the rest stay on the interpreter.
pub(in crate::sim) fn build_phases(
    program: &Program,
    phases: &[Phase],
    frames: &mut crate::sim::vm::frame::Vm,
    vals_base: *const u8,
    stride: usize,
    bits_off: usize,
    signal_narrow: &[bool],
) -> PhaseJit {
    // The narrow-store addressing constants (`stride`/`bits_off`) come
    // from the caller's `Value::jit_layout` probe — see the module
    // docs' addressing invariants.

    // Per-driver frame pointers, captured once: the compiled code
    // bakes them in (see the addressing invariants — these buffers
    // never reallocate).
    let frame_ptrs: Vec<*mut u64> = frames
        .frames
        .iter_mut()
        .map(|frame| frame.narrow.as_mut_ptr())
        .collect();

    // Phase eligibility first (a pure scan): qualifying phases get a
    // native function, the rest stay on the per-driver interpreter.
    let mut compiled: Vec<Option<FuncId>> = Vec::with_capacity(phases.len());
    let mut driver_counts: Vec<usize> = Vec::with_capacity(phases.len());
    let mut qualifying: Vec<usize> = Vec::new();
    for (p_idx, phase) in phases.iter().enumerate() {
        driver_counts.push(phase.drivers.len());
        if !phase_eligible(program, &phase.drivers, signal_narrow) {
            eprintln!("jit: phase {p_idx}: interpreter fallback");
            compiled.push(None);
            continue;
        }
        compiled.push(None);
        qualifying.push(p_idx);
    }

    let isa = native_isa();
    let frontend_config = isa.frontend_config();
    let mut module = JITModule::new(JITBuilder::with_isa(isa, default_libcall_names()));
    let mut ctx = module.make_context();
    let mut func_ctx = FunctionBuilderContext::new();

    for &p_idx in &qualifying {
        let drivers = &phases[p_idx].drivers;
        #[cfg(feature = "sim-profile")]
        crate::sim::profile::bump_jit_phase_compiled();
        let func_id = compile_phase(
            &mut module,
            &mut ctx,
            &mut func_ctx,
            program,
            drivers,
            p_idx,
            vals_base as usize,
            stride,
            bits_off,
            &frame_ptrs,
            frontend_config,
        );
        compiled[p_idx] = Some(func_id);
    }

    module.finalize_definitions().expect("jit: finalize phase functions");
    let phases_out: Vec<Option<JitPhaseFn>> = compiled
        .into_iter()
        .map(|slot| {
            slot.map(|func_id| unsafe {
                std::mem::transmute::<*const u8, JitPhaseFn>(
                    module.get_finalized_function(func_id),
                )
            })
        })
        .collect();
    module.clear_context(&mut ctx);

    let n_compiled = phases_out.iter().flatten().count();
    let out_len = driver_counts.iter().copied().max().unwrap_or(0);
    eprintln!(
        "jit: {n_compiled}/{} phases compiled, {} interpreter fallback",
        phases.len(),
        phases.len() - n_compiled,
    );
    PhaseJit {
        phases: phases_out,
        driver_counts,
        out: vec![0; out_len],
        module,
    }
}

fn native_isa() -> cranelift_codegen::isa::OwnedTargetIsa {
    let mut flags = settings::builder();
    flags.set("opt_level", "speed").expect("jit: opt_level flag");
    let isa_builder = cranelift_native::builder().expect("jit: host cpu support");
    isa_builder.finish(settings::Flags::new(flags)).expect("jit: isa construction")
}

/// A phase qualifies when every driver has bytecode (no alias copies,
/// nothing constant-folded) in the narrow subset, and every `Load`ed
/// signal is narrow (its live value is always the inline `Small`
/// variant — see `Value::jit_layout`).
fn phase_eligible(program: &Program, drivers: &[u32], signal_narrow: &[bool]) -> bool {
    drivers.iter().all(|&d| {
        let idx = d as usize;
        let Some(range) = program.entry[idx] else {
            return false;
        };
        if program.const_values[idx].is_some() {
            return false;
        }
        let body = &program.code[range.start as usize..(range.start + range.len) as usize];
        eligible(body)
            && body.iter().all(|ins| match ins {
                Ins::Load { signal, .. } => {
                    signal_narrow.get(signal.index()).copied().unwrap_or(false)
                }
                _ => true,
            })
    })
}

/// Compile one phase: all of its drivers concatenated into one native
/// function, driver k's `Ret` storing into `out[k]` and falling into
/// driver k+1's entry block.
#[allow(clippy::too_many_arguments)]
fn compile_phase(
    module: &mut JITModule,
    ctx: &mut cranelift_codegen::Context,
    func_ctx: &mut FunctionBuilderContext,
    program: &Program,
    drivers: &[u32],
    phase_idx: usize,
    vals_base: usize,
    stride: usize,
    bits_off: usize,
    frame_ptrs: &[*mut u64],
    frontend_config: cranelift_codegen::isa::TargetFrontendConfig,
) -> FuncId {
    let ptr_ty = cranelift_codegen::ir::types::I64;
    ctx.func.signature.params.clear();
    ctx.func.signature.returns.clear();
    ctx.func.signature.params.push(ir::AbiParam::new(ptr_ty));
    let func_id = module
        .declare_function(
            &format!("virdant_phase_{phase_idx}"),
            Linkage::Local,
            &ctx.func.signature,
        )
        .expect("jit: declare phase function");

    // Per-driver CFG data, computed up front so driver k's `Ret` can
    // jump to driver k+1's entry block: a Cranelift block per branch
    // target and per conditional fall-through, IPs range-relative.
    struct DriverCfg {
        range: CodeRange,
        starts: Vec<Ip>,
        blocks: Vec<ir::Block>,
        block_of: HashMap<Ip, ir::Block>,
    }
    let cfgs: Vec<DriverCfg> = drivers
        .iter()
        .map(|&d| {
            let range = program.entry[d as usize].expect("jit: driver range");
            let body_len = range.len as Ip;
            let mut starts: Vec<Ip> = vec![0];
            for (i, ins) in program.code
                [range.start as usize..(range.start + range.len) as usize]
                .iter()
                .enumerate()
            {
                let ip = i as Ip;
                for t in ins_targets(ins) {
                    if t >= range.start && t < range.start + range.len {
                        push_unique(&mut starts, t - range.start);
                    }
                }
                if is_conditional(ins) && ip + 1 < body_len {
                    push_unique(&mut starts, ip + 1);
                }
            }
            starts.sort_unstable();
            DriverCfg {
                range,
                starts,
                blocks: Vec::new(),
                block_of: HashMap::new(),
            }
        })
        .collect();

    let mut builder = FunctionBuilder::new(&mut ctx.func, func_ctx);
    let entry = builder.create_block();
    builder.switch_to_block(entry);
    let out = builder.append_block_param(entry, ptr_ty);

    // Baked pointers (see the addressing invariants in the module
    // docs): the state store's base, and each driver's frame.
    let state_base = builder.ins().iconst(ptr_ty, vals_base as i64);
    let frames: Vec<ir::Value> = drivers
        .iter()
        .map(|&d| builder.ins().iconst(ptr_ty, frame_ptrs[d as usize] as usize as i64))
        .collect();

    // Create every driver's blocks now (the `Ret` chaining needs the
    // next driver's entry block to exist before it is filled).
    let cfgs: Vec<DriverCfg> = cfgs
        .into_iter()
        .map(|mut cfg| {
            cfg.blocks = (0..cfg.starts.len()).map(|_| builder.create_block()).collect();
            cfg.block_of = cfg.starts.iter().copied().zip(cfg.blocks.iter().copied()).collect();
            cfg
        })
        .collect();
    // The entry block only sets up the baked pointers; evaluation
    // starts at driver 0's first block.
    builder.ins().jump(cfgs[0].blocks[0], &[]);

    // The memory flags for every access: `trusted` (aligned, notrap —
    // every pointer is engine-owned and in-bounds by the layout
    // invariants), plus `readonly` for the state reads.
    let ro_flags = MemFlagsData::trusted().with_readonly();
    let rw_flags = MemFlagsData::trusted();

    for (k, cfg) in cfgs.iter().enumerate() {
        let frame = frames[k];
        let chain = |builder: &mut FunctionBuilder| {
            // Driver k's terminal: fall into driver k+1's entry block,
            // or return when this was the last driver.
            match cfgs.get(k + 1) {
                Some(next) => builder.ins().jump(next.blocks[0], &[]),
                None => builder.ins().return_(&[]),
            }
        };
        let mut cache: HashMap<u16, ir::Value> = HashMap::new();
        let body =
            &program.code[cfg.range.start as usize..(cfg.range.start + cfg.range.len) as usize];
        let mut terminated = false;
        for (b_idx, &start_ip) in cfg.starts.iter().enumerate() {
            builder.switch_to_block(cfg.blocks[b_idx]);
            cache.clear();
            let mut ip = start_ip as usize;
            while ip < body.len() {
                if let Some(&next) = cfg.starts.get(b_idx + 1) {
                    if ip as Ip == next {
                        break;
                    }
                }
                match &body[ip] {
                    Ins::Load { signal, dst: Dst::Narrow(d) } => {
                        // Direct state addressing: the live narrow
                        // store, at the signal's fixed byte offset (see
                        // `Value::jit_layout` and the module docs).
                        let byte_off = (signal.index() * stride + bits_off) as i64;
                        let addr = builder.ins().iadd_imm_s(state_base, byte_off);
                        let v = builder.ins().load(ptr_ty, ro_flags, addr, 0);
                        write_slot(&mut builder, frame, *d, v, &mut cache, rw_flags);
                    }
                    Ins::LoadUnresolved { dst } => {
                        let v = builder.ins().iconst(ptr_ty, 0);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Const { bits, dst } => {
                        let v = builder.ins().iconst(ptr_ty, *bits as i64);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Zero { dst: Dst::Narrow(d), .. } => {
                        let v = builder.ins().iconst(ptr_ty, 0);
                        write_slot(&mut builder, frame, *d, v, &mut cache, rw_flags);
                    }
                    Ins::Jmp { to } => {
                        let target = cfg.block_of[&(to - cfg.range.start)];
                        builder.ins().jump(target, &[]);
                        terminated = true;
                        break;
                    }
                    Ins::BrTruthy { cond, kind, to } => {
                        let bits = read_slot(&mut builder, frame, *cond, &mut cache);
                        let cond_v = match kind {
                            // `eval.rs::eval_when`: `Bit` tests bit 0,
                            // `NonZero` tests any nonzero bit.
                            Truthiness::Bit => builder.ins().band_imm_u(bits, 1),
                            Truthiness::NonZero => bits,
                        };
                        let target = cfg.block_of[&(to - cfg.range.start)];
                        let fall = cfg.block_of[&(ip as Ip + 1)];
                        builder.ins().brif(cond_v, target, &[], fall, &[]);
                        terminated = true;
                        break;
                    }
                    Ins::TestTag { subj, tag_lo, tag_mask, tag, next_if_match } => {
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        // Transcribes `decode_ctor`'s tag read
                        // (`bits.slice(0, tag_width) == tag`).
                        let t = if (*tag_lo as usize) < 64 {
                            let shifted = builder.ins().ushr_imm_u(bits, *tag_lo as i64);
                            let masked = if *tag_mask == u64::MAX {
                                shifted
                            } else {
                                builder.ins().band_imm_u(shifted, *tag_mask as i64)
                            };
                            to_bool(&mut builder, IntCC::Equal, masked, *tag as i64)
                        } else {
                            // Tag bits above the narrow slot's width are
                            // zero (slot-width invariant).
                            builder.ins().iconst(ptr_ty, (*tag == 0) as i64)
                        };
                        let target = cfg.block_of[&(*next_if_match - cfg.range.start)];
                        let fall = cfg.block_of[&(ip as Ip + 1)];
                        builder.ins().brif(t, target, &[], fall, &[]);
                        terminated = true;
                        break;
                    }
                    Ins::TestBits { subj, value, next_if_match } => {
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let t = to_bool(&mut builder, IntCC::Equal, bits, *value as i64);
                        let target = cfg.block_of[&(*next_if_match - cfg.range.start)];
                        let fall = cfg.block_of[&(ip as Ip + 1)];
                        builder.ins().brif(t, target, &[], fall, &[]);
                        terminated = true;
                        break;
                    }
                    Ins::TestBit { subj, bit, next_if_match } => {
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let low = builder.ins().band_imm_u(bits, 1);
                        let t = to_bool(&mut builder, IntCC::Equal, low, *bit as i64);
                        let target = cfg.block_of[&(*next_if_match - cfg.range.start)];
                        let fall = cfg.block_of[&(ip as Ip + 1)];
                        builder.ins().brif(t, target, &[], fall, &[]);
                        terminated = true;
                        break;
                    }
                    Ins::Extract { subj, lo, width, dst } => {
                        // `(bits >> lo) & mask(width)`, 0 for `lo >= 64`.
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = if (*lo as usize) < 64 {
                            let shifted = builder.ins().ushr_imm_u(bits, *lo as i64);
                            apply_mask(&mut builder, shifted, narrow_mask(*width))
                        } else {
                            builder.ins().iconst(ptr_ty, 0)
                        };
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Bin { op, width, lhs, rhs, dst } => {
                        let a = read_slot(&mut builder, frame, *lhs, &mut cache);
                        let b = read_slot(&mut builder, frame, *rhs, &mut cache);
                        let v = emit_bin(&mut builder, *op, narrow_mask(*width), a, b);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Un { op, width, subj, dst } => {
                        let a = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = emit_un(&mut builder, *op, narrow_mask(*width), a);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::EncodeCtor { total_width, tag, tag_lo, parts, dst } => {
                        // Tag at `tag_lo`, each argument slot spliced at
                        // its declared bit offset, masked to total width.
                        let m = narrow_mask(*total_width);
                        let mut acc: Option<ir::Value> = None;
                        if (*tag_lo as usize) < 64 {
                            let c = builder.ins().iconst(ptr_ty, (*tag & m) as i64);
                            let shifted = shift_by(&mut builder, c, *tag_lo);
                            acc = Some(shifted);
                        }
                        for (slot, lo) in parts.iter() {
                            if (*lo as usize) < 64 {
                                let v = read_slot(&mut builder, frame, *slot, &mut cache);
                                let shifted = shift_by(&mut builder, v, *lo);
                                acc = Some(match acc {
                                    Some(a) => builder.ins().bor(a, shifted),
                                    None => shifted,
                                });
                            }
                        }
                        let base = acc.unwrap_or_else(|| builder.ins().iconst(ptr_ty, 0));
                        let v = apply_mask(&mut builder, base, m);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Splice { total_width, parts, dst } => {
                        // MSB-first concatenation with static offsets; the
                        // narrow destination only carries narrow parts.
                        let Dst::Narrow(d) = *dst else { unreachable!() };
                        let mut acc: Option<ir::Value> = None;
                        for (src, lo, _) in parts.iter() {
                            if (*lo as usize) < 64 {
                                let v = read_slot(&mut builder, frame, narrow_src(*src), &mut cache);
                                let shifted = shift_by(&mut builder, v, *lo);
                                acc = Some(match acc {
                                    Some(a) => builder.ins().bor(a, shifted),
                                    None => shifted,
                                });
                            }
                        }
                        let base = acc.unwrap_or_else(|| builder.ins().iconst(ptr_ty, 0));
                        let v = apply_mask(&mut builder, base, narrow_mask(*total_width));
                        write_slot(&mut builder, frame, d, v, &mut cache, rw_flags);
                    }
                    Ins::Index { subj, index, dst } => {
                        // `(bits >> index) & 1`, 0 for `index >= 64` (the
                        // slot's bits are zero above its width).
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = if (*index as usize) < 64 {
                            let shifted = builder.ins().ushr_imm_u(bits, *index as i64);
                            builder.ins().band_imm_u(shifted, 1)
                        } else {
                            builder.ins().iconst(ptr_ty, 0)
                        };
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::IndexDyn { subj, index, dst } => {
                        // `bits >> index & 1`, 0 for `index >= 64`.  The
                        // machine shift masks its amount, so mask the
                        // index explicitly and select past 64.
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let idx = read_slot(&mut builder, frame, *index, &mut cache);
                        let masked_idx = builder.ins().band_imm_u(idx, 63);
                        let shifted = builder.ins().ushr(bits, masked_idx);
                        let bit = builder.ins().band_imm_u(shifted, 1);
                        let big = to_bool(&mut builder, IntCC::UnsignedGreaterThanOrEqual, idx, 64);
                        let zero = builder.ins().iconst(ptr_ty, 0);
                        let v = builder.ins().select(big, zero, bit);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::IndexRange { subj, lo, hi, dst } => {
                        // `(bits >> lo) & mask(hi - lo)`, 0 for `lo >= 64`.
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = if (*lo as usize) < 64 {
                            let shifted = builder.ins().ushr_imm_u(bits, *lo as i64);
                            apply_mask(&mut builder, shifted, narrow_mask(hi.saturating_sub(*lo)))
                        } else {
                            builder.ins().iconst(ptr_ty, 0)
                        };
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Zext { subj, out_width, dst } => {
                        // Zero-extend: mask to the node's type width.
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = apply_mask(&mut builder, bits, narrow_mask(*out_width));
                        let Dst::Narrow(d) = *dst else { unreachable!() };
                        write_slot(&mut builder, frame, d, v, &mut cache, rw_flags);
                    }
                    Ins::Sext { subj, subj_width, out_width, dst } => {
                        // Transcribes `eval_sext`'s Small path.
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = if *out_width <= 64 {
                            if *out_width >= *subj_width {
                                let msb = if *subj_width == 0 {
                                    builder.ins().iconst(ptr_ty, 0)
                                } else {
                                    let shifted =
                                        builder.ins().ushr_imm_u(bits, (*subj_width - 1) as i64);
                                    let top = builder.ins().band_imm_u(shifted, 1);
                                    to_bool(&mut builder, IntCC::Equal, top, 1)
                                };
                                let hi_mask =
                                    narrow_mask(*out_width) & !narrow_mask(*subj_width);
                                let hi = builder.ins().iconst(ptr_ty, hi_mask as i64);
                                let spread = builder.ins().band(msb, hi);
                                let masked = apply_mask(&mut builder, bits, narrow_mask(*out_width));
                                builder.ins().bor(masked, spread)
                            } else {
                                apply_mask(&mut builder, bits, narrow_mask(*out_width))
                            }
                        } else {
                            unreachable!("sext past 64 bits filtered")
                        };
                        let Dst::Narrow(d) = *dst else { unreachable!() };
                        write_slot(&mut builder, frame, d, v, &mut cache, rw_flags);
                    }
                    Ins::Move { src, dst } => {
                        let v = read_slot(&mut builder, frame, *src, &mut cache);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Any { subj, dst } => {
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        let v = to_bool(&mut builder, IntCC::NotEqual, bits, 0);
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::All { subj, width, dst } => {
                        let bits = read_slot(&mut builder, frame, *subj, &mut cache);
                        // NB: reproduces `Value::is_all_ones`'s small-value
                        // quirk, a 64-bit value is "all ones" regardless
                        // of its bits.
                        let v = if *width == 64 {
                            builder.ins().iconst(ptr_ty, 1)
                        } else {
                            to_bool(
                                &mut builder,
                                IntCC::Equal,
                                bits,
                                narrow_mask(*width) as i64,
                            )
                        };
                        write_slot(&mut builder, frame, *dst, v, &mut cache, rw_flags);
                    }
                    Ins::Ret { src } => {
                        // Masked to the driver's width, exactly as
                        // `Value::from_u64` masks: the bits-only commit
                        // path compares these raw against masked stored
                        // bits, so the masks must agree.
                        let bits = read_slot(&mut builder, frame, *src, &mut cache);
                        let masked =
                            apply_mask(&mut builder, bits, narrow_mask(program.driver_widths
                                [drivers[k] as usize]));
                        let out_addr = builder.ins().iadd_imm_s(out, (k * 8) as i64);
                        builder.ins().store(rw_flags, masked, out_addr, 0);
                        chain(&mut builder);
                        terminated = true;
                        break;
                    }
                    Ins::Load { dst: Dst::Wide(_), .. }
                    | Ins::Zero { dst: Dst::Wide(_), .. }
                    | Ins::WideConst { .. }
                    | Ins::TestTagWide { .. }
                    | Ins::ExtractWide { .. }
                    | Ins::BinWide { .. }
                    | Ins::UnWide { .. }
                    | Ins::EncodeCtorWide { .. }
                    | Ins::IndexWide { .. }
                    | Ins::IndexDynWide { .. }
                    | Ins::IndexRangeWide { .. }
                    | Ins::ZextWide { .. }
                    | Ins::SextWide { .. }
                    | Ins::MoveWide { .. }
                    | Ins::AnyWide { .. }
                    | Ins::AllWide { .. }
                    | Ins::Trap
                    | Ins::RetWide { .. } => {
                        unreachable!("wide/trap instruction in a JIT-compiled phase")
                    }
                }
                ip += 1;
            }
            if !terminated {
                // Fall out of the walk into the next block start, if
                // any; else (the driver's last block, unterminated —
                // defensive, same as v1) store zeros and chain.
                if let Some(&next) = cfg.starts.get(b_idx + 1) {
                    builder.ins().jump(cfg.block_of[&next], &[]);
                } else {
                    let zero = builder.ins().iconst(ptr_ty, 0);
                    let out_addr = builder.ins().iadd_imm_s(out, (k * 8) as i64);
                    builder.ins().store(rw_flags, zero, out_addr, 0);
                    chain(&mut builder);
                }
            }
        }
    }
    builder.seal_all_blocks();
    builder.finalize(frontend_config);

    module
        .define_function(func_id, ctx)
        .expect("jit: define phase function");
    module.clear_context(ctx);

    func_id
}

/// A driver is JIT-eligible when every instruction is in the narrow
/// subset (≤ 64-bit slots, no `Trap`).
fn eligible(body: &[Ins]) -> bool {
    body.iter().all(|ins| match ins {
        Ins::Load { dst, .. } | Ins::Zero { dst, .. } | Ins::Splice { dst, .. }
        | Ins::Zext { dst, .. } | Ins::ExtractWide { dst, .. } => matches!(dst, Dst::Narrow(_)),
        // `+.`/`-.` stay on the interpreter: their widened result
        // semantics (one bit wider than the operands) are not worth
        // the Cranelift lowering when a phase can simply fall back.
        Ins::Bin { op: BinOp::AddCarry | BinOp::SubBorrow, .. } => false,
        Ins::WideConst { .. }
        | Ins::TestTagWide { .. }
        | Ins::BinWide { .. }
        | Ins::UnWide { .. }
        | Ins::EncodeCtorWide { .. }
        | Ins::IndexWide { .. }
        | Ins::IndexDynWide { .. }
        | Ins::IndexRangeWide { .. }
        | Ins::ZextWide { .. }
        | Ins::SextWide { .. }
        | Ins::MoveWide { .. }
        | Ins::AnyWide { .. }
        | Ins::AllWide { .. }
        | Ins::RetWide { .. }
        | Ins::Trap => false,
        Ins::Sext { out_width, .. } => *out_width <= 64,
        _ => true,
    })
}

fn is_conditional(ins: &Ins) -> bool {
    matches!(
        ins,
        Ins::BrTruthy { .. } | Ins::TestTag { .. } | Ins::TestBits { .. } | Ins::TestBit { .. }
    )
}

fn ins_targets(ins: &Ins) -> Vec<Ip> {
    match ins {
        Ins::Jmp { to } => vec![*to],
        Ins::BrTruthy { to, .. } => vec![*to],
        Ins::TestTag { next_if_match, .. } => vec![*next_if_match],
        Ins::TestBits { next_if_match, .. } => vec![*next_if_match],
        Ins::TestBit { next_if_match, .. } => vec![*next_if_match],
        _ => vec![],
    }
}

fn push_unique(v: &mut Vec<Ip>, x: Ip) {
    if !v.contains(&x) {
        v.push(x);
    }
}

fn narrow_src(src: Src) -> u16 {
    match src {
        Src::Narrow(s) => s,
        Src::Wide(_) => unreachable!("wide part in a narrow splice"),
    }
}

/// Read a narrow slot (cache-first; loads are `trusted` so the
/// address is always in-bounds by the frame layout).
fn read_slot(
    builder: &mut FunctionBuilder,
    frame: ir::Value,
    slot: u16,
    cache: &mut HashMap<u16, ir::Value>,
) -> ir::Value {
    if let Some(&v) = cache.get(&slot) {
        return v;
    }
    let addr = builder.ins().iadd_imm_s(frame, (slot as i64) * 8);
    let v = builder.ins().load(ir::types::I64, MemFlagsData::trusted(), addr, 0);
    cache.insert(slot, v);
    v
}

fn write_slot(
    builder: &mut FunctionBuilder,
    frame: ir::Value,
    slot: u16,
    v: ir::Value,
    cache: &mut HashMap<u16, ir::Value>,
    flags: MemFlagsData,
) {
    let addr = builder.ins().iadd_imm_s(frame, (slot as i64) * 8);
    builder.ins().store(flags, v, addr, 0);
    cache.insert(slot, v);
}

fn shift_by(builder: &mut FunctionBuilder, v: ir::Value, lo: u16) -> ir::Value {
    if lo > 0 {
        builder.ins().ishl_imm_u(v, lo as i64)
    } else {
        v
    }
}

fn apply_mask(builder: &mut FunctionBuilder, v: ir::Value, m: u64) -> ir::Value {
    if m == u64::MAX {
        v
    } else {
        builder.ins().band_imm_u(v, m as i64)
    }
}

fn to_bool(builder: &mut FunctionBuilder, cc: IntCC, x: ir::Value, y: i64) -> ir::Value {
    let t = builder.ins().icmp_imm_u(cc, x, y);
    builder.ins().uextend(ir::types::I64, t)
}

/// `bin_narrow` in IR: `exec.rs`'s exact Small-path semantics
/// (unmasked bitwise ops, bit-0-only logicals, raw-word comparisons).
fn emit_bin(builder: &mut FunctionBuilder, op: BinOp, m: u64, a: ir::Value, b: ir::Value) -> ir::Value {
    match op {
        BinOp::Add => {
            let sum = builder.ins().iadd(a, b);
            apply_mask(builder, sum, m)
        }
        BinOp::Sub => {
            let diff = builder.ins().isub(a, b);
            apply_mask(builder, diff, m)
        }
        BinOp::And => builder.ins().band(a, b),
        BinOp::Or => builder.ins().bor(a, b),
        BinOp::Xor => builder.ins().bxor(a, b),
        BinOp::Eq => icmp_widen(builder, IntCC::Equal, a, b),
        BinOp::Neq => icmp_widen(builder, IntCC::NotEqual, a, b),
        BinOp::Lt => icmp_widen(builder, IntCC::UnsignedLessThan, a, b),
        BinOp::Lte => icmp_widen(builder, IntCC::UnsignedLessThanOrEqual, a, b),
        BinOp::Gt => icmp_widen(builder, IntCC::UnsignedGreaterThan, a, b),
        BinOp::Gte => icmp_widen(builder, IntCC::UnsignedGreaterThanOrEqual, a, b),
        // Logical ops read bit 0 of their operands only.
        BinOp::LogicalAnd => {
            let and = builder.ins().band(a, b);
            builder.ins().band_imm_u(and, 1)
        }
        BinOp::LogicalOr => {
            let or = builder.ins().bor(a, b);
            builder.ins().band_imm_u(or, 1)
        }
        BinOp::LogicalXor => {
            let xor = builder.ins().bxor(a, b);
            builder.ins().band_imm_u(xor, 1)
        }
        // Unreachable: `eligible` marks the widened ops non-JIT-able
        // (their result is one bit wider than the operands), so the
        // translator never sees them; the interpreter replay implements
        // them.
        BinOp::AddCarry | BinOp::SubBorrow => {
            let _ = (a, b);
            unreachable!("jit: widened add/sub ops are not JIT-eligible")
        }
    }
}

fn icmp_widen(builder: &mut FunctionBuilder, cc: IntCC, a: ir::Value, b: ir::Value) -> ir::Value {
    let t = builder.ins().icmp(cc, a, b);
    builder.ins().uextend(ir::types::I64, t)
}

/// `un_narrow` in IR (`exec.rs`'s Small-path semantics).
fn emit_un(builder: &mut FunctionBuilder, op: UnOp, m: u64, a: ir::Value) -> ir::Value {
    match op {
        UnOp::Neg => {
            let neg = builder.ins().ineg(a);
            apply_mask(builder, neg, m)
        }
        UnOp::Inv => {
            let inv = builder.ins().bnot(a);
            apply_mask(builder, inv, m)
        }
        UnOp::Not => to_bool(builder, IntCC::Equal, a, 0),
    }
}
