//! Builtin implementations for `ext mod` instances.
//!
//! `ext` modules have no Virdant-level implementation (their Verilog
//! lives in `ext/*.sv` for synthesis/Verilator).  The simulator models
//! the common ones natively so designs like virdant-riscv run without
//! Verilator.  Each builtin is attached per instance and implemented
//! entirely through the `Sim`'s public API (`on_clock`, `on_change`,
//! `lock().set`, `get`) — the engine has no special cases for them.
//!
//! Registered ext instances whose def has no builtin are left
//! undriven (their outputs stay at their initial values).

use std::cell::RefCell;
use std::rc::Rc;

use bstr::{BString, ByteSlice};

use crate::analysis::elaboration::SignalId;
use crate::analysis::symbols::SymbolId;
use crate::db::Db;
use crate::sim::bits::BitPat;
use crate::sim::layout::TypeLayoutIndex;
use crate::sim::{Sim, Value};
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

/// Attach builtin behavior to every `ext mod` instance in the
/// elaborated design rooted at `top`.
pub fn attach(sim: &mut Sim, db: &Db, top: SymbolId) {
    let elaboration = sim.elaboration();
    let symboltable = db.get_symboltable();
    let instances: Vec<(BString, String)> = elaboration
        .modules()
        .iter()
        .filter_map(|(moddef, prefix)| {
            if !is_ext_mod(db, *moddef) {
                return None;
            }
            let fqn = symboltable.symbol(*moddef).fqn().to_string();
            Some((prefix.clone(), fqn))
        })
        .collect();
    drop(elaboration);

    for (prefix, fqn) in instances {
        let attached = match fqn.as_str() {
            "rf::Rf" => builtin_rf(sim, prefix.as_bstr().to_str_lossy().as_ref()),
            "mem::Mem" => builtin_mem(sim, prefix.as_bstr().to_str_lossy().as_ref()),
            "mem::Csrs" => builtin_csrs(sim, prefix.as_bstr().to_str_lossy().as_ref()),
            _ => {
                eprintln!("sim: no builtin for ext module `{fqn}` (instance `{}`)", prefix.as_bstr().to_str_lossy());
                false
            }
        };
        if attached {
            eprintln!("sim: builtin ext module `{fqn}` at `{}`", prefix.as_bstr().to_str_lossy());
        }
    }
}

/// True when the moddef at `symbol_id` is declared `ext mod`.
fn is_ext_mod(db: &Db, symbol_id: SymbolId) -> bool {
    let symboltable = db.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    let location = symbol.location();
    let parsing = db.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    matches!(node.payload(), AstNodePayload::ModDef(moddef) if moddef.is_ext)
}

// ─────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────

fn sig(sim: &Sim, prefix: &str, name: &str) -> Option<SignalId> {
    let path = format!("{prefix}.{name}");
    sim.try_resolve(path.as_bytes())
}

fn get_u64(sim: &Sim, id: SignalId) -> u64 {
    sim.get(id).bits().to_u64()
}

fn get_bit(sim: &Sim, id: SignalId) -> bool {
    sim.get(id).bits().get_bit(0)
}

fn set_u64(sim: &mut Sim, id: SignalId, value: u64) {
    let typ = sim.component_type(id);
    let width = TypeLayoutIndex::type_width_of(&typ);
    let bits = BitPat::from_u64(width, value);
    let mut lock = sim.lock();
    lock.set(id, Value::new(typ, bits));
}

fn set_bit(sim: &mut Sim, id: SignalId, b: bool) {
    let typ = sim.component_type(id);
    let mut lock = sim.lock();
    lock.set(id, Value::from_bool(typ, b));
}

/// Resolve a set of ports by name; `None` when any is missing.
macro_rules! ports {
    ($sim:expr, $prefix:expr, $($var:ident : $name:literal),* $(,)?) => {{
        match ($(sig($sim, $prefix, $name)),*) {
            ($(Some($var)),*) => Some(($($var),*)),
            _ => None,
        }
    }};
}

// ─────────────────────────────────────────────────────────────
// rf::Rf — 32×32 register file
// ─────────────────────────────────────────────────────────────
//
// Matches `ext/Rf.verilator.sv`: registered write on the rising edge
// (sampling the pre-edge `rd`/`rd_in`/`write`), combinational reads,
// x0 hardwired to zero.

fn builtin_rf(sim: &mut Sim, prefix: &str) -> bool {
    let Some((clock, rd, rs1, rs2, rd_in, write, rs1_out, rs2_out)) = ports!(sim, prefix,
        clock: "clock", rd: "rd", rs1: "rs1", rs2: "rs2", rd_in: "rd_in",
        write: "write", rs1_out: "rs1_out", rs2_out: "rs2_out") else
    {
        eprintln!("sim: builtin rf::Rf at `{prefix}`: missing ports");
        for name in ["clock", "rd", "rs1", "rs2", "rd_in", "write", "rs1_out", "rs2_out"] {
            if sig(sim, prefix, name).is_none() {
                eprintln!("  missing .{name}");
            }
        }
        return false;
    };

    let regs: Rc<RefCell<[u64; 32]>> = Rc::new(RefCell::new([0; 32]));
    // Pre-edge samples (NBA semantics): the write at a rising edge uses
    // the values settled during the previous low phase.
    let prev: Rc<RefCell<(u64, u64, bool)>> = Rc::new(RefCell::new((
        get_u64(sim, rd), get_u64(sim, rd_in), get_bit(sim, write),
    )));

    // Combinational read ports.  NOTE: never hold a `regs` borrow across
    // a `set_u64` — driving a value recursively dispatches watchers, and
    // a nested dispatch may re-enter this builtin.
    for (watch, out) in [(rs1, rs1_out), (rs2, rs2_out)] {
        let regs = regs.clone();
        sim.on_change(watch, Box::new(move |sim| {
            let idx = get_u64(sim, watch) & 0x1F;
            let value = {
                let x = regs.borrow();
                if idx == 0 { 0 } else { x[idx as usize] }
            };
            set_u64(sim, out, value);
        }));
    }

    {
        let regs = regs.clone();
        sim.on_clock(clock, Box::new(move |sim| {
            // 1. Apply the registered write (pre-edge samples), dropping
            //    the borrow immediately.
            {
                let (p_rd, p_rd_in, p_write) = *prev.borrow();
                if p_write && p_rd != 0 {
                    regs.borrow_mut()[(p_rd & 0x1F) as usize] = p_rd_in;
                }
            }
            // 2. Latch this edge's input samples for the next write.
            *prev.borrow_mut() = (get_u64(sim, rd), get_u64(sim, rd_in), get_bit(sim, write));
            // 3. Combinational read ports reflect the post-write state.
            let idx1 = get_u64(sim, rs1) & 0x1F;
            let idx2 = get_u64(sim, rs2) & 0x1F;
            let (v1, v2) = {
                let x = regs.borrow();
                (
                    if idx1 == 0 { 0 } else { x[idx1 as usize] },
                    if idx2 == 0 { 0 } else { x[idx2 as usize] },
                )
            };
            set_u64(sim, rs1_out, v1);
            set_u64(sim, rs2_out, v2);
        }));
    }

    // Initial combinational state.
    {
        let x = regs.borrow();
        set_u64(sim, rs1_out, 0);
        set_u64(sim, rs2_out, 0);
    }
    true
}

// ─────────────────────────────────────────────────────────────
// mem::Mem — 64 KiB RAM
// ─────────────────────────────────────────────────────────────
//
// Matches `ext/Mem.verilator.sv`: combinational read
// (`data_out = read ? mem[addr[15:2]] : 0`), registered write on the
// rising edge with byte-lane enables from `sel`.

fn builtin_mem(sim: &mut Sim, prefix: &str) -> bool {
    let Some((clock, addr, data_in, read, write, sel, data_out)) = ports!(sim, prefix,
        clock: "clock", addr: "addr", data_in: "data_in", read: "read",
        write: "write", sel: "sel", data_out: "data_out") else
    {
        return false;
    };

    const WORDS: usize = 16384; // 64 KiB
    let mem: Rc<RefCell<Vec<u64>>> = Rc::new(RefCell::new(vec![0; WORDS]));
    let prev: Rc<RefCell<(u64, u64, bool, u64)>> =
        Rc::new(RefCell::new((
            get_u64(sim, addr), get_u64(sim, data_in), get_bit(sim, write), get_u64(sim, sel),
        )));

    // Combinational read.  Never hold a `mem` borrow across a
    // `set_u64` (nested watcher dispatch may re-enter).
    for watched in [addr, read] {
        let mem = mem.clone();
        sim.on_change(watched, Box::new(move |sim| {
            let value = {
                let m = mem.borrow();
                if get_bit(sim, read) {
                    let idx = ((get_u64(sim, addr) >> 2) & 0x3FFF) as usize;
                    m[idx]
                } else {
                    0
                }
            };
            set_u64(sim, data_out, value);
        }));
    }

    sim.on_clock(clock, Box::new(move |sim| {
        {
            let (p_addr, p_data_in, p_write, p_sel) = *prev.borrow();
            if p_write {
                let mut m = mem.borrow_mut();
                let idx = ((p_addr >> 2) & 0x3FFF) as usize;
                let old = m[idx];
                let mut new = old;
                for lane in 0..4 {
                    if (p_sel >> lane) & 1 != 0 {
                        let shift = 8 * lane;
                        let mask = 0xFFu64 << shift;
                        new = (new & !mask) | (p_data_in & mask);
                    }
                }
                m[idx] = new;
            }
        }
        *prev.borrow_mut() = (
            get_u64(sim, addr), get_u64(sim, data_in), get_bit(sim, write), get_u64(sim, sel),
        );
        // Combinational read reflects the post-edge memory contents.
        let value = {
            let m = mem.borrow();
            if get_bit(sim, read) {
                let idx = ((get_u64(sim, addr) >> 2) & 0x3FFF) as usize;
                m[idx]
            } else {
                0
            }
        };
        set_u64(sim, data_out, value);
    }));

    true
}

// ─────────────────────────────────────────────────────────────
// mem::Csrs — 4096×32 CSR file
// ─────────────────────────────────────────────────────────────
//
// Matches `ext/Mem.verilator.sv`: `data_out` reads the array, except
// mcycle/cycle (0xB00/0xC00), which read a free-running cycle counter
// (incremented on every rising edge).  Registered writes.

fn builtin_csrs(sim: &mut Sim, prefix: &str) -> bool {
    let Some((clock, addr, data_in, data_out, write)) = ports!(sim, prefix,
        clock: "clock", addr: "addr", data_in: "data_in", data_out: "data_out",
        write: "write") else
    {
        return false;
    };

    const WORDS: usize = 4096;
    let mem: Rc<RefCell<Vec<u64>>> = Rc::new(RefCell::new(vec![0; WORDS]));
    let cycle_counter: Rc<RefCell<u64>> = Rc::new(RefCell::new(0));
    let prev: Rc<RefCell<(u64, u64, bool)>> =
        Rc::new(RefCell::new((get_u64(sim, addr), get_u64(sim, data_in), get_bit(sim, write))));

    {
        let mem = mem.clone();
        let cycle_counter = cycle_counter.clone();
        sim.on_change(addr, Box::new(move |sim| {
            let a = get_u64(sim, addr) & 0xFFF;
            let value = if a == 0xB00 || a == 0xC00 {
                *cycle_counter.borrow()
            } else {
                mem.borrow()[a as usize]
            };
            set_u64(sim, data_out, value);
        }));
    }

    sim.on_clock(clock, Box::new(move |sim| {
        {
            let (p_addr, p_data_in, p_write) = *prev.borrow();
            if p_write {
                mem.borrow_mut()[(p_addr & 0xFFF) as usize] = p_data_in;
            }
        }
        *cycle_counter.borrow_mut() += 1;
        *prev.borrow_mut() = (get_u64(sim, addr), get_u64(sim, data_in), get_bit(sim, write));
        let a = get_u64(sim, addr) & 0xFFF;
        let value = if a == 0xB00 || a == 0xC00 {
            *cycle_counter.borrow()
        } else {
            mem.borrow()[a as usize]
        };
        set_u64(sim, data_out, value);
    }));

    true
}
