//! Command execution for `vir poke`: the dispatch behind the parsed
//! script/REPL line, shared by all three run modes (direct REPL,
//! preloaded script, standalone script).

use bstr::ByteSlice as _;

use virdant::analysis::elaboration::SignalId;
use virdant::sim::Value;
use virdant::types::Type;

use super::ast::{BinKind, Command, Expr};
use super::{display, eval, poke_legal, Session};

/// What happens after a command runs.
pub enum Flow {
    /// Keep going.
    Continue,
    /// `exit` (or EOF): leave the session.
    Exit,
    /// An `assert` failed; the message is already rendered.
    AssertFailed(String),
    /// A `break` watch fired during the command's settlement; the
    /// message is already rendered.
    BreakHit(String),
}

/// A source line, for assertion diagnostics.
#[derive(Clone, Debug)]
pub struct SourceLine {
    pub number: usize,
    pub text: String,
}

/// Half of the virt platform's 1 MHz clock period (and the tick size
/// of `docs/vcd2wavedrom.py`'s default view).  The poke session never
/// runs the scheduler — the only other thing that moves time — so
/// each `tick` advances the clock by two half periods around its
/// falling and rising edges.
const HALF_PERIOD_PS: u64 = 500_000;

/// One full clock period of the virt platform's 1 MHz clock: the
/// timestep of `advance`.  Deliberately a fixed constant, independent
/// of the design under test — `advance` must move time even on
/// designs with no clock at all (where `tick` cannot).
const TIMESTEP_PS: u64 = 2 * HALF_PERIOD_PS;

/// Execute one command.
pub fn exec(
    session: &mut Session,
    command: &Command,
    source: Option<&SourceLine>,
) -> Result<Flow, String> {
    match command {
        Command::Cd(segments) => {
            session.cd(segments)?;
            Ok(Flow::Continue)
        }
        Command::CdAbs(segments) => {
            session.cd_abs(segments)?;
            Ok(Flow::Continue)
        }
        Command::CdTop => {
            session.cwd.clear();
            Ok(Flow::Continue)
        }
        Command::CdUp => {
            session.cd_up()?;
            Ok(Flow::Continue)
        }
        Command::Ls => {
            session.ls();
            Ok(Flow::Continue)
        }
        Command::Show => {
            session.show();
            Ok(Flow::Continue)
        }
        Command::Autoshow(on) => {
            // No argument toggles; an argument sets the state.
            let now = on.unwrap_or(!session.autoshow);
            session.autoshow = now;
            println!("autoshow is now {}", if now { "on" } else { "off" });
            Ok(Flow::Continue)
        }
        Command::Vcd(name) => {
            if let Some(old) = &session.vcd {
                return Err(format!(
                    "a VCD trace is already being written to `{old}`; `restart` the simulation to start a new trace"
                ));
            }
            let file = std::fs::File::create(name)
                .map_err(|e| format!("cannot create `{name}`: {e}"))?;
            // Educational delay: every non-clock signal's changes render
            // a twentieth of a clock period after the tick edge that
            // caused them — the clock stamps at its true time, so
            // waveforms show logic settling just after the edge, without
            // the engine modelling delay at all.
            session.sim.set_vcd_offset_ps(TIMESTEP_PS / 20);
            session.sim.attach_vcd(file);
            session.vcd = Some(name.clone());
            println!("vcd: writing trace to `{name}`");
            Ok(Flow::Continue)
        }
        Command::Peek { path } => {
            let signal_id = session.resolve_signal(path)?;
            let value = session.sim.get(signal_id);
            session.it = Some(value.clone());
            let full = session.sim.full_name(signal_id).to_str_lossy().into_owned();
            let typ = session.sim.component_type(signal_id);
            if matches!(typ, Type::Clock) {
                // Clocks are opaque: no value is shown (time is driven
                // by `tick`, not by values).
                println!(
                    "{} : {}",
                    full,
                    virdant::types::render::render_type(&session.db, &typ)
                );
            } else {
                println!(
                    "{} : {} = {}",
                    full,
                    virdant::types::render::render_type(&session.db, &typ),
                    display::render(&session.sim, &value)
                );
            }
            Ok(Flow::Continue)
        }
        Command::Poke { path, expr } => {
            let signal_id = session.resolve_signal(path)?;
            poke_legal(session, signal_id)?;
            let typ = session.sim.component_type(signal_id);
            let value = eval::eval_typed(session, expr, &typ)?;
            {
                let mut lock = session.sim.lock();
                lock.set(signal_id, value.clone());
            }
            let full = session.sim.full_name(signal_id).to_str_lossy().into_owned();
            println!("{} <= {}", full, display::render(&session.sim, &value));
            if session.autoshow {
                session.show();
            }
            Ok(check_break(session)?.unwrap_or(Flow::Continue))
        }
        Command::Watch { path } => {
            let signal_id = session.resolve_signal(path)?;
            let full = session.sim.full_name(signal_id).to_str_lossy().into_owned();
            session.register_watch(signal_id, full, false);
            Ok(Flow::Continue)
        }
        Command::Break { path } => {
            let signal_id = session.resolve_signal(path)?;
            let full = session.sim.full_name(signal_id).to_str_lossy().into_owned();
            session.register_watch(signal_id, full, true);
            Ok(Flow::Continue)
        }
        Command::Tick { count, clock } => {
            let signal_id: SignalId = match clock {
                Some(path) => session.resolve_signal(path)?,
                None => session.default_clock()?,
            };
            let clock_typ = session.sim.component_type(signal_id);
            if clock_typ != Type::Clock {
                let full = session.sim.full_name(signal_id).to_str_lossy().into_owned();
                return Err(format!(
                    "`{full}` is not a clock (it is a {})",
                    virdant::types::render::render_type(&session.db, &clock_typ)
                ));
            }
            for _ in 0..*count {
                // One full clock cycle, closed by the rising edge: the
                // tick runs the cycle's high phase and low phase, then
                // raises the clock at the cycle's end — the rising edge
                // is what transfers the clocked registers, via the sim's
                // `set` edge detection exactly like the virt board's
                // free-running clock.
                //
                // Closing on the posedge is the point: a `poke` issued
                // between ticks settles at the tick boundary, just after
                // the edge that closed the previous cycle, so it is the
                // stimulus *for the cycle the next tick runs*.  That
                // edge — the one this tick ends with — latches it.  This
                // is true to Virdant's model and draws the textbook
                // waveform: every transition, stimulus and registers
                // alike, lines up with a rising edge, and a registered
                // output's values lag its input by exactly one cycle.
                //
                // The tick opens with the clock high (the previous tick
                // closed on its posedge) — except at session start and
                // after `restart`, where the level is random.  There the
                // clock is forced low deterministically and the first
                // cycle simply has no high phase, so the posedge grid is
                // the same either way.
                if session.sim.get(signal_id).get_bit(0) {
                    // High phase, then the negedge at the cycle midpoint.
                    session.sim.advance_time(HALF_PERIOD_PS);
                    set_clock(session, signal_id, false);
                    session.sim.advance_time(HALF_PERIOD_PS);
                } else {
                    // No high phase this cycle: a full period low before
                    // the first posedge, keeping the posedge grid put.
                    session.sim.advance_time(HALF_PERIOD_PS);
                    session.sim.advance_time(HALF_PERIOD_PS);
                }
                set_clock(session, signal_id, true);
                if let Some(flow) = check_break(session)? {
                    return Ok(flow);
                }
            }
            if session.autoshow {
                session.show();
            }
            Ok(Flow::Continue)
        }
        Command::Advance { count } => {
            // Move time forward without driving any clock: the only
            // way to advance an unclocked design's timeline (and a
            // way to skip time between `tick`s on clocked ones).
            // Nothing is scheduled in poke mode and values settle on
            // `poke`, so a plain time skip needs no settlement pass.
            // Saturating rather than looping, so an absurd count
            // cannot hang the session.
            let total = count.saturating_mul(TIMESTEP_PS);
            session.sim.advance_time(total);
            println!("advanced to {} ps", session.sim.now());
            Ok(Flow::Continue)
        }
        Command::Assert { expr, line } => {
            match assert(session, expr, line, source)? {
                Some(message) => Ok(Flow::AssertFailed(message)),
                None => Ok(Flow::Continue),
            }
        }
        Command::Print { expr } => {
            let value = eval::eval(session, expr)?;
            println!("{}", display::render(&session.sim, &value));
            Ok(check_break(session)?.unwrap_or(Flow::Continue))
        }
        Command::Restart => {
            session.restart();
            println!("restarted");
            Ok(Flow::Continue)
        }
        Command::Top(name) => {
            session.set_top(name)?;
            Ok(Flow::Continue)
        }
        Command::Exit => Ok(Flow::Exit),
    }
}

/// Drive a Clock-typed signal to `high` and settle: the falling edge
/// is recorded but does nothing, the rising edge transfers every
/// register the clock drives (through the sim's edge detection, with
/// submodule/alias clocks cascading via `flow`).
fn set_clock(session: &mut Session, signal_id: SignalId, high: bool) {
    let typ_id = session.sim.component_type_id(signal_id);
    let value = Value::from_bool(typ_id, high);
    let mut lock = session.sim.lock();
    lock.set(signal_id, value);
}

/// If a `break` watch fired during the last settlement, render the
/// pause message and clear the flag.  `None` means nothing fired.
fn check_break(session: &mut Session) -> Result<Option<Flow>, String> {
    if let Some(path) = session.take_break() {
        let value = session
            .sim
            .try_resolve(path.as_bytes())
            .map(|signal_id| session.sim.get(signal_id))
            .ok_or_else(|| format!("internal: break path `{path}` no longer resolves"))?;
        return Ok(Some(Flow::BreakHit(format!(
            "break: {} = {}",
            path,
            display::render(&session.sim, &value)
        ))));
    }
    Ok(None)
}

/// Run one `assert`.  `Ok(None)` passes; `Ok(Some(message))` fails
/// with the rendered diagnostic.
fn assert(
    session: &mut Session,
    expr: &Expr,
    line: &str,
    source: Option<&SourceLine>,
) -> Result<Option<String>, String> {
    let value = eval::eval(session, expr)?;
    let truthy = if value.width() == 1 {
        value.get_bit(0)
    } else {
        !value.is_zero()
    };
    if truthy {
        return Ok(None);
    }
    let location = match source {
        Some(source) => format!("line {}: {}", source.number, source.text.trim()),
        None => line.to_string(),
    };
    let mut message = format!("assertion failed: {location}");
    // A failed comparison reports both sides with their concrete
    // (round-trippable) values.  Each side is evaluated with the other
    // side's type as context, so bare literals and `#Ctor` literals
    // coerce the same way they did in the actual comparison.
    if let Expr::Bin(
        op @ (BinKind::Eq | BinKind::Neq | BinKind::Lt | BinKind::Lte | BinKind::Gt | BinKind::Gte),
        lhs,
        rhs,
    ) = expr
    {
        let _ = op;
        let lhs_value = eval::eval(session, lhs);
        let rhs_value = match (&lhs_value, eval_side_type(session, rhs, lhs_value.as_ref().ok())) {
            (Ok(_), rhs) => rhs,
            (Err(_), _) => eval::eval(session, rhs),
        };
        let lhs_value = match (&lhs_value, eval_side_type(session, lhs, rhs_value.as_ref().ok())) {
            (Ok(_), lhs) => lhs,
            (Err(_), _) => eval::eval(session, lhs),
        };
        if let (Ok(l), Ok(r)) = (&lhs_value, &rhs_value) {
            message.push_str(&format!(
                "\n  lhs {} = {}\n  rhs {} = {}",
                render_expr(lhs),
                display::render(&session.sim, l),
                render_expr(rhs),
                display::render(&session.sim, &r)
            ));
        }
    }
    Ok(Some(message))
}

/// Evaluate `expr` with `other`'s type as the expected type (used for
/// assertion diagnostics; `None` falls back to context-free
/// evaluation).
fn eval_side_type(
    session: &Session,
    expr: &Expr,
    other: Option<&virdant::sim::Value>,
) -> Result<virdant::sim::Value, String> {
    match other {
        Some(value) => {
            let typ = session.sim.layouts().type_of(value.typ_id()).clone();
            eval::eval_typed(session, expr, &typ)
        }
        None => eval::eval(session, expr),
    }
}

/// Render a sub-expression back to a readable form (for assertion
/// diagnostics).  This is a minimal printer — enough to identify which
/// side failed.
fn render_expr(expr: &Expr) -> String {
    match expr {
        Expr::Path(spath) => {
            let head: String = match &spath.start {
                crate::ast::PathStart::Top => "top".to_string(),
                crate::ast::PathStart::It => "it".to_string(),
                crate::ast::PathStart::Ident(name) => name.clone(),
            };
            if spath.segments.is_empty() {
                head.to_string()
            } else {
                format!("{}.{}", head, spath.segments.join("."))
            }
        }
        Expr::NatLit(v) => format!("{}", v),
        Expr::WordLit(v, w) => format!("{}w{}", v, w),
        Expr::BitLit(b) => format!("{}", b),
        Expr::Ctor { name, args } => {
            if args.is_empty() {
                format!("#{}", name)
            } else {
                let rendered: Vec<String> = args.iter().map(render_expr).collect();
                format!("#{}({})", name, rendered.join(", "))
            }
        }
        Expr::Neg(e) => format!("-{}", render_expr(e)),
        Expr::Inv(e) => format!("~{}", render_expr(e)),
        Expr::Not(e) => format!("!{}", render_expr(e)),
        Expr::Bin(op, l, r) => format!(
            "{} {} {}",
            render_expr(l),
            match op {
                BinKind::Add => "+",
                BinKind::Sub => "-",
                BinKind::And => "&",
                BinKind::Or => "|",
                BinKind::Xor => "^",
                BinKind::Eq => "==",
                BinKind::Neq => "!=",
                BinKind::Lt => "<",
                BinKind::Lte => "<=",
                BinKind::Gt => ">",
                BinKind::Gte => ">=",
                BinKind::LogAnd => "&&",
                BinKind::LogOr => "||",
                BinKind::LogXor => "^^",
            },
            render_expr(r)
        ),
        Expr::Index(e, i) => format!("{}[{}]", render_expr(e), i),
        Expr::Slice(e, hi, lo) => format!("{}[{}..{}]", render_expr(e), hi, lo),
    }
}
