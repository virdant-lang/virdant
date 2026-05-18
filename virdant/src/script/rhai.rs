//! Rhai scripting support for Virdant simulation.
//!
//! Provides a Rhai engine with bindings to open Virdant projects/files,
//! create simulators, and control simulations via callbacks.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use bstr::BStr;
use rhai::{Dynamic, Engine, EvalAltResult, FnPtr, Module, AST};

use crate::db::Db;
use crate::sim::{Callback, Sim, Value};

use crate::util::{db_from_dir, db_from_file};
use crate::util::check_db;

/// Wrapper around `Db` for Rhai scripting.
/// Holds an `Arc<Db>` and provides methods to create simulators.
#[derive(Clone)]
pub struct ScriptDb {
    db: Arc<Db>,
}

impl ScriptDb {
    pub fn new(db: Db) -> Self {
        Self { db: Arc::new(db) }
    }

    /// Create a Sim for the given top module (e.g., "pkg::Top").
    pub fn sim(&mut self, top: &str) -> Result<ScriptSim, Box<EvalAltResult>> {
        let symboltable = self.db.get_symboltable();
        let top_symbol = symboltable
            .resolve(<&BStr>::from(top))
            .ok_or_else(|| format!("Could not resolve top module: {}", top))?;
        let sim = Sim::new(&self.db, top_symbol.id());
        Ok(ScriptSim::new(sim))
    }
}

/// Shared state that holds the engine and the functions-only AST.
struct ScriptContext {
    engine: Arc<Engine>,
    /// AST containing only functions (no global statements).
    fn_ast: Arc<AST>,
}

/// A clock timing specification exposed to Rhai scripts.
/// Mirrors the Rust [`crate::sim::Clock`] API.
#[derive(Clone)]
pub struct Clock {
    pub(crate) inner: crate::sim::Clock,
}

impl Clock {
    /// Construct a clock with a full period in picoseconds.
    pub fn with_period_ps(period_ps: i64) -> Clock {
        Clock { inner: crate::sim::Clock::with_period_ps(period_ps as u64) }
    }

    /// Construct a clock from a frequency in Hz.
    pub fn with_freq_hz(freq_hz: i64) -> Clock {
        Clock { inner: crate::sim::Clock::with_freq_hz(freq_hz as u64) }
    }
}

/// Wrapper around `Sim` for Rhai scripting.
/// Uses `Rc<RefCell<Sim>>` internally to allow mutable access from callbacks.
#[derive(Clone)]
pub struct ScriptSim {
    sim: Rc<RefCell<Sim>>,
}

impl ScriptSim {
    pub fn new(sim: Sim) -> Self {
        Self {
            sim: Rc::new(RefCell::new(sim)),
        }
    }

    /// Access the sim, using ACTIVE_SIM if in a callback or RefCell otherwise.
    fn with_sim<T, F: FnOnce(&mut Sim) -> T>(&self, f: F) -> T {
        ACTIVE_SIM.with(|active| {
            // Try to borrow immutably. If it fails (already borrowed mutably), we know
            // the pointer is being set, so we can safely dereference it.
            let ptr_to_use = match active.try_borrow() {
                Ok(borrow) => {
                    let ptr = *borrow;
                    drop(borrow);
                    ptr
                },
                Err(_) => {
                    // Borrow failed - someone is borrowing mutably (setting the pointer).
                    // We can't read it safely, so fall back to RefCell.
                    None
                }
            };

            if let Some(ptr) = ptr_to_use {
                // SAFETY: The pointer is valid for the duration of the callback.
                // We're inside the callback that set this pointer.
                let sim = unsafe { &mut *ptr };
                f(sim)
            } else {
                f(&mut self.sim.borrow_mut())
            }
        })
    }

    /// Get the current simulation time in picoseconds.
    pub fn now(&mut self) -> i64 {
        self.with_sim(|sim| sim.now() as i64)
    }

    /// Get the value of a signal by path (e.g., "top.clock").
    pub fn get(&mut self, path: &str) -> Dynamic {
        self.with_sim(|sim| {
            let signal_id = sim.signal(path);
            value_to_dynamic(&sim.get(signal_id))
        })
    }

    /// Set a signal to a value. Accepts integers (converted to appropriate type).
    pub fn set(&mut self, path: &str, value: Dynamic) {
        self.with_sim(|sim| {
            let signal_id = sim.signal(path);
            let typ = sim.component_type(signal_id);
            let val = dynamic_to_value(value, &typ);
            {
                let mut lock = sim.lock();
                lock.set(signal_id, val);
            }
        })
    }

    /// Attach a clock to the given signal path.
    pub fn attach_clock(&mut self, path: &str, clock: Clock) {
        self.with_sim(|sim| {
            let signal_id = sim.signal(path);
            sim.attach_clock(signal_id, clock.inner);
        })
    }

    /// Register a callback to fire at absolute time t_ps.
    pub fn at(&mut self, t_ps: i64, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        self.sim.borrow_mut().at(
            t_ps as u64,
            make_callback(sim_clone, cb),
        );
    }

    /// Register a callback to fire after delay_ps picoseconds.
    pub fn after(&mut self, delay_ps: i64, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        self.sim.borrow_mut().after(
            delay_ps as u64,
            make_callback(sim_clone, cb),
        );
    }

    /// Register a callback to fire at simulation start.
    pub fn at_start(&mut self, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        self.sim
            .borrow_mut()
            .at_start(make_callback(sim_clone, cb));
    }

    /// Register a callback to fire at simulation end.
    pub fn at_end(&mut self, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        self.sim
            .borrow_mut()
            .at_end(make_callback(sim_clone, cb));
    }

    /// Register a callback to fire on rising edges of the given clock signal.
    pub fn on_clock(&mut self, path: &str, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        let signal_id = self.sim.borrow_mut().signal(path);
        self.sim
            .borrow_mut()
            .on_clock(signal_id, make_callback(sim_clone, cb));
    }

    /// Register a callback to fire when the given signal changes value.
    pub fn on_change(&mut self, path: &str, cb: FnPtr) {
        let sim_clone = self.sim.clone();
        let signal_id = self.sim.borrow_mut().signal(path);
        self.sim
            .borrow_mut()
            .on_change(signal_id, make_callback(sim_clone, cb));
    }

    /// Request simulation shutdown.
    pub fn finish(&mut self) {
        self.with_sim(|sim| sim.finish());
    }

    /// Run the simulation until completion or finish() is called.
    pub fn run(&mut self) -> Result<(), Box<EvalAltResult>> {
        // Note: run() must NOT use with_sim because it needs to hold the borrow
        // for the entire duration, and callbacks will use ACTIVE_SIM.
        self.sim
            .borrow_mut()
            .run()
            .map_err(|e| format!("Simulation error: {:?}", e).into())
    }

    /// Assert a condition, failing with a message if false.
    pub fn assert_msg(&mut self, cond: bool, message: &str) -> Result<(), Box<EvalAltResult>> {
        if !cond {
            Err(format!("Assertion failed: {}", message).into())
        } else {
            Ok(())
        }
    }

    /// Assert a condition, failing with a generic message if false.
    pub fn assert_simple(&mut self, cond: bool) -> Result<(), Box<EvalAltResult>> {
        if !cond {
            Err("Assertion failed".into())
        } else {
            Ok(())
        }
    }
}


/// Helper to create a callback that invokes a Rhai function or closure.
/// Uses thread-local SCRIPT_CONTEXT to access the engine and AST.
/// Supports closures by including captured variables (curry arguments).
fn make_callback(
    sim: Rc<RefCell<Sim>>,
    fn_ptr: FnPtr,
) -> Callback {
    Box::new(move |rust_sim: &mut Sim| {
        // Store the active sim pointer in thread-local for the duration of the callback.
        // This allows ScriptSim methods to use the direct &mut Sim instead of RefCell.
        let rust_sim_ptr = rust_sim as *mut Sim;

        ACTIVE_SIM.with(|active| {
            *active.borrow_mut() = Some(rust_sim_ptr);
        });

        SCRIPT_CONTEXT.with(|ctx| {
            let (engine, fn_ast, fn_name) = {
                let ctx_borrow = ctx.borrow();
                let Some(script_ctx) = ctx_borrow.as_ref() else {
                    eprintln!("Rhai callback error: no script context set");
                    return;
                };
                (script_ctx.engine.clone(), script_ctx.fn_ast.clone(), fn_ptr.fn_name().to_string())
            };

            // Create a fresh ScriptSim wrapper pointing to our shared Sim.
            let script_sim = ScriptSim { sim: sim.clone() };

            // Build arguments: curry (captured) args first, then our sim argument.
            // This is how Rhai closures work - captured vars are prepended.
            let mut args: Vec<Dynamic> = fn_ptr.curry().iter().cloned().collect();
            args.push(Dynamic::from(script_sim));

            // Call the function by name with the combined arguments.
            let options = rhai::CallFnOptions::new()
                .eval_ast(true)
                .rewind_scope(false);
            let result: Result<Dynamic, _> = engine.call_fn_with_options::<Dynamic>(
                options,
                &mut rhai::Scope::new(),
                &fn_ast,
                &fn_name,
                args,
            );
            if let Err(e) = result {
                eprintln!("Rhai callback error: {}", e);
            }
        });

        // Clear the active sim pointer
        ACTIVE_SIM.with(|active| {
            *active.borrow_mut() = None;
        });
    })
}

// Thread-local storage for the script context during execution.
thread_local! {
    static SCRIPT_CONTEXT: RefCell<Option<ScriptContext>> = const { RefCell::new(None) };
    // During callbacks, points to the active &mut Sim from the callback.
    static ACTIVE_SIM: RefCell<Option<*mut Sim>> = const { RefCell::new(None) };
}

/// Convert a Virdant Value to a Rhai Dynamic.
fn value_to_dynamic(value: &Value) -> Dynamic {
    match value {
        Value::Bit(b) => Dynamic::from(*b),
        Value::Word(_width, v) => Dynamic::from(*v as i64),
        Value::X(_) => Dynamic::from("X"),
        Value::Z(_) => Dynamic::from("Z"),
        Value::Ctor(_, _, _) => Dynamic::from("Ctor"), // TODO: Better representation
    }
}

/// Convert a Rhai Dynamic to a Virdant Value based on the expected type.
fn dynamic_to_value(value: Dynamic, typ: &crate::types::Type) -> Value {
    use crate::types::Type;
    match typ {
        Type::Bit | Type::Reset => {
            if let Some(b) = value.as_bool().ok() {
                Value::Bit(b)
            } else if let Some(i) = value.as_int().ok() {
                Value::Bit(i != 0)
            } else {
                Value::X(typ.clone())
            }
        }
        Type::Word(width) => {
            if let Some(i) = value.as_int().ok() {
                Value::Word(*width, i as u64)
            } else {
                Value::X(typ.clone())
            }
        }
        _ => Value::X(typ.clone()),
    }
}

/// Open a single .vir file and return a ScriptDb.
fn open_file(path: &str) -> Result<ScriptDb, Box<EvalAltResult>> {
    let db = db_from_file(std::path::Path::new(path));
    validate_db(&db)?;
    Ok(ScriptDb::new(db))
}

/// Open a project directory (containing src/) and return a ScriptDb.
fn open_dir(path: &str) -> Result<ScriptDb, Box<EvalAltResult>> {
    let src_dir = std::path::Path::new(path).join("src");
    if !src_dir.exists() {
        return Err(format!("No src/ directory found in {}", path).into());
    }
    let db = db_from_dir(src_dir);
    validate_db(&db)?;
    Ok(ScriptDb::new(db))
}

/// Validate a Db and return an error if there are diagnostics.
fn validate_db(db: &Db) -> Result<(), Box<EvalAltResult>> {
    if let Err(diagnostics) = check_db(db) {
        let msg = diagnostics
            .iter()
            .map(|d| format!("{:?}", d))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(format!("Virdant diagnostics:\n{}", msg).into());
    }
    Ok(())
}

/// Create a Rhai Engine with all Virdant simulation bindings registered.
pub fn make_engine() -> Engine {
    let mut engine = Engine::new();

    // Register ScriptDb type and methods
    engine
        .register_type_with_name::<ScriptDb>("Db")
        .register_fn("sim", ScriptDb::sim);

    // Register ScriptSim type and methods
    engine
        .register_type_with_name::<ScriptSim>("Sim")
        .register_fn("now", ScriptSim::now)
        .register_fn("get", ScriptSim::get)
        .register_fn("set", ScriptSim::set)
        .register_fn("attach_clock", ScriptSim::attach_clock)
        .register_fn("at", ScriptSim::at)
        .register_fn("after", ScriptSim::after)
        .register_fn("at_start", ScriptSim::at_start)
        .register_fn("at_end", ScriptSim::at_end)
        .register_fn("on_clock", ScriptSim::on_clock)
        .register_fn("on_change", ScriptSim::on_change)
        .register_fn("finish", ScriptSim::finish)
        .register_fn("run", ScriptSim::run)
        .register_fn("assert", ScriptSim::assert_msg)
        .register_fn("assert", ScriptSim::assert_simple);

    // Register Clock type and static module
    engine.register_type_with_name::<Clock>("Clock");
    let mut clock_module = Module::new();
    clock_module.set_native_fn("with_period_ps", |period_ps: i64| -> Result<Clock, Box<EvalAltResult>> {
        Ok(Clock::with_period_ps(period_ps))
    });
    clock_module.set_native_fn("with_freq_hz", |freq_hz: i64| -> Result<Clock, Box<EvalAltResult>> {
        Ok(Clock::with_freq_hz(freq_hz))
    });
    engine.register_static_module("Clock", clock_module.into());

    // Register global functions
    engine
        .register_fn("open_file", open_file)
        .register_fn("open_dir", open_dir);

    engine
}

/// Run a Rhai script file with full simulation support.
/// This compiles the script, then runs it with the engine and AST available
/// for callback invocation.
pub fn run_script_file(path: &std::path::Path) -> Result<(), Box<EvalAltResult>> {
    let script = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read script: {}", e))?;

    let engine = make_engine();
    let ast = engine.compile(&script)?;

    // Extract just the function definitions from the AST.
    // This prevents re-running global statements when calling functions.
    let fn_ast = ast.clone_functions_only();

    let engine = Arc::new(engine);
    let fn_ast = Arc::new(fn_ast);

    // Store engine and functions-only AST in thread-local storage.
    SCRIPT_CONTEXT.with(|ctx| {
        *ctx.borrow_mut() = Some(ScriptContext {
            engine: Arc::clone(&engine),
            fn_ast: Arc::clone(&fn_ast),
        });
    });

    let result = engine.run_ast(&ast);

    // Clear the context
    SCRIPT_CONTEXT.with(|ctx| {
        *ctx.borrow_mut() = None;
    });

    result
}
