//! Lua scripting support for Virdant simulation.
//!
//! Provides a Lua engine with bindings to open Virdant projects/files,
//! create simulators, and control simulations via coroutines.
//!
//! Uses coroutines for suspension.
//! `sim.wait(clock)` yields the coroutine until the next rising edge of the clock.
//!
//! # Example
//!
//! ```lua
//! local db = open_file("mydesign.vir")
//! local sim = db:sim("pkg::Top")
//!
//! sim.attach_clock("top.clock", Clock.with_period_ps(1000))  -- 1ns period
//!
//! -- Coroutine-based testbench
//! sim.run(function()
//!     sim.set("top.reset", 1)
//!     sim.wait("top.clock")  -- Wait for clock edge
//!     sim.wait("top.clock")
//!     sim.set("top.reset", 0)
//!
//!     for i = 1, 10 do
//!         sim.wait("top.clock")
//!         print("Cycle " .. i .. ": counter = " .. sim.get("top.counter"))
//!     end
//!     sim.finish()
//! end)
//! ```

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use bstr::BStr;
use mlua::{Function, Lua, Result as LuaResult, Table, ThreadStatus, UserData, UserDataMethods, Value as LuaValue};

use crate::analysis::elaboration::SignalId;

/// A clock timing specification exposed to Lua scripts.
/// Mirrors the Rust [`crate::sim::Clock`] API.
#[derive(Clone)]
struct LuaClock {
    inner: crate::sim::Clock,
}

impl UserData for LuaClock {}
use crate::db::Db;
use crate::sim::{Sim, Value};
use crate::types::Type;
use crate::util::{check_db, db_from_dir, db_from_file};

/// Wrapper around `Db` for Lua scripting.
#[derive(Clone)]
struct LuaDb {
    db: Arc<Db>,
}

impl LuaDb {
    fn new(db: Db) -> Self {
        Self { db: Arc::new(db) }
    }
}

impl UserData for LuaDb {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("sim", |lua, this, top: String| {
            let symboltable = this.db.get_symboltable();
            let top_symbol = symboltable
                .resolve(<&BStr>::from(top.as_str()))
                .ok_or_else(|| mlua::Error::runtime(format!("Could not resolve top module: {}", top)))?;
            let sim = Sim::new(&this.db, top_symbol.id());
            create_sim_table(lua, sim, this.db.clone())
        });
    }
}

/// Create a Lua table representing a simulator with all methods bound.
fn create_sim_table(lua: &Lua, sim: Sim, db: Arc<Db>) -> LuaResult<Table> {
    let sim = Rc::new(RefCell::new(sim));
    let table = lua.create_table()?;

    // now() - get current simulation time
    let sim_clone = sim.clone();
    table.set("now", lua.create_function(move |_, ()| {
        Ok(sim_clone.borrow().now() as i64)
    })?)?;

    // get(path) - get signal value
    let sim_clone = sim.clone();
    table.set("get", lua.create_function(move |_, path: String| {
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        let value = sim.get(signal_id);
        Ok(value_to_lua(&value))
    })?)?;

    // set(path, value) - set signal value
    let sim_clone = sim.clone();
    let db_clone = db.clone();
    table.set("set", lua.create_function(move |_, (path, value): (String, LuaValue)| {
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        let typ = sim.component_type(signal_id);
        let val = lua_to_value(value, &typ, &db_clone);
        {
            let mut lock = sim.lock();
            lock.set(signal_id, val);
        }
        Ok(())
    })?)?;

    // attach_clock(path, clock) - attach a Clock to a signal
    let sim_clone = sim.clone();
    table.set("attach_clock", lua.create_function(move |_, (path, clock_ud): (String, mlua::AnyUserData)| {
        let clock = clock_ud.borrow::<LuaClock>()?;
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        sim.attach_clock(signal_id, clock.inner);
        Ok(())
    })?)?;

    // finish() - request simulation shutdown
    let sim_clone = sim.clone();
    table.set("finish", lua.create_function(move |_, ()| {
        sim_clone.borrow_mut().finish();
        Ok(())
    })?)?;

    // wait(clock) - yield the coroutine until the next rising edge of clock.
    // Defined in Lua so it can call coroutine.yield directly.
    let setup_wait = r#"
        return function(sim_table)
            sim_table.wait = function(clock)
                coroutine.yield(clock)
            end
        end
    "#;
    let setup_wait_fn: Function = lua.load(setup_wait).set_name("=sim.wait").eval()?;
    setup_wait_fn.call::<()>(table.clone())?;

    // run(coroutine_fn) - run simulation with coroutine support
    // The coroutine_fn closes over the sim table and can call sim.wait(clock) to yield.
    let sim_clone = sim.clone();
    table.set("run", lua.create_function(move |lua, func: Function| {
        run_with_coroutine(lua, sim_clone.clone(), func)
    })?)?;

    Ok(table)
}

/// A pending coroutine waiting for a clock edge.
struct PendingCoroutine {
    thread_key: mlua::RegistryKey,
    clock_signal: SignalId,
}

/// Run the simulation with a Lua coroutine.
///
/// The coroutine function closes over the `sim` table and can call `sim.wait(clock)`
/// to yield until the next rising edge of the specified clock signal.
fn run_with_coroutine(lua: &Lua, sim: Rc<RefCell<Sim>>, func: Function) -> LuaResult<()> {
    // Create the coroutine from the user function
    let thread = lua.create_thread(func)?;

    // Pending coroutines list
    let pending: Rc<RefCell<Vec<PendingCoroutine>>> = Rc::new(RefCell::new(Vec::new()));

    // Start the coroutine; the closure closes over sim, so no argument is needed.
    match thread.resume::<LuaValue>(()) {
        Ok(LuaValue::Nil) => {
            // Coroutine finished immediately
            return Ok(());
        }
        Ok(LuaValue::String(clock_path)) => {
            // Coroutine yielded waiting for a clock
            let clock_path_borrowed = clock_path.to_str()?;
            let clock_path_str: &str = clock_path_borrowed.as_ref();
            let clock_signal = sim.borrow_mut().signal(<&BStr>::from(clock_path_str));
            let thread_key = lua.create_registry_value(thread)?;
            pending.borrow_mut().push(PendingCoroutine {
                thread_key,
                clock_signal,
            });
        }
        Ok(_other) => {
            // Coroutine finished with a value (not yielded)
            return Ok(());
        }
        Err(e) => return Err(e),
    }

    // Now run the simulation with callbacks for pending coroutines
    // For each clock edge, we resume waiting coroutines
    while !pending.borrow().is_empty() {
        // Get all unique clocks we're waiting on
        let clocks: Vec<SignalId> = pending
            .borrow()
            .iter()
            .map(|p| p.clock_signal)
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();

        // Register a callback for each clock that stops the simulation
        // so we can resume coroutines
        for &clock in &clocks {
            sim.borrow_mut().on_clock(clock, Box::new(move |sim_ref: &mut Sim| {
                sim_ref.finish(); // Stop after this callback to return control
            }));
        }

        // Run until a clock callback fires
        let _ = sim.borrow_mut().run();

        // Resume all coroutines that were waiting for any clock
        // (In a more sophisticated implementation, we'd track which clock fired)
        let mut still_pending = Vec::new();
        for pc in pending.borrow_mut().drain(..) {
            let thread: mlua::Thread = lua.registry_value(&pc.thread_key)?;

            if thread.status() == ThreadStatus::Resumable {
                match thread.resume::<LuaValue>(()) {
                    Ok(LuaValue::Nil) => {
                        // Coroutine finished
                        lua.remove_registry_value(pc.thread_key)?;
                    }
                    Ok(LuaValue::String(clock_path)) => {
                        // Coroutine yielded again
                        let clock_path_borrowed = clock_path.to_str()?;
                        let clock_path_str: &str = clock_path_borrowed.as_ref();
                        let clock_signal = sim.borrow_mut().signal(<&BStr>::from(clock_path_str));
                        still_pending.push(PendingCoroutine {
                            thread_key: pc.thread_key,
                            clock_signal,
                        });
                    }
                    Ok(_other) => {
                        // Coroutine finished with a value
                        lua.remove_registry_value(pc.thread_key)?;
                    }
                    Err(e) => {
                        lua.remove_registry_value(pc.thread_key)?;
                        return Err(e);
                    }
                }
            } else {
                // Thread is not resumable (finished or errored)
                lua.remove_registry_value(pc.thread_key)?;
            }
        }

        *pending.borrow_mut() = still_pending;
    }

    Ok(())
}

/// Convert a Virdant Value to a Lua value.
fn value_to_lua(value: &Value) -> LuaValue {
    match value {
        Value::Bit(b) => LuaValue::Boolean(*b),
        Value::Word(_width, v) => LuaValue::Integer(*v as i64),
        // For X/Z/Ctor we return nil since we can't easily create strings without Lua context
        Value::X(_) | Value::Z(_) | Value::Ctor(_, _, _) => LuaValue::Nil,
    }
}

/// Convert a Lua value to a Virdant Value based on the expected type.
fn lua_to_value(value: LuaValue, typ: &Type, db: &Db) -> Value {
    match typ {
        Type::Bit | Type::Reset => {
            match value {
                LuaValue::Boolean(b) => Value::Bit(b),
                LuaValue::Integer(i) => Value::Bit(i != 0),
                LuaValue::Number(n) => Value::Bit(n != 0.0),
                _ => Value::X(typ.clone()),
            }
        }
        Type::Word(width) => {
            match value {
                LuaValue::Integer(i) => Value::Word(*width, i as u64),
                LuaValue::Number(n) => Value::Word(*width, n as u64),
                _ => Value::X(typ.clone()),
            }
        }
        Type::Usual(symbol_id) => {
            let typedef = db.get_typedef(*symbol_id);
            if typedef.kind == crate::common::TypeScheme::EnumDef {
                let tag: u64 = match value {
                    LuaValue::Integer(i) => i as u64,
                    LuaValue::Number(n) => n as u64,
                    _ => return Value::X(typ.clone()),
                };
                for (enumerant_id, enumerant_val) in &typedef.enumerant_values {
                    if *enumerant_val == tag {
                        return Value::Ctor(typ.clone(), *enumerant_id, vec![]);
                    }
                }
                Value::X(typ.clone())
            } else {
                Value::X(typ.clone())
            }
        }
        _ => Value::X(typ.clone()),
    }
}

/// Open a single .vir file and return a LuaDb.
fn open_file(_lua: &Lua, path: String) -> LuaResult<LuaDb> {
    let db = db_from_file(std::path::Path::new(&path));
    validate_db(&db)?;
    Ok(LuaDb::new(db))
}

/// Open a project directory (containing src/) and return a LuaDb.
fn open_dir(_lua: &Lua, path: String) -> LuaResult<LuaDb> {
    let src_dir = std::path::Path::new(&path).join("src");
    if !src_dir.exists() {
        return Err(mlua::Error::runtime(format!("No src/ directory found in {}", path)));
    }
    let db = db_from_dir(src_dir);
    validate_db(&db)?;
    Ok(LuaDb::new(db))
}

/// Validate a Db and return an error if there are diagnostics.
fn validate_db(db: &Db) -> LuaResult<()> {
    if let Err(diagnostics) = check_db(db) {
        let msg = diagnostics
            .iter()
            .map(|d| format!("{:?}", d))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(mlua::Error::runtime(format!("Virdant diagnostics:\n{}", msg)));
    }
    Ok(())
}

/// Run a Lua script file with full simulation support.
pub fn run_script_file(path: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    let script = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read script: {}", e))?;

    let lua = Lua::new();

    // Register global functions
    {
        let globals = lua.globals();

        // open_file(path) -> Db
        globals.set("open_file", lua.create_function(|lua, path: String| {
            open_file(lua, path)
        })?)?;

        // open_dir(path) -> Db
        globals.set("open_dir", lua.create_function(|lua, path: String| {
            open_dir(lua, path)
        })?)?;

        // Clock table: Clock.with_period_ps(period_ps) / Clock.with_freq_hz(freq_hz)
        let clock_table = lua.create_table()?;
        clock_table.set("with_period_ps", lua.create_function(|_, period_ps: i64| {
            Ok(LuaClock { inner: crate::sim::Clock::with_period_ps(period_ps as u64) })
        })?)?;
        clock_table.set("with_freq_hz", lua.create_function(|_, freq_hz: i64| {
            Ok(LuaClock { inner: crate::sim::Clock::with_freq_hz(freq_hz as u64) })
        })?)?;
        globals.set("Clock", clock_table)?;
    }

    // Execute the script.
    // The "@" prefix tells Lua this chunk name is a file path,
    // so error messages show "foo.lua:5" instead of "[string \"...\"]:5".
    let chunk_name = format!("@{}", path.display());
    lua.load(&script).set_name(chunk_name).exec()?;

    Ok(())
}
