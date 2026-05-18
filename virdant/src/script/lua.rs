//! Lua scripting support for Virdant simulation.
//!
//! Provides a Lua engine with bindings to open Virdant projects/files,
//! create simulators, and control simulations via coroutines.
//!
//! The key difference from Rhai is that Lua uses coroutines for suspension.
//! `sim.wait(clock)` yields the coroutine until the next rising edge of the clock.
//!
//! # Example
//!
//! ```lua
//! local db = open_file("mydesign.vir")
//! local sim = db:sim("pkg::Top")
//!
//! sim.add_clock("top.clock", 1000)  -- 1ns period
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
            let sim = Sim::new(Arc::clone(&this.db), top_symbol.id());
            create_sim_table(lua, sim)
        });
    }
}

/// Create a Lua table representing a simulator with all methods bound.
fn create_sim_table(lua: &Lua, sim: Sim) -> LuaResult<Table> {
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
    table.set("set", lua.create_function(move |_, (path, value): (String, LuaValue)| {
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        let typ = sim.component_type(signal_id);
        let val = lua_to_value(value, &typ);
        sim.set(signal_id, val);
        Ok(())
    })?)?;

    // add_clock(path, period_ps) - add a clock with given period
    let sim_clone = sim.clone();
    table.set("add_clock", lua.create_function(move |_, (path, period_ps): (String, i64)| {
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        sim.add_clock(signal_id, period_ps as u64);
        Ok(())
    })?)?;

    // add_clock_hz(path, freq_hz) - add a clock with given frequency
    let sim_clone = sim.clone();
    table.set("add_clock_hz", lua.create_function(move |_, (path, freq_hz): (String, i64)| {
        let mut sim = sim_clone.borrow_mut();
        let signal_id = sim.signal(<&BStr>::from(path.as_str()));
        sim.add_clock_hz(signal_id, freq_hz as u64);
        Ok(())
    })?)?;

    // finish() - request simulation shutdown
    let sim_clone = sim.clone();
    table.set("finish", lua.create_function(move |_, ()| {
        sim_clone.borrow_mut().finish();
        Ok(())
    })?)?;

//    // dump() - dump simulation state
//    let sim_clone = sim.clone();
//    table.set("dump", lua.create_function(move |_, ()| {
//        sim_clone.borrow().dump();
//        Ok(())
//    })?)?;

    // wait(clock) - yield the coroutine until the next rising edge of clock.
    // Defined in Lua so it can call coroutine.yield directly.
    let setup_wait = r#"
        return function(sim_table)
            sim_table.wait = function(clock)
                coroutine.yield(clock)
            end
        end
    "#;
    let setup_wait_fn: Function = lua.load(setup_wait).eval()?;
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
fn lua_to_value(value: LuaValue, typ: &Type) -> Value {
    match typ {
        Type::Bit => {
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
    }

    // Execute the script
    lua.load(&script).exec()?;

    Ok(())
}
