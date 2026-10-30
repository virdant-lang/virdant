//! The wasm backend of the Virdant web demo.
//!
//! Exposes two operations to the JS SPA:
//!
//!   - [`check`]: compile editor buffers (linked against the `lib/`
//!     packages embedded at build time) and return the diagnostics as
//!     JSON — the live-checking path behind the error report tab;
//!   - [`VirtVm`]: the `virt::Virt` board (see [`virt`], ported from
//!     `vir/src/vir/virt.rs`) on top of the `Sim` engine, driven in
//!     bounded chunks (`step`) from a JS rAF loop that paces the
//!     simulated clock against wall time.

mod virt;

use std::rc::Rc;

use bstr::{BString, ByteSlice as _};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

use virdant::analysis::platform;
use virdant::analysis::symbols::SymbolKind;
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::sim::{Clock, Sim, Value};

// The `lib/` packages the demo links every design against, embedded
// at build time (the native `db_from_*` constructors load the `lib/`
// directory from disk; wasm has no filesystem).
const LIB_BUILTIN: &str = include_str!("../../../lib/builtin.vir");
const LIB_ICE40: &str = include_str!("../../../lib/ice40.vir");
const LIB_VIRT: &str = include_str!("../../../lib/virt.vir");
const LIB_XILINX: &str = include_str!("../../../lib/xilinx.vir");

// ─────────────────────────────────────────────────────────────
// Sources / diagnostics JSON
// ─────────────────────────────────────────────────────────────

#[derive(Deserialize)]
pub struct Sources {
    pub files: Vec<SourceEntry>,
}

#[derive(Deserialize)]
pub struct SourceEntry {
    pub name: String,
    pub text: String,
}

#[derive(Serialize)]
pub struct CheckResponse {
    pub ok: bool,
    pub diagnostics: Vec<DiagJson>,
}

#[derive(Serialize)]
pub struct DiagJson {
    pub file: String,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub level: String,
    pub message: String,
}

/// Parse the JSON sources payload into (package name, text) pairs.
/// The package name of a source file is its stem, mirroring how the
/// native loader derives package names from filenames.
fn parse_sources(json: &str) -> Result<Vec<(BString, BString)>, String> {
    let sources: Sources = serde_json::from_str(json)
        .map_err(|e| format!("bad sources payload: {e}"))?;
    let mut files = Vec::new();
    for entry in sources.files {
        let name = entry.name.trim().trim_end_matches(".vir");
        if name.is_empty() {
            return Err("source file has an empty name".to_string());
        }
        files.push((BString::from(name.as_bytes()), BString::from(entry.text.as_bytes())));
    }
    Ok(files)
}

/// Build a `Db` from editor buffers plus the embedded `lib/` packages
/// (the wasm counterpart of `db_from_files_with_lib`).
fn build_db(mut files: Vec<(BString, BString)>) -> Db {
    let mut all: Vec<(BString, BString)> = vec![
        (BString::from("builtin"), BString::from(LIB_BUILTIN)),
        (BString::from("ice40"), BString::from(LIB_ICE40)),
        (BString::from("virt"), BString::from(LIB_VIRT)),
        (BString::from("xilinx"), BString::from(LIB_XILINX)),
    ];
    all.append(&mut files);
    virdant::util::db_from_mem(all)
}

/// Compile the given sources (linked against `lib/`) and serialize
/// every diagnostic.  Returns a JSON `CheckResponse`.
#[wasm_bindgen]
pub fn check(sources: &str) -> Result<String, String> {
    console_error_panic_hook::set_once();
    let files = parse_sources(sources)?;
    let db = build_db(files);
    let diagnostics = match virdant::util::check_db(&db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };

    let packages = db.get_packages();
    let diags = diagnostics.iter().map(|diagnostic| {
        let region = diagnostic.region();
        let span = region.span();
        let (start, end) = (span.start(), span.end());
        DiagJson {
            file: packages.name(region.package()).to_str_lossy().into_owned(),
            line: start.line(),
            col: start.col(),
            end_line: end.line(),
            end_col: end.col(),
            level: match diagnostic.level() {
                DiagnosticLevel::Error => "error",
                DiagnosticLevel::Warning => "warning",
                DiagnosticLevel::Info => "info",
            }.to_string(),
            message: diagnostic.message().to_string(),
        }
    }).collect::<Vec<_>>();

    let ok = !diags.iter().any(|d| d.level == "error");
    serde_json::to_string(&CheckResponse { ok, diagnostics: diags })
        .map_err(|e| format!("could not serialize diagnostics: {e}"))
}

// ─────────────────────────────────────────────────────────────
// The Virt VM
// ─────────────────────────────────────────────────────────────

/// Board state shared between the sim callbacks and the JS API
/// (`virt::BoardShared`).
type Shared = virt::BoardShared;

/// A running `virt::Virt` simulation.  The JS driver paces it against
/// wall time by repeatedly calling [`VirtVm::step`] from a rAF loop.
#[wasm_bindgen]
pub struct VirtVm {
    sim: Sim,
    period_ps: u64,
    clock_hz: u64,
    shared: Rc<Shared>,
    finished: bool,
}

#[wasm_bindgen]
impl VirtVm {
    /// Compile the sources, resolve the top module and its platform,
    /// elaborate the design, and wire up the board.  On failure the
    /// error is either a diagnostics JSON array (compile errors) or a
    /// plain message string.
    pub fn create(sources: &str, top: &str) -> Result<VirtVm, String> {
        console_error_panic_hook::set_once();
        let files = parse_sources(sources)?;
        let db = build_db(files);

        if let Err(diags) = virdant::util::check_db(&db) {
            return Err(render_diagnostics(&db, &diags));
        }

        let symboltable = db.get_symboltable();
        let top_symbol = symboltable
            .items()
            .into_iter()
            .find(|sym| sym.fqn.as_bytes() == top.as_bytes() && sym.kind == SymbolKind::ModDef)
            .ok_or_else(|| format!("Top module '{top}' not found"))?;

        // The top module's `for` clause selects the platform; the web
        // VM simulates the `virt::Virt` board only, like `vir run`.
        let mut builder = virdant::db::Builder::new(&db);
        let platform_id = match platform::resolve_platform_for(&mut builder, top_symbol.id()) {
            Ok(Some(platform_id)) => platform_id,
            Ok(None) => {
                return Err(format!("Top module '{top}' does not implement a platform (no `for` clause)"));
            }
            Err(diagnostic) => {
                return Err(diagnostic.message().to_string());
            }
        };
        let platform_symbol = symboltable.symbol(platform_id);
        let platform_name = platform_symbol.name().to_string();
        let fpga = platform::platform_fpga(&mut builder, platform_id);
        let fpga_matches = fpga.as_deref().map(|f| f.as_bstr()) == Some(b"virt".as_bstr());
        if platform_name != "Virt" || !fpga_matches {
            return Err(format!(
                "Top module '{top}' implements platform '{platform_name}', but the web demo simulates virt::Virt only"
            ));
        }

        // The clock runs at the frequency the platform declares (@period_ns).
        let clocks = platform::platform_clock_meta(&mut builder, platform_id);
        let Some((_, _, Some(period_ns))) = clocks.into_iter().next() else {
            return Err(format!("Platform '{platform_name}' declares no clock"));
        };
        let period_ps = (period_ns * 1000.0).round() as u64;
        let clock_hz = (1e9 / period_ns) as u64;
        let mut sim = Sim::new(&db, top_symbol.id(), Default::default());
        virdant::sim::builtins::attach(&mut sim, &db, false);

        let port = |sim: &Sim, name: &str| -> Result<virdant::analysis::elaboration::SignalId, String> {
            let path = format!("top.{name}");
            sim.try_resolve(path.as_bytes())
                .ok_or_else(|| format!("the top module has no `{name}` port (virt::Virt requires it)"))
        };
        let clock = port(&sim, virt::PORT_CLOCK)?;
        let tx = port(&sim, virt::PORT_TX)?;
        let rx = port(&sim, virt::PORT_RX)?;
        let exit = port(&sim, virt::PORT_EXIT)?;
        let spi_ck = port(&sim, virt::PORT_SPI_CK)?;
        let spi_cs = port(&sim, virt::PORT_SPI_CS)?;
        let spi_mosi = port(&sim, virt::PORT_SPI_MOSI)?;
        let spi_miso = port(&sim, virt::PORT_SPI_MISO)?;

        let shared: Rc<virt::BoardShared> = virt::BoardShared::new();

        // Start inputs in a sane state: UART idle high, flash idle low.
        {
            let rx_typ = sim.component_type_id(rx);
            let miso_typ = sim.component_type_id(spi_miso);
            let mut lock = sim.lock();
            lock.set(rx, Value::from_bool(rx_typ, true));
            lock.set(spi_miso, Value::from_bool(miso_typ, false));
        }

        sim.attach_clock(clock, Clock::with_period_ps(period_ps));

        // Step the SPI flash on EVERY clock edge (rising and falling),
        // registered before the on_clock watcher so the flash samples
        // each edge before the per-cycle board logic runs.  (See
        // `vir/src/vir/virt.rs`.)
        let mut flash_stepper = virt::FlashStepper {
            spi_ck,
            spi_cs,
            spi_mosi,
            spi_miso,
            flash: virt::SpiFlash::new(),
        };
        sim.on_change(clock, Box::new(move |sim| {
            flash_stepper.step(sim);
        }));

        let per_cycle = std::cell::RefCell::new(virt::PerCycle {
            tx,
            rx,
            exit,
            // Idle-high gate for auto-baud: ten nominal bit periods at
            // the platform-declared clock.
            uart_tx: virt::UartTx::new(10 * clock_hz / virt::BAUD),
            uart_rx: virt::UartRx::new(&virt::baud_timing(clock_hz, virt::BAUD)),
            prev_exit: None,
            cycle_count: 0,
            shared: shared.clone(),
        });
        sim.on_clock(clock, Box::new(move |sim| {
            per_cycle.borrow_mut().cycle(sim);
        }));

        Ok(VirtVm {
            sim,
            period_ps,
            clock_hz,
            shared,
            finished: false,
        })
    }

    /// Run up to `cycles` further clock cycles (fewer if the design
    /// exits).  Returns a JSON status:
    /// `{"running": bool, "exit": number|null, "cycles": number}`.
    pub fn step(&mut self, cycles: u64) -> Result<JsValue, JsValue> {
        if !self.finished {
            let end_ps = self.sim.now().saturating_add(cycles.saturating_mul(self.period_ps));
            match self.sim.run_until(end_ps) {
                Ok(()) => {}
            }
            if let Some(code) = self.shared.exit.get() {
                self.finished = true;
                let _ = code;
            }
        }
        Ok(serde_json::to_string(&StatusJson {
            running: !self.finished,
            exit: self.shared.exit.get().map(|c| c as u64),
            cycles: self.shared.cycles.get(),
        })
        .unwrap()
        .into())
    }

    /// Drain the UART TX output (design → console), as raw bytes.
    pub fn take_output(&mut self) -> Vec<u8> {
        std::mem::take(&mut *self.shared.tx_out.borrow_mut())
    }

    /// Queue bytes to transmit into the design's `rx` line
    /// (console → design).  No effect while `finished`.
    pub fn send_input(&mut self, bytes: &[u8]) {
        if !self.finished {
            self.shared.rx_in.borrow_mut().extend(bytes.iter().copied());
        }
    }

    /// Clock cycles simulated so far — what the JS driver paces.
    pub fn cycles(&self) -> u64 {
        self.shared.cycles.get()
    }

    /// The platform clock frequency in Hz (for the JS pacer).
    pub fn clock_hz(&self) -> u64 {
        self.clock_hz
    }

    /// Whether the simulation has ended (design exit).
    pub fn is_finished(&self) -> bool {
        self.finished
    }

    /// The exit code requested by the design, if it has exited.
    pub fn exit_code(&self) -> Option<u8> {
        self.shared.exit.get()
    }
}

#[derive(Serialize)]
struct StatusJson {
    running: bool,
    exit: Option<u64>,
    cycles: u64,
}

/// Render diagnostics the way the native CLI does (one line per
/// diagnostic), for use as an error string.
fn render_diagnostics(db: &Db, diagnostics: &[virdant::diagnostics::Diagnostic]) -> String {
    let packages = db.get_packages();
    let mut out = String::new();
    for diagnostic in diagnostics {
        let region = diagnostic.region();
        out.push_str(&format!(
            "{}   {}.vir {}   {}\n",
            match diagnostic.level() {
                DiagnosticLevel::Error => "ERROR",
                DiagnosticLevel::Warning => "WARNING",
                DiagnosticLevel::Info => "INFO",
            },
            packages.name(region.package()).to_str_lossy(),
            region.span(),
            diagnostic.message(),
        ));
    }
    if out.is_empty() {
        out.push_str("Check failed\n");
    }
    out
}
