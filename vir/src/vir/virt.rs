//! The `virt::Virt` board, simulated in Rust on top of the [`Sim`]
//! engine.
//!
//! This is the Rust twin of `virdant-virt/assets/sim.cpp` (the
//! Verilator harness): a board that presents the `virt::Virt` platform
//! contract to the design —
//!
//!   - UART at the configured baud (default 115200), 8N1, wired to
//!     stdin/stdout;
//!   - a 16 MiB SPI flash device (initially all `0xFF`), readable with
//!     the standard `0x03` READ command, continuously, with
//!     auto-incrementing address until CS deasserts;
//!   - the `exit : Valid[Word[8]]` port: while `@Invalid()`, the
//!     simulation runs; a `@Valid(n)` read on a clock tick ends the
//!     run with exit code `n`.
//!
//! The clock runs at the frequency the platform declares
//! (`@period_ns`).  The run is paced to real time unless
//! `--fps-no-cap` is given.
//!
//! Lives in the `vir` crate (not `virdant`): it is a Unix-only harness
//! (raw-mode terminal, SIGINT handling), so the compiler library stays
//! free of `nix`.  It touches the sim engine only through its public
//! API, including `Sim::decode_ctor` for the `exit` port.

use std::cell::Cell;
use std::io::{Read as _, Write as _};
use std::os::fd::BorrowedFd;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{self, TryRecvError};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use virdant::analysis::elaboration::SignalId;
use virdant::analysis::symbols::SymbolId;
use virdant::db::Db;
use virdant::sim::{Clock, Sim, Value};

/// Options for [`run`], straight from the `vir run` CLI.
#[derive(Debug, Default)]
pub struct VirtOptions {
    /// Preload the SPI flash image (up to 16 MiB).
    pub flash: Option<PathBuf>,
    /// Write a VCD trace to this file (defaults to `trace.vcd`).
    pub trace: Option<PathBuf>,
    /// Disable the real-time (FPS) pace cap.
    pub fps_no_cap: bool,
    /// Stop after this many emulated clock cycles (exit code 124).
    pub cycles: Option<u64>,
    /// On exit, report the average clock frequency achieved (MHz).
    pub fps: bool,
    /// Print informational board messages (builtin attachments, flash
    /// load).  Errors print regardless.
    pub verbose: bool,
    /// UART baud rate (8N1).  Resolved before the board starts:
    /// `--baud` on the command line, else `[virt].baud` in
    /// `virdant.toml`, else 115200 — and validated against the
    /// platform's declared clock (at least one bit per two cycles).
    /// The design must clock its UART bits at `clock_hz / baud` cycles
    /// per bit in both directions.
    pub baud: u64,
}

const FLASH_SIZE: u64 = 1 << 24; // 16 MiB

// Port names required by the `virt::Virt` platform contract.
const PORT_CLOCK: &str = "clock";
const PORT_TX: &str = "tx";
const PORT_RX: &str = "rx";
const PORT_EXIT: &str = "exit";
const PORT_SPI_CK: &str = "spi_ck";
const PORT_SPI_CS: &str = "spi_cs";
const PORT_SPI_MOSI: &str = "spi_mosi";
const PORT_SPI_MISO: &str = "spi_miso";

/// Run the `virt::Virt` board for the elaborated design rooted at
/// `top`.  Returns the process exit code.
pub fn run(db: &Db, top: SymbolId, clock_hz: u64, opts: VirtOptions) -> i32 {
    let mut sim = Sim::new(db, top, Default::default());
    virdant::sim::builtins::attach(&mut sim, db, opts.verbose);

    let port = |sim: &Sim, name: &str| -> SignalId {
        let path = format!("top.{name}");
        match sim.try_resolve(path.as_bytes()) {
            Some(id) => id,
            None => {
                eprintln!("virt: the top module has no `{name}` port (virt::Virt requires it)");
                std::process::exit(1);
            }
        }
    };
    let clock = port(&sim, PORT_CLOCK);
    let tx = port(&sim, PORT_TX);
    let rx = port(&sim, PORT_RX);
    let exit = port(&sim, PORT_EXIT);
    let spi_ck = port(&sim, PORT_SPI_CK);
    let spi_cs = port(&sim, PORT_SPI_CS);
    let spi_mosi = port(&sim, PORT_SPI_MOSI);
    let spi_miso = port(&sim, PORT_SPI_MISO);

    if let Some(path) = &opts.trace {
        let file = std::fs::File::create(path)
            .unwrap_or_else(|e| {
                eprintln!("virt: cannot create trace file {}: {e}", path.display());
                std::process::exit(1);
            });
        sim.attach_vcd(file);
    }

    // Start inputs in a sane state: UART idle high, flash idle low.
    {
        let rx_typ = sim.component_type_id(rx);
        let miso_typ = sim.component_type_id(spi_miso);
        let mut lock = sim.lock();
        lock.set(rx, Value::from_bool(rx_typ, true));
        lock.set(spi_miso, Value::from_bool(miso_typ, false));
    }

    sim.attach_clock(clock, Clock::with_freq_hz(clock_hz));

    // Non-blocking stdin: a reader thread pushes bytes through a
    // channel; the per-cycle logic drains it.
    let (sender, mut receiver) = mpsc::channel::<u8>();
    std::thread::spawn(move || {
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        let mut buf = [0u8; 256];
        loop {
            match stdin.read(&mut buf) {
                Ok(0) | Err(_) => {
                    break;
                }
                Ok(n) => {
                    for b in &buf[..n] {
                        if sender.send(*b).is_err() {
                            return;
                        }
                    }
                }
            }
        }
    });

    let start = Instant::now();
    FPS_START.lock().unwrap().replace(start);
    FPS_REPORT.store(opts.fps, Ordering::Relaxed);
    setup_terminal();

    let exit_code: Rc<Cell<Option<i32>>> = Rc::new(Cell::new(None));
    let cycles: Rc<Cell<u64>> = Rc::new(Cell::new(0));
    let max_cycles = opts.cycles.unwrap_or(u64::MAX);
    let fps_no_cap = opts.fps_no_cap;
    let mut flash_stepper = FlashStepper {
        spi_ck,
        spi_cs,
        spi_mosi,
        spi_miso,
        flash: SpiFlash::new(opts.flash.as_ref(), opts.verbose),
    };
    // The UART bit period is explicit configuration, not something
    // the board measures: `clock_hz / baud`, as an exact rational
    // (see `BaudTiming`).  Both the TX decoder and the RX transmitter
    // run at it from the first cycle.
    let uart_timing = baud_timing(clock_hz, opts.baud);
    let mut per_cycle = PerCycle {
        tx,
        rx,
        exit,
        uart_rx: UartRx::new(&uart_timing),
        uart_tx: UartTx::new(uart_timing),
        prev_exit: None,
        cycles: cycles.clone(),
        max_cycles,
        exit_code: exit_code.clone(),
    };

    // Step the SPI flash on EVERY clock edge (rising and falling):
    // the design's spi_ck can toggle on either board edge, and the
    // C++ harness steps its flash model after every eval.  Registered
    // before the on_clock watcher so the flash samples each edge
    // before the per-cycle board logic runs.
    sim.on_change(clock, Box::new(move |sim| {
        flash_stepper.step(sim);
    }));
    sim.on_clock(clock, Box::new(move |sim| {
        per_cycle.cycle(sim, &mut receiver, &start, fps_no_cap);
    }));

    let _ = sim.run();
    restore_term();
    report_fps();

    // Distinct status for hitting the cycle cap without an exit
    // request, so test drivers can tell a hang (124) from a clean
    // run (the design's requested code).
    match exit_code.get() {
        Some(code) => code,
        None if cycles.get() >= max_cycles => 124,
        None => 0,
    }
}

// ─────────────────────────────────────────────────────────────
// Per-cycle board logic (fired on each rising edge of the
// platform clock, after the design has settled)
// ─────────────────────────────────────────────────────────────

/// SPI-flash sampling: runs on EVERY clock edge (its own watcher,
/// its own state — never shares a borrow with the per-cycle logic).
struct FlashStepper {
    spi_ck: SignalId,
    spi_cs: SignalId,
    spi_mosi: SignalId,
    spi_miso: SignalId,
    flash: SpiFlash,
}

impl FlashStepper {
    /// One sampling point with the post-edge pin values, mirroring
    /// the C++ harness stepping its flash model after every eval.
    /// Drives `spi_miso` from the model's response.
    fn step(&mut self, sim: &mut Sim) {
        let bit = |sim: &Sim, id: SignalId| -> bool {
            sim.get(id).get_bit(0)
        };
        let cs = bit(sim, self.spi_cs);
        let ck = bit(sim, self.spi_ck);
        let mosi = bit(sim, self.spi_mosi);
        self.flash.step(cs, ck, mosi);
        let miso = if self.flash.miso_active() { self.flash.miso } else { false };
        // spi_miso only actually changes when the flash is actively
        // driving it; skip the lock()/unlock() watcher-sweep churn
        // (flow + queue scan) when the driven bit is unchanged.
        if bit(sim, self.spi_miso) != miso {
            let miso_typ = sim.component_type_id(self.spi_miso);
            let mut lock = sim.lock();
            lock.set(self.spi_miso, Value::from_bool(miso_typ, miso));
        }
    }
}

struct PerCycle {
    tx: SignalId,
    rx: SignalId,
    exit: SignalId,
    uart_tx: UartTx,
    uart_rx: UartRx,
    /// Last seen `exit`-port value; `decode_exit` is a pure function
    /// of the value, so it only needs to rerun when it changes.
    prev_exit: Option<Value>,
    cycles: Rc<Cell<u64>>,
    max_cycles: u64,
    exit_code: Rc<Cell<Option<i32>>>,
}

impl PerCycle {
    fn cycle(&mut self, sim: &mut Sim, receiver: &mut mpsc::Receiver<u8>, start: &Instant, fps_no_cap: bool) {
        // Real-time pacing ("FPS lock"): simulated time should
        // track wall time.
        if !fps_no_cap {
            let target_nanos: u128 = (sim.now() as u128) / 1_000; // ps → ns
            let elapsed = start.elapsed().as_nanos();
            if target_nanos > elapsed + 1_000_000 {
                let sleep = Duration::from_nanos((target_nanos - elapsed) as u64);
                std::thread::sleep(sleep);
            }
        }

        let bit = |sim: &Sim, id: SignalId| -> bool {
            sim.get(id).get_bit(0)
        };
        let tx = bit(sim, self.tx);

        self.uart_tx.cycle(tx);
        if self.cycles.get() % 1000 == 0 {
            self.uart_rx.poll_stdin(receiver);
        }
        self.uart_rx.cycle(receiver);

        // Drive the UART input for the next cycle (the design samples
        // it on the next rising edge).  spi_miso is driven by the
        // flash stepper, which runs on every clock edge.
        // uart_rx.pin only changes during UART start/stop bit
        // transitions; skip the lock()/unlock() watcher-sweep churn
        // when the driven bit is unchanged.
        if bit(sim, self.rx) != self.uart_rx.pin {
            let rx_typ = sim.component_type_id(self.rx);
            let mut lock = sim.lock();
            lock.set(self.rx, Value::from_bool(rx_typ, self.uart_rx.pin));
        }


        // Ctrl-C: shut down gracefully, flushing any pending UART
        // output (a completed frame not yet emitted, or a byte still
        // on the wire, which `flush` drops if it fails validation).
        if SIGINT_REQUESTED.load(Ordering::Relaxed) {
            self.uart_tx.flush();
            sim.finish();
            return;
        }

        // Exit request (virt::Virt `exit` port): a @Valid(n) on a
        // clock tick ends the simulation with exit code n.  The
        // decode allocates (heap `BitPat` + layout slot clones), so
        // skip it unless the port's raw value changed since the
        // last cycle — it stays `@Invalid()` for virtually the
        // whole run.
        let exit_value = sim.get(self.exit);
        if self.prev_exit.as_ref() != Some(&exit_value) {
            if let Some(code) = decode_exit(sim, &exit_value) {
                self.uart_tx.flush();
                self.exit_code.set(Some(code));
                sim.finish();
            }
            self.prev_exit = Some(exit_value);
        }

        self.cycles.set(self.cycles.get() + 1);
        FPS_CYCLES.store(self.cycles.get(), Ordering::Relaxed);
        if self.cycles.get() >= self.max_cycles {
            self.uart_tx.flush();
            sim.finish();
        }
    }

}

/// Decode the `exit : Valid[Word[8]]` value.  The Valid slot's
/// payload (the `Word[8]`) is the requested exit code.
fn decode_exit(sim: &Sim, value: &Value) -> Option<i32> {
    let ctor = sim.decode_ctor(value)?;
    if ctor.name != "Valid" {
        return None;
    }
    let code = ctor.payloads.first()?.to_u64();
    Some(code as u8 as i32)
}

// ─────────────────────────────────────────────────────────────
// SPI flash model (mode 0, READ 0x03 with auto-increment)
// ─────────────────────────────────────────────────────────────

struct SpiFlash {
    mem: Vec<u8>,
    state: FlashState,
    prev_cs: bool,
    prev_ck: bool,
    prev_mosi: bool,
    cmd: u8,
    cmd_bits: u32,
    addr: u32,
    addr_bits: u32,
    tx_byte: u8,
    tx_bits: u32,
    miso: bool,
}

#[derive(PartialEq)]
enum FlashState { Idle, Cmd, Addr, Data, Ignore }

impl SpiFlash {
    fn new(flash_path: Option<&PathBuf>, verbose: bool) -> SpiFlash {
        let mut mem = vec![0xFF; FLASH_SIZE as usize];
        if let Some(path) = flash_path {
            match std::fs::read(path) {
                Ok(bytes) => {
                    let n = bytes.len().min(mem.len());
                    mem[..n].copy_from_slice(&bytes[..n]);
                    if verbose {
                        eprintln!("virt: loaded {n} bytes from {}", path.display());
                    }
                }
                Err(e) => {
                    eprintln!("virt: cannot open flash image {}: {e}", path.display());
                    std::process::exit(1);
                }
            }
        }
        SpiFlash {
            mem,
            state: FlashState::Idle,
            prev_cs: true,
            prev_ck: false,
            prev_mosi: false,
            cmd: 0,
            cmd_bits: 0,
            addr: 0,
            addr_bits: 0,
            tx_byte: 0,
            tx_bits: 0,
            miso: false,
        }
    }

    /// One sampling point per clock cycle, with the post-edge pin
    /// values.  Matches Verilog NBA semantics like the C++ harness:
    /// edges are detected against the previous cycle's pins, and a
    /// rising CK samples the *pre-edge* `mosi`.
    fn step(&mut self, cs: bool, ck: bool, mosi: bool) {
        let cs_fell = self.prev_cs && !cs;
        let cs_rose = !self.prev_cs && cs;
        let ck_rose = !self.prev_ck && ck;
        let ck_fell = self.prev_ck && !ck;
        let active = !self.prev_cs; // cs was low before this edge

        if cs_fell {
            self.state = FlashState::Cmd;
            self.cmd = 0; self.cmd_bits = 0;
            self.addr = 0; self.addr_bits = 0;
            self.tx_bits = 0;
            self.miso = false;
        }
        if cs_rose {
            self.state = FlashState::Idle;
            self.miso = false;
        }

        if active {
            if ck_rose {
                let bit = self.prev_mosi; // sample pre-edge mosi (NBA semantics)
                match self.state {
                    FlashState::Cmd => {
                        self.cmd = (self.cmd << 1) | (bit as u8);
                        self.cmd_bits += 1;
                        if self.cmd_bits == 8 {
                            if self.cmd == 0x03 { // READ
                                self.state = FlashState::Addr;
                                self.addr = 0; self.addr_bits = 0;
                            } else {
                                // Only READ is emulated; ignore until CS releases.
                                self.state = FlashState::Ignore;
                            }
                        }
                    }
                    FlashState::Addr => {
                        self.addr = (self.addr << 1) | (bit as u32);
                        self.addr_bits += 1;
                        if self.addr_bits == 24 {
                            let a = self.addr & (FLASH_SIZE as u32 - 1);
                            self.tx_byte = self.mem[a as usize];
                            self.tx_bits = 0;
                            self.state = FlashState::Data;
                        }
                    }
                    _ => {}
                }
            }
            if ck_fell && self.state == FlashState::Data {
                self.miso = (self.tx_byte >> (7 - self.tx_bits)) & 1 != 0;
                self.tx_bits += 1;
                if self.tx_bits == 8 {
                    self.tx_bits = 0;
                    self.addr = (self.addr + 1) & (FLASH_SIZE as u32 - 1); // auto-increment, wraps
                    self.tx_byte = self.mem[self.addr as usize];
                }
            }
        }

        self.prev_cs = cs;
        self.prev_ck = ck;
        self.prev_mosi = mosi;
    }

    fn miso_active(&self) -> bool {
        self.state == FlashState::Data
    }
}

// ─────────────────────────────────────────────────────────────
// UART emulation
// ─────────────────────────────────────────────────────────────

/// Baud timing as an exact rational: `clock_hz / baud = p / q`
/// cycles per bit.  Counting time in ticks of 1/q cycle makes every
/// sample point an exact integer.
struct BaudTiming {
    ticks_per_cycle: u64,
    ticks_per_bit: u64,
}

fn baud_timing(clock_hz: u64, baud: u64) -> BaudTiming {
    // cycles_per_bit = clock_hz / baud = p / q (reduced).  Counting
    // time in ticks of 1/q cycle: q ticks per cycle, p per bit.
    let g = gcd(clock_hz, baud);
    BaudTiming {
        ticks_per_cycle: baud / g,
        ticks_per_bit: clock_hz / g,
    }
}

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a
}

/// TX: decode the design's transmission on `tx` (design → host stdout)
/// at the explicitly configured baud.
///
/// The bit period is `clock_hz / baud`, fixed by configuration
/// (`--baud` / `[virt].baud`; see `VirtOptions::baud`) — nothing is
/// measured and nothing is gated.  The contract on designs is
/// explicit: a design targeting `virt::Virt` must clock its UART bits
/// at `clock_hz / baud` cycles per bit, the same divisor in both
/// directions.  A design using a different divisor fails the frame
/// validation below and its bytes are dropped; run with a matching
/// `--baud` instead.
///
/// Frames are decoded retrospectively: transitions are buffered from
/// each start-bit falling edge until the frame closes — either when a
/// falling edge arrives past the stop-bit sample point (that fall is
/// the next start bit; anything sooner is a data-bit transition and
/// belongs to this frame), or when ten bit periods elapse with no new
/// start bit — then sampled at mid-bit points.  A frame only produces
/// a byte if the start/stop framing is valid and every recorded
/// transition is consistent with the decoded byte at the configured
/// period; otherwise it is dropped.
///
/// That drop rule is what makes power-up safe without a gate: the sim
/// starts every register at a random value, so `tx` can glitch low
/// for a few cycles before the design's reset settles it.  Nothing
/// is learned from a glitch (no period estimate to poison); the
/// glitch opens a frame that fails validation and is dropped, and
/// the next frame decodes normally.
struct UartTx {
    /// Total clock cycles observed (`cycle` is called once per cycle).
    cycles: u64,
    /// Exact cycles-per-bit: `clock_hz / baud` as a reduced rational
    /// p/q (see `BaudTiming`).  Boundaries and sample points are
    /// compared in units of 1/q cycle, so a non-integer period (1 MHz
    /// / 115200 = 8.68…) is handled without drift.
    timing: BaudTiming,
    /// Start-bit falling edge of the frame being buffered, if any.
    frame_start: Option<u64>,
    /// Edge timestamps (absolute cycles) since the start bit.
    frame_edges: Vec<u64>,
    last: bool,
}

impl UartTx {
    fn new(timing: BaudTiming) -> UartTx {
        UartTx {
            cycles: 0,
            timing,
            frame_start: None,
            frame_edges: Vec::new(),
            last: true,
        }
    }

    /// Decode and emit the buffered frame, then reset for the next.
    /// Called when the next start bit arrives (the frame is complete)
    /// or when ten bit periods have elapsed with no new start bit.
    fn finalize(&mut self) {
        let Some(t0) = self.frame_start.take() else { return };
        let edges = std::mem::take(&mut self.frame_edges);
        if let Some(byte) = decode_frame(t0, &edges, &self.timing) {
            let mut stdout = std::io::stdout().lock();
            let _ = stdout.write_all(&[byte]);
            let _ = stdout.flush();
        }
    }

    /// Flush any frame buffered but not yet closed (the last byte
    /// before a long idle or exit).
    fn flush(&mut self) {
        self.finalize();
    }

    /// One clock cycle; `tx` is the current pin value.
    fn cycle(&mut self, tx: bool) {
        self.cycles += 1;
        let c = self.cycles;
        // Frame position in ticks (1/q cycle) — exact under the
        // rational period, so the close thresholds below hold for
        // non-integer bit periods too.
        let into_frame = |t0: u64| -> u64 { (c - t0) * self.timing.ticks_per_cycle };
        if tx != self.last {
            if tx {
                // Rising edge within the buffered frame: a data-bit
                // transition.
                if self.frame_start.is_some() {
                    self.frame_edges.push(c);
                }
            } else if let Some(t0) = self.frame_start {
                // A falling edge inside an open frame is either a
                // data-bit transition (before the stop-bit sample
                // point) or the next frame's start bit.  Data
                // transitions only happen at bit boundaries 1..9,
                // which a conforming design reaches before 9.5
                // configured bit periods; the next start bit of a
                // back-to-back stream arrives at 10 design bit
                // periods, at or past 9.5 configured ones.
                if into_frame(t0) >= 19 * self.timing.ticks_per_bit / 2 {
                    // Next start bit: close this frame, then open
                    // the next.
                    self.finalize();
                    self.frame_start = Some(c);
                } else {
                    // Data-bit transition within the frame.
                    self.frame_edges.push(c);
                }
            } else {
                // No frame open: a start bit.  Nothing is gated and
                // nothing is measured: a power-up glitch opens a
                // frame that fails validation and is dropped.
                self.frame_start = Some(c);
            }
        } else if let Some(t0) = self.frame_start {
            // Ten bit periods with no new start bit: the frame is
            // complete (this flushes the final byte before a long
            // idle or exit).
            if into_frame(t0) >= 10 * self.timing.ticks_per_bit {
                self.finalize();
            }
        }
        self.last = tx;
    }
}

/// Decode one buffered UART frame at the configured period: sample
/// the recorded edge timeline at mid-bit points, `clock_hz / baud`
/// cycles per bit.  Returns the byte only if the start/stop framing
/// holds and every recorded transition is consistent with the decoded
/// byte at that period; otherwise the frame is dropped.
fn decode_frame(t0: u64, edges: &[u64], timing: &BaudTiming) -> Option<u8> {
    let q = timing.ticks_per_cycle;
    let p = timing.ticks_per_bit;
    // Edge positions in ticks after the start edge.
    let ticks: Vec<u64> = edges.iter().map(|&e| (e - t0) * q).collect();
    // Line value at sample point `s2` (in doubled ticks: the sample
    // sits at s2/2 ticks after the start edge; sample points sit at
    // half-bit offsets, and doubling keeps everything integral when p
    // is odd).  The line starts low and toggles with each recorded
    // edge.
    let value_at = |s2: u64| -> bool {
        let mut high = false;
        for &w in &ticks {
            if 2 * w > s2 {
                break;
            }
            high = !high;
        }
        high
    };
    if value_at(1 * p) {
        return None; // start bit: expect low
    }
    let mut data = 0u8;
    for j in 1..=8 {
        if value_at((2 * j + 1) * p) {
            data |= 1 << (j - 1); // LSB first
        }
    }
    if !value_at(19 * p) {
        return None; // stop bit: expect high
    }
    // Validate the recorded transitions against the decoded byte: the
    // byte's bit pattern determines where transitions belong (the
    // boundaries between differing consecutive bits), and every
    // recorded edge must be one of those transitions, in order, close
    // enough to its ideal position (k bit periods in) that the
    // mid-bit samples above are guaranteed to have landed in the
    // right bits.  That budget is the classic UART one: the design's
    // bit period may drift from the configured one by at most one
    // twentieth of a bit per bit position.  A frame whose edges fit
    // no byte at the configured period — a power-up glitch, or a
    // design running at a different divisor — is dropped.
    let mut bits = Vec::with_capacity(10);
    bits.push(false); // start bit
    for j in 1..=8 {
        bits.push((data >> (j - 1)) & 1 != 0); // LSB first
    }
    bits.push(true); // stop bit
    let mut expected: Vec<u64> = Vec::new();
    for k in 1..10 {
        if bits[k] != bits[k - 1] {
            expected.push(k as u64);
        }
    }
    if expected.len() != ticks.len() {
        return None;
    }
    for (&k, &w) in expected.iter().zip(&ticks) {
        let ideal = k * p;
        let drift = w.abs_diff(ideal);
        if drift * 20 > k * p {
            return None; // transition too far from its bit boundary
        }
    }
    Some(data)
}

/// RX: shift bytes from stdin into the design on `rx` (host → design).
///
/// Transmits at the same fixed, configured period as the TX decoder
/// (see `UartTx`): a UART shares one divisor in both directions, so
/// the first byte goes out at `clock_hz / baud` with nothing to
/// confirm.
struct UartRx {
    sending: bool,
    t: u64,
    next: u64,
    bit: u32, // 0 = start, 1..8 = data, 9 = stop
    data: u8,
    pin: bool, // value to drive on rx (true = idle)
    pending: std::collections::VecDeque<u8>,
    ticks_per_cycle: u64,
    ticks_per_bit: u64,
}

impl UartRx {
    fn new(timing: &BaudTiming) -> UartRx {
        UartRx {
            sending: false,
            t: 0,
            next: 0,
            bit: 0,
            data: 0,
            pin: true,
            pending: std::collections::VecDeque::new(),
            ticks_per_cycle: timing.ticks_per_cycle,
            ticks_per_bit: timing.ticks_per_bit,
        }
    }

    fn poll_stdin(&mut self, receiver: &mut mpsc::Receiver<u8>) {
        loop {
            match receiver.try_recv() {
                Ok(b) => self.pending.push_back(b),
                Err(TryRecvError::Disconnected) => break,
                Err(TryRecvError::Empty) => break,
            }
        }
    }

    /// One clock cycle: update the state machine and the pin value.
    fn cycle(&mut self, receiver: &mut mpsc::Receiver<u8>) {
        if !self.sending {
            self.pin = true;
            if !self.pending.is_empty() {
                self.data = self.pending.pop_front().unwrap();
                self.sending = true;
                self.bit = 0;
                self.t = 0;
                self.next = self.ticks_per_bit;
            }
        } else {
            self.t += self.ticks_per_cycle;
            if self.t >= self.next {
                self.next += self.ticks_per_bit;
                self.bit += 1;
                if self.bit == 10 { // start + 8 data + stop all done
                    self.sending = false;
                }
            }
        }
        if self.sending {
            self.pin = if self.bit == 0 {
                false // start bit
            } else if (1..=8).contains(&self.bit) {
                (self.data >> (self.bit - 1)) & 1 != 0 // LSB first
            } else {
                true // stop bit
            };
        }
        let _ = receiver;
    }
}

// ─────────────────────────────────────────────────────────────
// Average clock frequency report (--fps)
// ─────────────────────────────────────────────────────────────

static FPS_START: Mutex<Option<Instant>> = Mutex::new(None);

/// Set by the SIGINT handler.  The main loop notices it on the next
/// clock tick and shuts down gracefully (flushing pending UART
/// output) instead of dying mid-frame.
static SIGINT_REQUESTED: AtomicBool = AtomicBool::new(false);
static FPS_CYCLES: AtomicU64 = AtomicU64::new(0);
static FPS_REPORT: AtomicBool = AtomicBool::new(false);

/// Average clock frequency achieved (MHz), from the cycle count and
/// the elapsed wall time.  `None` if the run was too short to be
/// meaningful (no cycles, or no measurable elapsed time).
fn fps_mhz() -> Option<f64> {
    let cycles = FPS_CYCLES.load(Ordering::Relaxed);
    let secs = FPS_START.lock().ok()?.as_ref()?.elapsed().as_secs_f64();
    if cycles == 0 || secs <= 0.0 {
        return None;
    }
    Some(cycles as f64 / secs / 1e6)
}

/// Format a frequency with up to 4 decimal places (trailing zeros
/// trimmed).
fn fmt_mhz(mhz: f64) -> String {
    format!("{mhz:.4}")
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string()
}

/// Print the `--fps` summary (the average clock frequency in MHz)
/// after the simulation loop ends (naturally or via Ctrl-C).  Writes
/// straight to stdout so it works even when stdout has been closed
/// or redirected oddly.
fn report_fps() {
    if !FPS_REPORT.load(Ordering::Relaxed) {
        return;
    }
    let Some(mhz) = fps_mhz() else { return };
    let text = format!("Average clock frequency: {} MHz\n", fmt_mhz(mhz));
    let fd = unsafe { BorrowedFd::borrow_raw(1) };
    let _ = nix::unistd::write(fd, text.as_bytes());
}

// ─────────────────────────────────────────────────────────────
// Terminal raw mode (so keystrokes go straight to the UART)
// ─────────────────────────────────────────────────────────────

static SAVED_TERM: Mutex<Option<nix::sys::termios::Termios>> = Mutex::new(None);
static TERM_RESTORED: AtomicBool = AtomicBool::new(false);

extern "C" fn sigint_handler(_sig: i32) {
    // Restore the terminal right away (the main loop may take a
    // moment to notice the flag), then ask the main loop to shut
    // down gracefully so pending UART output is flushed instead of
    // being dropped mid-frame.
    restore_term();
    SIGINT_REQUESTED.store(true, Ordering::SeqCst);
}

fn restore_term() {
    if TERM_RESTORED.swap(true, Ordering::SeqCst) {
        return;
    }
    if let Ok(guard) = SAVED_TERM.lock() {
        if let Some(saved) = guard.as_ref() {
            let fd = unsafe { BorrowedFd::borrow_raw(0) };
            let _ = nix::sys::termios::tcsetattr(fd, nix::sys::termios::SetArg::TCSANOW, saved);
        }
    }
}

/// Put a tty stdin into raw mode (echo off, per-key delivery), like
/// the C++ harness.  Output post-processing, ICRNL, and SIGINT stay
/// enabled.
fn setup_terminal() {
    let fd = unsafe { BorrowedFd::borrow_raw(0) };
    let is_tty = nix::unistd::isatty(fd).unwrap_or(false);
    if !is_tty {
        return;
    }
    let Ok(saved) = nix::sys::termios::tcgetattr(fd) else { return };
    *SAVED_TERM.lock().unwrap() = Some(saved.clone());
    let mut raw = saved;
    nix::sys::termios::cfmakeraw(&mut raw);
    raw.output_flags |= nix::sys::termios::OutputFlags::OPOST
        | nix::sys::termios::OutputFlags::ONLCR;
    raw.input_flags |= nix::sys::termios::InputFlags::ICRNL;
    raw.local_flags |= nix::sys::termios::LocalFlags::ISIG;
    let _ = nix::sys::termios::tcsetattr(fd, nix::sys::termios::SetArg::TCSANOW, &raw);
    unsafe {
        let _ = nix::sys::signal::signal(nix::sys::signal::Signal::SIGINT, nix::sys::signal::SigHandler::Handler(sigint_handler));
    }
}


