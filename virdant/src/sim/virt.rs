//! The `virt::Virt` board, simulated in Rust on top of the [`Sim`]
//! engine.
//!
//! This is the Rust twin of `virdant-virt/assets/sim.cpp` (the
//! Verilator harness): a board that presents the `virt::Virt` platform
//! contract to the design —
//!
//!   - UART at 115200 8N1, wired to stdin/stdout;
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
//! Requires the `vir-bin` feature (the terminal/`nix` dependency); the
//! CLI lives in `vir run`.

#[cfg(feature = "vir-bin")]
mod imp {
    use std::cell::Cell;
    use std::io::{Read as _, Write as _};
    use std::os::fd::BorrowedFd;
    use std::path::PathBuf;
    use std::rc::Rc;
    use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
    use std::sync::mpsc::{self, TryRecvError};
    use std::sync::Mutex;
    use std::time::{Duration, Instant};

    use bstr::ByteSlice;

    use crate::analysis::elaboration::SignalId;
    use crate::analysis::symbols::SymbolId;
    use crate::db::Db;
    use crate::sim::layout::TypeLayoutIndex;
    use crate::sim::sim::Clock;
    use crate::sim::{Sim, Value};

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
    }

    const FLASH_SIZE: u64 = 1 << 24; // 16 MiB
    const BAUD: u64 = 115_200; // fixed 115200 8N1

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
        crate::sim::builtins::attach(&mut sim, db, opts.verbose);

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
        let mut per_cycle = PerCycle {
            tx,
            rx,
            exit,
            // Idle-high gate for auto-baud: ten nominal bit periods at
            // the platform-declared clock.  A floor against startup line
            // glitches, not a decode assumption.
            uart_tx: UartTx::new(10 * clock_hz / BAUD),
            uart_rx: UartRx::new(&baud_timing(clock_hz, BAUD)),
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
            self.uart_rx.set_bit_period(self.uart_tx.bit_period());
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


            // Exit request (virt::Virt `exit` port): a @Valid(n) on a
            // clock tick ends the simulation with exit code n.  The
            // decode allocates (heap `BitPat` + layout slot clones), so
            // skip it unless the port's raw value changed since the
            // last cycle — it stays `@Invalid()` for virtually the
            // whole run.
            let exit_value = sim.get(self.exit);
            if self.prev_exit.as_ref() != Some(&exit_value) {
                if let Some(code) = decode_exit(sim.type_layouts(), &exit_value) {
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
    fn decode_exit(layouts: &TypeLayoutIndex, value: &Value) -> Option<i32> {
        let (slot, payloads) = layouts.decode_ctor(value.typ_id(), &value.bitpat())?;
        if slot.name.as_bstr() != b"Valid".as_bstr() {
            return None;
        }
        let Some(code) = payloads.first().map(|c| c.to_u64()) else { return None; };
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

    /// TX: decode the design's transmission on `tx` (design → host stdout).
    ///
    /// Auto-baud: the design's bit period is *measured* from the `tx`
    /// line rather than derived from the platform's declared clock.  A
    /// design's UART divisor is fixed in cycles, so no frequency the
    /// platform declares can be trusted to predict it.  The measurement
    /// rests on two properties of every UART stream: no pulse is ever
    /// shorter than one bit period, and two adjacent bits that differ
    /// are exactly one period apart — so the minimum distance between
    /// consecutive edges *within a frame* is the bit period.  (Edges
    /// land on whole cycles in simulation, so the measurement is exact.)
    ///
    /// Frames are decoded retrospectively: transitions are buffered from
    /// each start-bit falling edge until the frame closes — either when
    /// ten estimated bit periods elapse (no new start bit yet), or when a
    /// falling edge arrives that far into the frame (that fall is the
    /// next start bit; anything sooner is a data-bit transition and
    /// belongs to this frame) — then sampled at mid-bit points.  A frame
    /// only produces a byte if every transition lands on a bit boundary
    /// and the start/stop framing is valid; otherwise it is dropped, so
    /// an unconverged period estimate cannot emit garbage for long — the
    /// first frame with two adjacent differing bits pins the true period
    /// down exactly.
    ///
    /// Because the sim starts every register at a random value, the line
    /// can glitch low for a few cycles before the design's reset settles
    /// it.  Such a glitch would otherwise be measured as a tiny "bit
    /// period" and poison the estimate, so until a period is confirmed a
    /// start bit is only trusted after the line has idled high for
    /// `gate_cycles` (ten nominal bit periods at the platform-declared
    /// clock — a floor, not a decode assumption).
    struct UartTx {
        /// Total clock cycles observed (`cycle` is called once per cycle).
        cycles: u64,
        /// Cycle number of the most recent `tx` edge.
        prev_edge: u64,
        /// Minimum distance between consecutive edges within a frame,
        /// ever observed, in cycles: the best-known bit-period estimate.
        /// Intra-frame only, so idle gaps and pre-reset line states
        /// cannot poison it.
        min_edge: Option<u64>,
        /// Bit period confirmed by a successfully decoded frame.  This,
        /// not the raw estimate, is what `rx` transmission should use.
        confirmed: Option<u64>,
        /// Start-bit falling edge of the frame being buffered, if any.
        frame_start: Option<u64>,
        /// Minimum distance between edges inside the buffered frame.
        frame_min: Option<u64>,
        /// Edge timestamps (absolute cycles) since the start bit.
        frame_edges: Vec<u64>,
        /// Cycle the line last became (or started out) high, for the
        /// idle-high gate.
        high_since: u64,
        /// Idle-high a start bit must have before it is trusted, while
        /// no period has been confirmed yet.
        gate_cycles: u64,
        last: bool,
    }

    impl UartTx {
        fn new(gate_cycles: u64) -> UartTx {
            UartTx {
                cycles: 0,
                prev_edge: 0,
                min_edge: None,
                confirmed: None,
                frame_start: None,
                frame_min: None,
                frame_edges: Vec::new(),
                high_since: 0,
                gate_cycles,
                last: true,
            }
        }

        /// The design's bit period as confirmed by a decoded frame, in
        /// cycles.  A UART uses the same divisor for both directions, so
        /// this is also the period at which the design samples `rx`.
        fn bit_period(&self) -> Option<u64> {
            self.confirmed
        }

        /// Decode and emit the buffered frame, then reset for the next.
        /// Called when the next start bit arrives (the frame is complete)
        /// or when ten bit periods have elapsed with no new start bit.
        fn finalize(&mut self) {
            let Some(t0) = self.frame_start.take() else { return };
            let edges = std::mem::take(&mut self.frame_edges);
            let frame_min = self.frame_min.take();

            // Candidate bit periods, best first: the confirmed period
            // (frames should decode with it; trying anything else first
            // would let a stale estimate hijack the decode), then the
            // running estimate, then this frame's own minimum (which
            // covers the very first frame, before any estimate exists).
            let mut candidates: Vec<u64> = Vec::new();
            if let Some(bit) = self.confirmed {
                candidates.push(bit);
            }
            if let Some(bit) = self.min_edge {
                if !candidates.contains(&bit) {
                    candidates.push(bit);
                }
            }
            if let Some(bit) = frame_min {
                if !candidates.contains(&bit) {
                    candidates.push(bit);
                }
            }

            for bit in candidates {
                if let Some(byte) = decode_frame(t0, &edges, bit) {
                    self.confirmed = Some(bit);
                    let mut stdout = std::io::stdout().lock();
                    let _ = stdout.write_all(&[byte]);
                    let _ = stdout.flush();
                    break;
                }
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
            if tx != self.last {
                let dist = c - self.prev_edge;
                self.prev_edge = c;
                if tx {
                    // Rising edge within the buffered frame.
                    if self.frame_start.is_some() {
                        self.min_edge = Some(self.min_edge.map_or(dist, |m| m.min(dist)));
                        self.frame_min = Some(self.frame_min.map_or(dist, |m| m.min(dist)));
                        self.frame_edges.push(c);
                    }
                    if self.confirmed.is_none() {
                        // The idle-high gate restarts.
                        self.high_since = c;
                    }
                } else if let Some(t0) = self.frame_start {
                    // A falling edge inside an open frame is either a
                    // data-bit transition (less than ten bit periods in)
                    // or the next frame's start bit.  Only a period
                    // estimate can tell them apart.
                    match self.min_edge.or(self.frame_min) {
                        Some(bit) if c - t0 >= 10 * bit => {
                            // Next start bit: close this frame, then open
                            // the next — but until a period is confirmed,
                            // only trust a start bit after a long idle
                            // (random-init line glitches don't get to
                            // poison the estimate).
                            self.finalize();
                            let trusted = self.confirmed.is_some()
                                || c - self.high_since >= self.gate_cycles;
                            if trusted {
                                self.frame_start = Some(c);
                            }
                        }
                        _ => {
                            // Data-bit transition within the frame.
                            self.min_edge = Some(self.min_edge.map_or(dist, |m| m.min(dist)));
                            self.frame_min = Some(self.frame_min.map_or(dist, |m| m.min(dist)));
                            self.frame_edges.push(c);
                        }
                    }
                } else {
                    // No frame open: a candidate start bit.  Until a
                    // period is confirmed, only trust one after a long
                    // idle (see struct doc).
                    let trusted = self.confirmed.is_some()
                        || c - self.high_since >= self.gate_cycles;
                    if trusted {
                        self.frame_start = Some(c);
                    }
                }
            } else if let Some(t0) = self.frame_start {
                // Ten bit periods with no new start bit: the frame is
                // complete (this flushes the final byte before a long
                // idle or exit).
                if let Some(bit) = self.min_edge.or(self.frame_min) {
                    if c - t0 >= 10 * bit {
                        self.finalize();
                    }
                }
            }
            self.last = tx;
        }
    }

    /// Decode one buffered UART frame: sample the recorded edge timeline
    /// at mid-bit points, `bit` cycles per bit.  Returns the byte only if
    /// every transition lands on a bit boundary and the framing holds.
    fn decode_frame(t0: u64, edges: &[u64], bit: u64) -> Option<u8> {
        for &e in edges {
            if (e - t0) % bit != 0 {
                return None; // a transition lands mid-bit: wrong period
            }
        }
        // Line value at 2*rel half-cycles after the start edge (sample
        // points sit at half-bit offsets; everything is compared doubled
        // to stay in integers).  The line starts low and toggles with
        // each recorded edge.
        let value_at = |rel2: u64| -> bool {
            let mut high = false;
            for &e in edges {
                if 2 * (e - t0) > rel2 {
                    break;
                }
                high = !high;
            }
            high
        };
        if value_at(1 * bit) {
            return None; // start bit: expect low
        }
        let mut data = 0u8;
        for j in 1..=8 {
            if value_at((2 * j + 1) * bit) {
                data |= 1 << (j - 1); // LSB first
            }
        }
        if !value_at(19 * bit) {
            return None; // stop bit: expect high
        }
        Some(data)
    }

    /// RX: shift bytes from stdin into the design on `rx` (host → design).
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
        /// Auto-baud: the design's measured bit period in cycles (its
        /// `tx` divisor), applied from the next byte on.  `None` until
        /// the design has transmitted something decodable; `rx` then
        /// falls back to `clock_hz / BAUD`.
        bit_cycles: Option<u64>,
        /// Ticks per bit captured when the current byte started.
        cur_ticks_per_bit: u64,
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
                bit_cycles: None,
                cur_ticks_per_bit: timing.ticks_per_bit,
            }
        }

        /// Adopt the bit period the TX decoder confirmed from the
        /// design's `tx` line, applied from the next byte on.
        fn set_bit_period(&mut self, cycles: Option<u64>) {
            self.bit_cycles = cycles;
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
                    self.cur_ticks_per_bit = self.bit_cycles
                        .map_or(self.ticks_per_bit, |b| b * self.ticks_per_cycle);
                    self.next = self.cur_ticks_per_bit;
                }
            } else {
                self.t += self.ticks_per_cycle;
                if self.t >= self.next {
                    self.next += self.cur_ticks_per_bit;
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

    // Shared with the SIGINT handler, which must print the summary
    // itself before exiting immediately.
    static FPS_START: Mutex<Option<Instant>> = Mutex::new(None);
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

    /// Print the `--fps` summary (the average clock frequency in MHz),
    /// on a natural exit or, via the SIGINT handler, on Ctrl-C.  Writes
    /// straight to stdout because the handler cannot take the stdout
    /// lock.
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
        restore_term();
        report_fps();
        // Immediate exit, bypassing atexit (matches the C++ harness's
        // _exit; termios is already restored above).
        std::process::exit(0);
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

}

pub use imp::run;
pub use imp::VirtOptions;
