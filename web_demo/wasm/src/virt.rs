//! The `virt::Virt` board, ported from `vir/src/vir/virt.rs` for the
//! wasm web demo.
//!
//! This is the same board logic — UART (115200 8N1, auto-baud on TX),
//! the 16 MiB SPI flash device (READ 0x03, auto-increment), the
//! `exit : Valid[Word[8]]` port — with the Unix shell removed:
//!
//!   - UART TX output goes into a shared byte buffer (drained by JS)
//!     instead of stdout;
//!   - UART RX input comes out of a shared byte queue (filled by JS)
//!     instead of a stdin reader thread;
//!   - real-time pacing is done by the JS driver (which schedules
//!     `Sim::run_until` chunks against wall time) instead of
//!     `std::thread::sleep` inside the per-cycle callback;
//!   - terminal raw mode and SIGINT handling do not exist; the
//!     browser console is the terminal.
//!
//! Keep this file in sync with `vir/src/vir/virt.rs`: the algorithms
//! (auto-baud frame decoding, SPI mode-0 sampling, baud rationals)
//! are copied verbatim where possible.

use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;

use virdant::analysis::elaboration::SignalId;
use virdant::sim::{Sim, Value};

pub const FLASH_SIZE: u64 = 1 << 24; // 16 MiB
pub const BAUD: u64 = 115_200; // fixed 115200 8N1

// Port names required by the `virt::Virt` platform contract.
pub const PORT_CLOCK: &str = "clock";
pub const PORT_TX: &str = "tx";
pub const PORT_RX: &str = "rx";
pub const PORT_EXIT: &str = "exit";
pub const PORT_SPI_CK: &str = "spi_ck";
pub const PORT_SPI_CS: &str = "spi_cs";
pub const PORT_SPI_MOSI: &str = "spi_mosi";
pub const PORT_SPI_MISO: &str = "spi_miso";

/// Board state shared between the sim callbacks and the JS-facing
/// [`VirtVm`](crate::VirtVm) API.  Single-threaded wasm, so
/// `Rc<...>`/`RefCell` replace the native build's channels and atomics.
pub struct BoardShared {
    /// UART TX bytes decoded from the design's `tx` line, drained by JS.
    pub tx_out: RefCell<Vec<u8>>,
    /// Bytes to transmit into the design's `rx` line, filled by JS.
    pub rx_in: RefCell<VecDeque<u8>>,
    /// Rising clock edges observed so far.
    pub cycles: Cell<u64>,
    /// The exit code requested via the `exit` port, if any.
    pub exit: Cell<Option<u8>>,
}

impl BoardShared {
    pub fn new() -> Rc<BoardShared> {
        Rc::new(BoardShared {
            tx_out: RefCell::new(Vec::new()),
            rx_in: RefCell::new(VecDeque::new()),
            cycles: Cell::new(0),
            exit: Cell::new(None),
        })
    }
}

// ─────────────────────────────────────────────────────────────
// Per-cycle board logic (fired on each rising edge of the
// platform clock, after the design has settled)
// ─────────────────────────────────────────────────────────────

/// SPI-flash sampling: runs on EVERY clock edge (its own watcher,
/// its own state — never shares a borrow with the per-cycle logic).
pub struct FlashStepper {
    pub spi_ck: SignalId,
    pub spi_cs: SignalId,
    pub spi_mosi: SignalId,
    pub spi_miso: SignalId,
    pub flash: SpiFlash,
}

impl FlashStepper {
    /// One sampling point with the post-edge pin values, mirroring
    /// the C++ harness stepping its flash model after every eval.
    /// Drives `spi_miso` from the model's response.
    pub fn step(&mut self, sim: &mut Sim) {
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

pub struct PerCycle {
    pub tx: SignalId,
    pub rx: SignalId,
    pub exit: SignalId,
    pub uart_tx: UartTx,
    pub uart_rx: UartRx,
    /// Last seen `exit`-port value; `decode_exit` is a pure function
    /// of the value, so it only needs to rerun when it changes.
    pub prev_exit: Option<Value>,
    pub shared: Rc<BoardShared>,
    /// Input-queue poll cadence: like the native build (which drains
    /// stdin every 1000 cycles), touching the shared queue on every
    /// cycle costs measurably at multi-MHz rates.
    pub cycle_count: u64,
}

impl PerCycle {
    pub fn cycle(&mut self, sim: &mut Sim) {
        let bit = |sim: &Sim, id: SignalId| -> bool {
            sim.get(id).get_bit(0)
        };
        let tx = bit(sim, self.tx);

        self.uart_tx.cycle(tx, &self.shared);
        self.uart_rx.set_bit_period(self.uart_tx.bit_period());
        self.cycle_count += 1;
        if self.cycle_count % 1000 == 0 {
            self.uart_rx.poll_input(&self.shared);
        }
        self.uart_rx.cycle();

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
        // clock tick ends the simulation with exit code n.  The decode
        // allocates (heap `BitPat` + layout slot clones), so skip it
        // unless the port's raw value changed since the last cycle —
        // it stays `@Invalid()` for virtually the whole run.
        let exit_value = sim.get(self.exit);
        if self.prev_exit.as_ref() != Some(&exit_value) {
            if let Some(code) = decode_exit(sim, &exit_value) {
                self.uart_tx.flush(&self.shared);
                self.shared.exit.set(Some(code));
                sim.finish();
            }
            self.prev_exit = Some(exit_value);
        }

        let cycles = self.shared.cycles.get() + 1;
        self.shared.cycles.set(cycles);
    }
}

/// Decode the `exit : Valid[Word[8]]` value.  The Valid slot's
/// payload (the `Word[8]`) is the requested exit code.
fn decode_exit(sim: &Sim, value: &Value) -> Option<u8> {
    let ctor = sim.decode_ctor(value)?;
    if ctor.name != "Valid" {
        return None;
    }
    let code = ctor.payloads.first()?.to_u64();
    Some(code as u8)
}

// ─────────────────────────────────────────────────────────────
// SPI flash model (mode 0, READ 0x03 with auto-increment)
// ─────────────────────────────────────────────────────────────

pub struct SpiFlash {
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
    /// Preload a flash image (like the native `--flash` option):
    /// the bytes are copied to the start of the device, the remainder
    /// stays erased (all `0xFF`).  Images larger than the device are
    /// truncated, matching the native build.
    pub fn with_image(image: &[u8]) -> SpiFlash {
        let mut mem = vec![0xFF; FLASH_SIZE as usize];
        let n = image.len().min(mem.len());
        mem[..n].copy_from_slice(&image[..n]);
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
pub struct BaudTiming {
    pub ticks_per_cycle: u64,
    pub ticks_per_bit: u64,
}

pub fn baud_timing(clock_hz: u64, baud: u64) -> BaudTiming {
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

/// TX: decode the design's transmission on `tx` (design → host console).
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
pub struct UartTx {
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
    pub fn new(gate_cycles: u64) -> UartTx {
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
    fn finalize(&mut self, shared: &BoardShared) {
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
                shared.tx_out.borrow_mut().push(byte);
                break;
            }
        }
    }

    /// Flush any frame buffered but not yet closed (the last byte
    /// before a long idle or exit).
    pub fn flush(&mut self, shared: &BoardShared) {
        self.finalize(shared);
    }

    /// One clock cycle; `tx` is the current pin value.  Decoded
    /// bytes are appended to the shared TX output buffer.
    pub fn cycle(&mut self, tx: bool, shared: &BoardShared) {
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
                        self.finalize(shared);
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
                    self.finalize(shared);
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

/// RX: shift bytes from the shared input queue into the design on
/// `rx` (host → design).
pub struct UartRx {
    sending: bool,
    t: u64,
    next: u64,
    bit: u32, // 0 = start, 1..8 = data, 9 = stop
    data: u8,
    pin: bool, // value to drive on rx (true = idle)
    pending: VecDeque<u8>,
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
    pub fn new(timing: &BaudTiming) -> UartRx {
        UartRx {
            sending: false,
            t: 0,
            next: 0,
            bit: 0,
            data: 0,
            pin: true,
            pending: VecDeque::new(),
            ticks_per_cycle: timing.ticks_per_cycle,
            ticks_per_bit: timing.ticks_per_bit,
            bit_cycles: None,
            cur_ticks_per_bit: timing.ticks_per_bit,
        }
    }

    /// Adopt the bit period the TX decoder confirmed from the
    /// design's `tx` line, applied from the next byte on.
    pub fn set_bit_period(&mut self, cycles: Option<u64>) {
        self.bit_cycles = cycles;
    }

    /// Drain the shared input queue into the pending buffer.
    pub fn poll_input(&mut self, shared: &BoardShared) {
        let mut queue = shared.rx_in.borrow_mut();
        while let Some(b) = queue.pop_front() {
            self.pending.push_back(b);
        }
    }

    /// One clock cycle: update the state machine and the pin value.
    /// Pending bytes come from the buffer drained by [`poll_input`].
    pub fn cycle(&mut self) {
        if !self.sending {
            self.pin = true;
            if let Some(data) = self.pending.pop_front() {
                self.data = data;
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
    }
}
