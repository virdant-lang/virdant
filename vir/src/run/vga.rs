//! The `virt::VirtVga` VGA monitor, simulated in Rust.
//!
//! This is the Rust port of `virdant-virt/assets/sim_vga.cpp`'s
//! `VgaMonitor` (the Verilator + SDL2 harness), rehosted onto the
//! [`Sim`] engine: the monitor is just another per-cycle device of the
//! [`virt::VirtVga`] board, alongside the UART and the SPI flash (see
//! `virt.rs`, which is the port of `sim.cpp`).
//!
//! The board-side contract it implements (the `virt::VirtVga`
//! platform, `lib/virt.vir`):
//!
//!   - `vga_r`/`vga_g`/`vga_b : Word[4]` — the RGB444 buses
//!   - `vga_hs`/`vga_vs : Bit` — active-high syncs on the Nexys4
//!     connector
//!
//! The monitor is *sync-driven*: nothing about the design's internal
//! pixel-clock divider or sync polarity is assumed.  The HSYNC/VSYNC
//! periods, pulse widths, and polarities are measured over the first
//! frames and matched against the VESA timing table below to recover
//! the frame geometry (visible area and porches); pixels are then
//! sampled from the RGB buses at the design's own measured pixel
//! rate.  A design can use any clock division (or none) and any of
//! the table's resolutions.
//!
//! Rendering uses the `sdl2` crate (system SDL2, like the C++
//! harness's `SDL2/SDL.h`).  Two deliberate deviations from the C++
//! harness:
//!
//!   - The C++ harness destroys and recreates the window on a mode
//!     change.  Here the window is kept for the run's lifetime and
//!     only the logical size and texture are re-created, which renders
//!     identically (the renderer scales the texture either way).
//!   - The C++ harness's `--trace` flag gated identification logging;
//!     here those messages go under the board's `--verbose` flag.
//!
//! In a headless environment (no display), SDL setup fails and the
//! monitor keeps simulating silently: UART, SPI flash, and `exit`
//! still work, the picture just cannot show.
//!
//! [`virt::VirtVga`]: crate::virt::VirtBoard::VirtVga

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::{Texture, TextureCreator, WindowCanvas};
use sdl2::video::WindowContext;
use sdl2::{EventPump, Sdl};

// ─────────────────────────────────────────────────────────────
// VESA timing table
// ─────────────────────────────────────────────────────────────
//
// One entry per video mode, in VESA/DMT order (front porch → sync →
// back porch, relative to the *sync pulse start* line/pixel origin).
//
// ─┬──────────────────────────────────────────────────────────────
//  │ ADD NEW RESOLUTIONS HERE
//  │
//  │ Append to VESA_MODES below.  All four quantities must be the
//  │ standard's exact values, because mode identification matches the
//  │ measured sync timings against them (within MATCH_TOLERANCE):
//  │
//  │   { "<name>", <pixel_clock_hz>,
//  │     { <visible>, <front>, <sync>, <back>, <hsync active low?> },
//  │     { <visible>, <front>, <sync>, <back>, <vsync active low?> } },
//  │
//  │ The two most popular modes (640x480@60, 800x600@60) are first.
// ─┴──────────────────────────────────────────────────────────────

/// One axis of a video mode.  Porches run back-to-sync-pulse-start:
/// the `front` porch precedes the sync pulse, the `back` porch follows
/// it, and the visible area starts after `back`.
#[derive(Debug)]
struct VesaTiming {
    /// Visible pixels (H) or lines (V).
    visible: u32,
    /// Front porch.
    front: u32,
    /// Sync pulse width.
    sync: u32,
    /// Back porch.
    back: u32,
    /// Sync polarity: true = active low.  (Deliberately *not* scored
    /// during identification; see `match_error`.)
    #[allow(dead_code)]
    active_low: bool,
}

impl VesaTiming {
    fn total(&self) -> u32 {
        self.visible + self.front + self.sync + self.back
    }
}

/// One VESA video mode.
#[derive(Debug)]
struct VesaMode {
    /// e.g. "640x480@60".
    name: &'static str,
    pixel_clock_hz: u64,
    h: VesaTiming,
    v: VesaTiming,
}

macro_rules! vesa_mode {
    ($name:literal, $pxclk:literal,
     h: [$hv:literal, $hf:literal, $hs:literal, $hb:literal, $hp:literal],
     v: [$vv:literal, $vf:literal, $vs:literal, $vb:literal, $vp:literal]) => {
        VesaMode {
            name: $name,
            pixel_clock_hz: $pxclk,
            h: VesaTiming { visible: $hv, front: $hf, sync: $hs, back: $hb, active_low: $hp },
            v: VesaTiming { visible: $vv, front: $vf, sync: $vs, back: $vb, active_low: $vp },
        }
    };
}

static VESA_MODES: [VesaMode; 13] = [
    //                mode          pixel clock  H: vis/f/sync/back, pol    V: vis/f/sync/back, pol
    vesa_mode!("640x480@60",  25_175_000, h: [ 640, 16,  96,  48, true ], v: [ 480, 10,  2, 33, true ]),
    vesa_mode!("800x600@60",  40_000_000, h: [ 800, 40, 128,  88, false], v: [ 600,  1,  4, 23, false]),
    vesa_mode!("640x480@72",  31_500_000, h: [ 640, 24,  40, 128, true ], v: [ 480,  9,  3, 28, true ]),
    vesa_mode!("640x480@75",  31_500_000, h: [ 640, 16,  64, 120, true ], v: [ 480,  1,  3, 16, false]),
    vesa_mode!("800x600@56",  36_000_000, h: [ 800, 24, 128,  72, false], v: [ 600,  1,  4, 20, false]),
    vesa_mode!("800x600@72",  50_000_000, h: [ 800, 56, 120,  64, false], v: [ 600, 37,  6, 23, false]),
    vesa_mode!("800x600@75",  49_500_000, h: [ 800, 16,  80, 160, false], v: [ 600,  1,  3, 21, false]),
    vesa_mode!("1024x768@60", 65_000_000, h: [1024, 24, 136, 160, true ], v: [ 768,  3,  6, 29, true ]),
    vesa_mode!("1024x768@75", 78_750_000, h: [1024, 16,  96, 176, false], v: [ 768,  1,  3, 28, false]),
    vesa_mode!("1280x720@60", 74_250_000, h: [1280,110,  40, 220, false], v: [ 720,  5,  5, 20, false]),
    vesa_mode!("1280x1024@60",108_000_000, h: [1280, 48, 112, 248, false], v: [1024,  1,  3, 38, false]),
    vesa_mode!("1600x1200@60",162_000_000, h: [1600, 64, 192, 304, false], v: [1200,  1,  3, 46, false]),
    vesa_mode!("1920x1080@60",148_500_000, h: [1920, 88,  44, 148, false], v: [1080,  4,  5, 36, false]),
];

/// Relative error tolerated when matching measured sync timings against
/// the table.  Generous enough for clock-divider rounding (640x480@60
/// from a 25 MHz /2 pixel clock is 0.8% off the 25.175 MHz standard),
/// tight enough that no two table modes can cross-match.
const MATCH_TOLERANCE: f64 = 0.04;

// ─────────────────────────────────────────────────────────────
// Sync measurement
// ─────────────────────────────────────────────────────────────

/// Period/pulse-width measurement for one sync signal, from pin edges.
/// All quantities are whole simulated cycles; `t` is the global cycle
/// counter at each edge.
struct SyncMeasure {
    have_prev: bool,
    prev: bool,
    falls: u64,
    rises: u64,
    last_fall: u64,
    last_rise: u64,
    period_fall: u64,
    period_rise: u64,
    gap_fr: u64,
    gap_rf: u64,
}

impl SyncMeasure {
    fn new() -> SyncMeasure {
        SyncMeasure {
            have_prev: false,
            prev: false,
            falls: 0,
            rises: 0,
            last_fall: 0,
            last_rise: 0,
            period_fall: 0,
            period_rise: 0,
            gap_fr: 0,
            gap_rf: 0,
        }
    }

    fn step(&mut self, pin: bool, t: u64) {
        if !self.have_prev {
            self.prev = pin;
            self.have_prev = true;
            return;
        }
        if pin == self.prev {
            return;
        }
        if !pin {
            // Falling edge.
            if self.falls > 0 {
                self.period_fall = t - self.last_fall;
            }
            if self.rises > 0 {
                self.gap_rf = t - self.last_rise;
            }
            self.last_fall = t;
            self.falls += 1;
        } else {
            // Rising edge.
            if self.rises > 0 {
                self.period_rise = t - self.last_rise;
            }
            if self.falls > 0 {
                self.gap_fr = t - self.last_fall;
            }
            self.last_rise = t;
            self.rises += 1;
        }
        self.prev = pin;
    }

    /// Two full pulses seen: periods and widths are measured.
    fn ready(&self) -> bool {
        self.falls >= 2 && self.rises >= 2
    }

    /// Pulse-start-to-pulse-start period, in cycles (fall/rise edges
    /// agree in steady state; average for noise immunity).
    fn period(&self) -> f64 {
        let mut p = 0.0;
        let mut n = 0;
        if self.period_fall != 0 {
            p += self.period_fall as f64;
            n += 1;
        }
        if self.period_rise != 0 {
            p += self.period_rise as f64;
            n += 1;
        }
        if n > 0 { p / n as f64 } else { 0.0 }
    }

    /// Time the pin spends low / high, in cycles (the shorter of the
    /// two is the sync pulse; the other is the inter-pulse gap).
    fn pulse_low(&self) -> f64 {
        self.gap_fr as f64
    }

    fn pulse_high(&self) -> f64 {
        self.gap_rf as f64
    }
}

// ─────────────────────────────────────────────────────────────
// SDL plumbing
// ─────────────────────────────────────────────────────────────

/// How often the SDL event queue is drained without a frame boundary
/// (in cycles), so the window stays responsive even without vsync.
const EVENT_POLL_INTERVAL: u64 = 500_000;

/// The SDL window, renderer, and texture for one resolution.
///
/// rust-sdl2's `Texture` borrows the `TextureCreator` that made it.
/// The monitor lives for the whole run anyway, so the creator is
/// leaked into a `&'static` and textures can be dropped and
/// re-created from it on a mode change (the window/canvas themselves
/// are owned here and outlive every texture).
struct SdlGfx {
    /// Keeps SDL initialized for the monitor's lifetime.
    _sdl: Sdl,
    event_pump: EventPump,
    canvas: WindowCanvas,
    /// Leaked: see the struct doc.  Textures borrow from it.
    texture_creator: &'static TextureCreator<WindowContext>,
    texture: Option<Texture<'static>>,
    tex_w: u32,
    tex_h: u32,
}

impl SdlGfx {
    /// Initialize SDL once and open a window of `w` × `h`, mirroring
    /// the C++ `sdl_setup`: accelerated renderer when available,
    /// software fallback, logical size locked to the mode so the
    /// window can be resized freely.
    fn open(w: u32, h: u32) -> Result<SdlGfx, String> {
        let sdl = sdl2::init()?;
        let event_pump = sdl.event_pump()?;
        let video = sdl.video()?;

        let build_window = |video: &sdl2::VideoSubsystem| -> Result<WindowCanvas, String> {
            let window = video
                .window("virt: virt::VirtVga", w, h)
                .resizable()
                .allow_highdpi()
                .position_centered()
                .build()
                .map_err(|e| format!("SDL_CreateWindow failed: {e}"))?;
            let canvas = match window.into_canvas().accelerated().build() {
                Ok(canvas) => canvas,
                Err(_) => {
                    // The failed builder consumed the window; open a
                    // fresh one for the software fallback, mirroring
                    // the C++ second `SDL_CreateRenderer` attempt.
                    let window = video
                        .window("virt: virt::VirtVga", w, h)
                        .resizable()
                        .allow_highdpi()
                        .position_centered()
                        .build()
                        .map_err(|e| format!("SDL_CreateWindow failed: {e}"))?;
                    window
                        .into_canvas()
                        .build()
                        .map_err(|e| format!("SDL_CreateRenderer failed: {e}"))?
                }
            };
            Ok(canvas)
        };

        let mut canvas = build_window(&video)?;
        canvas
            .set_logical_size(w, h)
            .map_err(|e| format!("SDL_RenderSetLogicalSize failed: {e}"))?;
        canvas.set_draw_color(Color::RGB(0, 0, 0));

        // Leak the creator so textures can be `Texture<'static>` and
        // replaced on a mode change (see the struct doc).
        let texture_creator: &'static TextureCreator<WindowContext> =
            Box::leak(Box::new(canvas.texture_creator()));
        let texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::RGB24, w, h)
            .map_err(|e| format!("SDL_CreateTexture failed: {e}"))?;

        Ok(SdlGfx {
            _sdl: sdl,
            event_pump,
            canvas,
            texture_creator,
            texture: Some(texture),
            tex_w: w,
            tex_h: h,
        })
    }

    /// Re-target the canvas at a new mode size, replacing the texture.
    /// The window itself is kept (see the module doc).
    fn resize(&mut self, w: u32, h: u32) -> Result<(), String> {
        if self.tex_w == w && self.tex_h == h {
            return Ok(());
        }
        self.canvas
            .set_logical_size(w, h)
            .map_err(|e| format!("SDL_RenderSetLogicalSize failed: {e}"))?;
        let texture = self
            .texture_creator
            .create_texture_streaming(PixelFormatEnum::RGB24, w, h)
            .map_err(|e| format!("SDL_CreateTexture failed: {e}"))?;
        self.texture = Some(texture);
        self.tex_w = w;
        self.tex_h = h;
        Ok(())
    }

    /// Drain the SDL event queue; flags window-close / ESC / q.
    fn poll_events(&mut self, quit: &mut bool) {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => *quit = true,
                Event::KeyDown { keycode: Some(keycode), .. } => {
                    if matches!(keycode, Keycode::Escape | Keycode::Q) {
                        *quit = true;
                    }
                }
                _ => {}
            }
        }
    }

    /// Blit the framebuffer and show it.
    fn present(&mut self, fb: &[u8], quit: &mut bool) {
        let Some(texture) = &mut self.texture else { return };
        let _ = texture.update(None, fb, (self.tex_w * 3) as usize);
        self.canvas.set_draw_color(Color::RGB(0, 0, 0));
        self.canvas.clear();
        let _ = self.canvas.copy(texture, None, None);
        self.canvas.present();
        self.poll_events(quit);
    }
}

// ─────────────────────────────────────────────────────────────
// VGA monitor
// ─────────────────────────────────────────────────────────────

// Fixed-point: all "fp" quantities below are in units of
// 1/FP_SCALE simulated clock cycles.
const FP_SHIFT: u32 = 16;
const FP_SCALE: u64 = 1 << FP_SHIFT;

/// The `virt::VirtVga` monitor: measures the sync pins, identifies the
/// VESA mode, captures the RGB444 pixel stream into a framebuffer, and
/// presents completed frames in an SDL2 window.
pub struct VgaMonitor {
    // Raw pin history
    prev_hs: bool,
    prev_vs: bool,
    have_prev: bool,
    /// Global sim cycle counter.
    t: u64,

    /// The `virt::VirtVga` platform clock frequency (`@period_ns`),
    /// needed to expect sync periods in cycles (the C++ harness baked
    /// it in as `VIRT_CLOCK_HZ` at compile time).
    clock_hz: u64,

    hs: SyncMeasure,
    vs: SyncMeasure,

    // Identification
    mode: Option<&'static VesaMode>,
    unrecognized_warns: u64,

    // Sync polarity as driven by the design (recovered from duty cycle:
    // the pulse is the <20% duty level), set at adoption time.
    hs_pulse_low: bool,
    vs_pulse_low: bool,

    // Capture geometry (fixed point cycles)
    /// Cycles per pixel.
    pixel_period_fp: u64,
    /// HSYNC pulse width, cycles.
    sync_width_fp: u64,
    /// First visible line, counted from vsync pulse start.
    v_origin_line: i64,
    /// Lines since vsync pulse start (-1 = pulse line not yet ended;
    /// see `cycle`).
    line_index: i64,
    /// Cycles since line (hsync pulse) start.
    line_time_fp: u64,
    /// Line time of the next pixel capture.
    next_capture_fp: u64,
    /// Next visible pixel x within the line.
    capture_x: u32,
    /// Capture starts at the next vsync pulse.
    frame_armed: bool,
    /// Capturing a frame now.
    frame_active: bool,
    /// This line is in the visible band.
    line_capture: bool,

    // SDL
    gfx: Option<SdlGfx>,
    /// Set after the first SDL failure: no window is attempted again
    /// this run (headless environments; see `adopt`).
    sdl_failed: bool,
    /// RGB24, w*h*3.
    fb: Vec<u8>,
    frames_presented: u64,
    quit: bool,
    /// Print identification/teardown messages (the board's
    /// `--verbose` flag).
    verbose: bool,
}

impl VgaMonitor {
    pub fn new(clock_hz: u64, verbose: bool) -> VgaMonitor {
        VgaMonitor {
            prev_hs: true,
            prev_vs: true,
            have_prev: false,
            t: 0,
            clock_hz,
            hs: SyncMeasure::new(),
            vs: SyncMeasure::new(),
            mode: None,
            unrecognized_warns: 0,
            hs_pulse_low: false,
            vs_pulse_low: false,
            pixel_period_fp: 0,
            sync_width_fp: 0,
            v_origin_line: 0,
            line_index: 0,
            line_time_fp: 0,
            next_capture_fp: 0,
            capture_x: 0,
            frame_armed: false,
            frame_active: false,
            line_capture: false,
            gfx: None,
            sdl_failed: false,
            fb: Vec::new(),
            frames_presented: 0,
            quit: false,
            verbose,
        }
    }

    /// Whether the user closed the window (or pressed ESC/q); the board
    /// ends the run when this goes true.
    pub fn quit(&self) -> bool {
        self.quit
    }

    /// Expected HSYNC period in cycles for `m`.
    fn exp_line_period(&self, m: &VesaMode) -> f64 {
        m.h.total() as f64 * self.clock_hz as f64 / m.pixel_clock_hz as f64
    }

    /// Worst-case relative error between the measured sync timings and
    /// `m`, or a value > MATCH_TOLERANCE if any quantity is off.
    ///
    /// Pulse *polarity* is deliberately not scored: sync pulses are
    /// <20% duty in every table mode, so the pulse is whichever level
    /// has the lower duty cycle, whatever the design's polarity.  (The
    /// Nexys4 reference drives active-high syncs; VESA modes specify
    /// varying polarity — a monitor recovers it, and so does this.)
    /// With polarity ignored, no two table modes share period/pulse
    /// timings, so identification stays unambiguous.
    fn match_error(&self, m: &VesaMode) -> f64 {
        let line = self.hs.period();
        let frame = self.vs.period();
        let exp_line = self.exp_line_period(m);
        let exp_frame = m.v.total() as f64 * exp_line;
        let exp_hs = m.h.sync as f64 * self.clock_hz as f64 / m.pixel_clock_hz as f64;

        // Vertical sync pulses are a couple of *lines* wide: scale the
        // expectation with the measured line period rather than the
        // table's own pixel clock, so a design running the mode at a
        // slightly-off pixel rate (25 MHz vs 25.175) still matches.
        // (v.sync is in *lines*; lines × cycles/line = cycles.)
        let exp_vs = m.v.sync as f64 * line;

        let hs_pulse = self.hs.pulse_low().min(self.hs.pulse_high());
        let vs_pulse = self.vs.pulse_low().min(self.vs.pulse_high());

        let errs = [
            (line - exp_line).abs() / exp_line,
            (frame - exp_frame).abs() / exp_frame,
            (hs_pulse - exp_hs).abs() / exp_hs,
            (vs_pulse - exp_vs).abs() / exp_vs,
        ];
        let mut worst = 0.0f64;
        for e in errs {
            worst = worst.max(e);
        }
        worst
    }

    /// Adopt the best-matching mode, or `None` if none is close.
    fn identify(&self) -> Option<&'static VesaMode> {
        let mut best = None;
        let mut best_err = MATCH_TOLERANCE;
        for m in &VESA_MODES {
            let e = self.match_error(m);
            if e < best_err {
                best = Some(m);
                best_err = e;
            }
        }
        best
    }

    /// Tear the identification down (e.g. on drift) to re-measure.
    fn drop_mode(&mut self, why: &str) {
        if self.verbose {
            eprintln!(
                "virt: vga: sync timings drifted from {} ({why}); re-measuring",
                self.mode.map(|m| m.name).unwrap_or("?")
            );
        }
        self.mode = None;
        self.frame_armed = false;
        self.frame_active = false;
        self.line_capture = false;
    }

    /// Adopt `m`: derive the capture geometry from the *measured*
    /// timings (so the monitor tracks the design's actual pixel rate,
    /// whatever divider it uses) and open the window.
    fn adopt(&mut self, m: &'static VesaMode) {
        self.mode = Some(m);

        // Polarity: whichever level has the lower duty cycle is the
        // pulse (see `match_error`).
        self.hs_pulse_low = self.hs.pulse_low() < self.hs.pulse_high();
        self.vs_pulse_low = self.vs.pulse_low() < self.vs.pulse_high();

        // Pixel rate: measured line period / total pixels per line.
        let line = self.hs.period(); // cycles
        let px = line / m.h.total() as f64; // cycles per pixel
        self.pixel_period_fp = (px * FP_SCALE as f64).round() as u64;

        // HSYNC pulse width as the design drives it.
        let hsw = if self.hs_pulse_low {
            self.hs.pulse_low()
        } else {
            self.hs.pulse_high()
        };
        self.sync_width_fp = (hsw * FP_SCALE as f64).round() as u64;

        // Vertical: the vsync pulse is vsync_lines lines long, then the
        // back porch, then the visible lines.
        let vsw = if self.vs_pulse_low {
            self.vs.pulse_low()
        } else {
            self.vs.pulse_high()
        };
        let vsync_lines = vsw / line;
        self.v_origin_line = vsync_lines.round() as i64 + m.v.back as i64;

        // Open (or retarget) the window.  Unrecoverable in a headless
        // environment: SDL is attempted exactly once; if it fails, the
        // window is given up for the whole run (silently, after one
        // message) and the board keeps simulating — UART, SPI flash,
        // and `exit` still work, the picture just cannot show.  (The
        // C++ harness retries `sdl_setup` from `adopt` on every cycle
        // in this situation, flooding stderr; not repeating that
        // here.)
        if self.sdl_failed {
            // No window: fall through to the capture setup so the
            // identification state machine stays coherent.
        } else if self.gfx.is_none() {
            match SdlGfx::open(m.h.visible, m.v.visible) {
                Ok(gfx) => self.gfx = Some(gfx),
                Err(e) => {
                    eprintln!("virt: {e}");
                    self.sdl_failed = true;
                }
            }
        } else if let Err(e) = self.gfx.as_mut().unwrap().resize(m.h.visible, m.v.visible) {
            eprintln!("virt: {e}");
            self.sdl_failed = true;
        }
        // Fresh zeroed framebuffer for the new mode (like the C++
        // `fb.assign(w*h*3, 0)` in `sdl_setup`).
        self.fb = vec![0; m.h.visible as usize * m.v.visible as usize * 3];
        self.frame_armed = true; // start capturing at the next vsync pulse
        self.frame_active = false;
        if self.verbose {
            eprintln!(
                "virt: vga: identified {} ({} MHz pixel clock; {:.2} cycles/pixel measured, {}-sync)",
                m.name,
                m.pixel_clock_hz / 1_000_000,
                px,
                if self.hs_pulse_low { "active-low" } else { "active-high" }
            );
        }
    }

    /// Re-check the measured timings against the adopted mode, at each
    /// frame boundary.
    fn check_drift(&mut self) {
        let Some(mode) = self.mode else { return };
        if self.match_error(mode) > MATCH_TOLERANCE {
            self.drop_mode("mode timings no longer match");
        }
    }

    /// One simulated clock cycle: `hs`/`vs` are the sync pins, `r`/`g`/`b`
    /// the 4-bit RGB buses, all sampled after the posedge eval.
    pub fn cycle(&mut self, hs_pin: bool, vs_pin: bool, r: u8, g: u8, b: u8) {
        self.t += 1;
        // Advance line time first: the line-start cycle below (the
        // cycle whose posedge begins the sync pulse) is time 0, so the
        // first visible pixel is captured a whole number of pixel
        // periods after it, aligned with the design's own counters.
        self.line_time_fp += FP_SCALE;
        self.hs.step(hs_pin, self.t);
        self.vs.step(vs_pin, self.t);

        // Sync *pulse starts*: edges into the design's pulse level
        // (polarity recovered at adoption; hs_pulse_low ? falling : rising).
        let mut hs_start = false;
        let mut vs_start = false;
        if self.have_prev {
            hs_start = hs_pin != self.prev_hs && (if self.hs_pulse_low { !hs_pin } else { hs_pin });
            vs_start = vs_pin != self.prev_vs && (if self.vs_pulse_low { !vs_pin } else { vs_pin });
        }
        self.have_prev = true;
        self.prev_hs = hs_pin;
        self.prev_vs = vs_pin;

        // ── identification (before capture, so mode is fresh) ──
        if self.mode.is_none() && self.hs.ready() && self.vs.ready() {
            match self.identify() {
                Some(m) => self.adopt(m),
                None if self.unrecognized_warns < 3 => {
                    self.unrecognized_warns += 1;
                    eprintln!(
                        "virt: vga: unrecognized sync timings after {} cycles \
                         (line {:.0}, frame {:.0}, hs pulse {:.0}, vs pulse {:.0}); \
                         add the mode to the VESA_MODES table in run/vga.rs?",
                        self.t,
                        self.hs.period(),
                        self.vs.period(),
                        self.hs.pulse_low().min(self.hs.pulse_high()),
                        self.vs.pulse_low().min(self.vs.pulse_high()),
                    );
                }
                None => {}
            }
        }

        // ── frame boundary ──
        if vs_start {
            if self.frame_active {
                self.present();
            }
            self.check_drift();
            if self.mode.is_some() && self.frame_armed {
                self.frame_active = true;
            }
            self.line_index = -1; // the vsync pulse start line; line 0 is the
                                  // next hsync pulse start (usually this cycle)
        }

        // ── line boundary ──
        if let (Some(mode), true) = (self.mode, hs_start) {
            self.line_index += 1;
            self.line_time_fp = 0;
            self.capture_x = 0;
            self.line_capture = self.frame_active
                && self.line_index >= self.v_origin_line
                && self.line_index < self.v_origin_line + mode.v.visible as i64;
            // First visible pixel: after the hsync pulse and the back
            // porch, both as the *design* runs them (measured pulse
            // width; porch in pixels at the measured pixel rate).
            self.next_capture_fp = self.sync_width_fp
                + (mode.h.back as u64) * self.pixel_period_fp;
        }

        // ── pixel capture ──
        if self.mode.is_some() && self.line_capture {
            let w = self.mode.unwrap().h.visible;
            while self.capture_x < w && self.line_time_fp >= self.next_capture_fp {
                let y = self.line_index - self.v_origin_line;
                if y >= 0 && y < self.mode.unwrap().v.visible as i64 {
                    let idx = ((y as u32 * w + self.capture_x) as usize) * 3;
                    self.fb[idx] = (r << 4) | (r & 0xF); // 4-bit → 8-bit
                    self.fb[idx + 1] = (g << 4) | (g & 0xF);
                    self.fb[idx + 2] = (b << 4) | (b & 0xF);
                }
                self.capture_x += 1;
                self.next_capture_fp += self.pixel_period_fp;
            }
        }

        // Window stays responsive even without vsync.
        if self.gfx.is_some() && self.t % EVENT_POLL_INTERVAL == 0 {
            self.poll_events();
        }
    }

    /// Drain the SDL event queue (if SDL is up).
    fn poll_events(&mut self) {
        if let Some(gfx) = &mut self.gfx {
            gfx.poll_events(&mut self.quit);
        }
    }

    /// Blit the framebuffer and show it.
    fn present(&mut self) {
        if let Some(gfx) = &mut self.gfx {
            gfx.present(&self.fb, &mut self.quit);
            self.frames_presented += 1;
        }
    }
}
