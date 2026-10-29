//! Screen output for the test runner.
//!
//! One seam, two adapters.  [`Display::open`] decides the mode once:
//! a terminal with a known height gets streamed output with an
//! in-place footer; everything else (pipes, redirects, size-less
//! terminals) gets plain streaming lines.  Callers never branch on
//! the mode again.
//!
//! Both adapters stream: each test's line is printed the moment the
//! test finishes, so the scrollback accumulates the whole run
//! naturally — during the run as well as after it — in completion
//! order.  On a terminal, a footer is repainted in place at the
//! bottom of the screen: one `⌛` row per in-flight test (never more
//! than `--jobs`, hence bounded) and the progress line on the bottom
//! row.  The footer is the only thing ever repainted; the screen
//! above it only ever scrolls, one line per finished test, and every
//! line that scrolls off lands in the scrollback in order.  Nothing
//! is overwritten except footer rows: the footer reserves its rows by
//! scrolling them out of the pre-run screen content, so the pre-run
//! screen survives too.
//!
//! At the end the footer is cleared and the summary prints as
//! ordinary output below the last test line; Ctrl+C clears the footer
//! and parks the cursor for the shell prompt.

pub mod footer;
pub mod term;

use std::io::Write;
use std::time::Duration;

use crate::discovery::Test;
use crate::exec::Status;

enum Mode {
    /// Terminal: streamed lines plus the in-place footer.
    Tty(footer::Footer),
    /// Pipe/redirect: plain lines, one per finished test.
    Pipe,
}

/// The output of the test run, behind one small interface.
///
/// Callers report events (`update`), animation ticks (`tick`), and
/// the two exits (`finish`, `interrupt`); everything about the mode,
/// the geometry, the truncation, and the terminal lives behind this
/// interface.
pub struct Display {
    mode: Mode,
    name_width: usize,
    statuses: Vec<Option<Status>>,
    in_flight: Vec<usize>,
    spinner: usize,
}

impl Display {
    /// Decide the mode once, from whether stdout is a terminal with a
    /// known height.  `jobs` bounds the footer's height.
    pub fn open(tests: &[Test], jobs: usize) -> Display {
        let tty = std::io::IsTerminal::is_terminal(&mut std::io::stdout());
        let screen_mode = tty && term::term_size().0.is_some();
        let name_width = tests.iter().map(|t| t.name.len()).max().unwrap_or(0);
        let mode = if screen_mode {
            let mut footer = footer::Footer::new(jobs);
            if let Some(screen) = term::Screen::probe() {
                // Reserve the footer region before anything prints:
                // take over the cursor's row, move down through the
                // blank rows below the prompt (scrolling printed lines
                // into the scrollback at the bottom margin), and anchor
                // the region on the terminal's saved cursor.  No
                // termios, no queries — the terminal's state is never
                // modified beyond cursor motion.
                let mut out = std::io::stdout().lock();
                footer.reserve(&mut out, screen);
                out.flush().ok();
            }
            Mode::Tty(footer)
        } else {
            Mode::Pipe
        };
        Display {
            mode,
            name_width,
            statuses: vec![None; tests.len()],
            in_flight: Vec::new(),
            spinner: 0,
        }
    }

    /// True when the terminal footer is in use (as opposed to plain
    /// streaming lines).
    pub fn is_tty(&self) -> bool {
        matches!(self.mode, Mode::Tty(_))
    }

    /// A test was claimed (`None`) or finished (`Some`).
    pub fn update(&mut self, tests: &[Test], idx: usize, status: Option<Status>) {
        let spinner = self.spinner;
        let name_width = self.name_width;
        match status {
            None => {
                self.in_flight.push(idx);
                if let Mode::Tty(footer) = &mut self.mode {
                    if let Some(screen) = term::Screen::probe() {
                        let mut out = std::io::stdout().lock();
                        let done = self.statuses.iter().flatten().count();
                        footer.repaint(
                            &mut out, screen, tests, &self.in_flight,
                            done, spinner, name_width,
                        );
                    }
                }
            }
            Some(status) => {
                self.statuses[idx] = Some(status);
                self.in_flight.retain(|&i| i != idx);
                match &mut self.mode {
                    Mode::Tty(footer) => {
                        if let Some(screen) = term::Screen::probe() {
                            let mut out = std::io::stdout().lock();
                            let done = self.statuses.iter().flatten().count();
                            footer.append(
                                &mut out, screen, tests, idx, status,
                                &self.in_flight, done, spinner, name_width,
                            );
                        } else {
                            // Size vanished mid-run: stream the line
                            // plainly rather than lose it.
                            let line =
                                term::format_row(&tests[idx], Some(status), 0, None, name_width);
                            println!("{line}");
                        }
                    }
                    Mode::Pipe => {
                        // One plain line per finished test, no
                        // in-progress lines, no escape codes.
                        let line =
                            term::format_row(&tests[idx], Some(status), 0, None, name_width);
                        println!("{line}");
                    }
                }
            }
        }
    }

    /// Animation tick: repaint the footer with a fresh spinner
    /// (terminal mode only; the pipe stays silent during the run so
    /// the per-test lines appear in completion order).
    pub fn tick(&mut self, tests: &[Test], spinner: usize) {
        self.spinner = spinner;
        if let Mode::Tty(footer) = &mut self.mode {
            if let Some(screen) = term::Screen::probe() {
                let mut out = std::io::stdout().lock();
                let done = self.statuses.iter().flatten().count();
                footer.repaint(
                    &mut out, screen, tests, &self.in_flight,
                    done, spinner, self.name_width,
                );
            }
        }
    }

    /// Final report: clear the footer and print the summary as
    /// ordinary output below the last streamed test line.  (The test
    /// lines themselves are already in the scrollback, in completion
    /// order.)
    pub fn finish(&mut self, tests: &[Test], log_paths: &[Option<String>], wall: Duration) {
        if let Mode::Tty(footer) = &mut self.mode {
            let mut out = std::io::stdout().lock();
            footer.clear(&mut out);
            out.flush().ok();
        }
        summary(tests, &self.statuses, log_paths, wall);
    }

    /// Ctrl+C: clear the footer region and park the cursor just above
    /// it; the streamed test lines above stay on screen and in the
    /// scrollback.  Pipe mode does nothing (the default SIGINT
    /// disposition applies, and the caller exits).
    pub fn interrupt(&mut self) {
        if let Mode::Tty(footer) = &mut self.mode {
            let mut out = std::io::stdout().lock();
            footer.clear(&mut out);
            out.flush().ok();
        }
    }

    /// Final statuses, for fail-fast bookkeeping and exit codes.
    pub fn statuses(&self) -> &[Option<Status>] {
        &self.statuses
    }
}

/// End summary for streaming output: counts per status, skips named
/// with reasons, failures re-listed in canonical order with their
/// evidence logs, wall time.  No screen constraints, so nothing is
/// truncated.
fn summary(
    tests: &[Test],
    statuses: &[Option<Status>],
    log_paths: &[Option<String>],
    wall: Duration,
) {
    let (mut passed, mut failed, mut skipped) = (0usize, 0usize, 0usize);
    for status in statuses.iter().flatten() {
        match status {
            Status::Pass => passed += 1,
            Status::Fail => failed += 1,
            Status::Skip => skipped += 1,
        }
    }

    let mut out = std::io::stdout().lock();
    writeln!(out).ok();
    writeln!(
        out,
        "{} tests: {} passed, {} failed, {} skipped ({:.2}s)",
        tests.len(),
        passed,
        failed,
        skipped,
        wall.as_secs_f64()
    )
    .ok();

    if skipped > 0 {
        writeln!(out).ok();
        writeln!(out, "Skipped ({}):", skipped).ok();
        for (test, status) in tests.iter().zip(statuses.iter()) {
            if let Some(Status::Skip) = status {
                let reason = test.skip.as_deref().unwrap_or("");
                let suffix = if reason.is_empty() { String::new() } else { format!(" ({reason})") };
                let desc = if test.description.is_empty() {
                    String::new()
                } else {
                    format!("  {}", test.description)
                };
                writeln!(out, " ⚠️ {}{desc}{suffix}", test.name).ok();
            }
        }
    }

    if failed > 0 {
        writeln!(out).ok();
        writeln!(out, "Failed ({}), in canonical order:", failed).ok();
        for (i, (test, status)) in tests.iter().zip(statuses.iter()).enumerate() {
            if let Some(Status::Fail) = status {
                let desc = if test.description.is_empty() {
                    String::new()
                } else {
                    format!("  {}", test.description)
                };
                writeln!(out, " ❌ {}{desc}", test.name).ok();
                if let Some(Some(log_path)) = log_paths.get(i) {
                    writeln!(out, "    log: {log_path}").ok();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discovery::test_stub;
    use std::time::Duration;

    fn tests(n: usize) -> Vec<Test> {
        (0..n).map(|i| test_stub(&format!("tests/t{i}"), "")).collect()
    }

    fn stream_display(n: usize) -> Display {
        // Display::open cannot know stdout is not a terminal during
        // tests, so construct the pipe mode directly.
        let mut display = Display::open(&tests(n), 4);
        display.mode = Mode::Pipe;
        display
    }

    #[test]
    fn pipe_mode_prints_one_line_per_finished_test() {
        let mut display = stream_display(3);
        let ts = tests(3);
        display.update(&ts, 1, Some(Status::Fail));
        assert_eq!(display.statuses()[1], Some(Status::Fail));
        assert!(!display.is_tty());
    }

    #[test]
    fn finish_reports_counts() {
        let mut display = stream_display(3);
        let ts = tests(3);
        display.update(&ts, 0, Some(Status::Pass));
        display.update(&ts, 2, Some(Status::Skip));
        display.finish(&ts, &[None, None, None], Duration::from_secs(1));
    }

    #[test]
    fn in_flight_tracks_started_and_finished() {
        let mut display = stream_display(3);
        let ts = tests(3);
        display.update(&ts, 0, None);
        display.update(&ts, 1, None);
        assert_eq!(display.in_flight, vec![0, 1]);
        display.update(&ts, 0, Some(Status::Pass));
        assert_eq!(display.in_flight, vec![1]);
    }
}
