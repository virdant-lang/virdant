//! Screen output.
//!
//! On a terminal the display is repainted in place and the terminal
//! **never scrolls**: every frame re-anchors the cursor at the bottom
//! margin (cursor-down clamps there without scrolling), so a resize
//! cannot desynchronize the display, nothing is ever pushed off the
//! top, and the scrollback is never written to.
//!
//! The test lines are listed from the top of the screen, packed with no
//! gaps, growing downward as tests are dispatched; one blank line
//! follows the last test line, then the progress line.  Once the test
//! lines reach the bottom of the screen, the list becomes a sliding
//! window over the canonical test list (the oldest lines are replaced
//! in place as new tests are dispatched) — until then the display
//! simply grows.
//!
//! At the end the summary is printed without clearing or scrolling:
//! the counts line replaces the progress line and the skips/failures
//! detail is written above it (replacing the bottommost test lines);
//! everything else stays on screen.
//!
//! Without a terminal (piped output) everything is printed at the end,
//! in canonical order, untruncated.

use std::io::{IsTerminal, Write};
use std::time::Duration;

use crate::discovery::Test;
use crate::exec::Status;

pub struct Renderer {
    /// True when stdout is a terminal.
    pub tty: bool,
    total: usize,
    name_width: usize,
    /// Terminal width in columns, when known (TTY only); re-queried
    /// every frame so a resize corrects itself.
    term_cols: std::cell::Cell<Option<usize>>,
    /// Terminal height in rows, when known (TTY only); re-queried
    /// every frame so a resize corrects itself.
    term_height: std::cell::Cell<Option<usize>>,
}

const SPINNER: [&str; 2] = ["⌛", "⏳"];

impl Renderer {
    pub fn new(tests: &[Test], term_size: (Option<u16>, Option<u16>)) -> Renderer {
        let (term_height, term_cols) = term_size;
        let tty = std::io::stdout().is_terminal();
        let name_width = tests.iter().map(|t| t.name.len()).max().unwrap_or(0);
        Renderer {
            tty,
            total: tests.len(),
            name_width,
            term_cols: std::cell::Cell::new(term_cols.map(|c| c as usize)),
            term_height: std::cell::Cell::new(term_height.map(|h| h as usize)),
        }
    }

    /// True when the in-place, never-scrolling screen display can be
    /// used (TTY with a known height).
    pub fn is_screen(&self) -> bool {
        self.tty && self.term_height.get().is_some()
    }

    /// Spinner symbol for a tick counter.
    pub fn spinner_char(&self, spinner: usize) -> &'static str {
        SPINNER[spinner % SPINNER.len()]
    }

    /// How many test rows fit above the blank separator and the
    /// progress line.
    fn capacity(&self) -> usize {
        let h = self.term_height.get().unwrap_or(0);
        h.saturating_sub(2).max(1)
    }

    /// Display width of a char: the status symbols (and other emoji)
    /// are 2 columns, the emoji variation selector is 0, East-Asian
    /// ambiguous characters (`—`, `§`) are assumed 2 (some terminals
    /// render them wide; assuming wide only ever over-truncates, never
    /// wraps), everything else 1.
    fn char_width(c: char) -> usize {
        match c {
            '\u{FE0F}' => 0,
            '\u{231B}' | '\u{23F3}' | '\u{26A0}' | '\u{2705}' | '\u{274C}'
            | '\u{2014}' | '\u{00A7}' => 2,
            _ => 1,
        }
    }

    /// Usable columns for line content: the terminal width minus one,
    /// so a line never occupies the final column — some terminals wrap
    /// (or enter the wrap-pending state) on a full-width line.
    fn usable_cols(&self) -> Option<usize> {
        self.term_cols.get().map(|c| c.saturating_sub(1))
    }

    /// Hard-truncate a line so it occupies at most the usable width —
    /// a wrapped line would scroll the screen, which must never happen.
    fn truncate(&self, line: &str) -> String {
        let Some(cols) = self.usable_cols() else {
            return line.to_string();
        };
        let mut width = 0usize;
        let mut out = String::new();
        for c in line.chars() {
            let w = Self::char_width(c);
            if width + w > cols {
                break;
            }
            width += w;
            out.push(c);
        }
        out
    }

    /// Cut a description at the TTY boundary: when it would run past
    /// the terminal's last column, it is truncated so that the last
    /// three columns of the line hold `...`.
    pub fn fit_description(prefix_cols: usize, desc: &str, term_cols: Option<usize>) -> String {
        let Some(cols) = term_cols else {
            return desc.to_string();
        };
        let available = cols.saturating_sub(prefix_cols);
        let char_len = desc.chars().count();
        if char_len <= available {
            return desc.to_string();
        }
        // Truncate to leave room for the `...` in the last three columns.
        match available.checked_sub(3) {
            Some(kept) if kept > 0 => {
                let kept_text: String = desc.chars().take(kept).collect();
                if kept_text.trim().is_empty() {
                    String::new()
                } else {
                    format!("{kept_text}...")
                }
            }
            _ => String::new(),
        }
    }

    /// Columns before the description begins: symbol (2 columns wide),
    /// space, padded name, two spaces of padding.
    fn description_prefix(&self) -> usize {
        2 + 1 + self.name_width + 2
    }

    /// The full one-line rendering of a test in a given status.
    /// `spinner` cycles only for in-flight rows.
    pub fn line(&self, test: &Test, status: Option<Status>, spinner: usize) -> String {
        let symbol = match status {
            Some(status) => status.symbol(),
            None => SPINNER[spinner % SPINNER.len()],
        };
        let mut line = format!("{} {:<width$}", symbol, test.name, width = self.name_width);
        let mut desc = test.description.clone();
        if let Some(status) = status {
            if status == Status::Skip && !test.skip.as_deref().unwrap_or("").is_empty() {
                if !desc.is_empty() {
                    desc.push(' ');
                }
                desc.push_str(&format!("({})", test.skip.as_deref().unwrap_or("")));
            }
        }
        if !desc.is_empty() {
            let fitted = Self::fit_description(self.description_prefix(), &desc, self.usable_cols());
            if !fitted.is_empty() {
                line.push_str("  ");
                line.push_str(&fitted);
            }
        }
        self.truncate(&line)
    }

    /// Repaint the display in place.  Never scrolls the terminal, and
    /// never writes to the scrollback — even across resizes, because
    /// every frame re-anchors the cursor at the bottom margin.
    ///
    /// The test lines are listed from the top of the screen, packed
    /// with no gaps, one blank line after the last, then the progress
    /// line.  The list grows downward as tests are dispatched; once it
    /// reaches the bottom of the screen it becomes a sliding window
    /// over the canonical test list (the oldest lines are replaced in
    /// place as new tests are dispatched).  Below the progress line,
    /// pre-existing screen content is left untouched.
    pub fn frame(&self, tests: &[Test], statuses: &[Option<Status>], frontier: usize, spinner: usize) {
        if !self.tty {
            return;
        }
        // Re-query the terminal every frame so a resize corrects itself
        // (the geometry is derived fresh, and every frame repaints the
        // whole display).
        let (h, w) = term_size();
        self.term_height.set(h.map(|x| x as usize));
        self.term_cols.set(w.map(|x| x as usize));
        if !self.is_screen() {
            return;
        }
        let h = self.term_height.get().unwrap_or(1);
        let capacity = self.capacity();
        let end = frontier.min(self.total.saturating_sub(1));
        let count = (end + 1).min(capacity); // test rows shown (all dispatched)
        let begin = end + 1 - count;
        let mut out = std::io::stdout().lock();

        // Re-anchor: cursor-down clamps at the bottom margin without
        // scrolling, so this is safe no matter where the cursor is
        // (resizes, previous partial frames, ...).  Then up to row 0.
        write!(out, "\x1b[999B").ok();
        write!(out, "\x1b[{}A", h - 1).ok();
        for i in 0..count {
            let idx = begin + i;
            let row = self.line(&tests[idx], statuses[idx], spinner);
            write!(out, "\r\x1b[2K{row}\n").ok();
        }
        // One blank line after the last test line, then the progress
        // line.  (Writing the blank also clears the previous frame's
        // progress line, which sat exactly here.)
        write!(out, "\r\x1b[2K\n").ok();
        let done = statuses.iter().flatten().count();
        let running = frontier.saturating_sub(done).min(self.total);
        let progress = self.truncate(&format!(
            "{} {done}/{} done, {running} running",
            self.spinner_char(spinner),
            self.total
        ));
        write!(out, "\r\x1b[2K{progress}").ok();
        out.flush().ok();
    }

    /// Final summary, printed without clearing or scrolling: the
    /// counts line replaces the progress line and the skips/failures
    /// detail is written above it (replacing the bottommost test
    /// lines); everything else stays on screen.
    pub fn end_summary(&self, tests: &[Test], statuses: &[Option<Status>], wall: Duration) {
        if !self.is_screen() {
            return;
        }
        // Re-query, same as frame(), in case of a late resize.
        let (h, w) = term_size();
        self.term_height.set(h.map(|x| x as usize));
        self.term_cols.set(w.map(|x| x as usize));
        let h = self.term_height.get().unwrap_or(1);
        let capacity = self.capacity();
        let count = self.total.min(capacity);
        let progress_row = count + 1; // where the progress line sits

        let (mut passed, mut failed, mut skipped) = (0usize, 0usize, 0usize);
        for status in statuses.iter().flatten() {
            match status {
                Status::Pass => passed += 1,
                Status::Fail => failed += 1,
                Status::Skip => skipped += 1,
            }
        }
        let mut detail: Vec<String> = Vec::new();
        if skipped > 0 {
            detail.push(String::new());
            detail.push(format!("Skipped ({skipped}):"));
            for (test, status) in tests.iter().zip(statuses.iter()) {
                if let Some(Status::Skip) = status {
                    let reason = test.skip.as_deref().unwrap_or("");
                    let suffix = if reason.is_empty() { String::new() } else { format!(" ({reason})") };
                    detail.push(format!("⚠️ {}{suffix}", test.name));
                }
            }
        }
        if failed > 0 {
            detail.push(String::new());
            detail.push(format!("Failed ({failed}), in canonical order:"));
            for (test, status) in tests.iter().zip(statuses.iter()) {
                if let Some(Status::Fail) = status {
                    detail.push(format!("❌ {}", test.name));
                }
            }
        }
        detail.push(String::new());

        // The detail is written upward from the progress line and can
        // replace the bottommost test lines; it can never exceed the
        // rows above the counts line.
        let s = detail.len().min(progress_row);
        let detail = &detail[detail.len() - s..];

        let counts = self.truncate(&format!(
            "{} tests: {passed} passed, {failed} failed, {skipped} skipped ({:.2}s)",
            tests.len(),
            wall.as_secs_f64()
        ));
        let mut out = std::io::stdout().lock();
        // Re-anchor at the bottom margin, then up to the progress row...
        write!(out, "\x1b[999B").ok();
        let up = h - 1 - progress_row;
        if up > 0 {
            // NB: CSI 0 A means "up 1" (0 defaults to 1), so only emit
            // the sequence for a non-zero distance.
            write!(out, "\x1b[{}A", up).ok();
        }
        // ...up to the detail top, and write the detail...
        if s > 0 {
            write!(out, "\x1b[{}A", s).ok();
            for line in detail {
                let line = self.truncate(line);
                write!(out, "\r\x1b[2K{line}\n").ok();
            }
        }
        // ...then the counts line on the progress row (no trailing
        // newline: nothing scrolls, the shell prompt joins this line).
        write!(out, "\r\x1b[2K{counts}").ok();
        out.flush().ok();
    }
}

/// Terminal size in (rows, columns), via TIOCGWINSZ.
pub fn term_size() -> (Option<u16>, Option<u16>) {
    unsafe {
        let mut ws: libc::winsize = std::mem::zeroed();
        if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut ws) == 0 {
            let rows = if ws.ws_row > 0 { Some(ws.ws_row) } else { None };
            let cols = if ws.ws_col > 0 { Some(ws.ws_col) } else { None };
            (rows, cols)
        } else {
            (None, None)
        }
    }
}

/// End summary for piped output: counts per status, skips named,
/// failures re-listed in canonical order, wall time.  No screen
/// constraints, so nothing is truncated.
pub fn summary(
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
