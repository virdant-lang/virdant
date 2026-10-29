//! Terminal primitives for the live footer: geometry probing,
//! cursor-relative row writes, and display-width-aware line shaping.
//!
//! The footer is anchored on the terminal's **saved cursor** (DECSC
//! at reserve time): every paint restores it with DECRC and walks the
//! region with relative cursor-up moves, clearing and writing each
//! row in place.  No linefeed is ever emitted by a paint — so a
//! repaint can never scroll the terminal, no matter where the cursor
//! is left or how a resize interleaves with a frame.  Scrolling is
//! explicit: [`Screen::scroll`] emits the display's only linefeeds,
//! one per streamed test line, each pushing the oldest screen line
//! into the scrollback in order.  The terminal's termios is never
//! touched.
//!
//! The line-shaping functions here are pure: they take widths as
//! arguments and return strings, so they are unit-testable without a
//! terminal.

use std::io::Write;

use crate::discovery::Test;
use crate::exec::Status;

pub const SPINNER: [&str; 2] = ["⌛", "⏳"];

/// Spinner symbol for a tick counter.
pub fn spinner_char(spinner: usize) -> &'static str {
    SPINNER[spinner % SPINNER.len()]
}

/// Terminal geometry, probed with TIOCGWINSZ.
#[derive(Debug, Clone, Copy)]
pub struct Screen {
    /// Rows; never zero (0 means "unknown" and callers skip frames).
    pub rows: usize,
    /// Columns, when known.
    pub cols: Option<usize>,
}

impl Screen {
    /// Probe the current geometry; `None` when the size is unknown or
    /// degenerate.
    pub fn probe() -> Option<Screen> {
        let (rows, cols) = term_size();
        let rows = rows.filter(|r| *r >= 1)? as usize;
        Some(Screen { rows, cols: cols.map(|c| c as usize) })
    }

    /// Usable columns for line content: one less than the width, so a
    /// line never occupies the final column (some terminals wrap, or
    /// enter the wrap-pending state, on a full-width line).
    pub fn usable_cols(&self) -> Option<usize> {
        self.cols.map(|c| c.saturating_sub(1))
    }

    /// Scroll the screen by one line: position the cursor on the
    /// bottom margin and emit a linefeed.  This is the *only*
    /// linefeed the in-place display ever emits after the reserve,
    /// and each one corresponds to a test line just appended above
    /// the footer — the line pushed off the top lands in the
    /// scrollback, in order.
    pub fn scroll<W: Write>(&self, out: &mut W) {
        let _ = write!(out, "\x1b[{};1H\n", self.rows);
    }
}

/// Overwrite the row the cursor is on: return to column 0, clear the
/// line, write the text.  Never emits a linefeed; the cursor is left
/// at the end of the written text.
pub fn write_at_cursor<W: Write>(out: &mut W, line: &str) {
    let _ = write!(out, "\r\x1b[2K{line}");
}

/// Move the cursor up `n` rows (clamped at the top of the screen;
/// never scrolls).  `n` must be non-zero — CSI 0 A means "up 1".
pub fn cursor_up<W: Write>(out: &mut W, n: usize) {
    debug_assert!(n >= 1);
    let _ = write!(out, "\x1b[{n}A");
}

/// Move the cursor down `n` rows (clamped at the bottom margin;
/// never scrolls).  `n` must be non-zero — CSI 0 B means "down 1".
pub fn cursor_down<W: Write>(out: &mut W, n: usize) {
    debug_assert!(n >= 1);
    let _ = write!(out, "\x1b[{n}B");
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



// --- line shaping (pure) -------------------------------------------------------

/// Display width of a char: the status symbols (and other emoji) are
/// 2 columns, the emoji variation selector is 0, East-Asian ambiguous
/// characters (`—`, `§`) are assumed 2 (some terminals render them
/// wide; assuming wide only ever over-truncates, never wraps),
/// everything else 1.
pub fn char_width(c: char) -> usize {
    match c {
        '\u{FE0F}' => 0,
        '\u{231B}' | '\u{23F3}' | '\u{26A0}' | '\u{2705}' | '\u{274C}'
        | '\u{2014}' | '\u{00A7}' => 2,
        _ => 1,
    }
}

/// Total display width of a line.
pub fn display_width(line: &str) -> usize {
    line.chars().map(char_width).sum()
}

/// Hard-truncate a line so it occupies at most `cols` columns — a
/// wrapped line would scroll the screen, which must never happen.
/// `None` means "width unknown": the line passes through untouched.
pub fn truncate_to(line: &str, cols: Option<usize>) -> String {
    let Some(cols) = cols else {
        return line.to_string();
    };
    let mut width = 0usize;
    let mut out = String::new();
    for c in line.chars() {
        let w = char_width(c);
        if width + w > cols {
            break;
        }
        width += w;
        out.push(c);
    }
    out
}

/// Truncate `line` to `cols` display columns, then pad it with spaces
/// to exactly `cols`, so an in-place overwrite covers the row's entire
/// previous content — no line-clear needed, hence no blank frame and
/// no flicker.  `None` passes the line through untouched.
pub fn pad_to(cols: Option<usize>, line: &str) -> String {
    let Some(cols) = cols else {
        return line.to_string();
    };
    let mut out = truncate_to(line, Some(cols));
    for _ in display_width(&out)..cols {
        out.push(' ');
    }
    out
}

/// Cut a description so the whole row fits: when it would run past
/// the terminal's last column, it is truncated so that the last three
/// columns of the line hold `...`.
pub fn fit_description(prefix_cols: usize, desc: &str, cols: Option<usize>) -> String {
    let Some(cols) = cols else {
        return desc.to_string();
    };
    let available = cols.saturating_sub(prefix_cols);
    if display_width(desc) <= available {
        return desc.to_string();
    }
    // Truncate to leave room for the `...` in the last three columns.
    match available.checked_sub(3) {
        Some(kept) if kept > 0 => {
            let mut width = 0usize;
            let mut kept_text = String::new();
            for c in desc.chars() {
                let w = char_width(c);
                if width + w > kept {
                    break;
                }
                width += w;
                kept_text.push(c);
            }
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
pub fn description_prefix(name_width: usize) -> usize {
    2 + 1 + name_width + 2
}

/// The full one-line rendering of a test in a given status.
/// `spinner` only matters for in-flight rows (`status == None`).
pub fn format_row(
    test: &Test,
    status: Option<Status>,
    spinner: usize,
    cols: Option<usize>,
    name_width: usize,
) -> String {
    let symbol = match status {
        Some(status) => status.symbol(),
        None => spinner_char(spinner),
    };
    let mut line = format!("{} {:<width$}", symbol, test.name, width = name_width);
    let mut desc = test.description.clone();
    if let Some(Status::Skip) = status {
        if let Some(reason) = test.skip.as_deref().filter(|r| !r.is_empty()) {
            if !desc.is_empty() {
                desc.push(' ');
            }
            desc.push_str(&format!("({reason})"));
        }
    }
    if !desc.is_empty() {
        let fitted = fit_description(description_prefix(name_width), &desc, cols);
        if !fitted.is_empty() {
            line.push_str("  ");
            line.push_str(&fitted);
        }
    }
    truncate_to(&line, cols)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discovery::test_stub;

    #[test]
    fn char_widths() {
        assert_eq!(char_width('a'), 1);
        assert_eq!(char_width('\u{2705}'), 2); // ✅
        assert_eq!(char_width('\u{FE0F}'), 0); // variation selector
        assert_eq!(char_width('\u{2014}'), 2); // —
    }

    #[test]
    fn truncate_respects_width_and_none() {
        assert_eq!(truncate_to("abcdefg", Some(5)), "abcde");
        assert_eq!(truncate_to("abcdefg", None), "abcdefg");
        // A 2-column emoji counts as 2 columns.
        assert_eq!(truncate_to("✅ab", Some(4)), "✅ab");
        assert_eq!(truncate_to("✅ab", Some(3)), "✅a");
    }

    #[test]
    fn fit_description_fits_and_truncates() {
        // Fits: untouched.
        assert_eq!(fit_description(10, "hello", Some(20)), "hello");
        // Truncates, leaving room for "...".
        let fitted = fit_description(10, "hello world", Some(20));
        assert!(fitted.ends_with("..."));
        // Nothing fits: empty.
        assert_eq!(fit_description(10, "hello world", Some(11)), "");
        // Unknown width: untouched.
        assert_eq!(fit_description(10, "hello world", None), "hello world");
    }

    #[test]
    fn format_row_pads_and_marks_skips() {
        let test = test_stub("tests/short", "a description");
        let row = format_row(&test, Some(Status::Pass), 0, None, 12);
        assert_eq!(
            row,
            format!("{} {:<12}  a description", Status::Pass.symbol(), "tests/short")
        );

        let mut skipping = test_stub("tests/short", "a description");
        skipping.skip = Some("why".to_string());
        let row = format_row(&skipping, Some(Status::Skip), 0, None, 12);
        assert_eq!(
            row,
            format!("{} {:<12}  a description (why)", Status::Skip.symbol(), "tests/short")
        );

        let mut with_reason = test_stub("tests/short", "");
        with_reason.skip = Some("why".to_string());
        let row = format_row(&with_reason, Some(Status::Skip), 0, None, 12);
        assert_eq!(
            row,
            format!("{} {:<12}  (why)", Status::Skip.symbol(), "tests/short")
        );
    }

    #[test]
    fn format_row_truncates_to_cols() {
        let test = test_stub("tests/x", "a very long description indeed");
        // The description is cut to leave room for the `...` at the
        // edge, and the whole row fits inside `cols`.
        let row = format_row(&test, Some(Status::Pass), 0, Some(30), 7);
        assert!(row.ends_with("..."), "{row}");
        assert!(display_width(&row) <= 30, "{row}");
    }

    #[test]
    fn in_flight_rows_show_the_spinner() {
        let test = test_stub("tests/x", "");
        assert_eq!(format_row(&test, None, 0, None, 7), format!("{} tests/x", spinner_char(0)));
        assert_eq!(format_row(&test, None, 1, None, 7), format!("{} tests/x", spinner_char(1)));
    }
}
