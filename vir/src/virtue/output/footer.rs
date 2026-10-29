//! The in-place footer: the only part of the screen that is ever
//! repainted.
//!
//! Completed tests are streamed: each line is printed the moment the
//! test finishes and scrolls into the scrollback like any other
//! output.  The footer is anchored where the shell leaves the cursor
//! — the fresh blank line after Enter, where output would naturally
//! begin: at reserve time the cursor's row is cleared (taking over
//! the line), `height - 1` linefeeds move down through the blank rows
//! below the prompt (or, at the bottom margin, scroll printed lines
//! into the scrollback in order), and the cursor walks back up to the
//! region's top row — where output would naturally begin — which is
//! saved with DECSC (`ESC 7`).  The region spans that row downward:
//! one `⌛` row per in-flight test (never more than `--jobs`, so the
//! region is bounded), with the progress line on the region's bottom
//! row.  Every paint restores the saved cursor (DECRC, `ESC 8`) and
//! walks the region downward with relative moves, clearing and
//! rewriting each row in place, then re-saves the anchor (DECSC)
//! before returning — the footer owns the cursor between operations
//! and always leaves it on the region's top row.  Paints emit no
//! linefeeds, and the terminal's termios is never touched.
//!
//! Every other line on the screen is history: each append is written
//! at the region's top row and pushed into the scrollback by the
//! single linefeed the append emits (see [`Screen::scroll`]).  The
//! rows the region took over were blank screen that was never output,
//! so no phantom blank lines enter the scrollback; pre-run lines and
//! every test line end up in the scrollback, in order.

use std::io::Write;

use super::term::{self, Screen};
use crate::discovery::Test;
use crate::exec::Status;

/// Height of the footer region: one row per possible in-flight test
/// plus the progress row, capped by the terminal height.
fn height_for(rows: usize, jobs: usize) -> usize {
    (jobs + 1).min(rows).max(1)
}

pub struct Footer {
    /// Job bound: at most this many tests run at once.
    jobs: usize,
    /// Region height at the last paint; the DECRC anchor sits on the
    /// region's top row.
    height: usize,
    /// What was last painted, per region row (padded to the full
    /// usable width) — the diff baseline.  `None` until the first
    /// paint.
    last: Option<Vec<String>>,
}

impl Footer {
    pub fn new(jobs: usize) -> Footer {
        Footer { jobs, height: 0, last: None }
    }

    /// Reserve the footer region.  The cursor's row is taken over
    /// (a no-op on the shell's fresh blank line; a partial line is
    /// deliberately abandoned), then `height - 1` linefeeds move down
    /// through the blank rows below the prompt — or, at the bottom
    /// margin, scroll printed lines into the scrollback, in order —
    /// and the cursor walks back up to the region's top row, which is
    /// saved with DECSC.  Every operation restores that anchor and
    /// re-saves it when done, so the region's absolute position never
    /// needs to be known and the save/restore pairing is balanced on
    /// every terminal (single-slot or stack semantics alike).
    pub fn reserve(&mut self, out: &mut impl Write, screen: Screen) {
        let fm = height_for(screen.rows, self.jobs);
        write!(out, "\r\x1b[2K").ok();
        for _ in 0..fm.saturating_sub(1) {
            writeln!(out).ok();
        }
        if fm > 1 {
            term::cursor_up(out, fm - 1); // back to the region's top row
        }
        write!(out, "\x1b7").ok(); // DECSC: the region's top row
        self.height = fm;
    }

    /// Repaint the footer in place: one `⌛` row per in-flight test
    /// (bottom-aligned above the progress line), blanks for unused
    /// slots, then the progress line on the region's bottom row.
    /// Emits no linefeeds.
    #[allow(clippy::too_many_arguments)]
    pub fn repaint(
        &mut self,
        out: &mut impl Write,
        screen: Screen,
        tests: &[Test],
        in_flight: &[usize],
        done: usize,
        spinner: usize,
        name_width: usize,
    ) {
        if self.height == 0 {
            return; // reserve must run first
        }
        let cols = screen.usable_cols();
        let prev_fm = self.height;
        let fm = height_for(screen.rows, self.jobs);

        // Desired content, top-down: one slot row per in-flight test
        // (bottom-aligned above the progress line), then the progress
        // line.  Every row is padded to the full usable width so a
        // changed row can be overwritten in place — no line-clears, no
        // blank frames, no flicker.  On a shrink, the rows the region
        // gives up are blanked the same way, once.
        let cap = fm.saturating_sub(1); // the bottom row is the progress line
        let shown = in_flight.len().min(cap);
        let base = cap - shown; // bottom-align the in-flight rows
        let mut lines: Vec<String> = Vec::with_capacity(fm.max(prev_fm));
        for j in 0..cap {
            let line = if j >= base {
                let idx = in_flight[j - base];
                term::format_row(&tests[idx], None, spinner, cols, name_width)
            } else {
                String::new()
            };
            lines.push(term::pad_to(cols, &line));
        }
        let progress = term::truncate_to(
            &format!(
                "{} {done}/{} done, {} running",
                term::spinner_char(spinner),
                tests.len(),
                in_flight.len()
            ),
            cols,
        );
        lines.push(term::pad_to(cols, &progress));
        for _ in fm..prev_fm {
            lines.push(term::pad_to(cols, "")); // a shrunken region's dropped rows
        }

        // Diff against what was last painted; only changed rows are
        // touched, jumping between them with relative moves.  Rows
        // whose content is unchanged are never written at all.
        let changed: Vec<usize> = (0..lines.len())
            .filter(|&j| self.last.as_ref().and_then(|l| l.get(j)) != lines.get(j))
            .collect();
        if !changed.is_empty() {
            write!(out, "\x1b8").ok(); // DECRC: the region's top row
            let mut offset = 0usize;
            for &j in &changed {
                if j > offset {
                    term::cursor_down(out, j - offset);
                }
                match cols {
                    // Overwrite in place: the padded line covers the
                    // row's full usable width, so no clear is needed.
                    Some(_) => {
                        let _ = write!(out, "\r{}", lines[j]);
                    }
                    // Width unknown: a clear is the only safe overwrite.
                    None => term::write_at_cursor(out, &lines[j]),
                }
                offset = j;
            }
            if offset > 0 {
                term::cursor_up(out, offset); // back to the region's top row
            }
            write!(out, "\x1b7").ok(); // DECSC: re-save the anchor
        }
        self.height = fm;
        self.last = Some(lines[..fm].to_vec());
    }

    /// Stream a completed test's line: overwrite the region's top row
    /// in place (the footer owns the cursor between operations and
    /// always leaves it there, so no save/restore is needed; the
    /// padded line covers the row's previous content, so no clear) —
    /// then scroll once (pushing the oldest screen line into the
    /// scrollback) and repaint the footer in its place.
    #[allow(clippy::too_many_arguments)]
    pub fn append(
        &mut self,
        out: &mut impl Write,
        screen: Screen,
        tests: &[Test],
        idx: usize,
        status: Status,
        in_flight: &[usize],
        done: usize,
        spinner: usize,
        name_width: usize,
    ) {
        let cols = screen.usable_cols();
        let line = term::format_row(&tests[idx], Some(status), 0, cols, name_width);
        match cols {
            Some(_) => {
                let _ = write!(out, "\r{}", term::pad_to(cols, &line));
            }
            None => term::write_at_cursor(out, &line),
        }
        // The scroll shifts the screen up one row; the painted-content
        // model follows (a blank row enters at the bottom).
        if let Some(l) = &mut self.last {
            l.remove(0);
            l.push(String::new());
        }
        screen.scroll(out);
        self.repaint(out, screen, tests, in_flight, done, spinner, name_width);
    }

    /// Clear the footer region and everything below it, parking the
    /// cursor on the region's top row (the shell prompt lands there).
    /// No linefeed, no scroll.
    pub fn clear(&self, out: &mut impl Write) {
        if self.height == 0 {
            return;
        }
        write!(out, "\x1b8").ok(); // DECRC: the region's bottom row
        if self.height > 1 {
            term::cursor_up(out, self.height - 1); // to the region's top row
        }
        write!(out, "\x1b[0J").ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::discovery::test_stub;

    fn screen(rows: usize, cols: usize) -> Screen {
        Screen { rows, cols: Some(cols) }
    }

    fn tests(n: usize) -> Vec<Test> {
        (0..n).map(|i| test_stub(&format!("tests/t{i:02}"), "")).collect()
    }

    #[test]
    fn repaint_never_emits_a_linefeed() {
        for rows in 1..=8 {
            for jobs in [1, 2, 4] {
                let ts = tests(10);
                let mut footer = Footer::new(jobs);
                let mut sink: Vec<u8> = Vec::new();
                footer.reserve(&mut sink, screen(rows, 40));
                footer.repaint(
                    &mut sink,
                    screen(rows, 40),
                    &ts,
                    &[], // no in-flight tests
                    10,
                    0,
                    9,
                );
                // Subsequent repaints (ticks) must never scroll.
                let after_reserve = sink.len();
                footer.repaint(
                    &mut sink,
                    screen(rows, 40),
                    &ts,
                    &[], // no in-flight tests
                    10,
                    1,
                    9,
                );
                assert!(
                    !sink[after_reserve..].contains(&b'\n'),
                    "rows={rows} jobs={jobs}: repaint emitted a linefeed"
                );
            }
        }
    }

    #[test]
    fn repaint_walks_the_region_with_relative_moves_only() {
        for rows in 1..=8 {
            for jobs in [1, 2, 4] {
                let fm = (jobs + 1).min(rows).max(1);
                let ts = tests(10);
                let mut footer = Footer::new(jobs);
                let mut sink: Vec<u8> = Vec::new();
                footer.reserve(&mut sink, screen(rows, 40));
                let after_reserve = sink.len();
                footer.repaint(&mut sink, screen(rows, 40), &ts, &[], 10, 0, 9);
                let paint = &sink[after_reserve..];
                let text = String::from_utf8_lossy(paint);
                // First paint: restore to the anchor, one padded
                // overwrite per region row walked downward, back up, and
                // a fresh save.  No clears, no linefeeds, no absolute
                // positioning.
                assert_eq!(text.matches("\x1b8").count(), 1, "rows={rows} jobs={jobs}");
                assert_eq!(text.matches('\r').count(), fm, "rows={rows} jobs={jobs}");
                assert_eq!(text.matches("\x1b[1B").count(), fm.saturating_sub(1));
                if fm > 1 {
                    assert!(text.contains(&format!("\x1b[{}A", fm - 1)), "return walk: {text:?}");
                }
                assert_eq!(text.matches("\x1b7").count(), 1);
                assert!(!text.contains('\n'), "rows={rows} jobs={jobs}");
                assert!(!text.contains("\x1b[2K"), "clearing flickers: {text:?}");
                assert!(!text.contains(";1H"), "absolute positioning: {text:?}");

                // A repaint with unchanged content emits nothing at all.
                let before = sink.len();
                footer.repaint(&mut sink, screen(rows, 40), &ts, &[], 10, 0, 9);
                assert_eq!(sink.len(), before, "rows={rows} jobs={jobs}: no-change repaint emitted bytes");
            }
        }
    }

    #[test]
    fn repaint_only_rewrites_changed_rows() {
        let ts = tests(10);
        let mut footer = Footer::new(4);
        let mut sink: Vec<u8> = Vec::new();
        footer.reserve(&mut sink, screen(10, 40));
        footer.repaint(&mut sink, screen(10, 40), &ts, &[], 10, 0, 9);
        // A tick with a new spinner: only the progress row changes.
        let before = sink.len();
        footer.repaint(&mut sink, screen(10, 40), &ts, &[], 10, 1, 9);
        let text = String::from_utf8_lossy(&sink[before..]);
        assert_eq!(text.matches('\r').count(), 1, "one changed row: {text:?}");
        assert!(text.contains("\x1b[4A"), "jump down to the progress row: {text:?}");
        assert!(text.contains("\x1b[4A"), "and back up");
        assert!(text.contains("1/10 done") || text.contains("10/10 done"), "{text:?}");
    }

    #[test]
    fn reserve_moves_down_and_saves_the_cursor() {
        // rows=6, jobs=2 → fm=3: two linefeeds (through the blank rows
        // below the prompt, or scrolling at the bottom margin), back up
        // to the region's top row, then DECSC anchors it.
        let mut footer = Footer::new(2);
        let mut sink: Vec<u8> = Vec::new();
        footer.reserve(&mut sink, screen(6, 40));
        let text = String::from_utf8_lossy(&sink);
        assert_eq!(sink.iter().filter(|&&b| b == b'\n').count(), 2);
        assert!(text.starts_with("\r\x1b[2K"));
        assert!(text.contains("\x1b[2A"), "walk back up: {text:?}");
        assert!(text.ends_with("\x1b7"));
        assert_eq!(footer.height, 3);
    }

    #[test]
    fn append_streams_one_line_with_one_scroll() {
        let ts = tests(10);
        let mut footer = Footer::new(2);
        let mut sink: Vec<u8> = Vec::new();
        footer.reserve(&mut sink, screen(6, 40));
        footer.repaint(&mut sink, screen(6, 40), &ts, &[0, 1], 8, 0, 9);
        let after_first_paint = sink.len();
        // Test 0 finishes: its line goes at the region's top, one scroll.
        footer.append(&mut sink, screen(6, 40), &ts, 0, Status::Pass, &[1], 9, 0, 9);
        let delta = &sink[after_first_paint..];
        // Exactly one linefeed: the scroll that pushes the appended
        // line's predecessor into the scrollback.
        assert_eq!(delta.iter().filter(|&&b| b == b'\n').count(), 1);
        let text = String::from_utf8_lossy(delta);
        // The scroll targets the bottom margin absolutely; the appended
        // line and the repaint are relative.
        assert!(text.contains("\x1b[6;1H\n"), "scroll not at the bottom margin");
        assert!(text.contains("✅"), "{text}");
    }

    #[test]
    fn footer_height_is_capped_by_jobs_and_rows() {
        assert_eq!(height_for(24, 16), 17);
        assert_eq!(height_for(24, 1), 2);
        assert_eq!(height_for(3, 16), 3); // capped by the terminal
        assert_eq!(height_for(1, 16), 1);
    }
}
