/// Virdant source code formatter (`vir format`).
///
/// Formats `.vir` files with 4-space indentation, preserving vertical
/// alignment of consecutive driver operators (`:=`, `<=`, `:=:`) and
/// type-annotation colons (`:`) within the same block.

use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Line classification
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum LineKind {
    /// Empty or whitespace-only line
    Blank,
    /// A line that is wholly or partly a single-line `//` comment
    Comment,
    /// A standalone open brace (`{` or `{` with trailing comment)
    OpenBrace,
    /// A standalone close brace (`}` or `}` with trailing comment)
    CloseBrace,
    /// A line containing a driver operator (`:=`, `<=`, `:=:`)
    Driver { before: String, op: String, after: String },
    /// A line containing a type-annotation colon (` : Type`)
    TypeAnnot { before: String, after: String },
    /// Everything else (if/match/import headers, etc.)
    Other(String),
}

// ---------------------------------------------------------------------------
// Per-line analysis
// ---------------------------------------------------------------------------

fn classify_line(raw: &str) -> LineKind {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return LineKind::Blank;
    }
    if trimmed.starts_with("//") || trimmed == "/*" || trimmed == "*/" {
        return LineKind::Comment;
    }

    // Pure brace lines
    if trimmed == "{" {
        return LineKind::OpenBrace;
    }
    if trimmed == "}" || trimmed.starts_with("}") {
        return LineKind::CloseBrace;
    }

    // Check for driver operators first (they may also contain colons)
    // We look for := , <= , :=: -- longest first to avoid := matching :=:
    if let Some(pos) = find_first_not_in_str(&trimmed, ":=:") {
        let before = trimmed[..pos].trim_end().to_string();
        let after = trimmed[pos + 3..].trim().to_string();
        return LineKind::Driver { before, op: ":=:".into(), after };
    }
    if let Some(pos) = find_first_not_in_str(&trimmed, ":=") {
        let before = trimmed[..pos].trim_end().to_string();
        let after = trimmed[pos + 2..].trim().to_string();
        return LineKind::Driver { before, op: ":=".into(), after };
    }
    if let Some(pos) = find_first_not_in_str(&trimmed, "<=") {
        let before = trimmed[..pos].trim_end().to_string();
        let after = trimmed[pos + 2..].trim().to_string();
        return LineKind::Driver { before, op: "<=".into(), after };
    }

    // Check for type-annotation colon (` : `)
    // We look for a colon that is NOT part of :: and NOT inside a string/comment
    // Simple heuristic: a colon preceded by non-whitespace, followed by whitespace
    if let Some(pos) = find_type_colon(&trimmed) {
        let before = trimmed[..pos].trim_end().to_string();
        let after = trimmed[pos + 1..].trim_start().to_string();
        return LineKind::TypeAnnot { before, after };
    }

    LineKind::Other(trimmed.to_string())
}

/// Find the first occurrence of `pattern` that is NOT inside a string literal.
/// Returns `Some(position)` or `None`.
fn find_first_not_in_str(text: &str, pattern: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    let m = pattern.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'"' {
            // skip past the string
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            if i < bytes.len() {
                i += 1;
            }
            continue;
        }
        if bytes[i..].starts_with(m) {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Find a colon that looks like a type annotation (not `::`, not `:=`, etc.).
fn find_type_colon(text: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'"' {
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            if i < bytes.len() {
                i += 1;
            }
            continue;
        }
        if bytes[i] == b':' {
            // Not :: and not := or :=: or <= or =>
            if i + 1 < bytes.len() && bytes[i + 1] == b':' {
                i += 2;
                continue;
            }
            if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                i += 2;
                continue;
            }
            // Also reject if preceded by whitespace and followed by = or >
            if i > 0 && (bytes[i - 1] == b'=' || bytes[i - 1] == b'>') {
                i += 1;
                continue;
            }
            return Some(i);
        }
        i += 1;
    }
    None
}

// ---------------------------------------------------------------------------
// Indentation tracking
// ---------------------------------------------------------------------------

/// A small stack-based tracker that updates indentation when entering/exiting
/// brace blocks.  It handles `{`, `}`, and combined `} else {` by counting
/// in the raw text (not tokenized).  It also tracks a pending indent for
/// continuation lines (e.g. `enum_match :=` on one line with the RHS on
/// the next).
/// Tracks indentation using brace depth.  Continuation lines (e.g.
/// `a :=` without RHS on the same line, or `case @X =>` with the
/// body on the next line) use a one-shot extra indent that applies
/// only to the immediately following non-blank line.
struct IndentTracker {
    depth: usize,
    /// One-shot extra indent for the next line only
    continuation: usize,
}

impl IndentTracker {
    fn new() -> Self {
        Self { depth: 0, continuation: 0 }
    }

    /// Returns the indent for the next non-blank line, consuming
    /// any one-shot continuation.
    fn current(&mut self) -> usize {
        let v = self.depth + self.continuation;
        self.continuation = 0;
        v
    }

    /// Returns the indent for a close-brace line (one level up from
    /// current depth, no continuation).
    fn close_indent(&self) -> usize {
        self.depth.saturating_sub(1)
    }

    /// Grant one-shot extra indent for the next line.
    fn push_continuation(&mut self) {
        self.continuation += 1;
    }

    /// Count opening and closing braces and update depth.
    fn process_line(&mut self, raw: &str) {
        for ch in raw.chars() {
            match ch {
                '{' => self.depth = self.depth.saturating_add(1),
                '}' => self.depth = self.depth.saturating_sub(1),
                _ => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Block-level grouping and alignment
// ---------------------------------------------------------------------------

/// A block is a sequence of consecutive non-blank lines at the same (or
/// similar) indentation level, separated by blank lines or structural
/// boundaries (open/close braces).
#[derive(Debug, Clone)]
struct BlockLine {
    indent: usize,
    kind: LineKind,
    raw: String,
}

/// Format a block of lines, applying alignment.
fn format_block<'a>(block: &'a [&'a BlockLine]) -> Vec<String> {
    if block.is_empty() {
        return vec![];
    }

    // Separate the block into sub-blocks based on comment boundaries.
    // Comments break alignment groups but remain in the output.
    let sub_blocks = split_block(block);

    let mut out: Vec<String> = Vec::new();
    for sb in sub_blocks {
        out.extend(format_sub_block(&sb));
    }
    out
}

fn split_block<'a>(block: &'a [&'a BlockLine]) -> Vec<Vec<&'a BlockLine>> {
    let mut groups: Vec<Vec<&BlockLine>> = vec![vec![]];
    for line in block {
        if matches!(line.kind, LineKind::Comment) {
            if !groups.last().unwrap().is_empty() {
                groups.push(vec![line]);
                groups.push(vec![]);
            } else {
                groups.last_mut().unwrap().push(line);
            }
        } else {
            groups.last_mut().unwrap().push(line);
        }
    }
    groups.retain(|g| !g.is_empty());
    groups
}

fn format_sub_block<'a>(lines: &'a [&'a BlockLine]) -> Vec<String> {
    if lines.is_empty() {
        return vec![];
    }

    // If all lines are comments, emit them as-is.
    if lines.iter().all(|l| matches!(l.kind, LineKind::Comment)) {
        return lines.iter().map(|l| format_line(l, &None, &None)).collect();
    }

    // Detect alignment groups within this sub-block.
    // We scan for consecutive runs of Driver-lines and TypeAnnot-lines.
    // NOTE: we only align within same-operator groups (:= together, <=
    // together, :=: together).
    let mut aligned_driver: Option<(usize, String)> = None; // (max_before_len, operator)
    let mut aligned_type: Option<usize> = None; // max_before_len

    // First pass: compute desired alignment columns.
    for line in lines {
        match &line.kind {
            LineKind::Driver { before, op, .. } => {
                let before_len = before.len();
                match &aligned_driver {
                    Some((cur, existing_op)) if *existing_op == *op => {
                        if before_len > *cur {
                            aligned_driver = Some((before_len, op.clone()));
                        }
                    }
                    None => {
                        aligned_driver = Some((before_len, op.clone()));
                    }
                    _ => {} // different operator => don't align across operators
                }
            }
            LineKind::TypeAnnot { before, .. } => {
                let before_len = before.len();
                match &aligned_type {
                    Some(cur) if before_len > *cur => {
                        aligned_type = Some(before_len);
                    }
                    None => {
                        aligned_type = Some(before_len);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    // Only apply alignment if there are at least 2 lines in this group
    // that share the alignment type (driver or type).
    let apply_driver_align = {
        let count = lines
            .iter()
            .filter(|l| match &l.kind {
                LineKind::Driver { op, .. } => aligned_driver
                    .as_ref()
                    .map(|(_, o)| *o == *op)
                    .unwrap_or(false),
                _ => false,
            })
            .count();
        count >= 2
    };

    let apply_type_align = {
        let count = lines
            .iter()
            .filter(|l| matches!(l.kind, LineKind::TypeAnnot { .. }))
            .count();
        count >= 2
    };

    let driver_align_pos = if apply_driver_align {
        aligned_driver.map(|(len, _)| len)
    } else {
        None
    };

    let type_align_pos = if apply_type_align {
        aligned_type
    } else {
        None
    };

    // Second pass: emit lines.
    let mut out: Vec<String> = Vec::new();
    for line in lines {
        out.push(format_line(line, &driver_align_pos, &type_align_pos));
    }
    out
}

fn format_line(
    line: &BlockLine,
    driver_align: &Option<usize>,
    type_align: &Option<usize>,
) -> String {
    let indent_str = "    ".repeat(line.indent);

    let formatted = match &line.kind {
        LineKind::Blank => String::new(),
        LineKind::Comment => {
            // Preserve the comment as-is but with proper indentation.
            // However, if the raw line already has some indentation from the
            // original, we want to replace it with our computed indent.
            let trimmed = line.raw.trim();
            format!("{}{}", indent_str, trimmed)
        }
        LineKind::OpenBrace => {
            format!("{}{{", indent_str)
        }
        LineKind::CloseBrace => {
            // Emit the full trimmed content at the proper indent.
            // Handles `}`, `} else {`, `} // comment`, etc.
            let trimmed = line.raw.trim();
            format!("{}{}", indent_str, trimmed)
        }
        LineKind::Driver { before, op, after } => {
            let before = before.trim().to_string();
            match driver_align {
                Some(max_len) if *max_len > before.len() => {
                    let padding = " ".repeat(max_len - before.len());
                    format!("{}{}{} {} {}", indent_str, before, padding, op, after)
                }
                _ => {
                    format!("{}{} {} {}", indent_str, before, op, after)
                }
            }
        }
        LineKind::TypeAnnot { before, after } => {
            let before = before.trim().to_string();
            match type_align {
                Some(max_len) if *max_len > before.len() => {
                    let padding = " ".repeat(max_len - before.len());
                    format!("{}{}{} : {}", indent_str, before, padding, after)
                }
                _ => {
                    format!("{}{} : {}", indent_str, before, after)
                }
            }
        }
        LineKind::Other(s) => {
            format!("{}{}", indent_str, s)
        }
    };

    // Trim trailing whitespace so that no line ends with spaces.
    // This is especially important when `after` is empty for Driver or
    // TypeAnnot lines (e.g. `a :=` with RHS on next line), where the
    // format string would otherwise leave a trailing space after the
    // operator.
    formatted.trim_end().to_string()
}

// ---------------------------------------------------------------------------
// Full formatter
// ---------------------------------------------------------------------------

fn format_vir_text(input: &str) -> String {
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return String::new();
    }

    let mut tracker = IndentTracker::new();
    let mut blocks: Vec<Vec<BlockLine>> = Vec::new();
    let mut current_block: Vec<BlockLine> = Vec::new();

    // Helper to flush the current block.
    let mut flush_block = |block: &mut Vec<BlockLine>| {
        if !block.is_empty() {
            blocks.push(block.drain(..).collect());
        }
    };

    for raw_line in lines {
        let kind = classify_line(raw_line);

        let is_empty = matches!(kind, LineKind::Blank);

        // Blank lines reset any continuation.
        if is_empty {
            tracker.continuation = 0;
            flush_block(&mut current_block);
            continue;
        }

        // For lines that start with a close brace, use the close-brace indent
        // (one level up from current depth, no continuation).
        let indent = if matches!(kind, LineKind::CloseBrace) {
            tracker.close_indent()
        } else {
            tracker.current() // consumes any one-shot continuation
        };

        // Detect multi-line continuations:
        // Driver operator with no RHS on this line (e.g. `a :=` followed
        // by RHS on next line).
        if let LineKind::Driver { after, .. } = &kind {
            if after.trim().is_empty() {
                tracker.push_continuation();
            }
        }
        // Match-arm arrow with no brace on this line (e.g. `case @X =>`
        // followed by expression on next line).
        if let LineKind::Other(s) = &kind {
            if s.ends_with("=>") && !s.contains('{') {
                tracker.push_continuation();
            }
        }

        // Process braces for the NEXT iteration's indent.
        tracker.process_line(raw_line);

        current_block.push(BlockLine {
            indent,
            kind,
            raw: raw_line.to_string(),
        });
    }

    flush_block(&mut current_block);

    // Format each block.
    let mut out_lines: Vec<String> = Vec::new();
    for (idx, block) in blocks.iter().enumerate() {
        if idx > 0 {
            out_lines.push(String::new());
        }
        let block_refs: Vec<&BlockLine> = block.iter().collect();
        for line in format_block(&block_refs) {
            out_lines.push(line);
        }
    }

    // Ensure exactly one trailing newline (last byte must be \n,
    // and there must not be \n\n at the end).
    let mut result = out_lines.join("\n");
    result.push('\n');
    result
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

fn format_file(path: &Path, in_place: bool) -> bool {
    let input = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", path.display(), e);
            return false;
        }
    };

    let formatted = format_vir_text(&input);

    if formatted == input {
        return false; // no change
    }

    if in_place {
        match fs::write(path, &formatted) {
            Ok(()) => {
                println!("Formatted {}", path.display());
                true
            }
            Err(e) => {
                eprintln!("Error writing {}: {}", path.display(), e);
                false
            }
        }
    } else {
        // Print to stdout
        print!("{formatted}");
        true
    }
}

/// Walk up from `start` looking for a directory containing
/// `Virdant.toml`.  Returns the path to that directory, or `None`.
fn find_project_root(start: &Path) -> Option<PathBuf> {
    let mut dir = Some(std::fs::canonicalize(start).unwrap_or_else(|_| start.to_path_buf()));
    while let Some(ref d) = dir {
        if d.join("Virdant.toml").exists() {
            return Some(d.clone());
        }
        dir = d.parent().map(|p| p.to_path_buf());
    }
    None
}

/// Recursively collect all `.vir` files under `dir`.
fn collect_vir_files(dir: &Path) -> Vec<PathBuf> {
    let mut result = Vec::new();
    let Ok(entries) = fs::read_dir(dir) else {
        return result;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            result.extend(collect_vir_files(&path));
        } else if path.extension().map_or(false, |ext| ext == "vir") {
            result.push(path);
        }
    }
    result
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Determine files to format.
    // args[0] is the program name or "format" subcommand (via exec_external).
    // We always skip the first element.
    let file_args: Vec<PathBuf> = args
        .iter()
        .skip(1)
        .filter(|s| !s.starts_with('-'))
        .map(PathBuf::from)
        .collect();

    let files: Vec<PathBuf> = if file_args.is_empty() {
        // Walk up to find the project root, then format src/*.vir.
        let cwd = std::env::current_dir().unwrap_or_else(|e| {
            eprintln!("Error getting current directory: {e}");
            std::process::exit(1);
        });
        let project_root = match find_project_root(&cwd) {
            Some(root) => root,
            None => {
                eprintln!("No Virdant.toml found in any parent directory.");
                std::process::exit(1);
            }
        };
        let src_dir = project_root.join("src");
        if !src_dir.is_dir() {
            eprintln!("ERROR: source directory not found: {}", src_dir.display());
            std::process::exit(1);
        }
        collect_vir_files(&src_dir)
    } else {
        file_args
    };

    if files.is_empty() {
        eprintln!("No .vir files found.");
        std::process::exit(0);
    }

    for path in &files {
        format_file(path, true);
    }
}