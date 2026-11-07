//! `@image`/`@image_hex` memory initialization: annotation parsing,
//! path resolution (relative to the project root), and word packing
//! for the two formats.
//!
//! Validation (existence, length, well-formedness) lives in the check
//! phase (`queries/check.rs`); the simulator and the Verilog backend
//! load images best-effort on top of it.

use std::path::{Path, PathBuf};

use bstr::{BStr, BString};
use bstr::ByteSlice as _;

use crate::common::source::Region;
use crate::common::Width;
use crate::common::WordValue;
use crate::syntax::ast::AstNode;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

/// The image format declared by the annotation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryImageKind {
    /// `@image("path")`: a raw binary file, word-packed in declaration
    /// order.
    Raw,
    /// `@image_hex("path")`: a text hex file, one word per line (à la
    /// `$readmemh`).
    Hex,
}

/// The `@image`/`@image_hex` annotation of a memory declaration: its
/// kind, its path argument, and the argument's region (for
/// diagnostics).  `None` when the declaration has no image annotation
/// (or its argument is malformed — the first annotation wins).
pub fn memory_image_annotation(
    parsing: &Parsing,
    decl: &AstNode<'_>,
) -> Option<(MemoryImageKind, BString, Region)> {
    let children = decl.children();
    let annotations = children.first()?;
    if !matches!(annotations.payload(), AstNodePayload::Annotations(_)) {
        return None;
    }
    for annotation in annotations.children() {
        let AstNodePayload::Annotation(payload) = annotation.payload() else {
            continue;
        };
        let kind = match parsing.string(payload.name).as_bytes() {
            b"image" => MemoryImageKind::Raw,
            b"image_hex" => MemoryImageKind::Hex,
            _ => continue,
        };
        // The path argument: a `Str` literal child.
        let arg = annotation.children().into_iter().next()?;
        let AstNodePayload::ExprStrLit(strlit) = arg.payload() else {
            return None;
        };
        // The string literal's interned text includes its quotes;
        // the path is the text between them.
        let raw = parsing.string(strlit.literal);
        let path: BString = raw
            .as_bytes()
            .strip_prefix(b"\"")
            .and_then(|rest| rest.strip_suffix(b"\""))
            .unwrap_or(raw.as_bytes())
            .to_owned()
            .into();
        return Some((kind, path, arg.region()));
    }
    None
}

/// Resolve an image path against the project root — the directory
/// holding `virdant.toml`.  Falls back to the path as
/// written (relative to the process working directory, like
/// `$readmemh`) when no project root is known.
pub fn resolve_image_path(project_root: Option<&Path>, path: &BStr) -> PathBuf {
    match project_root {
        Some(root) => root.join(Path::new(&path.to_str_lossy().into_owned())),
        None => PathBuf::from(path.to_str_lossy().into_owned()),
    }
}

/// Parse a text hex image: one word per line, hex digits; blank lines
/// and `//` line comments are skipped.  Values are masked to
/// `elem_width` bits (a line whose value exceeds `u64` is an error).
pub fn parse_hex_image(
    text: &str,
    elem_width: Width,
) -> Result<Vec<WordValue>, String> {
    let mask: WordValue = if elem_width >= 64 {
        WordValue::MAX
    } else {
        (1u64 << elem_width) - 1
    };
    let mut words = vec![];
    for (index, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with("//") {
            continue;
        }
        let digits: String = line.chars().filter(|c| *c != '_').collect();
        let value = u64::from_str_radix(&digits, 16)
            .map_err(|_| format!("line {}: not a hex word", index + 1))?;
        words.push(value & mask);
    }
    Ok(words)
}

/// Pack a raw binary image into words of `elem_width` bits, in
/// declaration order, most-significant byte first within each word;
/// the final word is zero-padded.
///
/// Returns `None` when `elem_width` exceeds 64 bits (the check phase
/// reports it).
pub fn parse_raw_image(bytes: &[u8], elem_width: Width) -> Option<Vec<WordValue>> {
    if elem_width == 0 || elem_width > 64 {
        return None;
    }
    let bytes_per_word = (elem_width as usize + 7) / 8;
    let mut words: Vec<WordValue> = Vec::with_capacity(bytes.len() / bytes_per_word + 1);
    for chunk in bytes.chunks(bytes_per_word) {
        let mut word: WordValue = 0;
        for byte in chunk {
            word = (word << 8) | *byte as WordValue;
        }
        words.push(word);
    }
    Some(words)
}
