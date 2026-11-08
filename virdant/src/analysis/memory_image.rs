//! `@image`/`@image_hex`/`@image_inline` memory initialization:
//! annotation parsing, path resolution (relative to the project root),
//! and word packing for the formats.
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
    /// `@image_inline("bytes")`: a string literal whose C-style
    /// escapes are decoded into the image's bytes, packed like a raw
    /// binary image.
    Inline,
}

/// Where an image's bytes come from: a file path (resolved against
/// the project root) or the decoded bytes of an `@image_inline`
/// string literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemoryImageSource {
    File(PathBuf),
    Inline(BString),
}

/// A memory image annotation resolved to the source its bytes come
/// from: the declared format plus a file path or the inline bytes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryImage {
    /// The image format declared by the annotation.
    pub kind: MemoryImageKind,
    /// Where the image's bytes come from.
    pub source: MemoryImageSource,
}

impl MemoryImage {
    /// Resolve an image annotation's argument to the image its bytes
    /// come from: the file-based kinds resolve the path against the
    /// project root; `@image_inline` decodes the string literal's
    /// C-style escapes (see `util::decode_string_escapes`) into the
    /// embedded bytes.
    pub fn resolve(
        project_root: Option<&Path>,
        kind: MemoryImageKind,
        arg: &BStr,
    ) -> MemoryImage {
        let source = match kind {
            MemoryImageKind::Inline => {
                MemoryImageSource::Inline(crate::util::decode_string_escapes(arg))
            }
            MemoryImageKind::Raw | MemoryImageKind::Hex => {
                MemoryImageSource::File(resolve_image_path(project_root, arg))
            }
        };
        MemoryImage { kind, source }
    }

    /// The image's bytes: the decoded bytes of an `@image_inline`
    /// literal, or the contents of the image file read from disk.
    pub fn bytes(&self) -> std::io::Result<BString> {
        match &self.source {
            MemoryImageSource::Inline(bytes) => Ok(bytes.clone()),
            MemoryImageSource::File(path) => std::fs::read(path).map(Into::into),
        }
    }

    /// The resolved image file's path, or `None` for an
    /// `@image_inline` (whose bytes are embedded).
    pub fn file_path(&self) -> Option<&Path> {
        match &self.source {
            MemoryImageSource::File(path) => Some(path),
            MemoryImageSource::Inline(_) => None,
        }
    }
}

/// The `@image`/`@image_hex`/`@image_inline` annotation of a memory
/// declaration: its kind, its raw argument (the string literal's text
/// between the quotes, *not* escape-decoded — call
/// `resolve_image_source` for that), and the argument's region (for
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
            b"image_inline" => MemoryImageKind::Inline,
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
fn resolve_image_path(project_root: Option<&Path>, path: &BStr) -> PathBuf {
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
