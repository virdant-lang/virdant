//! Standalone utility functions: the package-name rule shared by the
//! compiler and the tools, word/nat literal parsing, bit-width helpers
//! (`min_word_width`, `log2`), and `check_db` to run all checks and
//! partition diagnostics by severity.
//!
//! Filesystem loading of `.vir` files (directory scans, `Db`
//! construction from files) lives in the tools' crate
//! (`vircommon::load`), since the core is also built for wasm.

use std::sync::Arc;

use bstr::{BStr, BString};
use bstr::ByteSlice as _;

use crate::common::{Width, WordValue};
use crate::db::Db;
use crate::diagnostics::{Diagnostic, DiagnosticLevel};

/// Whether `name` is a valid package name: it must match the lexer's
/// identifier rule `[_a-zA-Z][_a-zA-Z0-9]*` (see `syntax/token.rs`),
/// because `import` accepts only an `Ident`.  Package names come from
/// `.vir` file stems, so a file whose stem is not a valid identifier
/// yields a package that no other file can ever import.
pub fn is_valid_package_name(name: &BStr) -> bool {
    let bytes = name.as_bytes();
    match bytes.split_first() {
        None => false,
        Some((first, rest)) => {
            let first_ok = first.is_ascii_alphabetic() || *first == b'_';
            let rest_ok = rest
                .iter()
                .all(|b| b.is_ascii_alphanumeric() || *b == b'_');
            first_ok && rest_ok
        }
    }
}

pub fn min_word_width(value: WordValue) -> Width {
    if value == 0 {
        0
    } else {
        u64::BITS as Width - u64::leading_zeros(value) as Width
    }
}

/// Returns `Some(k)` if `n` is a power of two (`n = 2^k`), else `None`.
pub fn log2(n: Width) -> Option<Width> {
    if n == 0 || (n & (n - 1)) != 0 {
        None
    } else {
        Some(n.trailing_zeros() as Width)
    }
}

/// Decodes the C-style escapes of a string literal's raw text (the
/// text between the quotes, as written in the source): the
/// single-character escapes `\a \b \e \f \n \r \t \v` and
/// `\\ \' \" \?`, octal escapes `\ooo` of one to three octal digits
/// (so `\0` is the NUL byte), and hex escapes `\xNN` of exactly two
/// hex digits.  An octal escape above `\377` wraps modulo 256, as in
/// C.  The lexer's `Str` rule (syntax/token.rs) accepts exactly these
/// escape forms; anything else reaching this function is copied
/// through verbatim.
pub fn decode_string_escapes(raw: &[u8]) -> BString {
    let mut decoded = BString::new(vec![]);
    let mut index = 0;
    while index < raw.len() {
        if raw[index] != b'\\' || index + 1 >= raw.len() {
            decoded.push(raw[index]);
            index += 1;
            continue;
        }
        let escape = raw[index + 1];
        match escape {
            b'a' => { decoded.push(0x07); index += 2; }
            b'b' => { decoded.push(0x08); index += 2; }
            b'e' => { decoded.push(0x1b); index += 2; }
            b'f' => { decoded.push(0x0c); index += 2; }
            b'n' => { decoded.push(b'\n'); index += 2; }
            b'r' => { decoded.push(b'\r'); index += 2; }
            b't' => { decoded.push(b'\t'); index += 2; }
            b'v' => { decoded.push(0x0b); index += 2; }
            b'\\' | b'\'' | b'"' | b'?' => {
                decoded.push(escape);
                index += 2;
            }
            b'x' => {
                // `\xNN`: exactly two hex digits (the lexer's `Str`
                // rule requires them); anything else is copied
                // through verbatim.
                let digits = raw.get(index + 2..index + 4);
                let Some(digits) = digits else {
                    decoded.push(raw[index]);
                    index += 1;
                    continue;
                };
                match std::str::from_utf8(digits).ok().and_then(|digits| u8::from_str_radix(digits, 16).ok()) {
                    Some(value) => {
                        decoded.push(value);
                        index += 4;
                    }
                    None => {
                        decoded.push(raw[index]);
                        index += 1;
                    }
                }
            }
            b'0'..=b'7' => {
                // `\ooo`: one to three octal digits; values above
                // `\377` wrap modulo 256, as in C.
                let mut value: u32 = 0;
                let mut digits = 0;
                while digits < 3
                    && index + 1 + digits < raw.len()
                    && (b'0'..=b'7').contains(&raw[index + 1 + digits])
                {
                    value = value * 8 + (raw[index + 1 + digits] - b'0') as u32;
                    digits += 1;
                }
                decoded.push((value & 0xff) as u8);
                index += 1 + digits;
            }
            _ => {
                // Not an escape form the lexer accepts; copy through.
                decoded.push(raw[index]);
                index += 1;
            }
        }
    }
    decoded
}

pub fn parse_word_literal(literal: &str) -> (WordValue, Option<Width>) {
    if let Some((value, width)) = literal.split_once('w') {
        // TODO: Handle overflow instead of panicking.
        (parse_nat_literal(value), Some(width.parse().unwrap()))
    } else {
        (parse_nat_literal(literal), None)
    }
}

pub fn parse_nat_literal(literal: &str) -> WordValue {
    let literal = literal.replace('_', "");
    // NOTE: The lexer (logos) can emit a bare `0x`/`0b` prefix as a `Nat`
    // token while the text is mid-edit. It has no digits, hence no value,
    // so it is treated as zero rather than panicking.
    if literal == "0x" || literal == "0b" {
        return 0;
    }
    // TODO: Handle overflow (PosOverflow) instead of panicking.
    if let Some(hex) = literal.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap()
    } else if let Some(bin) = literal.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap()
    } else {
        literal.parse().unwrap()
    }
}

pub fn check_db(db: &Db) -> Result<Arc<Vec<Diagnostic>>, Arc<Vec<Diagnostic>>> {
    let diagnostics = db.check();
    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
