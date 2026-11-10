//! Hand-written tokenizer for the `vir value` value-expression language.
//!
//! One [`lex`] call tokenizes one command-line argument bundle (the
//! CLI joins its positional arguments with spaces and parses the whole
//! thing at once).  The literal syntax is the same as Virdant source
//! (see `virdant/src/syntax/token.rs`): decimal, `0b...`, and `0x...`
//! literals with `_` separators between digits, plus `w<width>`
//! suffixes (`42w8`, `0b1010w4`) for sized words.

use bstr::ByteSlice;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    /// An identifier (type, constructor, or struct-field name).
    Ident(String),
    /// A `Nat` literal (decimal, `0b...`, or `0x...`; underscores
    /// allowed between digits).
    Nat(u128),
    /// A `Word` literal: a Nat with a `w<width>` suffix (`0b1010w4`).
    Word(u128, u16),
    True,
    False,
    At,
    Hash,
    Dollar,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    ColonColon,
    Eq,
}

/// A lexing error: the byte offset and a message.
#[derive(Clone, Debug, PartialEq)]
pub struct LexError {
    pub offset: usize,
    pub message: String,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Used in parse-error messages (`unexpected Tok::X` style).
        match self {
            Tok::Ident(name) => write!(f, "`{name}`"),
            Tok::Nat(v) => write!(f, "`{v}`"),
            Tok::Word(v, w) => write!(f, "`{v}w{w}`"),
            Tok::True => write!(f, "`true`"),
            Tok::False => write!(f, "`false`"),
            Tok::At => write!(f, "`@`"),
            Tok::Hash => write!(f, "`#`"),
            Tok::Dollar => write!(f, "`$`"),
            Tok::LParen => write!(f, "`(`"),
            Tok::RParen => write!(f, "`)`"),
            Tok::LBracket => write!(f, "`[`"),
            Tok::RBracket => write!(f, "`]`"),
            Tok::LBrace => write!(f, "`{{`"),
            Tok::RBrace => write!(f, "`}}`"),
            Tok::Comma => write!(f, "`,`"),
            Tok::Colon => write!(f, "`:`"),
            Tok::ColonColon => write!(f, "`::`"),
            Tok::Eq => write!(f, "`=`"),
        }
    }
}

/// Tokenize one input string.  Returns the tokens with their byte
/// offsets (`(start, tok, end)` triples), as the lalrpop parser
/// expects.
pub fn lex(input: &str) -> Result<Vec<(usize, Tok, usize)>, LexError> {
    let bytes = input.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        let start = i;
        // Identifiers.
        if ident_start(Some(b)) {
            while i < bytes.len() && ident_continue(bytes[i]) {
                i += 1;
            }
            let word = bytes[start..i].to_str_lossy().into_owned();
            let tok = keyword(&word).unwrap_or(Tok::Ident(word));
            out.push((start, tok, i));
            continue;
        }
        // Numeric literals (Nat/Word; decimal, binary, hex).
        if b.is_ascii_digit() {
            let (tok, next) = lex_number(bytes, start)?;
            i = next;
            out.push((start, tok, i));
            continue;
        }
        // Single-character tokens (`::` first).
        let (tok, len) = if bytes[i..].starts_with(b"::") {
            (Tok::ColonColon, 2)
        } else {
            match b {
                b'@' => (Tok::At, 1),
                b'#' => (Tok::Hash, 1),
                b'$' => (Tok::Dollar, 1),
                b'(' => (Tok::LParen, 1),
                b')' => (Tok::RParen, 1),
                b'[' => (Tok::LBracket, 1),
                b']' => (Tok::RBracket, 1),
                b'{' => (Tok::LBrace, 1),
                b'}' => (Tok::RBrace, 1),
                b',' => (Tok::Comma, 1),
                b':' => (Tok::Colon, 1),
                b'=' => (Tok::Eq, 1),
                _ => {
                    return Err(LexError {
                        offset: start,
                        message: format!("unexpected character '{}'", b as char),
                    })
                }
            }
        };
        i += len;
        out.push((start, tok, i));
    }
    Ok(out)
}

fn ident_start(b: Option<u8>) -> bool {
    matches!(b, Some(c) if c == b'_' || c.is_ascii_alphabetic())
}

fn ident_continue(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphanumeric()
}

fn keyword(word: &str) -> Option<Tok> {
    let tok = match word {
        "true" => Tok::True,
        "false" => Tok::False,
        _ => return None,
    };
    Some(tok)
}

/// Lex a numeric literal starting at `start` (known to be an ASCII
/// digit).  Returns the token and the offset just past it.
fn lex_number(bytes: &[u8], start: usize) -> Result<(Tok, usize), LexError> {
    let mut i = start;
    let radix = if bytes[i..].starts_with(b"0b") && radix_digit(bytes, i + 2, 2) {
        i += 2;
        2
    } else if bytes[i..].starts_with(b"0x") && radix_digit(bytes, i + 2, 16) {
        i += 2;
        16
    } else {
        10
    };
    let digits_start = i;
    let mut digits = String::new();
    while i < bytes.len() {
        let b = bytes[i];
        if b == b'_' {
            // An underscore must separate digits: reject `__` and a
            // trailing underscore (the next iteration re-checks).
            if !digit_allowed(bytes.get(i + 1).copied(), radix) {
                break;
            }
            i += 1;
            continue;
        }
        if digit_allowed(Some(b), radix) {
            digits.push(b as char);
            i += 1;
        } else {
            break;
        }
    }
    if i == digits_start {
        return Err(LexError {
            offset: start,
            message: "malformed numeric literal".to_string(),
        });
    }
    let value = u128::from_str_radix(&digits, radix).map_err(|_| LexError {
        offset: start,
        message: "numeric literal too large (limit: 128 bits)".to_string(),
    })?;
    // Optional `w<width>` suffix turns the Nat into a Word literal.
    if i < bytes.len() && bytes[i] == b'w' {
        let width_start = i + 1;
        let mut j = width_start;
        while j < bytes.len() && bytes[j].is_ascii_digit() {
            j += 1;
        }
        if j == width_start {
            return Err(LexError {
                offset: i,
                message: "malformed word literal: expected a width after 'w'".to_string(),
            });
        }
        let width: u16 = bytes[width_start..j].to_str_lossy().parse().map_err(|_| {
            LexError { offset: width_start, message: "word width too large".to_string() }
        })?;
        return Ok((Tok::Word(value, width), j));
    }
    Ok((Tok::Nat(value), i))
}

fn digit_allowed(b: Option<u8>, radix: u32) -> bool {
    match b {
        Some(b) if radix == 2 => b.is_ascii_digit() && b < b'2',
        Some(b) if radix == 16 => b.is_ascii_hexdigit(),
        Some(b) => b.is_ascii_digit(),
        None => false,
    }
}

/// True when a radix prefix (`0b`/`0x`) is actually followed by a
/// digit in that radix (so `0x` alone is not a prefix, and `0` lexes
/// as decimal zero).
fn radix_digit(bytes: &[u8], offset: usize, radix: u32) -> bool {
    digit_allowed(bytes.get(offset).copied(), radix)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(input: &str) -> Vec<Tok> {
        lex(input).unwrap().into_iter().map(|(_, t, _)| t).collect()
    }

    #[test]
    fn test_idents_and_keywords() {
        assert_eq!(
            toks("Valid Word8_foo"),
            vec![Tok::Ident("Valid".to_string()), Tok::Ident("Word8_foo".to_string())]
        );
        assert_eq!(toks("true"), vec![Tok::True]);
        assert_eq!(toks("false"), vec![Tok::False]);
    }

    #[test]
    fn test_numbers() {
        assert_eq!(toks("42"), vec![Tok::Nat(42)]);
        assert_eq!(toks("0b1010_0101"), vec![Tok::Nat(0b1010_0101)]);
        assert_eq!(toks("0xDEAD_BEEF"), vec![Tok::Nat(0xDEAD_BEEF)]);
        assert_eq!(toks("0b1010w4"), vec![Tok::Word(0b1010, 4)]);
        assert_eq!(toks("42w8"), vec![Tok::Word(42, 8)]);
        assert_eq!(toks("0xFFw16"), vec![Tok::Word(0xFF, 16)]);
        assert_eq!(toks("0"), vec![Tok::Nat(0)]);
    }

    #[test]
    fn test_malformed_numbers() {
        // `0b1__0` splits into a Nat and an identifier (as in the main
        // Virdant lexer); a trailing `_` before a non-digit ends the
        // literal.
        assert_eq!(toks("0b1__0"), vec![Tok::Nat(1), Tok::Ident("__0".to_string())]);
        assert_eq!(toks("1_000_ x"), vec![Tok::Nat(1000), Tok::Ident("_".to_string()), Tok::Ident("x".to_string())]);
        assert!(lex("42w").is_err());
        // `0x` without a hex digit splits into `0` and `x` (as in the
        // main Virdant lexer).
        assert_eq!(toks("0x"), vec![Tok::Nat(0), Tok::Ident("x".to_string())]);
    }

    #[test]
    fn test_punctuation() {
        assert_eq!(
            toks("@Valid(0) : $ { a = 1 } pkg::Foo[8] :: = ,"),
            vec![
                Tok::At,
                Tok::Ident("Valid".to_string()),
                Tok::LParen,
                Tok::Nat(0),
                Tok::RParen,
                Tok::Colon,
                Tok::Dollar,
                Tok::LBrace,
                Tok::Ident("a".to_string()),
                Tok::Eq,
                Tok::Nat(1),
                Tok::RBrace,
                Tok::Ident("pkg".to_string()),
                Tok::ColonColon,
                Tok::Ident("Foo".to_string()),
                Tok::LBracket,
                Tok::Nat(8),
                Tok::RBracket,
                Tok::ColonColon,
                Tok::Eq,
                Tok::Comma,
            ]
        );
    }

    #[test]
    fn test_unexpected_character() {
        let err = lex("0b1010 + 1").unwrap_err();
        assert!(err.message.contains("unexpected character"));
    }
}
