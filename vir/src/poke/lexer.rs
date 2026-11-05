//! Hand-written tokenizer for the `vir poke` scripting language.
//!
//! One [`lex`] call tokenizes one line (a whole `.poke` script is a
//! sequence of lines, so the parser runs per line).  `#` starts a
//! comment *unless* it is immediately followed by an identifier
//! character, in which case it lexes as the enum-constructor prefix
//! (`#Foo`); this keeps `#` doing double duty as comment marker and
//! constructor syntax without ambiguity in practice.

use bstr::ByteSlice;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    /// A path segment / identifier.
    Ident(String),
    /// A `Nat` literal (decimal, `0b...`, or `0x...`; underscores
    /// allowed between digits).
    Nat(u128),
    /// A `Word` literal: a Nat with a `w<width>` suffix (`0b1010w4`).
    Word(u128, u16),
    /// A quoted string literal (`"trace.vcd"`).  Used for file names;
    /// no escape sequences.
    Str(String),
    /// `/` — only legal inside a bare `vcd` file name; the expression
    /// grammar never produces it.
    Slash,
    // Keywords
    Cd,
    Peek,
    Poke,
    Watch,
    Break,
    Tick,
    Assert,
    Print,
    Restart,
    Exit,
    Ls,
    Show,
    Autoshow,
    Vcd,
    On,
    Off,
    True,
    False,
    Top,
    It,
    // Punctuation & operators
    Hash,
    Dot,
    DotDot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Plus,
    Minus,
    Amp,
    Pipe,
    Caret,
    Tilde,
    Bang,
    EqEq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    AmpAmp,
    PipePipe,
    CaretCaret,
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
            Tok::Str(s) => write!(f, "`\"{s}\"`"),
            Tok::Slash => write!(f, "`/`"),
            Tok::Cd => write!(f, "`cd`"),
            Tok::Peek => write!(f, "`peek`"),
            Tok::Poke => write!(f, "`poke`"),
            Tok::Watch => write!(f, "`watch`"),
            Tok::Break => write!(f, "`break`"),
            Tok::Tick => write!(f, "`tick`"),
            Tok::Assert => write!(f, "`assert`"),
            Tok::Print => write!(f, "`print`"),
            Tok::Restart => write!(f, "`restart`"),
            Tok::Exit => write!(f, "`exit`"),
            Tok::Ls => write!(f, "`ls`"),
            Tok::Show => write!(f, "`show`"),
            Tok::Autoshow => write!(f, "`autoshow`"),
            Tok::Vcd => write!(f, "`vcd`"),
            Tok::On => write!(f, "`on`"),
            Tok::Off => write!(f, "`off`"),
            Tok::True => write!(f, "`true`"),
            Tok::False => write!(f, "`false`"),
            Tok::Top => write!(f, "`top`"),
            Tok::It => write!(f, "`it`"),
            Tok::Hash => write!(f, "`#`"),
            Tok::Dot => write!(f, "`.`"),
            Tok::DotDot => write!(f, "`..`"),
            Tok::LParen => write!(f, "`(`"),
            Tok::RParen => write!(f, "`)`"),
            Tok::LBracket => write!(f, "`[`"),
            Tok::RBracket => write!(f, "]"),
            Tok::Comma => write!(f, "`,`"),
            Tok::Plus => write!(f, "`+`"),
            Tok::Minus => write!(f, "`-`"),
            Tok::Amp => write!(f, "`&`"),
            Tok::Pipe => write!(f, "`|`"),
            Tok::Caret => write!(f, "`^`"),
            Tok::Tilde => write!(f, "`~`"),
            Tok::Bang => write!(f, "`!`"),
            Tok::EqEq => write!(f, "`==`"),
            Tok::Neq => write!(f, "`!=`"),
            Tok::Lt => write!(f, "`<`"),
            Tok::Le => write!(f, "`<=`"),
            Tok::Gt => write!(f, "`>`"),
            Tok::Ge => write!(f, "`>=`"),
            Tok::AmpAmp => write!(f, "`&&`"),
            Tok::PipePipe => write!(f, "`||`"),
            Tok::CaretCaret => write!(f, "`^^`"),
        }
    }
}

/// Tokenize one line.  Returns the tokens with their byte offsets
/// (`(start, tok, end)` triples), as the lalrpop parser expects.
pub fn lex(line: &str) -> Result<Vec<(usize, Tok, usize)>, LexError> {
    let bytes = line.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        // `#` comment, unless followed by an identifier character
        // (constructor syntax: `#Foo`).
        if b == b'#' && !ident_start(bytes.get(i + 1).copied()) {
            break;
        }
        let start = i;
        // Identifiers and keywords.
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
        // String literals: `"..."`, terminated by the next `"`.  No
        // escape sequences — file names go here, and a backslash is a
        // perfectly legal (if unusual) character in one.
        if b == b'"' {
            let close = bytes[i + 1..]
                .iter()
                .position(|&c| c == b'"' || c == b'\n')
                .map(|pos| i + 1 + pos);
            let close = match close {
                Some(pos) if bytes[pos] == b'"' => pos,
                // A newline (or end of input) came first: unterminated.
                _ => {
                    return Err(LexError {
                        offset: start,
                        message: "unterminated string literal".to_string(),
                    })
                }
            };
            let text = bytes[i + 1..close].to_str_lossy().into_owned();
            i = close + 1;
            out.push((start, Tok::Str(text), i));
            continue;
        }
        // Multi-character operators first, then single characters.
        let (tok, len) = if bytes[i..].starts_with(b"..") {
            (Tok::DotDot, 2)
        } else if bytes[i..].starts_with(b"&&") {
            (Tok::AmpAmp, 2)
        } else if bytes[i..].starts_with(b"||") {
            (Tok::PipePipe, 2)
        } else if bytes[i..].starts_with(b"^^") {
            (Tok::CaretCaret, 2)
        } else if bytes[i..].starts_with(b"==") {
            (Tok::EqEq, 2)
        } else if bytes[i..].starts_with(b"!=") {
            (Tok::Neq, 2)
        } else if bytes[i..].starts_with(b"<=") {
            (Tok::Le, 2)
        } else if bytes[i..].starts_with(b">=") {
            (Tok::Ge, 2)
        } else {
            match b {
                b'.' => (Tok::Dot, 1),
                b'(' => (Tok::LParen, 1),
                b')' => (Tok::RParen, 1),
                b'[' => (Tok::LBracket, 1),
                b']' => (Tok::RBracket, 1),
                b',' => (Tok::Comma, 1),
                b'#' => (Tok::Hash, 1),
                b'+' => (Tok::Plus, 1),
                b'-' => (Tok::Minus, 1),
                b'&' => (Tok::Amp, 1),
                b'|' => (Tok::Pipe, 1),
                b'^' => (Tok::Caret, 1),
                b'~' => (Tok::Tilde, 1),
                b'!' => (Tok::Bang, 1),
                b'/' => (Tok::Slash, 1),
                b'<' => (Tok::Lt, 1),
                b'>' => (Tok::Gt, 1),
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
        "cd" => Tok::Cd,
        "peek" => Tok::Peek,
        "poke" => Tok::Poke,
        "watch" => Tok::Watch,
        "break" => Tok::Break,
        "tick" => Tok::Tick,
        "assert" => Tok::Assert,
        "print" => Tok::Print,
        "restart" => Tok::Restart,
        "exit" => Tok::Exit,
        "ls" => Tok::Ls,
        "show" => Tok::Show,
        "autoshow" => Tok::Autoshow,
        "vcd" => Tok::Vcd,
        "on" => Tok::On,
        "off" => Tok::Off,
        "true" => Tok::True,
        "false" => Tok::False,
        "top" => Tok::Top,
        "it" => Tok::It,
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

    fn toks(line: &str) -> Vec<Tok> {
        lex(line).unwrap().into_iter().map(|(_, t, _)| t).collect()
    }

    #[test]
    fn test_keywords_and_idents() {
        assert_eq!(
            toks("peek top.foo_bar"),
            vec![Tok::Peek, Tok::Top, Tok::Dot, Tok::Ident("foo_bar".to_string())]
        );
        assert_eq!(toks("it"), vec![Tok::It]);
    }

    #[test]
    fn test_numbers() {
        assert_eq!(toks("42"), vec![Tok::Nat(42)]);
        assert_eq!(toks("0b1010_0101"), vec![Tok::Nat(0b1010_0101)]);
        assert_eq!(toks("0xff"), vec![Tok::Nat(255)]);
        assert_eq!(toks("0b1010w4"), vec![Tok::Word(0b1010, 4)]);
        assert_eq!(toks("42w8"), vec![Tok::Word(42, 8)]);
        assert_eq!(toks("0"), vec![Tok::Nat(0)]);
    }

    #[test]
    fn test_comments_vs_ctor() {
        // `#` followed by a space starts a comment.
        assert_eq!(toks("peek x # rest of line"), vec![Tok::Peek, Tok::Ident("x".to_string())]);
        // `#` followed by an identifier char is a constructor.
        assert_eq!(
            toks("print #Valid"),
            vec![Tok::Print, Tok::Hash, Tok::Ident("Valid".to_string())]
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            toks("x[8..4] == y && !z"),
            vec![
                Tok::Ident("x".to_string()),
                Tok::LBracket,
                Tok::Nat(8),
                Tok::DotDot,
                Tok::Nat(4),
                Tok::RBracket,
                Tok::EqEq,
                Tok::Ident("y".to_string()),
                Tok::AmpAmp,
                Tok::Bang,
                Tok::Ident("z".to_string()),
            ]
        );
    }

    #[test]
    fn test_errors() {
        assert!(lex("peek x$").is_err());
        assert!(lex("42w").is_err());
    }
}
