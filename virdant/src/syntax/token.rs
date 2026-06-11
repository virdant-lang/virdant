use bstr::BStr;
use logos::Logos;
use logos::SpannedIter;

use crate::common::source::SourceOffset;

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct TokenError;

#[rustfmt::skip]
#[derive(Logos, Copy, Clone, Debug, PartialEq)]
#[logos(skip br"[ \n]+", skip br"//[^\n]*", error = TokenError)]
#[repr(u16)]
pub enum Token {
    // Literals
    #[regex(br"[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident = 1,

    // Nat: decimal, binary, or hex literals with optional underscores
    // Underscores can separate digits but not appear consecutively
    #[regex(br"[0-9]([_]?[0-9])*")]
    #[regex(br"0b[0-1]([_]?[0-1])*")]
    #[regex(br"0x[0-9a-fA-F]([_]?[0-9a-fA-F])*")]
    Nat,

    // Docstrings
    #[regex(br"//>[^\n]*")]
    DocComment,
    #[regex(br"//![^\n]*")]
    DocBang,

    // Word: Nat with w<width> suffix (width does NOT allow underscores)
    #[regex(br"[0-9]([_]?[0-9])*w[0-9]+")]
    #[regex(br"0b[0-1]([_]?[0-1])*w[0-9]+")]
    #[regex(br"0x[0-9a-fA-F]([_]?[0-9a-fA-F])*w[0-9]+")]
    Word,

    #[regex(br#""([^"\\]|\\t|\\n|\\r|\\0)*""#)]
    Str,

    // Punctuation
    #[token(b";")]   Semi,
    #[token(b".")]   Dot,
    #[token(b"..")]  DotDot,
    #[token(b"::")]  ColonColon,
    #[token(b"=")]   Eq,
    #[token(b"==")]  EqEq,
    #[token(b"!=")]  BangEq,
    #[token(b":=")]  EqColon,
    #[token(b":=:")] EqColoneq,
    #[token(b"<=")]  LtEq,
    #[token(b"=>")]  FatArrow,
    #[token(b"->")]  Arrow,
    #[token(b":")]   Colon,
    #[token(b",")]   Comma,
    #[token(b"$")]   Dollar,
    #[token(b"#")]   Hash,
    #[token(b"@")]   At,
    #[token(b"?")]   Question,

    // Operators
    #[token(b"+")]   Add,
    #[token(b"-")]   Sub,
    #[token(b"<")]   Lt,
    #[token(b">")]   Gt,
    #[token(b">=")]  GtEq,
    #[token(b"&&")]  AndAnd,
    #[token(b"&")]   And,
    #[token(b"||")]  PipePipe,
    #[token(b"|")]   Pipe,
    #[token(b"!")]   Bang,
    #[token(b"~")]   Tilde,
    #[token(b"^")]   Hat,
    #[token(b"^^")]  HatHat,

    // Groupings
    #[token(b"(")]   ParenLeft,
    #[token(b")")]   ParenRight,
    #[token(b"{")]   CurlyLeft,
    #[token(b"}")]   CurlyRight,
    #[token(b"[")]   BracketLeft,
    #[token(b"]")]   BracketRight,

    // Keywords
    #[token(b"pub")]       KwPub,
    #[token(b"export")]    KwExport,
    #[token(b"import")]    KwImport,
    #[token(b"mod")]       KwMod,
    #[token(b"ext")]       KwExt,
    #[token(b"builtin")]   KwBuiltin,
    #[token(b"type")]      KwType,
    #[token(b"union")]     KwUnion,
    #[token(b"struct")]    KwStruct,
    #[token(b"enum")]      KwEnum,
    #[token(b"fn")]        KwFn,
    #[token(b"width")]     KwWidth,
    #[token(b"incoming")]  KwIncoming,
    #[token(b"outgoing")]  KwOutgoing,
    #[token(b"reg")]       KwReg,
    #[token(b"wire")]      KwWire,
    #[token(b"submod")]    KwSubmod,
    #[token(b"on")]        KwOn,
    #[token(b"of")]        KwOf,
    #[token(b"socket")]    KwSocket,
    #[token(b"client")]    KwClient,
    #[token(b"server")]    KwServer,
    #[token(b"cosi")]      KwCosi,
    #[token(b"soci")]      KwSoci,
    #[token(b"if")]        KwIf,
    #[token(b"it")]        KwIt,
    #[token(b"else")]      KwElse,
    #[token(b"when")]      KwWhen,
    #[token(b"match")]     KwMatch,
    #[token(b"case")]      KwCase,
    #[token(b"unused")]    KwUnused,
    #[token(b"true")]      KwTrue,
    #[token(b"false")]     KwFalse,
    #[token(b"dontcare")]  KwDontcare,
    #[token(b"dyn")]       KwDyn,

    // Unused
    #[token(b"/*")]        SlashStar,
    #[token(b"*/")]        StarSlash,

    Error,
}

/// The list of all keyword strings recognized by the lexer.
/// Used by downstream checks (e.g. to reject a package name that
/// collides with a keyword).
pub const KEYWORDS: &[&str] = &[
    "pub", "export", "import", "mod", "ext", "builtin",
    "type", "union", "struct", "enum", "fn",
    "width", "incoming", "outgoing",
    "reg", "wire", "submod", "on", "of",
    "socket", "client", "server", "cosi", "soci",
    "if", "it", "else", "when",
    "match", "case", "unused",
    "true", "false", "dontcare",
];

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, SourceOffset, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| {
                let start = SourceOffset(span.start as u32);
                let end = SourceOffset(span.end as u32);
                let token = token.unwrap_or(Token::Error);
                Ok((start, token, end))
            })
    }
}

pub fn tokenize<'input>(input: &'input BStr) -> Lexer<'input> {
    Lexer {
        token_stream: Token::lexer(input).spanned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_and_collect(input: &str) -> Vec<(Token, &str)> {
        let input = BStr::new(input.as_bytes());
        tokenize(input)
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
            .into_iter()
            .map(|(_, token, end)| {
                let start_offset = 0;
                let end_offset = usize::from(end);
                (token, std::str::from_utf8(&input[start_offset..end_offset]).unwrap())
            })
            .collect()
    }

    fn get_token(input: &str) -> Token {
        let tokens = tokenize_and_collect(input);
        assert!(!tokens.is_empty(), "Expected at least one token");
        tokens[0].0
    }

    // Decimal literal tests with underscores
    #[test]
    fn test_decimal_with_single_underscore() {
        assert_eq!(get_token("1_000"), Token::Nat);
    }

    #[test]
    fn test_decimal_with_multiple_underscores() {
        assert_eq!(get_token("1_000_000"), Token::Nat);
    }

    #[test]
    fn test_decimal_no_underscore() {
        assert_eq!(get_token("123"), Token::Nat);
    }

    #[test]
    fn test_decimal_single_digit() {
        assert_eq!(get_token("5"), Token::Nat);
    }

    // Binary literal tests with underscores
    #[test]
    fn test_binary_with_single_underscore() {
        assert_eq!(get_token("0b1010_0101"), Token::Nat);
    }

    #[test]
    fn test_binary_with_multiple_underscores() {
        assert_eq!(get_token("0b1111_0000_1100"), Token::Nat);
    }

    #[test]
    fn test_binary_no_underscore() {
        assert_eq!(get_token("0b1010"), Token::Nat);
    }

    #[test]
    fn test_binary_single_digit() {
        assert_eq!(get_token("0b1"), Token::Nat);
    }

    // Hex literal tests with underscores
    #[test]
    fn test_hex_with_single_underscore() {
        assert_eq!(get_token("0xDEAD_BEEF"), Token::Nat);
    }

    #[test]
    fn test_hex_with_multiple_underscores() {
        assert_eq!(get_token("0xCAFE_BABE_DEAD"), Token::Nat);
    }

    #[test]
    fn test_hex_no_underscore() {
        assert_eq!(get_token("0xDEADbeef"), Token::Nat);
    }

    #[test]
    fn test_hex_single_digit() {
        assert_eq!(get_token("0xF"), Token::Nat);
    }

    // Word literal tests with underscores
    #[test]
    fn test_word_decimal_with_underscore() {
        assert_eq!(get_token("1_000w32"), Token::Word);
    }

    #[test]
    fn test_word_binary_with_underscore() {
        assert_eq!(get_token("0b1010_0101w8"), Token::Word);
    }

    #[test]
    fn test_word_hex_with_underscore() {
        assert_eq!(get_token("0xDEAD_BEEFw16"), Token::Word);
    }

    #[test]
    fn test_word_no_underscores() {
        assert_eq!(get_token("100w32"), Token::Word);
    }

    // Consecutive underscore tests
    #[test]
    fn test_consecutive_underscores_rejected_decimal() {
        let tokens = tokenize_and_collect("1__000");
        // Should tokenize as "1" (Nat) and "__000" (Ident)
        assert_eq!(tokens[0].0, Token::Nat);
        assert_eq!(tokens[1].0, Token::Ident);
    }

    #[test]
    fn test_consecutive_underscores_rejected_binary() {
        let tokens = tokenize_and_collect("0b1010__0101");
        // Should tokenize as "0b1010_" (Nat) and "__0101" (Ident)
        assert_eq!(tokens[0].0, Token::Nat);
        assert_eq!(tokens[1].0, Token::Ident);
    }

    #[test]
    fn test_consecutive_underscores_rejected_hex() {
        let tokens = tokenize_and_collect("0xDEAD__BEEF");
        // Should tokenize as "0xDEAD_" (Nat) and "__BEEF" (Ident)
        assert_eq!(tokens[0].0, Token::Nat);
        assert_eq!(tokens[1].0, Token::Ident);
    }

    // Width suffix does NOT allow underscores
    #[test]
    fn test_word_width_no_underscores() {
        assert_eq!(get_token("100w32"), Token::Word);
        assert_eq!(get_token("0xABCDw16"), Token::Word);
    }
}
