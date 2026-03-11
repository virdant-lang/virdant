use bstr::BStr;
use logos::Logos;
use logos::SpannedIter;

use crate::source::SourceOffset;

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct TokenError;

#[rustfmt::skip]
#[derive(Logos, Copy, Clone, Debug, PartialEq)]
#[logos(skip br"[ \t\n\f]+", skip br"//.*\n?", error = TokenError)]
#[repr(u16)]
pub enum Token {
    // Literals
    #[regex(br"[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident = 1,

    #[regex(br"[0-9]+")]
    #[regex(br"0b[0-1]+")]
    #[regex(br"0x[0-1a-fA-F]+")]
    #[regex(br"[0-9][_0-9]*[0-9]")]
    #[regex(br"0b[0-1][_0-1]*[0-1]")]
    #[regex(br"0x[0-1a-fA-F][_0-1a-fA-F]*[0-1a-fA-F]")]
    Nat,

    #[regex(br"[0-9]+w[0-9]+")]
    #[regex(br"0b[0-1]+w[0-9]+")]
    #[regex(br"0x[0-9a-fA-Z]+w[0-9]+")]

    #[regex(br"[0-9][_0-9]*[0-9]w[0-9]+")]
    #[regex(br"0b[0-1][_0-1]*[0-1]w[0-9]+")]
    #[regex(br"0x[0-9a-fA-Z][_0-9a-fA-Z]*[0-9a-fA-Z]w[0-9]+")]
    Word,

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

    // Operators
    #[token(b"+")]   Add,
    #[token(b"-")]   Sub,
    #[token(b"<")]   Lt,
    #[token(b">")]   Gt,
    #[token(b">=")]  GtEq,
    #[token(b"&&")]  AndAnd,
    #[token(b"||")]  PipePipe,
    #[token(b"!")]   Bang,
    #[token(b"~")]   Tilde,

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
    #[token(b"master")]    KwMaster,
    #[token(b"slave")]     KwSlave,
    #[token(b"mosi")]      KwMosi,
    #[token(b"miso")]      KwMiso,
    #[token(b"word")]      KwWord,
    #[token(b"sext")]      KwSext,
    #[token(b"zext")]      KwZext,
    #[token(b"if")]        KwIf,
    #[token(b"else")]      KwElse,
    #[token(b"match")]     KwMatch,
    #[token(b"true")]      KwTrue,
    #[token(b"false")]     KwFalse,

    Error,
}

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
