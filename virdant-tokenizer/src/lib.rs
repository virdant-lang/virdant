#[cfg(test)]
mod tests;

use virdant_common::location::{LineCol, Span, Pos};
use virdant_common::text::Text;


pub type TokenLen = u32;

#[derive(PartialEq, Eq, Clone)]
struct Token(TokenKind, Pos, TokenLen);

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.kind(), usize::from(self.pos()))
    }
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        self.0
    }

    pub fn pos(&self) -> Pos {
        self.1
    }

    pub fn len(&self) -> u32 {
        self.2
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Unknown,
    Eof,

    Ident,

    ParenLeft,
    ParenRight,
    CurlyLeft,
    CurlyRight,
    BraceLeft,
    BraceRight,

    Gt,
    Lt,

    Arrow,
    FatArrow,
    RevFatArrow,

    Eq,
    Colon,
    ColonColon,
    ColonEq,
    ColonEqColon,
    Comma,
    DotDot,
    Dot,
    Semicolon,

    Dollar,
    At,
    Hash,

    Str,
    Nat,
    Word,

    KwImport,
    KwMod,
    KwExt,
    KwType,
    KwUnion,
    KwStruct,
    KwEnum,
    KwFn,

    KwWidth,

    KwIncoming,
    KwOutgoing,
    KwReg,
    KwWire,

    KwSubmod,

    KwOn,
    KwOf,

    KwSocket,
    KwMaster,
    KwSlave,
    KwMosi,
    KwMiso,

    KwWord,
    KwSext,
    KwZext,
    KwIf,
    KwElse,
    KwMatch,
    KwTrue,
    KwFalse,
}

pub struct Tokenization {
    text: Text,
    token_kinds: Vec<TokenKind>,
    token_positions: Vec<Pos>,
    token_lens: Vec<u32>,
}

impl Tokenization {
    pub fn len(&self) -> usize {
        self.token_kinds.len()
    }

    pub fn kinds(&self) -> &[TokenKind] {
        &self.token_kinds
    }

    pub fn positions(&self) -> &[Pos] {
        &self.token_positions
    }

    pub fn has_errors(&self) -> bool {
        for token_kind in &self.token_kinds {
            if *token_kind == TokenKind::Unknown {
                return true;
            }
        }
        false
    }

    pub fn error_positions(&self) -> Vec<Pos> {
        let mut results = vec![];
        for (token_kind, pos) in self.token_kinds.iter().zip(self.token_positions.iter()) {
            if *token_kind == TokenKind::Unknown {
                results.push(*pos);
            }
        }
        results
    }

    pub fn linecol(&self, pos: Pos) -> LineCol {
        let pos = usize::from(pos);

        let mut lineno = 1;
        let mut col = 1;

        for (i, ch) in self.text.iter().copied().enumerate() {
            if pos == i {
                break;
            }
            if ch == b'\n' {
                col = 1;
                lineno += 1;
            } else {
                col += 1;
            }
        }

        LineCol::new(lineno, col)
    }

    pub fn span(&self, pos: Pos) -> Span {
        for (i, other_pos) in self.token_positions.iter().enumerate() {
            if pos == *other_pos {
                let len = self.token_lens[i];
                let start = self.linecol(pos);
                let end_pos = Pos::new(u32::from(pos) + len as u32);
                let end = self.linecol(end_pos);
                return Span::new(start, end);
            }
        }
        panic!("No token found at position {pos:?}")
    }
}

pub fn tokenize(text: Text) -> Tokenization {
    let tokenizer = Tokenizer::new(text.clone());

    let mut token_kinds = vec![];
    let mut token_pos = vec![];
    let mut token_lens = vec![];

    for Token(token_kind, pos, len) in tokenizer.tokens() {
        token_kinds.push(token_kind);
        token_pos.push(pos);
        token_lens.push(len);
    }



    Tokenization {
        text,
        token_kinds,
        token_positions: token_pos,
        token_lens,
    }
}

struct Tokenizer {
    text: Text,
    pos: u32,
    token_kinds: Vec<TokenKind>,
    token_pos: Vec<Pos>,
    token_lens: Vec<u32>,
}

impl Tokenizer {
    fn new(text: Text) -> Self {
        let mut tokenizer = Tokenizer {
            text,
            pos: 0,
            token_kinds: vec![],
            token_pos: vec![],
            token_lens: vec![],
        };
        tokenizer.tokenize();
        tokenizer
    }

    fn tokenize(&mut self) {
        while let Some(token) = self.token() {
            self.token_kinds.push(token.kind());
            self.token_pos.push(token.pos());
            self.token_lens.push(token.len());
        }
    }

    fn tokens(&self) -> Vec<Token> {
        let mut results = vec![];
        for ((token_kind, token_pos), len) in self.token_kinds.iter().zip(self.token_pos.iter()).zip(self.token_lens.iter()) {
            results.push(Token(*token_kind, *token_pos, *len));
        }

        results.push(Token(TokenKind::Eof, Pos::new(self.pos), 0));
        results
    }

    fn token(&mut self) -> Option<Token> {
        while let Some(head_char) = self.peek() {
            if head_char.is_ascii_whitespace() {
                self.consume();
            } else if self.text[self.pos as usize..].starts_with(b"//") {
                self.consume_comment();
            } else {
                break;
            }
        }

        if let Some(head_char) = self.peek() {
            if head_char == b'_' || head_char.is_ascii_alphabetic() {
                Some(self.tokenize_identifierlike())
            } else if head_char.is_ascii_digit() {
                Some(self.tokenize_number())
            } else if let Some(token) = self.tokenize_punctuation() {
                Some(token)
            } else {
                let pos = Pos::new(self.pos);
                self.pos += 1;
                Some(Token(TokenKind::Unknown, pos, 1))
            }
        } else {
            None
        }
    }

    fn peek(&self) -> Option<u8> {
        self.text.get(self.pos as usize).copied()
    }

    fn consume(&mut self) -> Option<u8> {
        match self.peek() {
            Some(peek_char) => {
                self.pos += 1;
                Some(peek_char)
            },
            None => {
                None
            },
        }
    }

    fn consume_comment(&mut self) {
        assert!(self.text[self.pos as usize..].starts_with(b"//"));
        while let Some(consume_char) = self.consume() {
            if consume_char == b'\n' {
                break
            }
        }
    }

    fn tokenize_punctuation(&mut self) -> Option<Token>  {
        let pos = self.pos;
        let input = &self.text[self.pos as usize..];

        let (token_kind, len) = 'result: {
            macro_rules! punc_token {
                ($str:literal, $token_kind:path) => {
                    if input.starts_with($str) {
                        break 'result ($token_kind, $str.len() as u32);
                    };
                }
            }

            punc_token!(b":=:", TokenKind::ColonEqColon);

            punc_token!(b"->", TokenKind::Arrow);
            punc_token!(b"=>", TokenKind::FatArrow);
            punc_token!(b"<=", TokenKind::RevFatArrow);
            punc_token!(b":=", TokenKind::ColonEq);
            punc_token!(b"::",  TokenKind::ColonColon);
            punc_token!(b"..", TokenKind::DotDot);

            punc_token!(b"=",  TokenKind::Eq);
            punc_token!(b".",  TokenKind::Dot);
            punc_token!(b":",  TokenKind::Colon);
            punc_token!(b",",  TokenKind::Comma);
            punc_token!(b";",  TokenKind::Semicolon);
            punc_token!(b"$",  TokenKind::Dollar);
            punc_token!(b"@",  TokenKind::At);
            punc_token!(b"#",  TokenKind::Hash);

            punc_token!(b"<",  TokenKind::Lt);
            punc_token!(b">",  TokenKind::Gt);

            punc_token!(b"{",  TokenKind::CurlyLeft);
            punc_token!(b"}",  TokenKind::CurlyRight);
            punc_token!(b"(" , TokenKind::ParenLeft);
            punc_token!(b")",  TokenKind::ParenRight);
            punc_token!(b"[",  TokenKind::BraceLeft);
            punc_token!(b"]",  TokenKind::BraceRight);

            (TokenKind::Unknown, 0)
        };

        if let TokenKind::Unknown = token_kind {
            None
        } else {
            self.pos += len;
            Some(Token(token_kind, Pos::new(pos), len))
        }
    }

    fn tokenize_identifierlike(&mut self) -> Token {
        let start_pos = self.pos;

        let peek_char = self.consume().unwrap();
        assert!(peek_char.is_ascii_alphabetic() || peek_char == b'_');

        while let Some(peek_char) = self.peek() {
            if peek_char.is_ascii_alphanumeric() || peek_char == b'_' {
                self.consume();
            } else {
                break;
            }
        }

        let ident = String::from_utf8_lossy(&self.text[start_pos as usize..self.pos as usize]);

        let token_kind = 'result: {
            macro_rules! kw_token {
                ($str:literal, $token_kind:path) => {
                    if ident == $str {
                        break 'result $token_kind;
                    };
                }
            }

            kw_token!("import", TokenKind::KwImport);
            kw_token!("mod", TokenKind::KwMod);
            kw_token!("ext", TokenKind::KwExt);
            kw_token!("type", TokenKind::KwType);
            kw_token!("union", TokenKind::KwUnion);
            kw_token!("struct", TokenKind::KwStruct);
            kw_token!("enum", TokenKind::KwEnum);
            kw_token!("fn", TokenKind::KwFn);

            kw_token!("width", TokenKind::KwWidth);

            kw_token!("incoming", TokenKind::KwIncoming);
            kw_token!("outgoing", TokenKind::KwOutgoing);
            kw_token!("reg", TokenKind::KwReg);
            kw_token!("wire", TokenKind::KwWire);

            kw_token!("submod", TokenKind::KwSubmod);

            kw_token!("of", TokenKind::KwOf);
            kw_token!("on", TokenKind::KwOn);

            kw_token!("socket", TokenKind::KwSocket);
            kw_token!("master", TokenKind::KwMaster);
            kw_token!("slave", TokenKind::KwSlave);
            kw_token!("mosi", TokenKind::KwMosi);
            kw_token!("miso", TokenKind::KwMiso);

            kw_token!("word", TokenKind::KwWord);
            kw_token!("sext", TokenKind::KwSext);
            kw_token!("zext", TokenKind::KwZext);
            kw_token!("if", TokenKind::KwIf);
            kw_token!("else", TokenKind::KwElse);
            kw_token!("match", TokenKind::KwMatch);
            kw_token!("true", TokenKind::KwTrue);
            kw_token!("false", TokenKind::KwFalse);

            TokenKind::Ident
        };

        Token(token_kind, Pos::new(start_pos), ident.len() as u32)
    }

    fn tokenize_number(&mut self) -> Token {
        let start_pos = self.pos;

        let input = &self.text[self.pos as usize..];
        let base: u8 = if input.starts_with(b"0x") {
            self.consume();
            self.consume();
            16
        } else if input.starts_with(b"0b") {
            self.consume();
            self.consume();
            2
        } else {
            10
        };

        if base == 2 {
            while let Some(peek_char) = self.peek() {
                if peek_char == b'0' || peek_char == b'1' || peek_char == b'_' {
                    self.consume();
                } else {
                    break;
                }
            }
        } else if base == 16 {
            while let Some(peek_char) = self.peek() {
                if peek_char.is_ascii_hexdigit() || peek_char == b'_' {
                    self.consume();
                } else {
                    break;
                }
            }
        } else {
            while let Some(peek_char) = self.peek() {
                if peek_char.is_ascii_digit() || peek_char == b'_' {
                    self.consume();
                } else {
                    break;
                }
            }
        }

        let has_width = if let Some(b'w') = self.peek() {
            self.consume();
            while let Some(peek_char) = self.peek() {
                if peek_char.is_ascii_digit() {
                    self.consume();
                } else {
                    break;
                }
            }
            true
        } else {
            false
        };

        let token_kind = if has_width {
            TokenKind::Word
        } else {
            TokenKind::Nat
        };

        let len = self.pos - start_pos;

        Token(token_kind, Pos::new(start_pos), len as u32)
    }
}

