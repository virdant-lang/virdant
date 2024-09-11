use crate::location::Pos;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenizeErr(pub String);

pub type TokenLen = u8;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token(TokenKind, Pos, TokenLen);

impl Token {
    pub fn kind(&self) -> TokenKind {
        self.0
    }

    pub fn pos(&self) -> Pos {
        self.1
    }

    pub fn len(&self) -> u8 {
        self.2
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Unknown,
    Eof,

    Ident,
    Equals,

    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftBracket,
    RightBracket,

    Arrow,
    FatArrow,

    Colon,
    Semicolon,

    Dollar,
    At,

    StrLit,
    NatLit,
    WordLit,

    KwMod,
    KwExt,
    KwType,
    KwUnion,
    KwStruct,
    KwEnum,

    KwIncoming,
    KwOutgoing,
    KwReg,
    KwWire,

    KwOn,

    KwSocket,
    KwMaster,
    KwSlave,
    KwMosi,
    KwMiso,
}

pub fn tokenize(input: &[u8]) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.tokenize()
}

struct Tokenizer<'a> {
    input: &'a [u8],
    pos: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Tokenizer {
            input,
            pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.token() {
            tokens.push(token);
        }
        tokens.push(Token(TokenKind::Eof, Pos::new(self.pos), 0));
        tokens
    }

    pub fn token(&mut self) -> Option<Token> {
        while let Some(head_char) = self.peek() {
            if head_char.is_ascii_whitespace() {
                self.consume();
            } else if self.input.starts_with(b"//") {
                self.consume_comment();
            } else {
                break;
            }
        }

        if let Some(head_char) = self.peek() {
            if head_char == b'_' || head_char.is_ascii_alphabetic() {
                Some(self.tokenize_ident())
            } else if head_char.is_ascii_digit() {
                Some(self.tokenize_nat())
            } else {
                self.tokenize_punctuation()
            }
        } else {
            None
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos as usize).copied()
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
        assert!(self.input[self.pos as usize..].starts_with(b"//"));
        while let Some(consume_char) = self.consume() {
            if consume_char == b'\n' {
                break
            }
        }
    }

    fn tokenize_punctuation(&mut self) -> Option<Token>  {
        let pos = self.pos;
        let input = &self.input[self.pos as usize..];

        let (token_kind, len) = 'result: {
            macro_rules! punc_token {
                ($str:literal, $token_kind:path) => {
                    if input.starts_with($str) {
                        break 'result ($token_kind, $str.len() as u32);
                    };
                }
            }

            punc_token!(b"=>", TokenKind::FatArrow);
            punc_token!(b"=",  TokenKind::Equals);
            punc_token!(b":",  TokenKind::Colon);
            punc_token!(b";",  TokenKind::Semicolon);
            punc_token!(b"{",  TokenKind::LeftCurly);
            punc_token!(b"}",  TokenKind::RightCurly);
            punc_token!(b"(" ,  TokenKind::LeftParen);
            punc_token!(b")",  TokenKind::RightParen);
            punc_token!(b"[",  TokenKind::LeftBracket);
            punc_token!(b"]",  TokenKind::RightBracket);

            (TokenKind::Unknown, 0)
        };

        if let TokenKind::Unknown = token_kind {
            None
        } else {
            assert!(len < 256, "Token length cannot exceed 256");
            self.pos += len;
            Some(Token(token_kind, Pos::new(pos), len as u8))
        }
    }

    fn tokenize_ident(&mut self) -> Token {
        let start_pos = self.pos;
        while let Some(peek_char) = self.peek() {
            if peek_char.is_ascii_alphabetic() || peek_char == b'_' {
                self.consume();
            } else {
                break;
            }
        }

        let ident = String::from_utf8_lossy(&self.input[start_pos as usize..self.pos as usize]);
        let token_kind = 'result: {
            macro_rules! kw_token {
                ($str:literal, $token_kind:path) => {
                    if ident == $str {
                        break 'result $token_kind;
                    };
                }
            }

            kw_token!("mod", TokenKind::KwMod);
            kw_token!("ext", TokenKind::KwExt);
            kw_token!("type", TokenKind::KwType);
            kw_token!("union", TokenKind::KwUnion);
            kw_token!("struct", TokenKind::KwStruct);
            kw_token!("enum", TokenKind::KwEnum);

            kw_token!("incoming", TokenKind::KwIncoming);
            kw_token!("outgoing", TokenKind::KwOutgoing);
            kw_token!("reg", TokenKind::KwReg);
            kw_token!("wire", TokenKind::KwWire);

            kw_token!("on", TokenKind::KwOn);

            kw_token!("socket", TokenKind::KwSocket);
            kw_token!("master", TokenKind::KwMaster);
            kw_token!("slave", TokenKind::KwSlave);
            kw_token!("mosi", TokenKind::KwMosi);
            kw_token!("miso", TokenKind::KwMiso);

            TokenKind::Ident
        };

        assert!(ident.len() < 256, "Token length cannot exceed 256");
        Token(token_kind, Pos::new(start_pos), ident.len() as u8)
    }

    fn tokenize_nat(&mut self) -> Token {
        let start_pos = self.pos;
        while let Some(peek_char) = self.peek() {
            if peek_char.is_ascii_digit() || peek_char == b'_' {
                self.consume();
            } else {
                break;
            }
        }

        let len = self.pos - start_pos;

        assert!(len < 256, "Token length cannot exceed 256");
        Token(TokenKind::NatLit, Pos::new(start_pos), len as u8)
    }
}

#[test]
pub fn test_tokenizer() {
    let text = "
        mod Top {
            incoming clock : Clock;

            reg r : Word[1] on clock;
            r <= 1;
        }
    ";

    let text = text.as_bytes();

    for tok in tokenize(text) {
        let start_idx = u32::from(tok.pos()) as usize;
        let end_idx = start_idx + tok.len() as usize;
        eprintln!("TOKEN: {tok:?} \t\t{:?}", String::from_utf8_lossy(&text[start_idx..end_idx]));
    }
}
