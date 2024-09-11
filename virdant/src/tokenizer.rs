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
    Path,

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

    Equals,
    Colon,
    Comma,
    DotDot,
    Semicolon,

    Dollar,
    At,
    Hash,

    StrLit,
    Nat,
    WordLit,

    KwImport,
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
            } else if self.input[self.pos as usize..].starts_with(b"//") {
                self.consume_comment();
            } else {
                break;
            }
        }

        if let Some(head_char) = self.peek() {
            if head_char == b'_' || head_char.is_ascii_alphabetic() {
                Some(self.tokenize_identifierlike())
            } else if head_char.is_ascii_digit() {
                Some(self.tokenize_nat())
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

            punc_token!(b"->", TokenKind::Arrow);
            punc_token!(b"=>", TokenKind::FatArrow);
            punc_token!(b"=",  TokenKind::Equals);
            punc_token!(b"..", TokenKind::DotDot);
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
            assert!(len < 256, "Token length cannot exceed 256");
            self.pos += len;
            Some(Token(token_kind, Pos::new(pos), len as u8))
        }
    }

    fn tokenize_identifierlike(&mut self) -> Token {
        let start_pos = self.pos;

        const MAX_PARTS: usize = 5;
        let mut parts: [u32; MAX_PARTS] = [0; MAX_PARTS];
        let mut part_count = 1;
        let mut part_start = start_pos;

        loop {
            while let Some(peek_char) = self.peek() {
                if peek_char.is_ascii_alphabetic() || peek_char == b'_' {
                    self.consume();
                } else {
                    break;
                }
            }

            if let Some(ch) = self.peek() {
                if ch == b'.' {
                    self.consume();
                    part_count += 1;
                    parts[part_count] = part_start;
                    part_start = self.pos;
                    assert!(part_count < MAX_PARTS, "Path parts cannot exceed {MAX_PARTS}");
                } else {
                    break;
                }
            }
        }

        let ident = String::from_utf8_lossy(&self.input[start_pos as usize..self.pos as usize]);

        if part_count == 1 {
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
        } else {
            Token(TokenKind::Path, Pos::new(start_pos), ident.len() as u8)
        }
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
        Token(TokenKind::Nat, Pos::new(start_pos), len as u8)
    }
}

pub fn pretty_token_debug(text: &str) {
    let toks = tokenize(text.as_bytes()) ;

    let mut tok_iter = toks.into_iter();
    let mut current_tok = tok_iter.next();

    let mut max_pos = 0;

    for (lineno, line) in text.lines().enumerate() {
        max_pos += line.len();
        println!("{lineno}: {line}");
        loop {
            if let Some(tok) = current_tok.clone() {
                if usize::from(tok.pos()) <= max_pos {
                    eprint!("{:?} ", tok.kind());
                    current_tok = tok_iter.next();
                } else {
                    eprintln!();
                    eprintln!();
                    break;
                }
            } else {
                eprintln!();
                eprintln!();
                break;
            }
        }
    }
}
