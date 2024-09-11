use crate::tokenizer::{tokenize, Token, TokenKind};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Ast;

pub fn parse(input: &str) -> Result<Arc<Ast>, Vec<ParseError>> {
    let mut parser = Parser::new(&input);
    let ast = parser.parse();
    if parser.errors.is_empty() {
        Ok(ast)
    } else {
        Err(parser.errors)
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Unexpected(Token),
    Expected(TokenKind, Token),
    ExpectedItemStart(Token),
    Unknown(Token),
}

struct Parser<'a> {
    text: &'a [u8],
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Self {
        let text_bytes = text.as_bytes();
        let tokens = tokenize(text_bytes);
        Parser {
            text: text_bytes,
            tokens,
            pos: 0,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Arc<Ast> {
        while let TokenKind::KwImport = self.peek() {
            if let Err(e) = self.parse_import() {
                self.errors.push(e);
            }
        }

        while !self.is_eof() {
            if let Err(error) = self.parse_item() {
                self.errors.push(error);
                self.recover_item();
            }
        }
        Arc::new(Ast)
    }

    fn recover_item(&mut self) {
        loop {
            let token_kind = self.peek();
            if token_kind == TokenKind::Eof {
                return;
            } else if token_kind == TokenKind::KwMod {
                return;
            } if token_kind == TokenKind::KwExt {
                return;
            } if token_kind == TokenKind::KwSocket {
                return;
            } if token_kind == TokenKind::KwEnum {
                return;
            } if token_kind == TokenKind::KwStruct {
                return;
            } if token_kind == TokenKind::KwUnion {
                return;
            }
            self.take();
        }
    }

    fn peek(&self) -> TokenKind {
        self.tokens[self.pos].kind()
    }

    fn consume(&mut self, token_kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.take();
        if token.kind() != token_kind {
            Err(ParseError::Expected(token_kind, token.clone()))
        } else {
            Ok(token)
        }
    }

    fn take(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        if token.kind() == TokenKind::Unknown {
            self.errors.push(ParseError::Unexpected(token.clone()));
        }
        token
    }

    fn consume_if(&mut self, token_kind: TokenKind) -> Option<Token> {
        if self.peek() == token_kind {
            Some(self.take())
        } else {
            None
        }
    }

    fn is_eof(&self) -> bool {
        self.peek() == TokenKind::Eof
    }

    fn token_str(&self, token: Token) -> std::borrow::Cow<str> {
        let idx_start = usize::from(token.pos());
        let idx_end = idx_start + usize::from(token.len());
        String::from_utf8_lossy(&self.text[idx_start..idx_end])
    }

    fn consume_ident(&mut self) -> Result<String, ParseError> {
        let token = self.consume(TokenKind::Ident)?;
        Ok(self.token_str(token).to_string())
    }

    fn parse_import(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwImport)?;
        let name = self.consume_ident();
        eprintln!("IMPORT {name:?}");
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast))
    }

    fn parse_item(&mut self) -> Result<Arc<Ast>, ParseError> {
        match self.peek() {
            TokenKind::KwMod => {
                self.parse_moddef()
            },
            _ => {
                let token = self.take();
                let error = ParseError::ExpectedItemStart(token);
                self.errors.push(error.clone());
                Err(error)
            },
        }
    }

    fn parse_moddef(&mut self) -> Result<Arc<Ast>, ParseError> {
        let is_ext = self.consume_if(TokenKind::KwExt).is_some();
        self.consume(TokenKind::KwMod)?;
        let name = self.consume_ident();
        self.consume(TokenKind::CurlyLeft)?;
        eprintln!("MODDEF {name:?}{}", if is_ext { " EXT" } else { "" });
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_moddef_statement()?;
        }
        self.consume(TokenKind::CurlyRight)?;
        Ok(Arc::new(Ast))
    }

    fn parse_moddef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let token = self.take();

        let ast = match token.kind() {
            TokenKind::KwIncoming => {
                let name = self.consume_ident();
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                eprintln!("  INCOMING {name:?} : {typ:?}");
                Ok(Arc::new(Ast))
            },
            TokenKind::KwOutgoing => {
                let name = self.consume_ident();
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                eprintln!("  OUTGOING {name:?} : {typ:?}");
                Ok(Arc::new(Ast))
            },
            TokenKind::KwWire => {
                let name = self.consume_ident();
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                eprintln!("  WIRE {name:?} : {typ:?}");
                Ok(Arc::new(Ast))
            },
            TokenKind::KwReg => {
                let name = self.consume_ident();
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::KwOn)?;
                let clock = self.parse_expr()?;
                self.consume(TokenKind::Semicolon)?;
                eprintln!("  REG {name:?} : {typ:?} ON {clock:?}");
                Ok(Arc::new(Ast))
            },
            _ => {
                return Err(ParseError::Unknown(token));
            },
        };
        ast
    }

    fn parse_qualident(&mut self) -> Result<Arc<Ast>, ParseError> {
        let ident = self.consume_ident();
        Ok(Arc::new(Ast))
    }

    fn parse_type(&mut self) -> Result<Arc<Ast>, ParseError> {
        let name = self.parse_qualident()?;
        if self.consume_if(TokenKind::BraceLeft).is_some() {
            let param = self.consume(TokenKind::Nat)?;
            self.consume(TokenKind::BraceRight)?;
        }
        Ok(Arc::new(Ast))
    }

    fn parse_expr(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.parse_qualident()?;
        Ok(Arc::new(Ast))
    }
}


#[test]
pub fn test_parse() {
    let text = " import foo;

        mod Top {
            incoming clock : Clock;
            reg r : Word[8] on clock;
        }

        mod Buffer {
            incoming clock : Clock;
        }
    ";

    if let Err(errors) = parse(&text) {
        eprintln!("ERRORS:");
        for error in errors {
            eprintln!("    {error:?}");
        }
    }
}
