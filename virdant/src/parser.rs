use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::ItemKind;
use std::marker::PhantomData;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ParseError {
    Unexpected(Token),
    Expected(TokenKind, Token),
    ExpectedItemStart(Token),
    Unknown(Token),
}

impl ParseError {
    pub fn token(&self) -> Token {
        match self {
            ParseError::Unexpected(token) => token,
            ParseError::Expected(_expected_token_kind, token) => token,
            ParseError::ExpectedItemStart(token) => token,
            ParseError::Unknown(token) => token,
        }.clone()
    }
}

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    pos: usize,
    errors: Vec<ParseError>,
    tag: std::marker::PhantomData<&'a ()>,
}

impl<'a> Parser<'a> {
    pub fn new(text: Arc<[u8]>) -> Self {
        let tokenizer = Tokenizer::new(text);
        Parser {
            tokenizer,
            pos: 0,
            errors: vec![],
            tag: PhantomData,
        }
    }

    pub fn ast(&mut self) -> Arc<Ast> {
        let mut imports = vec![];
        while let TokenKind::KwImport = self.peek() {
            match self.parse_import() {
                Ok(import) => imports.push(import),
                Err(e) => self.errors.push(e),
            }
        }

        let mut items = vec![];
        while !self.is_eof() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(error) => {
                    self.errors.push(error);
                    self.recover_item();
                },
            }
        }
        Arc::new(Ast::Package { imports, items })
    }

    pub fn errors(&self) -> Vec<ParseError> {
        self.errors.clone()
    }

    pub fn tokens(&self) -> &[Token] {
        self.tokenizer.tokens()
    }

    pub fn text(&self) -> &[u8] {
        self.tokenizer.text()
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
        self.tokens()[self.pos].kind()
    }

    fn debug_ahead(&self) {
        for token in &self.tokens()[self.pos..self.pos + 11] {
            eprintln!("    {:?}({})", token.kind(), usize::from(token.pos()));
        }
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
        let token = self.tokens()[self.pos].clone();
        self.pos += 1;
        if token.kind() == TokenKind::Unknown {
            self.errors.push(ParseError::Unexpected(token.clone()));
        }
        token
    }

    fn consume_if(&mut self, token_kind: TokenKind) -> bool {
        if self.peek() == token_kind {
            self.take();
            true
        } else {
            false
        }
    }

    fn is_eof(&self) -> bool {
        self.peek() == TokenKind::Eof
    }

    fn token_str(&self, token: Token) -> std::borrow::Cow<str> {
        let idx_start = usize::from(token.pos());
        let idx_end = idx_start + usize::from(token.len());
        String::from_utf8_lossy(&self.text()[idx_start..idx_end])
    }

    fn consume_ident(&mut self) -> Result<Token, ParseError> {
        self.consume(TokenKind::Ident)
    }

    fn parse_import(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwImport)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::Import(name)))
    }

    fn parse_item(&mut self) -> Result<Arc<Ast>, ParseError> {
        match self.peek() {
            TokenKind::KwMod | TokenKind::KwExt => self.parse_moddef(),
            TokenKind::KwUnion => self.parse_uniondef(),
            TokenKind::KwStruct => self.parse_structdef(),
            TokenKind::KwEnum => self.parse_enumdef(),
            _ => {
                let token = self.take();
                let error = ParseError::ExpectedItemStart(token);
                self.errors.push(error.clone());
                Err(error)
            },
        }
    }

    fn parse_moddef(&mut self) -> Result<Arc<Ast>, ParseError> {
        let is_ext = self.consume_if(TokenKind::KwExt);
        self.consume(TokenKind::KwMod)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::CurlyLeft)?;
        eprintln!("MODDEF {name:?}{}", if is_ext { " EXT" } else { "" });
        let mut stmts = vec![];
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_moddef_statement()?;
            stmts.push(stmt);
        }
        self.consume(TokenKind::CurlyRight)?;

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::ModDef,
            stmts,
            width: None,
        }))
    }

    fn parse_moddef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let ast = match self.peek() {
            TokenKind::KwIncoming => {
                let token = self.take();
                let name = self.consume_ident()?;
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Component {
                    kind: ComponentKind::Incoming,
                    name,
                    typ,
                    on: None,
                }))
            },
            TokenKind::KwOutgoing => {
                let token = self.take();
                let name = self.consume_ident()?;
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Component {
                    kind: ComponentKind::Outgoing,
                    name,
                    typ,
                    on: None,
                }))
            },
            TokenKind::KwWire => {
                let token = self.take();
                let name = self.consume_ident()?;
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Component {
                    kind: ComponentKind::Wire,
                    name,
                    typ,
                    on: None,
                }))
            },
            TokenKind::KwReg => {
                let token = self.take();
                let name = self.consume_ident()?;
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                self.consume(TokenKind::KwOn)?;
                let on = Some(self.parse_expr()?);
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Component {
                    kind: ComponentKind::Reg,
                    name,
                    typ,
                    on,
                }))
            },
            TokenKind::Ident => {
                self.parse_driver()
            },
            _ => {
                let token = self.take();
                return Err(ParseError::Unknown(token));
            },
        };
        ast
    }

    fn parse_uniondef(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwUnion)?;
        self.consume(TokenKind::KwType)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::CurlyLeft)?;
        let mut stmts = vec![];
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_uniondef_statement()?;
            stmts.push(stmt);
        }
        self.consume(TokenKind::CurlyRight)?;

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::ModDef,
            stmts,
            width: None,
        }))
    }

    fn parse_uniondef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let ctor_name = self.consume_ident()?; 
        let params = self.parse_param_list()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::CtorDef(ctor_name, params)))
    }

    fn parse_structdef(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwStruct)?;
        self.consume(TokenKind::KwType)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::CurlyLeft)?;
        let mut stmts = vec![];
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_structdef_statement()?;
            stmts.push(stmt);
        }
        self.consume(TokenKind::CurlyRight)?;

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::ModDef,
            stmts,
            width: None,
        }))
    }

    fn parse_structdef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let field_name = self.consume_ident()?; 
        self.consume(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::FieldDef(field_name, typ)))
    }

    fn parse_param_list(&mut self) -> Result<Vec<Arc<Ast>>, ParseError> {
        let mut params = vec![];
        self.consume(TokenKind::ParenLeft)?;

        while self.peek() == TokenKind::Ident {
            let name = self.consume_ident()?;
            self.consume(TokenKind::Colon)?;
            let typ = self.parse_type()?;
            params.push(Arc::new(Ast::Param(name, typ)));

            while self.consume_if(TokenKind::Comma) {
                let name = self.consume_ident()?;
                self.consume(TokenKind::Colon)?;
                let typ = self.parse_type()?;
                params.push(Arc::new(Ast::Param(name, typ)));
            }
            self.consume_if(TokenKind::Comma);
        }

        self.consume(TokenKind::ParenRight)?;
        Ok(params)
    }

    fn parse_enumdef(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwEnum)?;
        self.consume(TokenKind::KwType)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::KwWidth)?;
        let width = self.consume(TokenKind::Nat)?;
        self.consume(TokenKind::CurlyLeft)?;
        let mut stmts = vec![];
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_enumdef_statement()?;
            stmts.push(stmt);
        }
        self.consume(TokenKind::CurlyRight)?;

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::EnumDef,
            stmts,
            width: Some(width),
        }))
    }

    fn parse_enumdef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let name = self.consume_ident()?; 
        self.consume(TokenKind::Eq)?;
        let value = self.consume(TokenKind::Word)?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::EnumerantDef(name, value)))
    }

    fn parse_path(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut result = vec![];
        result.push(self.consume_ident()?);
        while self.consume_if(TokenKind::Dot) {
            result.push(self.consume_ident()?);
        }
        Ok(result)
    }

    fn parse_qualident(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut result = vec![];
        result.push(self.consume_ident()?);
        while self.consume_if(TokenKind::ColonColon) {
            result.push(self.consume_ident()?);
        }
        Ok(result)
    }

    fn parse_type(&mut self) -> Result<Arc<Ast>, ParseError> {
        let name = self.parse_qualident()?;
        let mut args = None;
        if self.consume_if(TokenKind::BraceLeft) {
            let width = self.consume(TokenKind::Nat)?;
            args = Some(vec![Arc::new(
                Ast::TypeArgWidth { width, }
            )]);
            self.consume(TokenKind::BraceRight)?;
        }
        Ok(Arc::new(Ast::Type {
            name,
            args,
        }))
    }

    fn parse_expr(&mut self) -> Result<Arc<Ast>, ParseError> {
        if self.peek() == TokenKind::KwIf {
            self.parse_expr_if()
        } else if self.peek() == TokenKind::KwMatch {
            self.parse_expr_match()
        } else if self.peek() == TokenKind::Dollar {
            self.parse_expr_struct()
        } else {
            self.parse_expr_call()
        }
    }

    fn parse_expr_if(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwIf)?;
        let subject = self.parse_expr()?;
        self.consume(TokenKind::CurlyLeft)?;
        let true_branch = self.parse_expr()?;
        self.consume(TokenKind::CurlyRight)?;
        self.consume(TokenKind::KwElse)?;

        let mut elseifs = vec![(subject, true_branch)];

        while self.consume_if(TokenKind::KwIf) {
            let elseif_subject = self.parse_expr()?;
            self.consume(TokenKind::CurlyLeft)?;
            let elseif_expr = self.parse_expr()?;
            self.consume(TokenKind::CurlyRight)?;
        self.consume(TokenKind::KwElse)?;
            elseifs.push((elseif_subject, elseif_expr));
        }

        self.consume(TokenKind::CurlyLeft)?;
        let false_branch = self.parse_expr()?;
        self.consume(TokenKind::CurlyRight)?;

        let mut result = false_branch;
        for (elseif_subject, elseif_expr) in elseifs.into_iter().rev() {
            result = Arc::new(Ast::If { 
                subject: elseif_subject,
                true_branch: elseif_expr,
                false_branch: result,
            });
        }

        Ok(result)
    }

    fn parse_expr_match(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwMatch)?;
        let subject = self.parse_expr()?;
        self.consume(TokenKind::CurlyLeft)?;
        let mut arms = vec![];

        while !self.consume_if(TokenKind::CurlyRight) {
            arms.push(self.parse_arm()?);
        }

        Ok(Arc::new(Ast::Match { subject, arms }))
    }

    fn parse_arm(&mut self) -> Result<Arc<Ast>, ParseError> {
        let pat = self.parse_pat()?;
        self.consume(TokenKind::FatArrow)?;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::MatchArm { pat, expr }))
    }

    fn parse_pat(&mut self) -> Result<Arc<Ast>, ParseError> {
        if self.consume_if(TokenKind::At) {
            let ctor_name = self.consume_ident()?;
            let subpats = self.parse_pat_list()?;

            Ok(Arc::new(Ast::PatAt(ctor_name, Some(subpats))))
        } else if self.consume_if(TokenKind::Hash) {
            todo!()
        } else if self.consume_if(TokenKind::KwElse) {
            todo!()
        } else {
            let name = self.consume_ident()?;
            Ok(Arc::new(Ast::PatBind(name)))
        }
    }

    fn parse_pat_list(&mut self) -> Result<Vec<Arc<Ast>>, ParseError> {
        let mut subpats = vec![];
        self.consume(TokenKind::ParenLeft)?;
        if self.is_start_pat() {
            subpats.push(self.parse_pat()?);

            while self.consume_if(TokenKind::Comma) {
                subpats.push(self.parse_pat()?);
            }
            self.consume_if(TokenKind::Comma);
        }
        self.consume(TokenKind::ParenRight)?;
        Ok(subpats)
    }

    fn is_start_pat(&self) -> bool {
        self.peek() == TokenKind::At ||
        self.peek() == TokenKind::Hash ||
        self.peek() == TokenKind::Ident ||
        self.peek() == TokenKind::KwElse
    }

    fn parse_expr_struct(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::Dollar)?;
        let struct_name = self.consume_ident()?;
        let assigns = self.parse_assigns()?;
        Ok(Arc::new(Ast::Struct(struct_name, assigns)))
    }

    fn parse_assigns(&mut self) -> Result<Vec<Arc<Ast>>, ParseError> {
        let mut results = vec![];
        self.consume(TokenKind::CurlyLeft)?;
        while self.peek() == TokenKind::Ident {
            let field_name = self.consume_ident()?;
            self.consume(TokenKind::Eq)?;
            let expr = self.parse_expr()?;
            let assign = Arc::new(Ast::Assign(field_name, expr));
            results.push(assign);
            if !self.consume_if(TokenKind::Comma) {
                break;
            }
        }
        self.consume(TokenKind::CurlyRight)?;
        Ok(results)
    }

    fn parse_expr_call(&mut self) -> Result<Arc<Ast>, ParseError> {
        let mut result = if self.consume_if(TokenKind::KwWord) {
            let exprs = self.parse_expr_list()?;
            Arc::new(Ast::Word(exprs))
        } else {
            self.parse_expr_base()?
        };

        loop {
            if self.consume_if(TokenKind::Arrow) {
                let method = self.consume_ident()?;
                let args = self.parse_expr_list()?;
                result = Arc::new(Ast::MethodCall(result, method, args));
            } else if self.consume_if(TokenKind::BraceLeft) {
                let j = self.consume(TokenKind::Nat)?;
                if self.consume_if(TokenKind::DotDot) {
                    let i = self.consume(TokenKind::Nat)?;
                    result = Arc::new(Ast::IdxRange(result, j, i));
                } else {
                    result = Arc::new(Ast::Idx(result, j));
                }
                self.consume(TokenKind::BraceRight)?;
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Arc<Ast>>, ParseError> {
        let mut exprs = vec![];
        self.consume(TokenKind::ParenLeft)?;

        while self.is_start_expr() {
            exprs.push(self.parse_expr()?);
            while self.consume_if(TokenKind::Comma) {
                exprs.push(self.parse_expr()?);
            }
            self.consume_if(TokenKind::Comma);
        }

        self.consume(TokenKind::ParenRight)?;
        Ok(exprs)
    }

    fn is_start_expr(&self) -> bool {
        self.peek() == TokenKind::Word ||
        self.peek() == TokenKind::KwFalse ||
        self.peek() == TokenKind::KwTrue ||
        self.peek() == TokenKind::Nat ||
        self.peek() == TokenKind::KwWord ||
        self.peek() == TokenKind::ParenLeft ||
        self.peek() == TokenKind::At ||
        self.peek() == TokenKind::Hash ||
        self.peek() == TokenKind::Ident
    }

    fn parse_expr_base(&mut self) -> Result<Arc<Ast>, ParseError> {
        if self.peek() == TokenKind::ParenLeft {
            self.consume(TokenKind::ParenLeft)?;
            let expr = self.parse_expr()?;
            self.consume(TokenKind::ParenRight)?;
            Ok(expr)
        } else if self.peek() == TokenKind::KwFalse || self.peek() == TokenKind::KwTrue {
            let token = self.take();
            Ok(Arc::new(Ast::Lit(token)))
        } else if self.peek() == TokenKind::Nat || self.peek() == TokenKind::Word {
            self.parse_expr_word()
        } else if self.peek() == TokenKind::At {
            self.parse_expr_ctor()
        } else {
            let path = self.parse_path()?;
            Ok(Arc::new(Ast::Reference(path)))
        }
    }

    fn parse_expr_ctor(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::At)?;
        let name = self.consume_ident()?;
        let exprs = self.parse_expr_list()?;
        Ok(Arc::new(Ast::Ctor(name, exprs)))
    }

    fn parse_expr_word(&mut self) -> Result<Arc<Ast>, ParseError> {
        let token = self.take();
        Ok(Arc::new(Ast::Lit(token)))
    }

    fn parse_driver(&mut self) -> Result<Arc<Ast>, ParseError> {
        let target = self.parse_path()?;
        let driver_type = self.peek();
        let driver_type_token = if driver_type == TokenKind::RevFatArrow {
            self.consume(TokenKind::RevFatArrow)?
        } else {
            self.consume(TokenKind::ColonEq)?
        };

        let driver = self.parse_expr()?;
        self.consume(TokenKind::Semicolon)?;

        Ok(Arc::new(Ast::Driver(target, driver_type_token, driver)))
    }
}

pub type Ident = Token;
pub type Path = Vec<Token>;
pub type QualIdent = Vec<Token>;
pub type StaticIndex = Token;
pub type Width = Token;

#[derive(Debug, Clone)]
pub enum Ast {
    None,
    Package { imports: Vec<Arc<Ast>>, items: Vec<Arc<Ast>> },
    Import(Ident),
    Item { kind: ItemKind, name: Ident, stmts: Vec<Arc<Ast>>, width: Option<Token> },
    Type { name: QualIdent, args: Option<Vec<Arc<Ast>>> },
    TypeArgWidth { width: Width },
    Component { kind: ComponentKind, name: Ident, typ: Arc<Ast>, on: Option<Arc<Ast>> },
    Driver(Path, Token, Arc<Ast>),
    FieldDef(Ident, Arc<Ast>),
    CtorDef(Ident, Vec<Arc<Ast>>),
    EnumerantDef(Ident, Token),
    Reference(Path),
    Lit(Token),
    Word(Vec<Arc<Ast>>),
    Bit(bool),
    UnOp(Ident, Arc<Ast>),
    BinOp(Arc<Ast>, Ident, Arc<Ast>),
    MethodCall(Arc<Ast>, Ident, Vec<Arc<Ast>>),
    Struct(Ident, Vec<Arc<Ast>>),
    Assign(Ident, Arc<Ast>),
    FnCall(Ident, Vec<Arc<Ast>>),
    Field(Arc<Ast>, Ident),
    Ctor(Ident, Vec<Arc<Ast>>),
    Param(Ident, Arc<Ast>),
    Enumerant(Ident),
    As(Arc<Ast>, Arc<Ast>),
    Idx(Arc<Ast>, StaticIndex),
    IdxRange(Arc<Ast>, StaticIndex, StaticIndex),
    Cat(Vec<Arc<Ast>>),
    Zext(Arc<Ast>),
    If { subject: Arc<Ast>, true_branch: Arc<Ast>, false_branch: Arc<Ast> },
    Match { subject: Arc<Ast>, arms: Vec<Arc<Ast>> },
    MatchArm { pat: Arc<Ast>, expr: Arc<Ast> },
    PatBind(Token),
    PatAt(Token, Option<Vec<Arc<Ast>>>),
}

#[derive(Debug, Clone)]
pub enum ComponentKind {
    Incoming,
    Outgoing,
    Reg,
    Wire,
}
