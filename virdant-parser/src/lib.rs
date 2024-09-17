#[cfg(test)]
mod tests;

use std::sync::Arc;

use virdant_common::text::Text;
use virdant_common::location::Pos;
use virdant_tokenizer::{tokenize, TokenKind, Tokenization, Token};
use virdant_common::ItemKind;

pub fn parse(text: Text) -> Arc<Ast> {
    let mut parser = Parser::new(text);
    parser.ast()
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Unexpected(Token),
    Expected(TokenKind, Token),
    ExpectedItemStart(Token),
    Unknown(Token),
}

impl ParseError {
    pub fn pos(&self) -> Pos {
        match self {
            ParseError::Unexpected(token) => token.pos(),
            ParseError::Expected(_expected_token_kind, token) => token.pos(),
            ParseError::ExpectedItemStart(token) => token.pos(),
            ParseError::Unknown(token) => token.pos(),
        }.clone()
    }
}

pub struct Parser {
    tokenization: Tokenization,
    pos: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(text: Text) -> Self {
        let tokenization = tokenize(text);
        Parser {
            tokenization,
            pos: 0,
            errors: vec![],
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

    fn recover_item(&mut self) {
        loop {
            let token_kind = self.peek();
            if token_kind == TokenKind::Eof {
                return;
            } else if token_kind == TokenKind::KwSubmod {
                return;
            } else if token_kind == TokenKind::KwMod {
                return;
            } else if token_kind == TokenKind::KwExt {
                return;
            } else if token_kind == TokenKind::KwSocket {
                return;
            } else if token_kind == TokenKind::KwEnum {
                return;
            } else if token_kind == TokenKind::KwStruct {
                return;
            } else if token_kind == TokenKind::KwUnion {
                return;
            } else if token_kind == TokenKind::KwFn {
                return;
            }
            self.take();
        }
    }

    fn peek(&self) -> TokenKind {
        self.tokenization.kinds()[self.pos]
    }

    fn peek2(&self) -> TokenKind {
        if self.pos + 1 > self.tokenization.len() {
            TokenKind::Eof
        } else {
            self.tokenization.kinds()[self.pos + 1]
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
        let token_kind = self.tokenization.kinds()[self.pos];
        let pos = self.tokenization.positions()[self.pos];
        let len = self.tokenization.token_lens()[self.pos];
        let token = Token::new(token_kind, pos, len, self.tokenization.text());
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
            TokenKind::KwSubmod | TokenKind::KwMod | TokenKind::KwExt => self.parse_moddef(),
            TokenKind::KwUnion => self.parse_uniondef(),
            TokenKind::KwStruct => self.parse_structdef(),
            TokenKind::KwSocket => self.parse_socketdef(),
            TokenKind::KwEnum => self.parse_enumdef(),
            TokenKind::KwFn => self.parse_fndef(),
            _ => {
                let token = self.take();
                let error = ParseError::ExpectedItemStart(token);
                self.errors.push(error.clone());
                Err(error)
            },
        }
    }

    fn parse_moddef(&mut self) -> Result<Arc<Ast>, ParseError> {
        let mut attrs = vec![];
        if self.consume_if(TokenKind::KwExt) {
            attrs.push(Arc::new(Ast::ItemExt));
        }

        if !self.consume_if(TokenKind::KwSubmod) {
            self.consume(TokenKind::KwMod)?;
        }

        let name = self.consume_ident()?;
        self.consume(TokenKind::CurlyLeft)?;
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
            attrs,
        }))
    }

    fn parse_moddef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let ast = match self.peek() {
            TokenKind::KwIncoming => {
                self.take();
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
                self.take();
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
                self.take();
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
                self.take();
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
            TokenKind::KwMod => {
                self.take();
                let name = self.consume_ident()?;
                self.consume(TokenKind::KwOf)?;
                let moddef = self.parse_qualident()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Submod {
                    name,
                    moddef,
                }))
            },
            TokenKind::KwMaster | TokenKind::KwSlave => {
                let role = self.take();
                self.consume(TokenKind::KwSocket)?;
                let name = self.consume_ident()?;
                self.consume(TokenKind::KwOf)?;
                let socketdef = self.parse_qualident()?;
                self.consume(TokenKind::Semicolon)?;
                Ok(Arc::new(Ast::Socket {
                    role,
                    name,
                    socketdef,
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
            attrs: vec![],
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
            attrs: vec![],
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

        let width_attr = Arc::new(Ast::ItemWidth { width });

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::EnumDef,
            stmts,
            attrs: vec![width_attr],
        }))
    }

    fn parse_enumdef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let name = self.consume_ident()?;
        self.consume(TokenKind::Eq)?;
        let value = self.parse_expr_word()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::EnumerantDef(name, value)))
    }

    fn parse_socketdef(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwSocket)?;
        let name = self.consume_ident()?;
        self.consume(TokenKind::CurlyLeft)?;
        let mut stmts = vec![];
        while TokenKind::CurlyRight != self.peek() {
            let stmt = self.parse_socketdef_statement()?;
            stmts.push(stmt);
        }
        self.consume(TokenKind::CurlyRight)?;

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::SocketDef,
            attrs: vec![],
            stmts,
        }))
    }

    fn parse_socketdef_statement(&mut self) -> Result<Arc<Ast>, ParseError> {
        let dir = if self.peek() == TokenKind::KwMiso || self.peek() == TokenKind::KwMosi {
            self.take()
        } else {
            return Err(ParseError::Unknown(self.take()));
        };

        let name = self.consume_ident()?;
        self.consume(TokenKind::Colon)?;
        let typ = self.parse_type()?;
        self.consume(TokenKind::Semicolon)?;
        Ok(Arc::new(Ast::ChannelDef(dir, name, typ)))
    }

    fn parse_fndef(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::KwFn)?;
        let name = self.consume_ident()?;
        let params = self.parse_param_list()?;
        self.consume(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        self.consume(TokenKind::CurlyLeft)?;
        let body = self.parse_expr()?;
        self.consume(TokenKind::CurlyRight)?;

        let sig = Arc::new(Ast::ItemSig { params, ret });

        Ok(Arc::new(Ast::Item {
            name,
            kind: ItemKind::FnDef,
            attrs: vec![sig],
            stmts: vec![body],
        }))
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
            let ctor_name = self.consume_ident()?;
            Ok(Arc::new(Ast::PatAt(ctor_name, None)))
        } else if self.consume_if(TokenKind::KwElse) {
            Ok(Arc::new(Ast::PatElse))
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
        } else if self.consume_if(TokenKind::KwSext) {
            self.consume(TokenKind::ParenLeft)?;
            let expr = self.parse_expr()?;
            self.consume(TokenKind::ParenRight)?;
            Arc::new(Ast::Sext(expr))
        } else if self.consume_if(TokenKind::KwZext) {
            self.consume(TokenKind::ParenLeft)?;
            let expr = self.parse_expr()?;
            self.consume(TokenKind::ParenRight)?;
            Arc::new(Ast::Zext(expr))
        } else if self.peek() == TokenKind::Ident && 
            (self.peek2() == TokenKind::ColonColon || self.peek2() == TokenKind::ParenLeft) {
            self.parse_expr_fncall()?
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
        self.peek() == TokenKind::KwIf ||
        self.peek() == TokenKind::Ident
    }

    fn parse_expr_fncall(&mut self) -> Result<Arc<Ast>, ParseError> {
        let name = self.parse_qualident()?;
        let exprs = self.parse_expr_list()?;
        Ok(Arc::new(Ast::FnCall(name, exprs)))
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
        } else if self.peek() == TokenKind::Hash {
            self.parse_expr_enumerant()
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

    fn parse_expr_enumerant(&mut self) -> Result<Arc<Ast>, ParseError> {
        self.consume(TokenKind::Hash)?;
        let name = self.consume_ident()?;
        Ok(Arc::new(Ast::Enumerant(name)))
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
        } else if driver_type == TokenKind::ColonEq {
            self.consume(TokenKind::ColonEq)?
        } else {
            self.consume(TokenKind::ColonEqColon)?
        };

        if driver_type_token.kind() == TokenKind::RevFatArrow || driver_type_token.kind() == TokenKind::ColonEq {
            let driver = self.parse_expr()?;
            self.consume(TokenKind::Semicolon)?;
            Ok(Arc::new(Ast::Driver(target, driver_type_token, driver)))
        } else {
            let source = self.parse_path()?;
            self.consume(TokenKind::Semicolon)?;
            Ok(Arc::new(Ast::DriverSocket(target, source)))
        }
    }
}

pub type Ident = Token;
pub type Path = Vec<Token>;
pub type QualIdent = Vec<Token>;
pub type StaticIndex = Token;
pub type Width = Token;

#[derive(Debug, Clone)]
pub enum Ast {
    Error,
    Package { imports: Vec<Arc<Ast>>, items: Vec<Arc<Ast>> },
    Import(Ident),
    Item { kind: ItemKind, name: Ident, attrs: Vec<Arc<Ast>>, stmts: Vec<Arc<Ast>> },
    ItemWidth { width: Token },
    ItemSig { params: Vec<Arc<Ast>>, ret: Arc<Ast> },
    ItemExt,
    Type { name: QualIdent, args: Option<Vec<Arc<Ast>>> },
    TypeArgWidth { width: Width },
    Component { kind: ComponentKind, name: Ident, typ: Arc<Ast>, on: Option<Arc<Ast>> },
    Submod { name: Ident, moddef: QualIdent },
    Socket { role: Ident, name: Ident, socketdef: QualIdent },
    Driver(Path, Token, Arc<Ast>),
    DriverSocket(Path, Path),
    FieldDef(Ident, Arc<Ast>),
    CtorDef(Ident, Vec<Arc<Ast>>),
    EnumerantDef(Ident, Arc<Ast>),
    ChannelDef(Token, Ident, Arc<Ast>),
    Reference(Path),
    Lit(Token),
    Word(Vec<Arc<Ast>>),
    Bit(bool),
    UnOp(Ident, Arc<Ast>),
    BinOp(Arc<Ast>, Ident, Arc<Ast>),
    MethodCall(Arc<Ast>, Ident, Vec<Arc<Ast>>),
    Struct(Ident, Vec<Arc<Ast>>),
    Assign(Ident, Arc<Ast>),
    FnCall(QualIdent, Vec<Arc<Ast>>),
    Field(Arc<Ast>, Ident),
    Ctor(Ident, Vec<Arc<Ast>>),
    Param(Ident, Arc<Ast>),
    Enumerant(Ident),
    As(Arc<Ast>, Arc<Ast>),
    Idx(Arc<Ast>, StaticIndex),
    IdxRange(Arc<Ast>, StaticIndex, StaticIndex),
    Cat(Vec<Arc<Ast>>),
    Zext(Arc<Ast>),
    Sext(Arc<Ast>),
    If { subject: Arc<Ast>, true_branch: Arc<Ast>, false_branch: Arc<Ast> },
    Match { subject: Arc<Ast>, arms: Vec<Arc<Ast>> },
    MatchArm { pat: Arc<Ast>, expr: Arc<Ast> },
    PatBind(Token),
    PatAt(Token, Option<Vec<Arc<Ast>>>),
    PatElse,
}

impl Ast {
    pub fn children(&self) -> Vec<Arc<Ast>> {
        match self {
            Ast::Error => vec![],
            Ast::Package { imports, items } => {
                let mut results = imports.clone();
                results.extend(items.iter().cloned());
                results
            },
            Ast::Import(_) => vec![],
            Ast::Item { kind: _, name: _, attrs, stmts } => {
                let mut results = attrs.clone();
                results.extend(stmts.iter().cloned());
                results
            },
            Ast::ItemWidth { width: _ } => vec![],
            Ast::ItemSig { params, ret } => {
                let mut results = params.clone();
                results.push(ret.clone());
                results
            },
            Ast::ItemExt => vec![],
            Ast::Type { name: _, args } => {
                if let Some(args) = args {
                    args.clone()
                } else {
                    vec![]
                }
            },
            Ast::TypeArgWidth { width: _ } => vec![],
            Ast::Component { kind: _, name: _, typ, on } => {
                let mut results = vec![typ.clone()];
                if let Some(on) = on {
                    results.push(on.clone());
                }
                results
            },
            Ast::Submod { name: _, moddef: _ } => vec![],
            Ast::Socket { role: _, name: _, socketdef: _ } => vec![],
            Ast::Driver(_, _, expr) => vec![expr.clone()],
            Ast::DriverSocket(_, _) => vec![],
            Ast::FieldDef(_, typ) => vec![typ.clone()],
            Ast::CtorDef(_, params) => params.clone(),
            Ast::EnumerantDef(_, value) => vec![value.clone()],
            Ast::ChannelDef(_, _, typ) => vec![typ.clone()],
            Ast::Reference(_) => vec![],
            Ast::Lit(_) => vec![],
            Ast::Word(_) => vec![],
            Ast::Bit(_) => vec![],
            Ast::UnOp(_, subject) => vec![subject.clone()],
            Ast::BinOp(lhs, _, rhs) => vec![lhs.clone(), rhs.clone()],
            Ast::MethodCall(subject, _, args) => {
                let mut results = vec![subject.clone()];
                results.extend(args.iter().cloned());
                results
            },
            Ast::Struct(_, assigns) => assigns.clone(),
            Ast::Assign(_, expr) => vec![expr.clone()],
            Ast::FnCall(_, args) => args.clone(),
            Ast::Field(subject, _) => vec![subject.clone()],
            Ast::Ctor(_, args) => args.clone(),
            Ast::Param(_, typ) => vec![typ.clone()],
            Ast::Enumerant(_) => vec![],
            Ast::As(subject, typ) => vec![subject.clone(), typ.clone()],
            Ast::Idx(subject, _) => vec![subject.clone()],
            Ast::IdxRange(subject, _, _) => vec![subject.clone()],
            Ast::Cat(args) => args.clone(),
            Ast::Zext(subject) => vec![subject.clone()],
            Ast::Sext(subject) => vec![subject.clone()],
            Ast::If { subject, true_branch, false_branch } => vec![subject.clone(), true_branch.clone(), false_branch.clone()],
            Ast::Match { subject, arms } => {
                let mut results = vec![subject.clone()];
                results.extend(arms.iter().cloned());
                results
            },
            Ast::MatchArm { pat, expr } => vec![pat.clone(), expr.clone()],
            Ast::PatBind(_) => vec![],
            Ast::PatAt(_, subpats) => {
                if let Some(subpats) = subpats {
                    subpats.clone()
                } else {
                    vec![]
                }
            },
            Ast::PatElse => vec![],
        }
    }

    pub fn has_errors(&self) -> bool {
        if let Ast::Error = self {
            return true;
        } else {
            for child in self.children() {
                if child.has_errors() {
                    return true;
                }
            }
            false
        }
    }
}

#[derive(Debug, Clone)]
pub enum ComponentKind {
    Incoming,
    Outgoing,
    Reg,
    Wire,
}
