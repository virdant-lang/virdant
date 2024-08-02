use pest::iterators::Pair;

use crate::ItemKind;
use crate::parse::Rule;
use crate::location::*;

/// A node of the parse tree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ast<'a>(Pair<'a, Rule>);

impl<'a> Ast<'a> {
    pub(crate) fn new(pair: Pair<'a, Rule>) -> Self {
        Ast(pair)
    }

    fn rule(&self) -> Rule {
        self.pair().as_rule()
    }

    /// Get a child node with a given tag.
    pub fn get(&'a self, tag: &'a str) -> Option<Ast<'a>> {
        self.pair().clone().into_inner().find_first_tagged(tag).map(|pair| Ast(pair))
    }

    pub fn has(&'a self, tag: &'a str) -> bool {
        self.get(tag).is_some()
    }

    /// Get the underlying string for a child node with a given tag.
    pub fn get_as_str(&'a self, tag: &'a str) -> Option<&'a str> {
        self.pair().clone().into_inner().find_first_tagged(tag).map(|pair| pair.as_str())
    }

    /// Get the span in the source file for this node of the parse tree.
    pub fn span(&self) -> Span {
        let span = self.pair().as_span();
        let (start_line, start_col) = span.start_pos().line_col();
        let (end_line, end_col) = span.end_pos().line_col();
        Span::new(Pos::new(start_line, start_col), Pos::new(end_line, end_col))
    }

    /// Get the child nodes of this node in the parse tree.
    pub fn children<'b>(self: &'b Ast<'a>) -> impl Iterator<Item = Ast<'a>> where 'a: 'b {
        let inner = self.pair().clone().into_inner();
        inner
            .filter(|pair| pair.as_rule() != Rule::EOI)
            .map(move |pair| Ast(pair))
    }

    pub fn child(&self, i: usize) -> Ast {
        let inner = self.pair().clone().into_inner();
        inner
            .filter(|pair| pair.as_rule() != Rule::EOI)
            .map(|pair| Ast(pair))
            .nth(i)
            .unwrap()
    }

    /// Get the underlying string for this node of the parse tree.
    pub fn as_str(&self) -> &str {
        self.pair().as_str()
    }

    pub fn summary(&self) -> String {
        let text = self.pair().as_str();
        let lines: Vec<&str> = text.lines().collect();
        if lines.len() == 0 {
            format!("[{:?}]", self.rule())
        } else {
            let first_line = lines[0];
            let rule = self.rule();
            let truncated_line = if lines.len() == 1 {
                first_line.to_string()
            } else {
                format!("{first_line} ...")
            };

            format!("[{rule:?}] {truncated_line:?}")
        }
    }

    fn pair(&self) -> &Pair<'a, Rule> {
        &self.0
    }

    pub fn is_import(&self) -> bool { self.rule() == Rule::import }
    pub fn is_item(&self) -> bool { self.rule() == Rule::item }

    pub fn is_moddef(&self) -> bool { self.rule() == Rule::moddef }
    pub fn is_uniondef(&self) -> bool { self.rule() == Rule::uniondef }
    pub fn is_structdef(&self) -> bool { self.rule() == Rule::structdef }
    pub fn is_builtindef(&self) -> bool { self.rule() == Rule::builtindef }
    pub fn is_portdef(&self) -> bool { self.rule() == Rule::portdef }

    pub fn is_statement(&self) -> bool {
        self.rule() == Rule::moddef_statement ||
        self.rule() == Rule::uniondef_statement ||
        self.rule() == Rule::structdef_statement ||
        self.rule() == Rule::portdef_statement
    }

    pub fn is_submodule(&self) -> bool { self.rule() == Rule::moddef_statement_mod }
    pub fn is_driver(&self) -> bool { self.rule() == Rule::moddef_statement_driver }

    pub fn is_expr(&self) -> bool { self.rule() == Rule::expr }
    pub fn is_expr_if(&self) -> bool { self.rule() == Rule::expr_if }
    pub fn is_expr_match(&self) -> bool { self.rule() == Rule::expr_match }
    pub fn is_expr_call(&self) -> bool { self.rule() == Rule::expr_call }
    pub fn is_expr_base(&self) -> bool { self.rule() == Rule::expr_base }
    pub fn is_wordlit(&self) -> bool { self.rule() == Rule::wordlit }
    pub fn is_path(&self) -> bool { self.rule() == Rule::path }

    pub fn is_kw_if(&self) -> bool { self.rule() == Rule::kw_if }

    pub fn is_list(&self) -> bool {
        self.rule() == Rule::arg_list ||
        self.rule() == Rule::type_list ||
        self.rule() == Rule::expr_list ||
        self.rule() == Rule::pat_list
    }

    pub fn is_type(&self) -> bool { self.rule() == Rule::r#type }
    pub fn is_nat(&self) -> bool { self.rule() == Rule::nat  }

    pub fn package(&self) -> Option<&str> { self.get_as_str("package") }
    pub fn name(&self) -> Option<&str> { self.get_as_str("name") }
    pub fn of(&self) -> Option<&str> { self.get_as_str("of") }
    pub fn method(&self) -> Option<&str> { self.get_as_str("method") }

    pub fn typ(&self) -> Option<Ast> { self.get("type") }
    pub fn expr(&self) -> Option<Ast> { self.get("expr") }
    pub fn subject(&self) -> Option<Ast> { self.get("subject") }

    pub fn args(&self) -> Option<Vec<Ast>> { self.get("args").map(|args| args.children().collect()) }
    pub fn i(&self) -> Option<u64> { self.get("i").map(|ast| str::parse(ast.as_str()).unwrap()) }
    pub fn j(&self) -> Option<u64> { self.get("j").map(|ast| str::parse(ast.as_str()).unwrap()) }

    pub fn item_kind(&self) -> Option<ItemKind> {
        match self.child(0).rule() {
            Rule::moddef => Some(ItemKind::ModDef),
            Rule::uniondef => Some(ItemKind::UnionDef),
            Rule::structdef => Some(ItemKind::StructDef),
            Rule::builtindef => Some(ItemKind::BuiltinDef),
            Rule::portdef => Some(ItemKind::PortDef),
            _ => None,
        }
    }
}

