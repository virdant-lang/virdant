use bstr::BString;

use super::macros::{verilog_write, verilog_writeln};

use crate::verilog::Writer;
use super::Expr;

use crate::common::{Radix, Width};

#[derive(Debug)]
pub enum Stmt {
    AssignBlocking(AssignBlocking),
    AssignNonBlocking(AssignNonBlocking),
    Display(Display),
    Assert(Assert),
    Fatal,
    Finish,
    Case(Case),
    CaseZ(CaseZ),
    If(If),
}

impl Stmt {
    pub(super) fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        match self {
            Stmt::AssignBlocking(assign_blocking) => {
                verilog_write!(writer, "{} = ", &assign_blocking.name)?;
                writer.skip_indent();
                assign_blocking.expr.write(writer)?;
                writer.skip_indent();
                verilog_writeln!(writer, ";")?;
            }
            Stmt::AssignNonBlocking(assign_non_blocking) => {
                verilog_write!(writer, "{} <= ", &assign_non_blocking.name)?;
                writer.skip_indent();
                assign_non_blocking.expr.write(writer)?;
                writer.skip_indent();
                verilog_writeln!(writer, ";")?;
            }
            Stmt::Display(display) => {
                verilog_write!(writer, "$display(")?;
                verilog_write!(writer, "{}, ", &display.message)?;
                for expr in &display.exprs {
                    writer.skip_indent();
                    expr.write(writer)?;
                }
                writer.skip_indent();
                verilog_writeln!(writer, ");")?;
            }
            Stmt::Assert(assert) => {
                let [expr] = assert.exprs.as_slice() else {
                    panic!("verilog assert expects exactly one expression");
                };
                verilog_write!(writer, "if (!(")?;
                writer.skip_indent();
                expr.write(writer)?;
                writer.skip_indent();
                verilog_writeln!(writer, ")) begin")?;
                writer.indent();
                verilog_writeln!(writer, "$fatal;")?;
                writer.dedent();
                verilog_writeln!(writer, "end")?;
            }
            Stmt::Fatal => {
                verilog_writeln!(writer, "$fatal;")?;
            }
            Stmt::Finish => {
                verilog_writeln!(writer, "$finish;")?;
            }
            Stmt::Case(case) => case.write(writer)?,
            Stmt::CaseZ(casez) => casez.write(writer)?,
            Stmt::If(if_stmt) => {
                verilog_write!(writer, "if (")?;
                writer.skip_indent();
                if_stmt.cond.write(writer)?;
                writer.skip_indent();
                verilog_writeln!(writer, ") begin")?;
                writer.indent();
                for stmt in &if_stmt.stmts {
                    stmt.write(writer)?;
                }
                writer.dedent();
                verilog_writeln!(writer, "end")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct If {
    pub cond: Expr,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct AssignBlocking {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct AssignNonBlocking {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Display {
    pub message: BString,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Assert {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Case {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

#[derive(Debug)]
pub struct CaseZ {
    pub subject: Expr,
    pub items: Vec<CaseItem>,
}

#[derive(Debug)]
pub struct CaseItem {
    pub pattern: CasePattern,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum CasePattern {
    Expr(Expr),
    PatternLit(PatternLit),
    Default,
}

#[derive(Debug)]
pub struct PatternLit {
    pub width: Width,
    pub radix: Radix,
    pub pattern: String,
}
