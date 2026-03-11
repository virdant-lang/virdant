mod expr;
mod stmt;

#[cfg(test)]
mod tests;

use crate::common::PortDir;
use crate::common::Radix;
use std::io::Write;

type Width = u64;

pub struct Verilog {
    pub files: Vec<VerilogFile>,
}

pub struct VerilogFile {
    pub name: String,
    pub modules: Vec<Module>,
}

pub struct Module {
    pub name: String,
    pub ports: Vec<Port>,
    pub elements: Vec<Element>,
}

pub struct Port {
    pub name: String,
    pub dir: PortDir,
    pub width: Width,
}

pub enum Element {
    Wire(Wire),
    Reg(Reg),
    Assign(Assign),
    Always(Always),
    Initial(Initial),
}

pub struct Wire {
    pub name: String,
    pub width: Width,
    pub expr: Option<Expr>,
}

pub struct Reg {
    pub name: String,
    pub width: Width,
    pub expr: Option<Expr>,
}

pub struct Assign {
    pub name: String,
    pub expr: Expr,
}

pub struct Always {
    pub clock: Option<String>,
    pub stmts: Vec<Stmt>,
}

pub struct Initial {
    pub stmts: Vec<Stmt>,
}

pub enum Stmt {
    AssignBlocking(stmt::AssignBlocking),
    AssignNonBlocking(stmt::AssignNonBlocking),
    Display(stmt::Display),
    Assert(stmt::Assert),
    Fatal,
    Finish,
    Case(stmt::Case),
    CaseZ(stmt::CaseZ),
}

pub enum Expr {
    Reference(expr::Reference),
    BinOp(expr::BinOp),
    UnOp(expr::UnOp),
    BitLit(expr::BitLit),
    WordLit(expr::WordLit),
    StrLit(expr::StrLit),
    If(expr::If),
    Concat(expr::Concat),
    Repeat(expr::Repeat),
    Index(expr::Index),
    IndexRange(expr::IndexRange),
}

pub enum BinOp {
    Pow,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    AShl,
    AShr,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Ne,
    CaseEq,
    CaseNe,
    BitAnd,
    BitXor,
    BitXnor,
    BitOr,
    LogAnd,
    LogOr,
}

pub enum UnOp {
    Pos,
    Neg,
    LogNot,
    BitNot,
    RedAnd,
    RedNand,
    RedOr,
    RedNor,
    RedXor,
    RedXnor,
}

struct Writer<'f> {
    file: &'f mut dyn Write,
    indent: u16,
    skip_indent: bool,
}

impl<'f> Writer<'f> {
    fn new(file: &'f mut dyn Write) -> Writer<'f> {
        Writer {
            file,
            indent: 0,
            skip_indent: false,
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent -= 1;
    }

    fn skip_indent(&mut self) {
        // disabled later by a call to verilog_write
        self.skip_indent = true;
    }
}

macro_rules! verilog_write {
    ($writer:expr, $fmt:literal) => {{
        verilog_write!($writer, $fmt,)
    }};
    ($writer:expr, $fmt:literal, $($arg:expr),*) => {{
        if $writer.skip_indent {
           $writer.skip_indent = false;
        } else {
            let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        write!($writer.file, $fmt, $($arg)*)
    }};
}

macro_rules! verilog_writeln {
    ($writer:expr, $fmt:literal) => {{
        verilog_writeln!($writer, $fmt,)
    }};
    ($writer:expr, $fmt:literal, $($arg:expr),*) => {{
        if $writer.skip_indent {
           $writer.skip_indent = false;
        } else {
            let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        writeln!($writer.file, $fmt, $($arg)*)
    }};
    ($writer:expr) => {{
        if $writer.skip_indent {
           $writer.skip_indent = false;
        } else {
            let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
            write!($writer.file, "{indentation}")?;
        }
        writeln!($writer.file)
    }};
}

impl Verilog {
    pub fn validate(&self) -> Result<(), ()> {
        // TODO check all verilog names are valid file paths
        // TODO check all verilog names are all unique
        for verilog_file in &self.files {
            verilog_file.validate()?;
        }
        Ok(())
    }

    pub fn write_in_dir(&self, dir: &std::path::Path) -> std::io::Result<()> {
        self.validate().unwrap();
        for verilog_file in &self.files {
            let filepath = dir.join(verilog_file.name.clone());
            let mut file = std::fs::File::create(filepath)?;
            let mut writer = Writer::new(&mut file);
            verilog_file.write_to_file(&mut writer)?;
        }
        Ok(())
    }

    pub fn write_to_stdout(&self) -> std::io::Result<()> {
        self.validate().unwrap();
        for verilog_file in &self.files {
            let stdout = &mut std::io::stdout();
            let mut writer = Writer::new(stdout);
            writeln!(writer.file, "// {}", &verilog_file.name)?;
            verilog_file.write_to_file(&mut writer)?;
        }
        Ok(())
    }
}

impl VerilogFile {
    pub fn validate(&self) -> Result<(), ()> {
        for module in &self.modules {
            module.validate()?;
        }
        Ok(())
    }

    fn write_to_file(&self, writer: &mut Writer) -> std::io::Result<()> {
        for module in &self.modules {
            module.write(writer)?;
        }
        Ok(())
    }
}

impl Module {
    pub fn validate(&self) -> Result<(), ()> {
        // TODO check all port names are unique
        // TODO check that all module names are unique
        for element in &self.elements {
            element.validate()?;
        }
        Ok(())
    }

    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        let name = &self.name;
        verilog_writeln!(writer, "module {name}(")?;
        writer.indent();
        for (i, port) in self.ports.iter().enumerate() {
            let is_last = i + 1 == self.ports.len();
            port.write(writer, is_last)?;
        }
        writer.dedent();
        verilog_writeln!(writer, ");")?;

        writer.indent();
        for element in &self.elements {
            element.write(writer)?;
        }
        writer.dedent();

        verilog_writeln!(writer, "endmodule")?;
        verilog_writeln!(writer)?;
        Ok(())
    }
}

impl Port {
    fn write(&self, f: &mut Writer, is_last: bool) -> std::io::Result<()> {
        let dir = match self.dir {
            PortDir::Input => "input ",
            PortDir::Output => "output",
        };
        let width = if self.width == 1 {
            format!("")
        } else {
            let top = self.width - 1;
            format!("[{top}:0] ")
        };
        let name = &self.name;
        let comma = if is_last { "" } else { "," };
        verilog_writeln!(f, "{dir} {width:>18} {name}{comma}")?;
        Ok(())
    }
}

impl Element {
    pub fn validate(&self) -> Result<(), ()> {
        // TODO check no names conflict with verilog keywords
        // TODO check no duplicate names
        Ok(())
    }

    fn write(&self, f: &mut Writer) -> std::io::Result<()> {
        match self {
            Element::Wire(wire) => wire.write(f),
            Element::Reg(reg) => reg.write(f),
            Element::Assign(assign) => assign.write(f),
            Element::Always(always) => always.write(f),
            Element::Initial(initial) => initial.write(f),
        }
    }
}

impl Wire {
    fn write(&self, f: &mut Writer) -> std::io::Result<()> {
        let name = &self.name;
        let width_padding_len = 10;
        if self.width < 2 {
            let padding = " ".repeat(width_padding_len);
            verilog_write!(f, "wire {padding} {name}")?;
        } else {
            let top = self.width - 1;
            let width_str = format!("[{top}:0]");
            assert!(width_padding_len >= width_str.len());
            let padding = " ".repeat(width_padding_len - width_str.len());
            verilog_write!(f, "wire {padding}{width_str} {name}")?;
        }

        if let Some(expr) = &self.expr {
            f.skip_indent();
            verilog_write!(f, " = ")?;
            f.skip_indent();
            expr.write(f)?;
        }
        f.skip_indent();
        verilog_writeln!(f, ";")?;
        Ok(())
    }
}

impl Reg {
    fn write(&self, f: &mut Writer) -> std::io::Result<()> {
        let name = &self.name;
        let width_padding_len = 10;
        if self.width < 2 {
            let padding = " ".repeat(width_padding_len);
            verilog_write!(f, "reg  {padding} {name}")?;
        } else {
            let top = self.width - 1;
            let width_str = format!("[{top}:0]");
            assert!(width_padding_len >= width_str.len());
            let padding = " ".repeat(width_padding_len - width_str.len());
            verilog_write!(f, "reg  {padding}{width_str} {name}")?;
        }

        if let Some(expr) = &self.expr {
            f.skip_indent();
            verilog_write!(f, " = ")?;
            f.skip_indent();
            expr.write(f)?;
        }
        f.skip_indent();
        verilog_writeln!(f, ";")?;
        Ok(())
    }
}

impl Assign {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        let name = &self.name;
        verilog_write!(writer, "assign {name} = ")?;
        self.expr.write(writer)?;
        writeln!(writer.file, ";")?;
        Ok(())
    }
}

impl Always {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        if let Some(clock) = &self.clock {
            verilog_writeln!(writer, "always @(posedge {clock}) begin")?;
        } else {
            verilog_writeln!(writer, "always @(*) begin")?;
        }
        writer.indent();
        for stmt in &self.stmts {
            stmt.write(writer)?;
        }
        writer.dedent();
        verilog_writeln!(writer, "end")?;
        Ok(())
    }
}

impl Initial {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        verilog_writeln!(writer, "initial begin")?;
        writer.indent();
        for stmt in &self.stmts {
            stmt.write(writer)?;
        }
        writer.dedent();
        verilog_writeln!(writer, "end")?;
        Ok(())
    }
}

impl Stmt {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
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
                for expr in &display.exprs {
                    writer.skip_indent();
                    expr.write(writer)?;
                }
                writer.skip_indent();
                verilog_writeln!(writer, ");")?;
            }
            Stmt::Assert(assert) => {
                verilog_write!(writer, "$assert(")?;
                for expr in &assert.exprs {
                    writer.skip_indent();
                    expr.write(writer)?;
                }
                writer.skip_indent();
                verilog_writeln!(writer, ");")?;
            }
            Stmt::Fatal => {
                verilog_writeln!(writer, "$fatal;")?;
            }
            Stmt::Finish => {
                verilog_writeln!(writer, "$finish;")?;
            }
            Stmt::Case(case) => case.write(writer)?,
            Stmt::CaseZ(casez) => casez.write(writer)?,
        }
        Ok(())
    }
}

impl stmt::Case {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        verilog_write!(writer, "case (")?;
        self.subject.write(writer)?;
        writer.skip_indent();
        verilog_writeln!(writer, ")")?;
        writer.indent();
        for item in &self.items {
            item.write(writer)?;
        }
        writer.dedent();
        verilog_writeln!(writer, "endcase")?;
        Ok(())
    }
}

impl stmt::CaseZ {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        verilog_write!(writer, "casez (")?;
        self.subject.write(writer)?;
        writer.skip_indent();
        verilog_writeln!(writer, ")")?;
        writer.indent();
        for item in &self.items {
            item.write(writer)?;
        }
        writer.dedent();
        verilog_writeln!(writer, "endcase")?;
        Ok(())
    }
}

impl stmt::CaseItem {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        match &self.pattern {
            stmt::CasePattern::Default => verilog_write!(writer, "default")?,
            pattern => {
                verilog_write!(writer, "")?;
                pattern.write(writer)?;
            }
        }

        writer.skip_indent();
        verilog_writeln!(writer, ": begin")?;
        writer.indent();
        for stmt in &self.stmts {
            stmt.write(writer)?;
        }
        writer.dedent();
        verilog_writeln!(writer, "end")?;
        Ok(())
    }
}

impl stmt::CasePattern {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        match self {
            stmt::CasePattern::Expr(expr) => expr.write(writer)?,
            stmt::CasePattern::PatternLit(pattern_lit) => pattern_lit.write(writer)?,
            stmt::CasePattern::Default => write!(writer.file, "default")?,
        }
        Ok(())
    }
}

impl stmt::PatternLit {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        let width = self.width;
        let pattern = &self.pattern;
        match self.radix {
            Radix::Dec => write!(writer.file, "{width}'d{pattern}")?,
            Radix::Hex => write!(writer.file, "{width}'h{pattern}")?,
            Radix::Bin => write!(writer.file, "{width}'b{pattern}")?,
        }
        Ok(())
    }
}

impl Expr {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        match self {
            Expr::Reference(reference) => write!(writer.file, "{}", reference.name)?,
            Expr::BinOp(expr_bin_op) => {
                write!(writer.file, "(")?;
                expr_bin_op.lhs.write(writer)?;
                match expr_bin_op.op {
                    BinOp::Pow => write!(writer.file, " ** ")?,
                    BinOp::Mul => write!(writer.file, " * ")?,
                    BinOp::Div => write!(writer.file, " / ")?,
                    BinOp::Mod => write!(writer.file, " % ")?,
                    BinOp::Add => write!(writer.file, " + ")?,
                    BinOp::Sub => write!(writer.file, " - ")?,
                    BinOp::Shl => write!(writer.file, " << ")?,
                    BinOp::Shr => write!(writer.file, " >> ")?,
                    BinOp::AShl => write!(writer.file, " <<< ")?,
                    BinOp::AShr => write!(writer.file, " >>> ")?,
                    BinOp::Lt => write!(writer.file, " < ")?,
                    BinOp::Lte => write!(writer.file, " <= ")?,
                    BinOp::Gt => write!(writer.file, " > ")?,
                    BinOp::Gte => write!(writer.file, " >= ")?,
                    BinOp::Eq => write!(writer.file, " == ")?,
                    BinOp::Ne => write!(writer.file, " != ")?,
                    BinOp::CaseEq => write!(writer.file, " === ")?,
                    BinOp::CaseNe => write!(writer.file, " !== ")?,
                    BinOp::BitAnd => write!(writer.file, " & ")?,
                    BinOp::BitXor => write!(writer.file, " ^ ")?,
                    BinOp::BitXnor => write!(writer.file, " ^~ ")?,
                    BinOp::BitOr => write!(writer.file, " | ")?,
                    BinOp::LogAnd => write!(writer.file, " && ")?,
                    BinOp::LogOr => write!(writer.file, " || ")?,
                }
                expr_bin_op.rhs.write(writer)?;
                write!(writer.file, ")")?;
            }
            Expr::UnOp(expr_un_op) => {
                match expr_un_op.op {
                    UnOp::Pos => write!(writer.file, "+")?,
                    UnOp::Neg => write!(writer.file, "-")?,
                    UnOp::LogNot => write!(writer.file, "!")?,
                    UnOp::BitNot => write!(writer.file, "~")?,
                    UnOp::RedAnd => write!(writer.file, "&")?,
                    UnOp::RedNand => write!(writer.file, "~&")?,
                    UnOp::RedOr => write!(writer.file, "|")?,
                    UnOp::RedNor => write!(writer.file, "~|")?,
                    UnOp::RedXor => write!(writer.file, "^")?,
                    UnOp::RedXnor => write!(writer.file, "~^")?,
                }
                write!(writer.file, "(")?;
                expr_un_op.expr.write(writer)?;
                write!(writer.file, ")")?;
            }
            Expr::BitLit(expr_bit_lit) => {
                if expr_bit_lit.value {
                    write!(writer.file, "1'b1")?
                } else {
                    write!(writer.file, "1'b0")?
                }
            }
            Expr::WordLit(expr_word_lit) => {
                let width = expr_word_lit.width;
                let value = expr_word_lit.value;
                match expr_word_lit.radix {
                    Radix::Dec => write!(writer.file, "{width}'d{value}")?,
                    Radix::Hex => write!(writer.file, "{width}'h{value:x}")?,
                    Radix::Bin => write!(writer.file, "{width}'b{value:b}")?,
                }
            }
            Expr::StrLit(expr_str_lit) => {
                let value = str_escape(&expr_str_lit.value);
                write!(writer.file, "{value}")?;
            }
            Expr::If(expr_if) => {
                write!(writer.file, "(")?;
                expr_if.cond.write(writer)?;
                write!(writer.file, " ? ")?;
                expr_if.then_expr.write(writer)?;
                write!(writer.file, " : ")?;
                expr_if.else_expr.write(writer)?;
                write!(writer.file, ")")?;
            }
            Expr::Concat(concat) => {
                write!(writer.file, "{{")?;
                for (i, expr) in concat.exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer.file, ", ")?;
                    }
                    expr.write(writer)?;
                }
                write!(writer.file, "}}")?;
            }
            Expr::Repeat(repeat) => {
                write!(writer.file, "{{")?;
                repeat.count.write(writer)?;
                write!(writer.file, "{{")?;
                for (i, expr) in repeat.exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer.file, ", ")?;
                    }
                    expr.write(writer)?;
                }
                write!(writer.file, "}}}}")?;
            }
            Expr::Index(index) => {
                index.subject.write(writer)?;
                write!(writer.file, "[")?;
                index.index.write(writer)?;
                write!(writer.file, "]")?;
            }
            Expr::IndexRange(index) => {
                index.subject.write(writer)?;
                write!(writer.file, "[")?;
                index.index_hi.write(writer)?;
                write!(writer.file, ":")?;
                index.index_lo.write(writer)?;
                write!(writer.file, "]")?;
            }
        }
        Ok(())
    }
}

fn str_escape(s: &str) -> String {
    // FIXME This should handle Verilog style escape strings
    // and should escape all quotes, etc.
    format!("\"{s}\"")
}
