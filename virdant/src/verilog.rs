mod expr;

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
    pub name: String,
    pub on_expr: Expr,
    pub stmts: Vec<Stmt>,
}

pub struct Initial {
    pub name: String,
    pub stmts: Vec<Stmt>,
}

pub enum Stmt {
    AssignBlocking(AssignBlocking),
    AssignNonBlocking(AssignNonBlocking),

}

pub enum Expr {
    Reference(expr::Reference),
    BinOp(expr::BinOp),
    UnOp(expr::UnOp),
    BitLit(expr::BitLit),
    WordLit(expr::WordLit),
    Index(expr::Index),
    IndexRange(expr::IndexRange),
}

pub enum BinOp {
    Add,
}

pub enum UnOp {
    Neg,
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
    ($writer:expr, $fmt:literal $($arg:tt)*) => {{
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
    ($writer:expr, $fmt:literal $($arg:tt)*) => {{
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
            //            Element::Always(always) => always.write(f),
            //            Element::Initial(initial) => initial.write(f),
            _ => todo!(),
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

impl Expr {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        match self {
            Expr::Reference(reference) => write!(writer.file, "{}", reference.name)?,
            Expr::BinOp(expr_bin_op) => {
                write!(writer.file, "(")?;
                expr_bin_op.lhs.write(writer)?;
                match expr_bin_op.op {
                    BinOp::Add => write!(writer.file, " + ")?,
                }
                expr_bin_op.rhs.write(writer)?;
                write!(writer.file, ")")?;
            }
            Expr::UnOp(expr_un_op) => {
                match expr_un_op.op {
                    UnOp::Neg => write!(writer.file, "-")?,
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
