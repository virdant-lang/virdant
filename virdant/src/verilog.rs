use crate::common::PortDir;
use crate::common::Radix;
use std::io::Write;

pub struct Verilog {
    pub files: Vec<File>,
}

pub struct File {
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
    pub width: u64,
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
}

pub struct Reg {
    pub name: String,
}

pub struct Assign {
    pub name: String,
    pub expr: Expr,
}

pub struct Always {
    pub name: String,
    pub on_expr: Expr,
    pub expr: Expr,
}

pub struct Initial {
    pub name: String,
    pub expr: Expr,
}

pub enum Expr {
    Reference(expr::Reference),
    BinOp(expr::BinOp),
    UnOp(expr::UnOp),
    BitLit(expr::BitLit),
    WordLit(expr::WordLit),
}

mod expr {
    use super::*;

    pub struct Reference {
        pub name: String,
    }

    pub struct BinOp {
        pub op: super::BinOp,
        pub lhs: Box<Expr>,
        pub rhs: Box<Expr>,
    }

    pub struct UnOp {
        pub op: super::UnOp,
        pub expr: Box<Expr>,
    }

    pub struct BitLit {
        pub value: bool,
    }

    pub struct WordLit {
        pub value: u128, // TODO
        pub width: u16,
        pub radix: Radix,
    }
}

pub enum BinOp {
    Add,
}

pub enum UnOp {
    Neg,
}

struct Writer<'f> {
    file: &'f mut std::fs::File,
    indent: u16,
}

impl<'f> Writer<'f> {
    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent -= 1;
    }
}

macro_rules! verilog_write {
    ($writer:expr, $fmt:literal $($arg:tt)*) => {{
        let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
        write!($writer.file, "{indentation}")?;
        write!($writer.file, $fmt, $($arg)*)
    }};
}

macro_rules! verilog_writeln {
    ($writer:expr, $fmt:literal $($arg:tt)*) => {{
        let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
        write!($writer.file, "{indentation}")?;
        writeln!($writer.file, $fmt, $($arg)*)
    }};
    ($writer:expr) => {{
        let indentation = str::repeat(" ", ($writer.indent * 4) as usize);
        write!($writer.file, "{indentation}")?;
        writeln!($writer.file)
    }};
}

impl Verilog {
    pub fn write(&self, dir: &std::path::Path) -> std::io::Result<()> {
        for file in &self.files {
            file.write(dir)?;
        }
        Ok(())
    }
}

impl File {
    fn write(&self, dir: &std::path::Path) -> std::io::Result<()> {
        let filepath = dir.join(self.name.clone());
        let mut file = std::fs::File::create(filepath)?;
        let mut writer = Writer {
            file: &mut file,
            indent: 0,
        };
        for module in &self.modules {
            module.write(&mut writer)?;
        }
        Ok(())
    }
}

impl Module {
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
        verilog_writeln!(f, "wire {name};")?;
        verilog_writeln!(f)?;
        Ok(())
    }
}

impl Reg {
    fn write(&self, f: &mut Writer) -> std::io::Result<()> {
        let name = &self.name;
        verilog_writeln!(f, "reg {name};")?;
        verilog_writeln!(f)?;
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
        write!(writer.file, "(")?;
        match self {
            Expr::Reference(reference) => write!(writer.file, "{}", reference.name)?,
            Expr::BinOp(expr_bin_op) => {
                expr_bin_op.lhs.write(writer)?;
                match expr_bin_op.op {
                    BinOp::Add => write!(writer.file, " + ")?,
                }
                expr_bin_op.rhs.write(writer)?;
            }
            Expr::UnOp(expr_un_op) => {
                match expr_un_op.op {
                    UnOp::Neg => write!(writer.file, "-")?,
                }
                expr_un_op.expr.write(writer)?;
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
        }
        write!(writer.file, ")")?;
        Ok(())
    }
}

#[test]
fn test_verilog() {
    let verilog = Verilog {
        files: vec![File {
            name: "foo.v".to_string(),
            modules: vec![Module {
                name: "Top".to_string(),
                ports: vec![
                    Port {
                        name: "inp".to_string(),
                        dir: PortDir::Input,
                        width: 8,
                    },
                    Port {
                        name: "out".to_string(),
                        dir: PortDir::Output,
                        width: 1,
                    },
                ],
                elements: vec![Element::Assign(Assign {
                    name: "out".to_string(),
                    expr: Expr::BinOp(expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::Reference(expr::Reference {
                            name: "inp".to_string(),
                        })),
                        rhs: Box::new(Expr::WordLit(expr::WordLit {
                            value: 4095,
                            width: 13,
                            radix: Radix::Hex,
                        })),
                    }),
                })],
            }],
        }],
    };

    std::fs::create_dir_all("build").unwrap();
    verilog.write(&std::path::PathBuf::from("build")).unwrap();
}
