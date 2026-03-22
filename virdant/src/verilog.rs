pub mod expr;
mod stmt;
mod macros;

#[cfg(test)]
mod tests;

use crate::common::{PortDir, Radix, Width};

use self::macros::{verilog_write, verilog_writeln};
pub use self::stmt::{Assert, AssignBlocking, AssignNonBlocking, Display, Stmt};

use std::collections::HashSet;
use std::io::Write;

const DIR_WIDTH: usize = 6;
const KIND_WIDTH: usize = 4;
const WIDTH_WIDTH: usize = 10;

#[derive(Debug)]
pub enum PortKind {
    Wire,
    Reg,
}

#[derive(Debug)]
pub struct Verilog {
    pub files: Vec<VerilogFile>,
}

#[derive(Debug)]
pub struct VerilogFile {
    pub name: String,
    pub modules: Vec<Module>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub is_ext: bool,
    pub ports: Vec<Port>,
    pub elements: Vec<Element>,
}

#[derive(Debug)]
pub struct Port {
    pub name: String,
    pub kind: PortKind,
    pub dir: PortDir,
    pub width: Width,
}

#[derive(Debug)]
pub enum Element {
    Wire(Wire),
    Reg(Reg),
    Assign(Assign),
    Always(Always),
    Initial(Initial),
    Submodule(Submodule),
}

#[derive(Debug)]
pub struct Wire {
    pub name: String,
    pub width: Width,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct Reg {
    pub name: String,
    pub width: Width,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct Assign {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Always {
    pub clock: Option<Expr>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Initial {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Submodule {
    pub name: String,
    pub submodule_name: String,
    pub ports: Vec<String>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
            if module.is_ext {
                continue;
            }
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
            PortDir::Input => "input",
            PortDir::Output => "output",
        };
        let kind = match self.kind {
            PortKind::Wire => "wire",
            PortKind::Reg => "reg",
        };
        let comma = if is_last { "" } else { "," };
        let width = display_width(self.width);
        let name = &self.name;
        verilog_writeln!(
            f,
            "{dir:<DIR_WIDTH$} {kind:<KIND_WIDTH$} {width:>WIDTH_WIDTH$}  {name}{comma}"
        )?;

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
            Element::Submodule(submodule) => submodule.write(f),
        }
    }
}

impl Wire {
    fn write(&self, f: &mut Writer) -> std::io::Result<()> {
        let kind = "wire";
        let name = &self.name;
        let width = display_width(self.width);
        verilog_write!(f, "{kind:<KIND_WIDTH$} {width:>WIDTH_WIDTH$}  {name}")?;

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
        let kind = "reg";
        let name = &self.name;
        let width = display_width(self.width);
        verilog_write!(f, "{kind:<KIND_WIDTH$} {width:>WIDTH_WIDTH$}  {name}")?;

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

fn display_width(width: Width) -> String {
    if width < 2 {
        String::new()
    } else {
        let top = width - 1;
        format!("[{top}:0]")
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
            verilog_write!(writer, "always @(posedge ")?;
            clock.write(writer)?;
            writer.skip_indent();
            verilog_writeln!(writer, ") begin")?;
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

impl Submodule {
    fn write(&self, writer: &mut Writer) -> std::io::Result<()> {
        let submodule_name = &self.submodule_name;
        let name = &self.name;

        if self.ports.is_empty() {
            verilog_writeln!(writer, "{submodule_name} {name}();")?;
            return Ok(());
        }

        verilog_writeln!(writer, "{submodule_name} {name}(")?;
        writer.indent();
        let port_width = self.ports.iter().map(|p| p.len()).max().unwrap_or(0);
        for (i, port) in self.ports.iter().enumerate() {
            let comma = if i + 1 == self.ports.len() { "" } else { "," };
            let wire_name = valid_verilog_name(&format!("{}.{}", self.name, port));
            verilog_writeln!(writer, ".{port:<port_width$}({wire_name}){comma}")?;
        }
        writer.dedent();
        verilog_writeln!(writer, ");")?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Normalization pass: lift Concat/Repeat sub-expressions into SSA temp wires
// so that the emitted Verilog is accepted by iverilog.
//
// Problematic patterns:
//   1. A `Repeat` appearing directly inside a `Concat` — the nested parens
//      confuse iverilog's parser.
//   2. A `Concat` or `Repeat` used as the subject of `Index` / `IndexRange` —
//      iverilog only permits bit-selects on plain identifiers.
//
// Temp-wire names use the prefix `__vir_tmp_` followed by a counter.  The
// counter is incremented until an unused name is found, guaranteeing that
// generated names never shadow any user-defined identifier from the VirIR.
// ---------------------------------------------------------------------------

/// Returns the known bit-width of an expression, or `None` when it cannot be
/// determined without external type information.
fn compute_expr_width(expr: &Expr) -> Option<Width> {
    match expr {
        Expr::BitLit(_) => Some(1),
        Expr::WordLit(w) => Some(w.width),
        Expr::Concat(c) if c.width > 0 => Some(c.width),
        Expr::Repeat(r) if r.width > 0 => Some(r.width),
        Expr::Index(_) => Some(1),
        _ => None,
    }
}

struct Normalizer {
    used_names: HashSet<String>,
    counter: usize,
    new_elements: Vec<Element>,
}

impl Normalizer {
    fn fresh_name(&mut self, hint: &str) -> String {
        loop {
            let name = format!("\\temp${hint}_{} ", self.counter);
            self.counter += 1;
            if !self.used_names.contains(&name) {
                self.used_names.insert(name.clone());
                return name;
            }
        }
    }

    /// Replace `expr` with a reference to a fresh temp wire, and record the
    /// wire declaration + continuous assignment for later insertion.
    fn extract_to_tmp(&mut self, expr: &mut Expr, width: Width, hint: &str) {
        let name = self.fresh_name(hint);
        let old = std::mem::replace(
            expr,
            Expr::Reference(expr::Reference { name: name.clone() }),
        );
        self.new_elements.push(Element::Wire(Wire { name: name.clone(), width, expr: None }));
        self.new_elements.push(Element::Assign(Assign { name, expr: old }));
    }

    fn normalize_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::BinOp(b) => {
                self.normalize_expr(b.lhs.as_mut());
                self.normalize_expr(b.rhs.as_mut());
            }
            Expr::UnOp(u) => {
                self.normalize_expr(u.expr.as_mut());
            }
            Expr::If(i) => {
                self.normalize_expr(i.cond.as_mut());
                self.normalize_expr(i.then_expr.as_mut());
                self.normalize_expr(i.else_expr.as_mut());
            }
            Expr::Concat(c) => {
                for child in c.exprs.iter_mut() {
                    self.normalize_expr(child);
                }
                // Extract any Repeat children so they aren't nested inside {}.
                for child in c.exprs.iter_mut() {
                    let width = if matches!(child, Expr::Repeat(_)) {
                        compute_expr_width(child)
                    } else {
                        None
                    };
                    if let Some(w) = width {
                        self.extract_to_tmp(child, w, "word");
                    }
                }
            }
            Expr::Repeat(r) => {
                self.normalize_expr(r.count.as_mut());
                for child in r.exprs.iter_mut() {
                    self.normalize_expr(child);
                }
            }
            Expr::Index(i) => {
                self.normalize_expr(i.subject.as_mut());
                self.normalize_expr(i.index.as_mut());
                // Only a plain identifier (Reference) is a valid bit-select
                // subject in iverilog.  Anything else — Concat, Repeat,
                // literals, or complex expressions — must be lifted into a wire.
                let subject_width = if !matches!(i.subject.as_ref(), Expr::Reference(_)) {
                    compute_expr_width(i.subject.as_ref())
                } else {
                    None
                };
                if let Some(w) = subject_width {
                    self.extract_to_tmp(i.subject.as_mut(), w, "index");
                }
            }
            Expr::IndexRange(i) => {
                self.normalize_expr(i.subject.as_mut());
                self.normalize_expr(i.index_hi.as_mut());
                self.normalize_expr(i.index_lo.as_mut());
                let subject_width = if !matches!(i.subject.as_ref(), Expr::Reference(_)) {
                    compute_expr_width(i.subject.as_ref())
                } else {
                    None
                };
                if let Some(w) = subject_width {
                    self.extract_to_tmp(i.subject.as_mut(), w, "indexrange");
                }
            }
            // Leaf nodes — nothing to do.
            Expr::Reference(_) | Expr::BitLit(_) | Expr::WordLit(_) | Expr::StrLit(_) => {}
        }
    }

    fn normalize_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::AssignBlocking(a) => self.normalize_expr(&mut a.expr),
            Stmt::AssignNonBlocking(a) => self.normalize_expr(&mut a.expr),
            Stmt::Display(d) => {
                for e in d.exprs.iter_mut() {
                    self.normalize_expr(e);
                }
            }
            Stmt::Assert(a) => {
                for e in a.exprs.iter_mut() {
                    self.normalize_expr(e);
                }
            }
            Stmt::Case(c) => {
                self.normalize_expr(&mut c.subject);
                for item in c.items.iter_mut() {
                    for s in item.stmts.iter_mut() {
                        self.normalize_stmt(s);
                    }
                }
            }
            Stmt::CaseZ(c) => {
                self.normalize_expr(&mut c.subject);
                for item in c.items.iter_mut() {
                    for s in item.stmts.iter_mut() {
                        self.normalize_stmt(s);
                    }
                }
            }
            Stmt::Fatal | Stmt::Finish => {}
        }
    }
}

impl Verilog {
    /// Normalize every module in every file in place.
    pub fn normalize(&mut self) {
        for file in self.files.iter_mut() {
            file.normalize();
        }
    }
}

impl VerilogFile {
    /// Normalize every module in this file in place.
    pub fn normalize(&mut self) {
        for module in self.modules.iter_mut() {
            module.normalize();
        }
    }
}

impl Module {
    /// Lift `Concat`/`Repeat` sub-expressions that cannot be used directly in
    /// certain Verilog contexts (as bit-select subjects or nested inside a
    /// concatenation) into fresh `wire` + `assign` SSA temporaries.
    pub fn normalize(&mut self) {
        // Seed the used-name set with every declared port, wire, and reg so
        // that generated names are guaranteed never to collide.
        let used_names: HashSet<String> = self.ports.iter()
            .map(|p| p.name.clone())
            .chain(self.elements.iter().filter_map(|e| match e {
                Element::Wire(w) => Some(w.name.clone()),
                Element::Reg(r) => Some(r.name.clone()),
                _ => None,
            }))
            .collect();

        let mut norm = Normalizer { used_names, counter: 0, new_elements: Vec::new() };

        for element in self.elements.iter_mut() {
            match element {
                Element::Assign(assign) => norm.normalize_expr(&mut assign.expr),
                Element::Always(always) => {
                    if let Some(clock) = always.clock.as_mut() {
                        norm.normalize_expr(clock);
                    }
                    for stmt in always.stmts.iter_mut() {
                        norm.normalize_stmt(stmt);
                    }
                }
                Element::Initial(initial) => {
                    for stmt in initial.stmts.iter_mut() {
                        norm.normalize_stmt(stmt);
                    }
                }
                Element::Wire(wire) => {
                    if let Some(expr) = wire.expr.as_mut() {
                        norm.normalize_expr(expr);
                    }
                }
                Element::Reg(reg) => {
                    if let Some(expr) = reg.expr.as_mut() {
                        norm.normalize_expr(expr);
                    }
                }
                Element::Submodule(_) => {}
            }
        }

        if !norm.new_elements.is_empty() {
            // Prepend the new wire declarations and assigns so they appear
            // before any element that references them.
            norm.new_elements.extend(std::mem::take(&mut self.elements));
            self.elements = norm.new_elements;
        }
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
                write!(writer.file, "({{")?;
                for (i, expr) in concat.exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer.file, ", ")?;
                    }
                    expr.write(writer)?;
                }
                write!(writer.file, "}})")?;
            }
            Expr::Repeat(repeat) => {
                write!(writer.file, "({{")?;
                repeat.count.write(writer)?;
                write!(writer.file, "{{")?;
                for (i, expr) in repeat.exprs.iter().enumerate() {
                    if i > 0 {
                        write!(writer.file, ", ")?;
                    }
                    expr.write(writer)?;
                }
                write!(writer.file, "}}}})")?;
            }
            Expr::Index(index) => {
                write!(writer.file, "(")?;
                index.subject.write(writer)?;
                write!(writer.file, "[")?;
                index.index.write(writer)?;
                write!(writer.file, "])")?;
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

#[rustfmt::skip]
const VERILOG_KEYWORDS: &[&str] = &[
    "always", "and", "assign", "automatic", "begin", "buf", "bufif0", "bufif1", "case",
    "casex", "casez", "cell", "cmos", "config", "deassign", "default", "defparam", "design",
    "disable", "edge", "else", "end", "endcase", "endconfig", "endfunction", "endgenerate",
    "endmodule", "endprimitive", "endspecify", "endtable", "endtask", "event", "for", "force",
    "forever", "fork", "function", "generate", "genvar", "highz0", "highz1", "if", "ifnone",
    "incdir", "include", "initial", "inout", "input", "instance", "integer", "join", "large",
    "liblist", "library", "localparam", "macromodule", "medium", "module", "nand", "negedge",
    "nmos", "nor", "noshowcancelled", "not", "notif0", "notif1", "or", "output", "parameter",
    "pmos", "posedge", "primitive", "pull0", "pull1", "pulldown", "pullup", "pulsestyle_ondetect",
    "pulsestyle_onevent", "rcmos", "real", "realtime", "reg", "release", "repeat", "rnmos",
    "rpmos", "rtran", "rtranif0", "rtranif1", "scalared", "showcancelled", "signed", "small",
    "specify", "specparam", "strong0", "strong1", "supply0", "supply1", "table", "task",
    "time", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand",
    "trior", "trireg", "unsigned", "use", "uwire", "vectored", "wait", "wand",
    "weak0", "weak1", "while", "wire", "wor", "xnor", "xor",
];

/// Takes a Virdant path and converts it to a valid Verilog identifier.
/// For paths that are already valid Verilog identifiers, it preserves the name.
/// If the path conflicts with a Verilog keyword, it prefixes the name with a \.
/// If the path contains .'s, it will prefix the name with a \.
pub fn valid_verilog_name(path: &str) -> String {
    if is_simple_verilog_identifier(path) && !VERILOG_KEYWORDS.contains(&path) {
        path.to_string()
    } else {
        format!(r"\{path} ")
    }
}

/// Returns a Verilog identifier only if it can be emitted exactly as written.
pub fn exact_verilog_name(name: &str) -> String {
    let verilog_name = valid_verilog_name(name);
    if verilog_name != name {
        // TODO Support exported names and port names that require escaping while preserving the source spelling.
        panic!("Name `{name}` cannot be preserved exactly in Verilog");
    }
    verilog_name
}

/// Returns whether a path is already a plain, unescaped Verilog identifier.
fn is_simple_verilog_identifier(path: &str) -> bool {
    let mut chars = path.chars();

    let Some(first) = chars.next() else {
        return false;
    };

    if !matches!(first, 'a'..='z' | 'A'..='Z' | '_') {
        return false;
    }

    chars.all(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
}
