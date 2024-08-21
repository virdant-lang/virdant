use crate::common::*;
use crate::design::*;
use crate::context::Context;
use crate::ComponentClass;
use crate::Flow;
use std::collections::HashMap;
use std::io::Write;

type VerilogError = Box<dyn std::error::Error>;

type SsaName = String;

impl Design {
    pub fn verilog<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), VerilogError> {
        std::fs::create_dir_all(path.as_ref()).unwrap();
        let layout = Layout;

        let mut verilog = Verilog {
            layout,
            gensym: 0,
        };

        let packages = self.packages();
        for package in packages {
            if package.name() == "builtin" {
                continue;
            }
            let filepath = path.as_ref().join(format!("{}.v", package.name()));
            let writer: &mut dyn Write = &mut std::fs::File::create(filepath)?;

            verilog.verilog_package(writer, package)?;
        }

        Ok(())
    }
}

struct Verilog {
    layout: Layout,
    gensym: usize,
}

impl Verilog {
    fn verilog_package(&mut self, f: &mut dyn Write, package: Package) -> Result<(), VerilogError> {
        let items = package.items();
        for (i, item) in items.iter().enumerate() {
            match item.kind() {
                ItemKind::ModDef(moddef) => self.verilog_moddef(f, moddef)?,
                ItemKind::FnDef(fndef) => self.verilog_fndef(f, fndef)?,
                ItemKind::UnionDef(uniondef) => writeln!(f, "// UnionDef {}", uniondef.name())?,
                ItemKind::StructDef(structdef) => writeln!(f, "// StructDef {}", structdef.name())?,
                ItemKind::EnumDef(enumdef) => writeln!(f, "// EnumDef {}", enumdef.name())?,
                ItemKind::BuiltinDef(_builtindef) => (),
                ItemKind::SocketDef(socketdef) => writeln!(f, "// SocketDef {}", socketdef.name())?,
            }

            if i + 1 < items.len() {
                writeln!(f)?;
            }
        }
        Ok(())
    }

    fn verilog_moddef(&mut self, f: &mut dyn Write, moddef: ModDef) -> Result<(), VerilogError> {
        writeln!(f, "// ModDef {}", moddef.name())?;
        if moddef.is_ext() {
            writeln!(f, "`include \"ext/{}.v\"", moddef.name())?;
            return Ok(());
        }

        writeln!(f, "module {}(", moddef.name())?;

        let ports = moddef.ports();
        for (i, component) in ports.iter().enumerate() {
            let typ = component.typ();
            let width_str = self.width_str(&typ);

            let dir = if component.flow() == Flow::Sink {
                "output "
            } else {
                "input  "
            };
            write!(f, "    {dir}{width_str:>10} {}", component.path().join("__"))?;

            if i + 1 < ports.len() {
                writeln!(f, ",")?;
            } else {
                writeln!(f, "")?;
            }
        }

        writeln!(f, ");")?;

        for submodule in moddef.submodules() {
            let submodule_moddef = submodule.of();
            let ports = submodule_moddef.ports();
            for port in ports.iter() {
                let port_name = format!("{}__{}", submodule.name(), port.path().join("__"));
                let typ = port.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "    wire {width_str}{port_name};")?;
            }

            writeln!(f, "    {} {}(", submodule.of().name(), submodule.name())?;
            for (i, port) in ports.iter().enumerate() {
                let port_name = format!("{}__{}", submodule.name(), port.path().join("__"));
                write!(f, "        .{}({})", port.path().join("__"), port_name)?;
                if i + 1 < ports.len() {
                    writeln!(f, ",")?;
                } else {
                    writeln!(f)?;
                }
            }
            writeln!(f, "    );")?;
        }

        for component in moddef.components() {
            if component.flow() != Flow::Source {
                self.verilog_component(f, component)?;
            }
        }

        writeln!(f, "endmodule")?;
        Ok(())
    }

    fn verilog_component(&mut self, f: &mut dyn Write, component: Component) -> Result<(), VerilogError> {
        writeln!(f, "    //  verilog_component: {}", component.name())?;

        match component.class() {
            ComponentClass::Port => {
                let expr = if let Some(expr) = component.driver() {
                    expr
                } else {
                    return Ok(());
                };
                let typ = component.typ();
                let component_name = component.path().join("__");
                writeln!(f, "    // outgoing {component_name} : {typ}")?;
                let ssa = self.verilog_expr(f, expr, Context::empty())?;
                writeln!(f, "    assign {component_name} = {ssa};")?;
                writeln!(f)?;
            },
            ComponentClass::SubPort => {
                if let Flow::Sink = component.flow() {
                    let expr = component.driver().unwrap();
                    let typ = component.typ();
                    let component_name = component.name();
                    writeln!(f, "    // submodule port {component_name} : {typ}")?;
                    let ssa = self.verilog_expr(f, expr, Context::empty())?;
                    writeln!(f, "    assign {} = {ssa};", component.path().join("__"))?;
                    writeln!(f)?;
                }
            },
            ComponentClass::Wire => {
                let expr = component.driver().unwrap();
                let typ = component.typ();
                let width_str = self.width_str(&typ);
                let component_name = component.name();
                writeln!(f, "    // node {component_name} : {typ}")?;
                writeln!(f, "    wire {width_str} {component_name};")?;
                let ssa = self.verilog_expr(f, expr, Context::empty())?;
                writeln!(f, "    assign {component_name} = {ssa};")?;
                writeln!(f)?;
            },
            ComponentClass::Reg => {
                let expr = component.driver().unwrap();
                let typ = component.typ();
                let width_str = self.width_str(&typ);
                let component_name = component.name();
                writeln!(f, "    // reg {component_name} : {typ}")?;
                writeln!(f, "    reg  {width_str} {component_name};")?;
                let clk = "clock"; //component.clock().unwrap();
                let connect_ssa = self.verilog_expr(f, expr.clone(), Context::empty())?;
                writeln!(f, "    always @(posedge {clk}) begin")?;
                writeln!(f, "        {component_name} <= {connect_ssa};")?;
                writeln!(f, "    end")?;
                writeln!(f)?;
                writeln!(f, "    initial begin")?;
                writeln!(f, "        {component_name} <= 1;")?;
                writeln!(f, "    end")?;
                writeln!(f)?;
            },
        }

        Ok(())
    }

    fn verilog_fndef(&mut self, f: &mut dyn Write, fndef: FnDef) -> Result<(), VerilogError> {
        writeln!(f, "// FnDef {}", fndef.name())?;

        let name = fndef.name();
        let mut context = Context::empty();
        writeln!(f, "module {name}(")?;
        for (param_name, param_typ) in fndef.params() {
            context = context.extend(param_name.clone(), param_name.clone());
            let width_str = self.width_str(&param_typ);
            writeln!(f, "    input {width_str}{param_name},")?;
        }
        let width_str = self.width_str(&fndef.ret());
        writeln!(f, "    output {width_str}__out")?;
        writeln!(f, ");")?;

        let out_ssa = self.verilog_expr(f, fndef.body(), context)?;
        writeln!(f, "    assign __out = {out_ssa};")?;
        writeln!(f, "endmodule")?;
        Ok(())
    }

    fn width_str(&self, typ: &Type) -> String {
        let n = self.layout.width(typ);
        if n == 1 {
            "".to_string()
        } else {
            let max_bit = n - 1;
            format!("[{max_bit}:0] ")
        }
    }

    fn verilog_expr(&mut self, f: &mut dyn Write, expr: Expr, ctx: Context<String, SsaName>) -> Result<SsaName, VerilogError> {
        match &expr {
            Expr::Reference(reference) => {
                match reference.referent() {
                    Referent::Binding(binding) => {
                        let ssa = ctx.lookup(&binding.name().to_string()).unwrap();
                        Ok(format!("{ssa}"))
                    },
                    Referent::Component(component) => {
                        if component.is_local() {
                            Ok(component.name())
                        } else {
                            let path = component.path();
                            let sm = &path[0];
                            let port = path[1..].join("__");
                            Ok(format!("{sm}__{port}"))
                        }
                    },
                }
            },
            Expr::Bit(bit) => {
                let gs = self.gensym();
                if bit.value() {
                    writeln!(f, "    wire {gs} = 1'b1;")?;
                } else {
                    writeln!(f, "    wire {gs} = 1'b0;")?;
                }
                Ok(gs)
            },
            Expr::Word(word) => {
                let gs = self.gensym();
                let typ = expr.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "    wire {width_str} {gs} = {}'d{};", word.width(), word.value())?;
                Ok(gs)
            },
            Expr::Cat(cat) => {
                let gs = self.gensym();
                let mut arg_ssas: Vec<SsaName> = vec![];
                for arg in cat.args() {
                    let arg_ssa = self.verilog_expr(f, arg.clone(), ctx.clone())?;
                    arg_ssas.push(arg_ssa);
                }
                let width_str = self.width_str(&cat.typ());
                writeln!(f, "    wire {width_str} {gs} = {{{}}};", arg_ssas.join(", "))?;
                Ok(gs)
            },
            Expr::Idx(idx) => {
                let subject = idx.subject();
                let i = idx.idx();
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(f, subject, ctx)?;
                writeln!(f, "    wire {gs} = {subject_ssa}[{i}];")?;
                Ok(gs)
            },
            Expr::IdxRange(idx) => {
                let subject = idx.subject();
                let i = idx.idx_lo();
                let j = idx.idx_hi();
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(f, subject.clone(), ctx)?;
                let end = j - 1;
                let width_str = self.width_str(&idx.typ());
                writeln!(f, "    wire {width_str} {gs} = {subject_ssa}[{end}:{i}];")?;
                Ok(gs)
            },
            Expr::MethodCall(meth) => {
                let subject = meth.subject();
                let args = meth.args();
                let method = meth.method().name().to_string();
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(f, subject.clone(), ctx.clone())?;
                let mut args_ssa: Vec<SsaName> = vec![];
                self.verilog_expr(f, subject.clone(), ctx.clone())?;
                for arg in args {
                    let arg_ssa = self.verilog_expr(f, arg.clone(), ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }
                let width_str = self.width_str(&expr.typ());

                match method.as_str() {
                    "value" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa};")?,
                    "add" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} + {};", args_ssa[0])?,
                    "inc" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} + 1;")?,
                    "dec" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} - 1;")?,
                    "sub" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} - {};", args_ssa[0])?,
                    "and" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} & {};", args_ssa[0])?,
                    "or"  => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} | {};", args_ssa[0])?,
                    "not" => writeln!(f, "    wire {width_str} {gs} = ~{subject_ssa};")?,
                    "xor" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} ^ {};", args_ssa[0])?,
                    "eq"  => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} == {};", args_ssa[0])?,
                    "mux" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} ? {};", args_ssa.join(" : "))?,
                    "sll" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} << {};", args_ssa.join(" : "))?,
                    "srl" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} >> {};", args_ssa.join(" : "))?,
                    "lt"  => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} < {};", args_ssa.join(" : "))?,
                    "lte" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} <= {};", args_ssa.join(" : "))?,
                    "gt"  => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} > {};", args_ssa.join(" : "))?,
                    "gte" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa} >= {};", args_ssa.join(" : "))?,
                    "get" => writeln!(f, "    wire {width_str} {gs} = {subject_ssa}[{}];", args_ssa[0])?,
                    _ => panic!("Unknown method: {}", method),
                }
                Ok(gs)
            },
            Expr::FnCall(fncall) => {
                let args = fncall.args();
                let fnname = fncall.name().to_string();
                let gs = self.gensym();

                let mut args_ssa: Vec<SsaName> = vec![];
                for arg in args {
                    let arg_ssa = self.verilog_expr(f, arg.clone(), ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }
                let width_str = self.width_str(&expr.typ());
                let instance_gs = self.gensym_hint(&format!("fn_{fnname}"));
                writeln!(f, "    wire {width_str}{gs};")?;
                writeln!(f, "    {fnname} {instance_gs}(")?;

                let fndef = fncall.fndef();
                for ((param_name, _param_typ), arg_ssa) in fndef.params().into_iter().zip(args_ssa) {
                    writeln!(f, "        .{param_name}({arg_ssa}),")?;
                }

                writeln!(f, "        .__out({gs})")?;
                writeln!(f, "    );")?;
                Ok(gs)
            },
            Expr::If(if_) => {
                let c = if_.subject();
                let a = if_.truebranch();
                let b = if_.falsebranch();

                let gs = self.gensym();
                let cond_ssa = self.verilog_expr(f, c.clone(), ctx.clone())?;
                let a_ssa = self.verilog_expr(f, a.clone(), ctx.clone())?;
                let b_ssa = self.verilog_expr(f, b.clone(), ctx.clone())?;
                let width_str = self.width_str(&if_.typ());
                writeln!(f, "    wire {width_str} {gs} = {cond_ssa} ? {a_ssa} : {b_ssa};")?;
                Ok(gs)
            },
            Expr::Ctor(inner) => {
                let gs = self.gensym();
                let ctor = inner.ctor();
                let typ = inner.typ();

                let mut args_ssa: Vec<SsaName> = vec![];
                for arg in inner.args() {
                    let arg_ssa = self.verilog_expr(f, arg, ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }

                let width = self.layout.width(&typ);
                let width_str = format!("[{}:0]", width - 1);

                let tag_width = self.layout.tag_width(&ctor.uniondef());
                let tag = self.layout.tag(&ctor);

                let fill = "1";
                let fill_width = width - self.layout.ctor_used_width(&ctor);
                let fill_space = if fill_width > 0 {
                    let contents = fill.repeat(fill_width as usize);
                    format!("{fill_width}'b{contents}, ")
                } else {
                    "".to_string()
                };

                write!(f, "    wire {width_str} {gs} = {{ ")?;
                write!(f, "{fill_space}")?;

                for arg_ssa in args_ssa.iter().rev() {
                    write!(f, "{arg_ssa}, ")?;
                }
                write!(f, "{tag_width}'d{tag}")?;
                writeln!(f, "}};")?;

                Ok(gs)
            },
            Expr::Enumerant(inner) => {
                let gs = self.gensym();
                let enumerant = inner.enumerant();
                let value = enumerant.value();
                let typ = inner.typ();

                let width = self.layout.width(&typ);
                let width_str = format!("[{}:0]", width - 1);

                writeln!(f, "    wire {width_str} {gs} = {value};")?;

                Ok(gs)
            },
            Expr::Match(inner) => {
                let subject = inner.subject();
                let typ = inner.typ();

                let width = self.layout.width(&typ);
                let width_str = format!("[{}:0]", width - 1);

                let gs = self.gensym_hint("match");
                let subject_ssa = self.verilog_expr(f, subject.clone(), ctx.clone())?;

                let mut arm_ssas: Vec<SsaName> = vec![];
                writeln!(f, "    // match arm")?;
                for (pat, expr) in inner.arms() {
                    let new_ctx = ctx.extend_from(&self.deconstruct_by_pat(f, &subject_ssa, &pat));
                    let arm_ssa = self.verilog_expr(f, expr, new_ctx)?;
                    arm_ssas.push(arm_ssa);
                }

                writeln!(f, "    reg {width_str} {gs};")?;

                writeln!(f, "    always @(*) begin")?;
                writeln!(f, "        casez ({subject_ssa})")?;

                for ((pat, _expr), arm_ssa) in inner.arms().iter().zip(arm_ssas) {
                    writeln!(f, "            {}: {gs} <= {arm_ssa};", self.pat_mask(&pat))?;
                }

                writeln!(f, "            default: {gs} <= 32'bx;")?;
                writeln!(f, "        endcase")?;
                writeln!(f, "    end")?;

                Ok(gs)
            },
            Expr::Struct(inner) => {
                let structdef = inner.structdef();
                let mut field_ssas = vec![];

                let assigns: HashMap<String, Expr> = inner.assigns().into_iter()
                    .map(|(field, e)| (field.name().to_string(), e))
                    .collect();

                for field in self.layout.field_slot_order(structdef) {
                    let e = assigns[field.name()].clone();
                    let field_ssa = self.verilog_expr(f, e, ctx.clone())?;
                    field_ssas.push(field_ssa);
                }

                let gs = self.gensym_hint("struct");
                let typ = inner.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "    wire {width_str}{gs} = {{{}}};", field_ssas.join(", "))?;
                Ok(gs)
            },
            Expr::Field(inner) => {
                let subject = inner.subject();

                let gs = self.gensym_hint(&format!("field__{}", inner.field().name()));
                let subject_ssa = self.verilog_expr(f, subject.clone(), ctx.clone())?;

                let (offset, width) = self.layout.field_slot(&inner.field());

                let width_str = if width > 1 {
                    format!("[{}:0] ", width - 1)
                } else {
                    "".to_string()
                };

                let index_str = format!("[{}:{}]", offset + width - 1, offset);

                writeln!(f, "    wire {width_str}{gs} = {subject_ssa}{index_str};")?;

                Ok(gs)
            },
            Expr::Sext(inner) => {
                let arg = inner.arg();
                let typ = inner.typ();
                let arg_typ = arg.typ();

                let ext_width: Width = typ.typ.width() - arg_typ.typ.width();
                let sign_idx = arg_typ.typ.width() - 1;

                let bit_gs = self.gensym_hint("bit");
                let gs = self.gensym();

                let arg_ssa = self.verilog_expr(f, inner.arg(), ctx)?;
                writeln!(f, "    wire {bit_gs} = {arg_ssa}[{sign_idx}];")?;

                let width_str = self.width_str(&typ);
                writeln!(f, "    wire {width_str}{gs} = {{{{{ext_width}{{{bit_gs}}}}}, {arg_ssa}}};")?;
                Ok(gs)
            },
            Expr::Zext(inner) => {
                let arg = inner.arg();
                let typ = inner.typ();
                let arg_typ = arg.typ();

                let ext_width: Width = typ.typ.width() - arg_typ.typ.width();

                let gs = self.gensym();
                let arg_ssa = self.verilog_expr(f, inner.arg(), ctx)?;

                let width_str = self.width_str(&typ);
                writeln!(f, "    wire {width_str}{gs} = {{{{{ext_width}{{1'b0}}}}, {arg_ssa}}};")?;
                Ok(gs)
            },
        }
    }

    fn pat_mask(&self, pat: &Pat) -> PatMask {
        match pat {
            Pat::CtorAt(typ, ctor, pats) => {
                let width = self.layout.width(&typ);
                let ctor_unused_width = width - self.layout.ctor_used_width(&ctor);
                let tag_width = self.layout.tag_width(&ctor.uniondef());
                let tag = self.layout.tag(ctor);

                let mut patmask = PatMask(vec![(None, ctor_unused_width)]);
                for pat in pats.iter().rev() {
                    patmask = patmask.append(&self.pat_mask(pat));
                }
                let tag_patmask = PatMask(vec![(Some(tag), tag_width)]);
                patmask = patmask.append(&tag_patmask);
                patmask
            },
            Pat::EnumerantAt(typ, enumerant) => {
                let width = self.layout.width(&typ);
                let value = enumerant.value();
                PatMask(vec![(Some(value), width)])
            },
            Pat::Bind(typ, _) => {
                let width = self.layout.width(&typ);
                PatMask(vec![(None, width)])
            },
            Pat::Else(typ) => {
                let width = self.layout.width(&typ);
                PatMask(vec![(None, width)])
            },
        }
    }

    fn deconstruct_by_pat(
        &mut self,
        f: &mut dyn Write,
        subject_ssa: &SsaName,
        pat: &Pat,
    ) -> Context<String, SsaName> {
        self.deconstruct_by_pat_within(f, subject_ssa, pat, 0)
    }

    fn deconstruct_by_pat_within(
        &mut self,
        f: &mut dyn Write,
        subject_ssa: &SsaName,
        pat: &Pat,
        offset: Offset,
    ) -> Context<String, SsaName> {
        match pat {
            Pat::CtorAt(_typ, ctor, pats) => {
                let mut ctx = Context::empty();
                for (i, pat) in pats.iter().enumerate() {
                    let (slot_offset, _slot_width) = self.layout.ctor_slot(ctor, i);
                    ctx = ctx.extend_from(&self.deconstruct_by_pat_within(f, subject_ssa, pat, offset + slot_offset));
                }
                ctx
            },
            Pat::EnumerantAt(_, _) => Context::empty(),
            Pat::Bind(typ, binding) => {
                let gs = self.gensym();
                let width_str = self.width_str(typ);
                let width = self.layout.width(typ);
                writeln!(f, "    wire {width_str}{gs} = {subject_ssa}[{}:{}];", offset + width - 1, offset).unwrap();
                Context::empty().extend(binding.name().to_string(), gs)
            },
            Pat::Else(_) => Context::empty(),
        }
    }

    fn gensym(&mut self) -> SsaName {
        self.gensym += 1;
        format!("__TEMP_{}", self.gensym)
    }

    fn gensym_hint(&mut self, hint: &str) -> SsaName {
        self.gensym += 1;
        format!("__TEMP_{}_{hint}", self.gensym)
    }
}

struct PatMask(Vec<(Option<WordVal>, Width)>);

impl std::fmt::Display for PatMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut total_width = 0;
        for (_val, width) in &self.0 {
            total_width += width;
        }

        write!(f, "{total_width}'b")?;
        for (val, width) in &self.0 {
            let width = *width as usize;

            if let Some(val) = val {
                write!(f, "{val:0>width$b}")?;
            } else {
                let val = "?".repeat(width);
                write!(f, "{val}")?;
            }
        }

        Ok(())
    }
}

impl PatMask {
    fn append(&self, other: &PatMask) -> PatMask {
        let mut inner = self.0.clone();
        inner.extend(other.0.iter());
        PatMask(inner)
    }
}

pub struct Layout;

impl Layout {
    pub fn width(&self, typ: &Type) -> Width {
        match typ.scheme() {
            TypeScheme::StructDef(structdef) => {
                let mut width = 0;
                for field in structdef.fields() {
                    width += self.width(&field.typ());
                }
                width
            },
            TypeScheme::UnionDef(uniondef) => {
                let mut width = 0;
                for ctor in uniondef.ctors() {
                    let variant_width = self.ctor_used_width(&ctor);
                    if variant_width > width {
                        width = variant_width;
                    }
                }
                width
            },
            TypeScheme::EnumDef(enumdef) => {
                *enumdef.info.width.unwrap()
            },
            TypeScheme::BuiltinDef(_builtindef) => {
                if typ.typ.is_clock() {
                    1
                } else if typ.typ.is_bit() {
                    1
                } else if typ.typ.is_word() {
                    typ.typ.width()
                } else {
                    unreachable!()
                }
            },
        }
    }

    pub fn tag_width(&self, uniondef: &UnionDef) -> Width {
        clog2(uniondef.ctors().len() as u64)
    }

    pub fn ctor_used_width(&self, ctor: &Ctor) -> Width {
        let mut width = self.tag_width(&ctor.uniondef());

        for (_arg_name, arg_typ) in ctor.params() {
            width += self.width(&arg_typ);
        }
        width
    }

    pub fn tag(&self, ctor: &Ctor) -> WordVal {
        let uniondef = ctor.uniondef();
        for (i, ctor_search) in uniondef.ctors().into_iter().enumerate() {
            if ctor_search.name() == ctor.name() {
                return i.try_into().unwrap();
            }
        }
        unreachable!()
    }

    pub fn ctor_slot(&self, ctor: &Ctor, slot: usize) -> (Offset, Width) {
        let mut offset = self.tag_width(&ctor.uniondef());

        for (i, (_arg_name, arg_typ)) in ctor.params().into_iter().enumerate() {
            let width = self.width(&arg_typ);
            if i == slot {
                return (offset, width);
            } else {
                offset += width;
            }
        }

        unreachable!()
    }

    pub fn field_slot_order(&self, structdef: StructDef) -> Vec<Field> {
        let mut fields = structdef.fields();
        fields.sort_by_key(|field| self.field_slot(field).0);
        fields
    }

    pub fn field_slot(&self, field: &Field) -> (Offset, Width) {
        let mut offset = 0;
        for current_field in field.structdef().fields() {
            let width = self.width(&field.typ());
            if current_field.name() == field.name() {
                return (offset, width);
            } else {
                offset += width;
            }
        }
        unreachable!()
    }
}
