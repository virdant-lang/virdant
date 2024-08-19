use virdant::{Offset, WordVal, clog2, Width, Tag};
use virdant::design::*;
use virdant::context::Context;
use virdant::ComponentClass;
use virdant::Flow;
use std::collections::HashMap;
use std::io::Write;

type VerilogError = Box<dyn std::error::Error>;

type SsaName = String;

pub fn to_mlir<P: AsRef<std::path::Path>>(design: Design, path: P) -> Result<(), VerilogError> {
    std::fs::create_dir_all(path.as_ref()).unwrap();
    let layout = Layout;

    let mut mlir = Mlir {
        layout,
        gensym: 0,
    };

    let packages = design.packages();
    for package in packages {
        if package.name() == "builtin" {
            continue;
        }
        let filepath = path.as_ref().join(format!("{}.mlir", package.name()));
        let writer: &mut dyn Write = &mut std::fs::File::create(filepath)?;

        mlir.mlir_package(writer, package)?;
    }

    Ok(())
}

struct Mlir {
    layout: Layout,
    gensym: usize,
}

impl Mlir {
    fn mlir_package(&mut self, f: &mut dyn Write, package: Package) -> Result<(), VerilogError> {
        writeln!(f, "module {{")?;


        let items = package.items();
        for (i, item) in items.iter().enumerate() {
            match item.kind() {
                ItemKind::ModDef(moddef) => self.mlir_moddef(f, moddef)?,
                ItemKind::FnDef(fndef) => self.mlir_fndef(f, fndef)?,
                ItemKind::UnionDef(uniondef) => writeln!(f, "    // UnionDef {}", uniondef.name())?,
                ItemKind::StructDef(structdef) => writeln!(f, "    // StructDef {}", structdef.name())?,
                ItemKind::BuiltinDef(_builtindef) => (),
                ItemKind::SocketDef(socketdef) => writeln!(f, "    // SocketDef {}", socketdef.name())?,
            }

            if i + 1 < items.len() {
                writeln!(f)?;
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }

    fn mlir_moddef(&mut self, f: &mut dyn Write, moddef: ModDef) -> Result<(), VerilogError> {
        writeln!(f, "    // ModDef {}", moddef.name())?;
        if moddef.is_ext() {
            writeln!(f, "    // is external")?;
            return Ok(());
        }

        writeln!(f, "    virdant.module @{} {{", moddef.name())?;

        let ports = moddef.ports();
        for (i, component) in ports.iter().enumerate() {
            let typ = component.typ();
            let typ_str = self.typ_str(&typ);

            let dir = if component.flow() == Flow::Sink {
                let expr = component.driver().unwrap();
                let ssa = self.mlir_expr(f, expr, Context::empty())?;
                format!("virdant.outgoing({ssa})")
            } else {
                "virdant.incoming ".to_string()
            };
            writeln!(f, "        %{} = {dir} : {typ_str}", component.path().join("__"))?;
        }

        for submodule in moddef.submodules() {
            let submodule_moddef = submodule.of();
            let ports = submodule_moddef.ports();
            for port in ports.iter() {
                let port_name = format!("{}__{}", submodule.name(), port.path().join("__"));
                let typ = port.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "        wire {width_str}{port_name};")?;
            }

            writeln!(f, "        {} {}(", submodule.of().name(), submodule.name())?;
            for (i, port) in ports.iter().enumerate() {
                let port_name = format!("{}__{}", submodule.name(), port.path().join("__"));
                write!(f, "            .{}({})", port.path().join("__"), port_name)?;
                if i + 1 < ports.len() {
                    writeln!(f, "    ,")?;
                } else {
                    writeln!(f)?;
                }
            }
            writeln!(f, "        );")?;
        }

        for component in moddef.components() {
            if component.flow() != Flow::Source {
                self.mlir_component(f, component)?;
            }
        }

        writeln!(f, "    }}")?;
        Ok(())
    }

    fn mlir_component(&mut self, f: &mut dyn Write, component: Component) -> Result<(), VerilogError> {
        writeln!(f, "        // mlir_component: {}", component.name())?;

        match component.class() {
            ComponentClass::Port => {
                let expr = if let Some(expr) = component.driver() {
                    expr
                } else {
                    return Ok(());
                };
                let typ = component.typ();
                let component_name = component.path().join("__");
                writeln!(f, "        // outgoing {component_name} : {typ}")?;
                let ssa = self.mlir_expr(f, expr, Context::empty())?;
                writeln!(f, "        assign {component_name} = {ssa};")?;
                writeln!(f)?;
            },
            ComponentClass::SubPort => {
                if let Flow::Sink = component.flow() {
                    let expr = component.driver().unwrap();
                    let typ = component.typ();
                    let component_name = component.name();
                    writeln!(f, "        // submodule port {component_name} : {typ}")?;
                    let ssa = self.mlir_expr(f, expr, Context::empty())?;
                    writeln!(f, "        assign {} = {ssa};", component.path().join("__"))?;
                    writeln!(f)?;
                }
            },
            ComponentClass::Node => {
                let expr = component.driver().unwrap();
                let typ = component.typ();
                let typ_str = self.typ_str(&typ);
                let component_name = component.name();
                writeln!(f, "        // node {component_name} : {typ}")?;
                let ssa = self.mlir_expr(f, expr, Context::empty())?;
                writeln!(f, "        %{component_name} = virdant.node({ssa}) : {typ_str}")?;
                writeln!(f)?;
            },
            ComponentClass::Reg => {
                let expr = component.driver().unwrap();
                let typ = component.typ();
                let typ_str = self.typ_str(&typ);
                let component_name = component.name();
                writeln!(f, "        // reg {component_name} : {typ}")?;
                let connect_ssa = self.mlir_expr(f, expr.clone(), Context::empty())?;
                let clk = "%clock"; //component.clock().unwrap();
                writeln!(f, "        %{component_name} = virdant.reg({connect_ssa}, {clk}) : {typ_str}")?;
                writeln!(f)?;
            },
        }

        Ok(())
    }

    fn mlir_fndef(&mut self, f: &mut dyn Write, fndef: FnDef) -> Result<(), VerilogError> {
        writeln!(f, "    // FnDef {}", fndef.name())?;

        let name = fndef.name();
        let mut context = Context::empty();
        writeln!(f, "    module {name}(")?;
        for (param_name, param_typ) in fndef.params() {
            context = context.extend(param_name.clone(), param_name.clone());
            let width_str = self.width_str(&param_typ);
            writeln!(f, "        input {width_str}{param_name},")?;
        }
        writeln!(f, "        output __out")?;
        writeln!(f, "    );")?;

        let out_ssa = self.mlir_expr(f, fndef.body(), context)?;
        writeln!(f, "        assign __out = {out_ssa};")?;
        writeln!(f, "    endmodule")?;
        Ok(())
    }

    fn typ_str(&self, typ: &Type) -> String {
        match typ.scheme() {
            TypeScheme::StructDef(_) => todo!(),
            TypeScheme::UnionDef(_) => todo!(),
            TypeScheme::BuiltinDef(_) => {
                if typ.is_clock() {
                    "!virdant.clock".to_string()
                } else if typ.is_bit() {
                    "!virdant.bit".to_string()
                } else if typ.is_word() {
                    let width = typ.width();
                    format!("!virdant.word<{width}>")
                } else {
                    unreachable!()
                }
            },
        }
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

    fn mlir_expr(&mut self, f: &mut dyn Write, expr: Expr, ctx: Context<String, SsaName>) -> Result<SsaName, VerilogError> {
        match &expr {
            Expr::Reference(reference) => {
                match reference.referent() {
                    Referent::Binding(binding) => {
                        let ssa = ctx.lookup(&binding.name().to_string()).unwrap();
                        Ok(format!("{ssa}"))
                    },
                    Referent::Component(component) => {
                        if component.is_local() {
                            Ok(format!("%{}", component.name()))
                        } else {
                            let path = component.path();
                            let sm = &path[0];
                            let port = path[1..].join("__");
                            Ok(format!("%{sm}__{port}"))
                        }
                    },
                }
            },
            Expr::Bit(bit) => {
                let gs = self.gensym();
                if bit.value() {
                    writeln!(f, "        {gs} = virdant.const(1)")?;
                } else {
                    writeln!(f, "        {gs} = virdant.const(0)")?;
                }
                Ok(gs)
            },
            Expr::Word(word) => {
                let gs = self.gensym();
                let typ = expr.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "        wire {width_str} {gs} = {}'d{};", word.width(), word.value())?;
                Ok(gs)
            },
            Expr::Cat(cat) => {
                let gs = self.gensym();
                let mut arg_ssas: Vec<SsaName> = vec![];
                for arg in cat.args() {
                    let arg_ssa = self.mlir_expr(f, arg.clone(), ctx.clone())?;
                    arg_ssas.push(arg_ssa);
                }
                let width_str = self.width_str(&cat.typ());
                writeln!(f, "        wire {width_str} {gs} = {{{}}};", arg_ssas.join(", "))?;
                Ok(gs)
            },
            Expr::Idx(idx) => {
                let subject = idx.subject();
                let i = idx.idx();
                let gs = self.gensym();
                let subject_ssa = self.mlir_expr(f, subject, ctx)?;
                writeln!(f, "        wire {gs} = {subject_ssa}[{i}];")?;
                Ok(gs)
            },
            Expr::IdxRange(idx) => {
                let subject = idx.subject();
                let i = idx.idx_lo();
                let j = idx.idx_hi();
                let gs = self.gensym();
                let subject_ssa = self.mlir_expr(f, subject.clone(), ctx)?;
                let end = j - 1;
                let width_str = self.width_str(&idx.typ());
                writeln!(f, "        wire {width_str} {gs} = {subject_ssa}[{end}:{i}];")?;
                Ok(gs)
            },
            Expr::MethodCall(meth) => {
                let subject = meth.subject();
                let args = meth.args();
                let method = meth.method().name().to_string();
                let gs = self.gensym();
                let subject_ssa = self.mlir_expr(f, subject.clone(), ctx.clone())?;
                let mut args_ssa: Vec<SsaName> = vec![];
                self.mlir_expr(f, subject.clone(), ctx.clone())?;
                for arg in args {
                    let arg_ssa = self.mlir_expr(f, arg.clone(), ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }
                let typ_str = self.typ_str(&expr.typ());

                match method.as_str() {
                    "add" => writeln!(f, "        {gs} = virdant.add({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "inc" => writeln!(f, "        {gs} = virdant.inc({subject_ssa}) : {typ_str}")?,
                    "dec" => writeln!(f, "        {gs} = virdant.dec({subject_ssa}) : {typ_str}")?,
                    "sub" => writeln!(f, "        {gs} = virdant.sub({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "and" => writeln!(f, "        {gs} = virdant.and({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "or"  => writeln!(f, "        {gs} = virdant.or({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "not" => writeln!(f, "        {gs} = virdant.not({subject_ssa}) : {typ_str}")?,
                    "xor" => writeln!(f, "        {gs} = virdant.xor({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "eq"  => writeln!(f, "        {gs} = virdant.eq({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    "mux" => writeln!(f, "        {gs} = virdant.if({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "sll" => writeln!(f, "        {gs} = virdant.sll({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "srl" => writeln!(f, "        {gs} = virdant.sra({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "lt"  => writeln!(f, "        {gs} = virdant.lt({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "lte" => writeln!(f, "        {gs} = virdant.lte({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "gt"  => writeln!(f, "        {gs} = virdant.gt({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "gte" => writeln!(f, "        {gs} = virdant.gte({subject_ssa}, {}) : {typ_str}", args_ssa.join(","))?,
                    "get" => writeln!(f, "        {gs} = virdant.get({subject_ssa}, {}) : {typ_str}", args_ssa[0])?,
                    _ => panic!("Unknown method: {}", method),
                }
                Ok(gs)
            },
            Expr::If(if_) => {
                let c = if_.subject();
                let a = if_.truebranch();
                let b = if_.falsebranch();

                let gs = self.gensym();
                let cond_ssa = self.mlir_expr(f, c.clone(), ctx.clone())?;
                let a_ssa = self.mlir_expr(f, a.clone(), ctx.clone())?;
                let b_ssa = self.mlir_expr(f, b.clone(), ctx.clone())?;
                let width_str = self.width_str(&if_.typ());
                writeln!(f, "        wire {width_str} {gs} = {cond_ssa} ? {a_ssa} : {b_ssa};")?;
                Ok(gs)
            },
            Expr::Ctor(inner) => {
                let gs = self.gensym();
                let ctor = inner.ctor();
                let typ = inner.typ();

                let mut args_ssa: Vec<SsaName> = vec![];
                for arg in inner.args() {
                    let arg_ssa = self.mlir_expr(f, arg, ctx.clone())?;
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

                write!(f, "        wire {width_str} {gs} = {{ ")?;
                write!(f, "    {fill_space}")?;

                for arg_ssa in args_ssa.iter().rev() {
                    write!(f, "    {arg_ssa}, ")?;
                }
                write!(f, "    {tag_width}'d{tag}")?;
                writeln!(f, "    }};")?;

                Ok(gs)
            },
            Expr::Match(inner) => {
                let subject = inner.subject();
                let typ = inner.typ();

                // TODO
                let uniondef = match subject.typ().scheme() {
                    TypeScheme::StructDef(_) => todo!(),
                    TypeScheme::BuiltinDef(_) => todo!(),
                    TypeScheme::UnionDef(uniondef) => uniondef,
                };

                let width = self.layout.width(&typ);
                let width_str = format!("[{}:0]", width - 1);

                let gs = self.gensym_hint("match");
                let subject_ssa = self.mlir_expr(f, subject.clone(), ctx.clone())?;

                let tag_ssa = self.gensym();
                let tag_width = self.layout.tag_width(&uniondef);
                let tag_top = tag_width - 1;

                let mut arm_ssas: Vec<(Tag, _, SsaName)> = vec![];
                writeln!(f, "        // match arm")?;
                for (pat, expr) in inner.arms() {
                    match pat {
                        Pat::CtorAt(ctor, pats) => {
                            writeln!(f, "        // case {}", ctor.name())?;
                            let tag = self.layout.tag(&ctor);
                            let mut new_ctx = ctx.clone();
                            writeln!(f, "        // (pats are {pats:?})")?;
                            for (i, pat) in pats.iter().enumerate() {
                                let (offset, width) = self.layout.ctor_slot(&ctor, i);
                                let width_minus_1 = width - 1;
                                if let Pat::Bind(binding) = pat {
                                    let x_ssa = self.gensym_hint(binding.name());
                                    new_ctx = new_ctx.extend(binding.name().to_string(), x_ssa.clone());
                                    let bot_bit = offset;
                                    let top_bit = offset + width - 1;
                                    writeln!(f, "        // binding variable {} to slot", binding.name())?;
                                    writeln!(f, "        wire [{width_minus_1}:0] {x_ssa} = {subject_ssa}[{top_bit}:{bot_bit}];")?;
                                } else {
                                    panic!()
                                }
                            }
                            let arm_ssa = self.mlir_expr(f, expr.clone(), new_ctx)?;
                            arm_ssas.push((tag, ctor.name().to_string(), arm_ssa));
                        },
                        _ => todo!(),
                    }
                }

                writeln!(f, "        // project tag ({tag_width} bits)")?;
                let tag_width_str = if tag_width == 1 {
                    format!("")
                } else {
                    format!("[{tag_top}:0]")
                };

                let subject_tag_idx = if tag_width == 1 {
                    format!("[0]")
                } else {
                    format!("[{tag_top}:0]")
                };

                writeln!(f, "        reg {width_str} {gs};")?;

                writeln!(f, "        wire {tag_width_str} {tag_ssa} = {subject_ssa}{subject_tag_idx};")?;

                writeln!(f, "        always @(*) begin")?;
                writeln!(f, "            case ({tag_ssa})")?;

                for (tag, ctor, arm_ssa) in &arm_ssas {
                    writeln!(f, "                // @{ctor}:")?;
                    writeln!(f, "                {tag}: {gs} <= {arm_ssa};")?;
                }

                writeln!(f, "                default: {gs} <= 32'bx;")?;
                writeln!(f, "            endcase")?;
                writeln!(f, "        end")?;

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
                    let field_ssa = self.mlir_expr(f, e, ctx.clone())?;
                    field_ssas.push(field_ssa);
                }

                let gs = self.gensym_hint("struct");
                let typ = inner.typ();
                let width_str = self.width_str(&typ);
                writeln!(f, "        wire {width_str}{gs} = {{{}}};", field_ssas.join(", "))?;
                Ok(gs)
            },
            Expr::Field(inner) => {
                let subject = inner.subject();

                let gs = self.gensym_hint(&format!("field__{}", inner.field().name()));
                let subject_ssa = self.mlir_expr(f, subject.clone(), ctx.clone())?;

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
        }
    }

    fn gensym(&mut self) -> SsaName {
        self.gensym += 1;
        format!("%__TEMP_{}", self.gensym)
    }

    fn gensym_hint(&mut self, hint: &str) -> SsaName {
        self.gensym += 1;
        format!("%__TEMP_{}_{hint}", self.gensym)
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
            TypeScheme::BuiltinDef(_builtindef) => {
                if typ.is_clock() {
                    1
                } else if typ.is_bit() {
                    1
                } else if typ.is_word() {
                    typ.width()
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
