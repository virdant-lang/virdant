use crate::context::Context;
use crate::expr::Width;
use crate::ComponentClass;
use crate::Flow;
use crate::Virdant;
use crate::expr::Referent;
use crate::expr::Path;
use crate::expr::Ident;
use std::collections::HashMap;
use std::io::Write;
use crate::expr::Typed;
use crate::design::*;

type VerilogError = Box<dyn std::error::Error>;

type SsaName = String;

impl Design {
    pub fn verilog<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), VerilogError> {
        std::fs::create_dir_all(path.as_ref()).unwrap();

        let mut verilog = Verilog {
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
    gensym: usize,
}

impl Verilog {
    fn verilog_package(&mut self, f: &mut dyn Write, package: Package) -> Result<(), VerilogError> {
        let items = package.items();
        for (i, item) in items.iter().enumerate() {
            match item.kind() {
                ItemKind::ModDef(moddef) => self.verilog_moddef(f, moddef)?,
                ItemKind::UnionDef(uniondef) => writeln!(f, "// UnionDef {}", uniondef.name())?,
                ItemKind::StructDef(structdef) => writeln!(f, "// StructDef {}", structdef.name())?,
                ItemKind::PortDef(portdef) => writeln!(f, "// PortDef {}", portdef.name())?,
                ItemKind::BuiltinDef(_builtindef) => (),
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

//        for submodule in moddef.submodules() {
//        }

        for component in moddef.components() {
            if !component.is_port() {
                self.verilog_component(f, component)?;
            }
        }

        writeln!(f, "endmodule")?;
        Ok(())
    }

    fn verilog_component(&mut self, f: &mut dyn Write, component: Component) -> Result<(), VerilogError> {
        writeln!(f, "    {}", component.path().join("."))?;
        /*
        let f = self.writers.get_mut(component.package().name()).unwrap();

        match component.kind() {
            ComponentKind::Incoming => todo!(),
            ComponentKind::Outgoing => todo!(),
            ComponentKind::Node => todo!(),
            ComponentKind::Reg => todo!(),
        }
*/

        /*
        if component.is_outgoing() {
            let expr = component.driver().unwrap();
            let typ = component.typ();
            let component_name = component.id().name();
            writeln!(f, "    // outgoing {component_name} : {typ}")?;
            let ssa = self.verilog_expr(expr, Context::empty())?;
            writeln!(f, "    assign {component_name} = {ssa};")?;
            writeln!(f)?;
        } else if component.is_node() {
            let expr = component.driver().unwrap();
            let typ = expr.typ();
            let width_str = make_width_str(self.design, typ.clone());
            let component_name = component.id().name();
            writeln!(f, "    // node {component_name} : {typ}")?;
            writeln!(f, "    wire {width_str} {component_name};")?;
            let ssa = self.verilog_expr(expr, Context::empty())?;
            writeln!(f, "    assign {component_name} = {ssa};")?;
            writeln!(f)?;
        } else if component.is_reg() {
            let expr = component.driver().unwrap();
            let typ = expr.typ();
            let width_str = make_width_str(self.design, typ.clone());
            let component_name = component.id().name();
            writeln!(f, "    // reg {component_name} : {typ}")?;
            writeln!(f, "    reg  {width_str} {component_name};")?;
            let clk = "clock"; //component.clock().unwrap();
            let connect_ssa = self.verilog_expr(expr.clone(), Context::empty())?;
            writeln!(f, "    always @(posedge {clk}) begin")?;
            writeln!(f, "        {component_name} <= {connect_ssa};")?;
            writeln!(f, "    end")?;
            writeln!(f)?;
            writeln!(f, "    initial begin")?;
            writeln!(f, "        {component_name} <= 1;")?;
            writeln!(f, "    end")?;
            writeln!(f)?;
        }
*/

        Ok(())
    }

    fn width_str(&self, typ: &Type) -> String {
        let n = self.bitwidth(typ);
        if n == 1 {
            "".to_string()
        } else {
            let max_bit = n - 1;
            format!("[{max_bit}:0] ")
        }
    }

    fn bitwidth(&self, typ: &Type) -> Width {
        match typ.scheme() {
            TypeScheme::StructDef(_) => todo!(),
            TypeScheme::UnionDef(_) => todo!(),
            TypeScheme::BuiltinDef(builtindef) => {
                if builtindef.name() == "Clock" {
                    1
                } else if builtindef.name() == "Bit" {
                    1
                } else if builtindef.name() == "Word" {
                    if let TypeArg::Nat(width) = typ.args().unwrap()[0] {
                        width
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            },
        }
    }


    /*
    fn verilog_moddef(&mut self, moddef: ModDef) -> Result<(), VerilogError> {
        let moddef_name: Ident = moddef_id.name();
        let moddef = self.design.structure_moddef(moddef_id)?;

        if moddef.is_ext() {
            writeln!(f, "`include \"ext/{moddef_name}.v\"")?;
            writeln!(f)?;
            return Ok(());
        }
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        writeln!(f, "module {}(", moddef_name.clone())?;
        let ports = moddef.ports();
        for (i, port) in ports.iter().enumerate() {
            let is_last = i + 1 == ports.len();
            self.verilog_port(port.clone(), is_last)?;
        }
        writeln!(f, ");")?;

        for submodule in moddef.submodules() {
            self.verilog_submodule(submodule)?;
        }


        for component in moddef.components() {
            if component.is_outgoing() || component.is_reg() || component.is_node() {
                self.verilog_component(component)?;
            }
        }

        writeln!(f, "endmodule")?;
        writeln!(f)?;
        Ok(())
    }
*/
}
    /*
    fn verilog_port(&mut self, port: Component, is_last_port: bool) -> Result<(), VerilogError> {
        let direction = if port.is_incoming() {
            "input  "
        } else {
            "output "
        };
        let port_name = port.id().name();
        let typ = port.typ();

        if let Type::Word(1) = typ {
            write!(f, "    {direction} wire            {port_name}")?;
        } else if let Type::Word(n) = typ {
            let max_bit = n - 1;
            let width_str = format!("[{max_bit}:0]");
            let padded_width_str = format!("{width_str: >8}");
            write!(f, "    {direction} wire  {padded_width_str} {port_name}")?;
        } else if let Type::Clock = typ {
            write!(f, "    {direction} wire            {port_name}")?;
        } else {
            let width_str = make_width_str(self.design, typ.clone());
            write!(f, "    {direction} wire  {width_str}     {port_name}")?;
        }

        if is_last_port {
            writeln!(f)?;
        } else {
            writeln!(f, ",")?;
        }

        Ok(())
    }


    fn verilog_submodule(&mut self, submodule: Submodule) -> Result<(), VerilogError> {
        writeln!(f, "    // Submodule {} of {}", submodule.id(), submodule.moddef())?;
        let moddef_id = submodule.moddef();
        let submodule_moddef = self.design.structure_moddef(submodule.moddef())?;
        let ports = submodule_moddef.ports();

        // Create wires which bridge between the module and the submodule
        for port in &ports {
            let typ = port.typ();
            let width_str = make_width_str(self.design, typ);
            let submodule_name = submodule.id().name();
            let port_name = port.id().name();
            writeln!(f, "    wire {width_str} __TEMP_{submodule_name}_{port_name};")?;
        }

        // Create drive the submodule's incoming ports.
        for port in &ports {
            if port.is_incoming() {
                let path = submodule.id().name().as_path().join(&port.id().name().as_path());
                eprintln!("IN MODULE {moddef_id}");
                eprintln!("INST'ING MODULE {}", submodule_moddef.id());
                eprintln!("LOOKING FOR DRIVER FOR {path}");
                let expr = submodule.driver_for(path);
                let gs = self.verilog_expr(expr, Context::empty())?;
                let submodule_name = submodule.id().name();
                let port_name = port.id().name();
                writeln!(f, "    assign __TEMP_{submodule_name}_{port_name} = {gs};")?;
            }
        }


        // Instantiate the module and connect the intermediary wires.
        writeln!(f, "    {} {}(", submodule_moddef.id().name(), submodule.id().name())?;
        for (i, port) in ports.iter().enumerate() {
            let last_port = i + 1 == ports.len();
            let submodule_name = submodule.id().name();
            let port_name = port.id().name();
            write!(f, "        .{port_name}(__TEMP_{submodule_name}_{port_name})")?;
            if last_port {
                writeln!(f)?;
            } else {
                writeln!(f, ",")?;
            }
        }
        writeln!(f, "    );")?;
        Ok(())
    }

    fn verilog_expr(&mut self, expr: Arc<TypedExpr>, ctx: Context<Ident, SsaName>) -> Result<SsaName, VerilogError> {
        match expr.as_ref() {
            TypedExpr::Reference(_typ, Referent::Binding(x)) => {
                let ssa = ctx.lookup(x).unwrap();
                Ok(format!("{ssa}"))
            },
            TypedExpr::Reference(_typ, Referent::Component(component_id)) => {
                let path: Path = component_id.name().into();
                Ok(format!("{path}"))
            },
            TypedExpr::Reference(_typ, Referent::NonLocalComponent(submodule_element_id, component_id)) => {
                let path: Path = submodule_element_id.name().as_path().join(&component_id.name().into());
                let parts = path.parts();
                let sm = &parts[0];
                let port = &parts[1];
                Ok(format!("__TEMP_{sm}_{port}"))
            },
            TypedExpr::Word(_typ, w) => {
                let gs = self.gensym();
                let typ = expr.typ();
                let width_str = make_width_str(self.design, typ);
                writeln!(f, "    wire {width_str} {gs} = {};", w.value)?;
                Ok(gs)
            },
            TypedExpr::Cat(typ, args) => {
                let gs = self.gensym();
                let mut arg_ssas: Vec<SsaName> = vec![];
                for arg in args {
                    let arg_ssa = self.verilog_expr(arg.clone(), ctx.clone())?;
                    arg_ssas.push(arg_ssa);
                }
                let width_str = make_width_str(self.design, typ.clone());
                writeln!(f, "    wire {width_str} {gs} = {{{}}};", arg_ssas.join(", "))?;
                Ok(gs)
            },
            TypedExpr::Idx(_typ, subject, i) => {
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(subject.clone(), ctx)?;
                writeln!(f, "    wire {gs} = {subject_ssa}[{i}];")?;
                Ok(gs)
            },
            TypedExpr::IdxRange(typ, subject, j, i) => {
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(subject.clone(), ctx)?;
                let end = *j - 1;
                let width_str = make_width_str(self.design, typ.clone());
                writeln!(f, "    wire {width_str} {gs} = {subject_ssa}[{end}:{i}];")?;
                Ok(gs)
            },
            TypedExpr::MethodCall(_typ, subject, method, args) => {
                let gs = self.gensym();
                let subject_ssa = self.verilog_expr(subject.clone(), ctx.clone())?;
                let mut args_ssa: Vec<SsaName> = vec![];
                self.verilog_expr(subject.clone(), ctx.clone())?;
                for arg in args {
                    let arg_ssa = self.verilog_expr(arg.clone(), ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }
                let typ = expr.typ();
                let width_str = make_width_str(self.design, typ.clone());

                match method.as_str() {
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
            TypedExpr::Ctor(typ, ctor, args) => {
                let gs = self.gensym();

                let layout = self.design.union_layout(typ.clone())?;

                let mut args_ssa: Vec<SsaName> = vec![];
                for arg in args {
                    let arg_ssa = self.verilog_expr(arg.clone(), ctx.clone())?;
                    args_ssa.push(arg_ssa);
                }

                let width_str = make_width_str(self.design, typ.clone());

                let tag = self.design.union_ctor_tag(typ.clone(), ctor.clone())?;
                let fill = "1";

                write!(f, "    wire {width_str} {gs} = {{ ")?;
                for arg_ssa in args_ssa.iter().rev() {
                    write!(f, "{arg_ssa}, ")?;
                }
                let tag_width = layout.tag_width();
                writeln!(f, "{tag_width}'d{tag} }};")?;

                if layout.ctor_width(ctor.clone()) < layout.width() {
                    let bot_bit = layout.ctor_width(ctor.clone());
                    let top_bit = layout.width() - 1;
                    let bits = top_bit - bot_bit + 1;

                    writeln!(f, "    // fill remaining space with {fill}")?;
                    writeln!(f, "    assign {gs}[{top_bit}:{bot_bit}] = {bits}'b{};", fill.repeat(bits as usize))?;
                }

                Ok(gs)
            },
            TypedExpr::As(_typ, subject, _typ_ast) => {
                self.verilog_expr(subject.clone(), ctx.clone())
            },
            TypedExpr::If(_typ, c, a, b) => {
                let gs = self.gensym();
                let cond_ssa = self.verilog_expr(c.clone(), ctx.clone())?;
                let a_ssa = self.verilog_expr(a.clone(), ctx.clone())?;
                let b_ssa = self.verilog_expr(b.clone(), ctx.clone())?;
                let typ = expr.typ();
                let width_str = make_width_str(self.design, typ.clone());
                writeln!(f, "    wire {width_str} {gs} = {cond_ssa} ? {a_ssa} : {b_ssa};")?;
                Ok(gs)
            },
            /*
            TypedExpr::Let(typ, x, _ascription, e, b) => {
                let gs = self.gensym();
                let e_ssa = self.verilog_expr(e.clone(), ctx.clone())?;
                let new_ctx = ctx.extend(x.clone(), e_ssa);
                let b_ssa = self.verilog_expr(b.clone(), new_ctx)?;
                let width_str = make_width_str(self.virdant, typ.clone());
                writeln!(f, "    wire {width_str} {gs} = {b_ssa};")?;
                Ok(gs)
            },
*/
            TypedExpr::Match(_typ, subject, _ascription, arms) => {
                let gs = self.gensym_hint("match");
                let subject_ssa = self.verilog_expr(subject.clone(), ctx.clone())?;
                let typ = expr.typ();
                let layout = self.design.union_layout(subject.typ())?;
                let width_str = make_width_str(self.design, typ.clone());

                let tag_ssa = self.gensym();
                let tag_width = layout.tag_width();
                let tag_top = tag_width - 1;

                let mut arm_ssas: Vec<(Tag, Ident, SsaName)> = vec![];
                writeln!(f, "    // match arm")?;
                for TypedMatchArm(pat, e) in arms {
                    match pat {
                        TypedPat::At(_typ, ctor, pats) => {
                            writeln!(f, "    // case {ctor}")?;
                            let tag = layout.tag_for(ctor.clone());
                            let mut new_ctx = ctx.clone();
                            writeln!(f, "    // (pats are {pats:?})")?;
                            for (i, pat) in pats.iter().enumerate() {
                                let (offset, width) = layout.ctor_slot(ctor.clone(), i);
                                let width_minus_1 = width - 1;
                                if let TypedPat::Bind(_typ, x) = pat {
                                    let x_ssa = self.gensym_hint(&x.to_string());
                                    new_ctx = new_ctx.extend(x.clone(), x_ssa.clone());
                                    let bot_bit = offset;
                                    let top_bit = offset + width - 1;
                                    writeln!(f, "    // binding variable {x} to slot")?;
                                    writeln!(f, "    wire [{width_minus_1}:0] {x_ssa} = {subject_ssa}[{top_bit}:{bot_bit}];")?;
                                } else {
                                    panic!()
                                }
                            }
                            let arm_ssa = self.verilog_expr(e.clone(), new_ctx)?;
                            arm_ssas.push((tag, ctor.clone(), arm_ssa));
                        },
                        _ => todo!(),
                    }
                }

                writeln!(f, "    // project tag ({tag_width} bits)")?;
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

                writeln!(f, "    reg {width_str} {gs};")?;

                writeln!(f, "    wire {tag_width_str} {tag_ssa} = {subject_ssa}{subject_tag_idx};")?;

                writeln!(f, "    always @(*) begin")?;
                writeln!(f, "        case ({tag_ssa})")?;

                for (tag, ctor, arm_ssa) in &arm_ssas {
                    writeln!(f, "            // @{ctor}:")?;
                    writeln!(f, "            {tag}: {gs} <= {arm_ssa};")?;
                }

                writeln!(f, "            default: {gs} <= 32'bx;")?;
                writeln!(f, "        endcase")?;
                writeln!(f, "    end")?;

                Ok(gs)
            },
            TypedExpr::Struct(typ, _name, fields) => {
                let gs = self.gensym();
                writeln!(f, "    {gs} = ...{expr:?}")?;
                todo!()
            },
            _ => {
                let gs = self.gensym();
                writeln!(f, "    {gs} = ...{expr:?}")?;
                todo!()
            },
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
    */

pub enum Layout {
}

impl Layout {
    pub fn bitwidth(&self) -> usize {
        todo!()
    }
}
