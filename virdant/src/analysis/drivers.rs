use std::sync::Arc;

use bstr::BStr;
use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::analysis::component::{ComponentAnalysis, ComponentId};
use crate::analysis::symbols::SymbolId;
use crate::common::{DriverType, Flow};
use crate::db::Builder;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct DriverAnalysis {
    drivers: IndexMap<ComponentId, Vec<Driver>>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub enum Driver {
    Expr(DriverType, Location),
    Bidirectional(Location),
    If(DriverIf),
    Match(DriverMatch),
}

impl Driver {
    pub fn driver_type(&self) -> DriverType {
        match self {
            Driver::Expr(dt, _) => *dt,
            Driver::Bidirectional(_) => DriverType::Continuous,
            Driver::If(driver_if) => driver_if.driver_type,
            Driver::Match(driver_match) => driver_match.driver_type,
        }
    }

    pub fn location(&self) -> Option<Location> {
        match self {
            Driver::Expr(_, loc) => Some(loc.clone()),
            Driver::Bidirectional(loc) => Some(loc.clone()),
            Driver::If(driver_if) => driver_if.clauses.first().map(|(loc, _)| loc.clone()),
            Driver::Match(driver_match) => Some(driver_match.subject.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DriverIf {
    pub driver_type: DriverType,
    pub clauses: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}

#[derive(Debug, Clone)]
pub struct DriverMatch {
    pub driver_type: DriverType,
    pub subject: Location,
    pub arms: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}

impl DriverAnalysis {
    pub fn drivers(&self) -> &IndexMap<ComponentId, Vec<Driver>> {
        &self.drivers
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    #[allow(unused)]
    pub fn dump(&self, component_analysis: &ComponentAnalysis) {
        let mut entries: Vec<_> = self.drivers.iter().collect();
        entries.sort_by_key(|(comp_id, _)| {
            component_analysis
                .component(**comp_id)
                .map(|c| c.path())
                .unwrap_or_default()
        });
        for (comp_id, drivers) in entries {
            let name = component_analysis
                .component(*comp_id)
                .map(|c| c.path().to_str_lossy().into_owned())
                .unwrap_or_else(|| format!("{comp_id:?}"));
//            let drivertype = match c {
//            };
            eprintln!("{name}");
            for driver in drivers {
                dump_driver(driver, 1);
            }
        }
    }
}

fn dump_driver(driver: &Driver, level: usize) {
    let pad = "    ".repeat(level);
    match driver {
        Driver::Expr(driver_type, loc) => eprintln!("{pad} {driver_type:?}{loc:?}"),
        Driver::Bidirectional(loc) => eprintln!("{pad} Bidirectional{loc:?}"),
        Driver::If(driver_if) => {
            for (i, (cond_loc, sub_driver)) in driver_if.clauses.iter().enumerate() {
                let keyword = if i == 0 { "if" } else { "else if" };
                eprintln!("{pad}{keyword} {cond_loc:?}:");
                dump_driver(sub_driver, level + 1);
            }
            if let Some(else_driver) = &driver_if.else_clause {
                eprintln!("{pad}else:");
                dump_driver(else_driver, level + 1);
            }
        }
        Driver::Match(driver_match) => {
            eprintln!("{pad}match {:?}:", driver_match.subject);
            for (pat_loc, sub_driver) in &driver_match.arms {
                eprintln!("{pad}  arm {pat_loc:?}:");
                dump_driver(sub_driver, level + 1);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                eprintln!("{pad}  else:");
                dump_driver(else_driver, level + 1);
            }
        }
    }
}

pub(crate) fn build_driver_analysis(
    builder: &mut Builder,
    symbol_id: SymbolId,
) -> Arc<DriverAnalysis> {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(symbol_id).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(symbol_id);

    let (drivers, diagnostics) = collect_block_drivers(item_ast.children(), &component_analysis, None);

//    driver_analysis.dump(&component_analysis);

    Arc::new(DriverAnalysis {
        drivers,
        diagnostics,
    })
}

fn collect_block_drivers(
    stmts: Vec<AstNode<'_>>,
    component_analysis: &ComponentAnalysis,
    it_context: Option<&BStr>,
) -> (IndexMap<ComponentId, Vec<Driver>>, Vec<Diagnostic>) {
    let mut result: IndexMap<ComponentId, Vec<Driver>> = IndexMap::new();
    let mut diagnostics: Vec<Diagnostic> = vec![];

    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::Driver(driver) => {
                let Some(target) = stmt.target() else { continue };
                let mut target_str = stmt.parsing.string(target).to_owned();
                if let Some(ctx) = it_context {
                    if target_str.starts_with(b"it.") {
                        let suffix = target_str[3..].to_owned();
                        target_str.clear();
                        target_str.extend_from_slice(ctx);
                        target_str.push(b'.');
                        target_str.extend_from_slice(&suffix);
                    } else if target_str == b"it" {
                        target_str = ctx.to_owned();
                    }
                } else if target_str.starts_with(b"it.") || target_str == b"it" {
                    diagnostics.push(crate::diagnostics::ItNotInItBlock {
                        region: stmt.region(),
                    }.into());
                }
                let Some(component) = component_analysis.resolve(target_str.as_bstr()) else {
                    if !target_str.starts_with(b"it.") && target_str != b"it" {
                        diagnostics.push(crate::diagnostics::UnresolvedComponent {
                            region: stmt.region(),
                            path: target_str,
                        }.into());
                    }
                    continue;
                };
                let expr = stmt.driver().unwrap().location();
                result.entry(component.id()).or_default().push(Driver::Expr(driver.driver_type, expr));
            }
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);
                let Some(lhs_path) = lhs_node.path() else { continue };
                let Some(rhs_path) = rhs_node.path() else { continue };
                let mut lhs_str = parsing.string(lhs_path).to_owned();
                let mut rhs_str = parsing.string(rhs_path).to_owned();

                if let Some(ctx) = it_context {
                    if lhs_str.starts_with(b"it.") {
                        let suffix = lhs_str[3..].to_owned();
                        lhs_str.clear();
                        lhs_str.extend_from_slice(ctx);
                        lhs_str.push(b'.');
                        lhs_str.extend_from_slice(&suffix);
                    } else if lhs_str == b"it" {
                        lhs_str = ctx.to_owned();
                    }
                    if rhs_str.starts_with(b"it.") {
                        let suffix = rhs_str[3..].to_owned();
                        rhs_str.clear();
                        rhs_str.extend_from_slice(ctx);
                        rhs_str.push(b'.');
                        rhs_str.extend_from_slice(&suffix);
                    } else if rhs_str == b"it" {
                        rhs_str = ctx.to_owned();
                    }
                } else {
                    if lhs_str.starts_with(b"it.") || lhs_str == b"it" {
                        diagnostics.push(crate::diagnostics::ItNotInItBlock {
                            region: lhs_node.region(),
                        }.into());
                    }
                    if rhs_str.starts_with(b"it.") || rhs_str == b"it" {
                        diagnostics.push(crate::diagnostics::ItNotInItBlock {
                            region: rhs_node.region(),
                        }.into());
                    }
                }

                let lhs_prefix = format!("{}.", lhs_str.to_str_lossy());
                let rhs_prefix = format!("{}.", rhs_str.to_str_lossy());

                for (lhs_full_path, lhs_component) in component_analysis.components() {
                    let lhs_path_str = lhs_full_path.to_str_lossy();
                    if !lhs_path_str.starts_with(&lhs_prefix) {
                        continue;
                    }

                    let suffix = &lhs_path_str[lhs_prefix.len()..];
                    let rhs_full_path = BString::from(format!("{}{}", rhs_prefix, suffix).into_bytes());

                    let Some(rhs_component) = component_analysis.resolve(rhs_full_path.as_bstr()) else {
                        continue;
                    };

                    let location = stmt.location();
                    match (lhs_component.flow(), rhs_component.flow()) {
                        (Flow::Source, Flow::Sink) => {
                            result.entry(rhs_component.id()).or_default()
                                .push(Driver::Bidirectional(location));
                        }
                        (Flow::Sink, Flow::Source) => {
                            result.entry(lhs_component.id()).or_default()
                                .push(Driver::Bidirectional(location));
                        }
                        _ => {}
                    }
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                if children.len() == 2 {
                    let it_block = &children[1];
                    if matches!(it_block.payload(), AstNodePayload::It) {
                        let block = &it_block.children()[0];

                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };

                        let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, Some(name_with_context.as_bstr()));
                        for (comp_id, mut drivers) in block_drivers {
                            result.entry(comp_id).or_default().append(&mut drivers);
                        }
                        diagnostics.append(&mut block_diags);
                    }
                }
            }
            AstNodePayload::Socket(socket) => {
                let name = stmt.parsing.string(socket.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];

                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };

                        let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, Some(name_with_context.as_bstr()));
                        for (comp_id, mut drivers) in block_drivers {
                            result.entry(comp_id).or_default().append(&mut drivers);
                        }
                        diagnostics.append(&mut block_diags);
                    }
                }
            }
            AstNodePayload::Component(component) => {
                let name = stmt.parsing.string(component.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];

                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };

                        let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, Some(name_with_context.as_bstr()));
                        for (comp_id, mut drivers) in block_drivers {
                            result.entry(comp_id).or_default().append(&mut drivers);
                        }
                        diagnostics.append(&mut block_diags);
                    }
                }
            }
            AstNodePayload::ModDefStmtIf => {
                // Children: [cond_0, block_0, cond_1, block_1, ..., (else_block?)]
                // An else block is present when the total number of children is odd;
                // when even, all children are cond/block pairs with no trailing else.
                let children = stmt.children();
                let has_else = children.len() % 2 == 1;
                let num_cond_block_pairs = children.len() / 2;

                // Recursively collect drivers from each if/elif clause.
                let clause_drivers: Vec<(Location, IndexMap<ComponentId, Vec<Driver>>)> =
                    (0..num_cond_block_pairs)
                        .map(|i| {
                            let cond = &children[2 * i];
                            let block = &children[2 * i + 1];
                            let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, it_context);
                            diagnostics.append(&mut block_diags);
                            (cond.location(), block_drivers)
                        })
                        .collect();

                // Recursively collect drivers from the else block (if present).
                let else_drivers = if has_else {
                    let (else_drivers, mut else_diags) = collect_block_drivers(children.last().unwrap().children(), component_analysis, it_context);
                    diagnostics.append(&mut else_diags);
                    else_drivers
                } else {
                    IndexMap::new()
                };

                // Gather all component IDs driven in any clause or the else block.
                let mut all_components: IndexSet<ComponentId> = IndexSet::new();
                for (_, cd) in &clause_drivers {
                    all_components.extend(cd.keys().copied());
                }
                all_components.extend(else_drivers.keys().copied());

                // Build a Driver::If for each driven component and merge into the result.
                for comp_id in all_components {
                    let clauses: Vec<(Location, Box<Driver>)> = clause_drivers.iter()
                        .filter_map(|(cond_loc, cd)| {
                            cd.get(&comp_id)?.first().map(|d| (cond_loc.clone(), Box::new(d.clone())))
                        })
                        .collect();

                    let else_clause = else_drivers.get(&comp_id)
                        .and_then(|ds| ds.first())
                        .map(|d| Box::new(d.clone()));

                    // Infer the driver type from the first available sub-driver.
                    let driver_type = clauses.first()
                        .map(|(_, d)| d.driver_type())
                        .or_else(|| else_clause.as_ref().map(|d| d.driver_type()))
                        .unwrap_or(DriverType::Continuous);

                    result.entry(comp_id).or_default().push(Driver::If(DriverIf {
                        driver_type,
                        clauses,
                        else_clause,
                    }));
                }
            }
            AstNodePayload::ModDefStmtMatch => {
                // Children: [subject, pat_0, block_0, pat_1, block_1, ..., pat_N, block_N]
                let children = stmt.children();
                let subject = &children[0];
                let num_arms = (children.len() - 1) / 2;

                // Recursively collect drivers from each arm's block.
                let arm_drivers: Vec<(Location, IndexMap<ComponentId, Vec<Driver>>)> =
                    (0..num_arms)
                        .map(|i| {
                            let pat = &children[2 * i + 1];
                            let block = &children[2 * i + 2];
                            let (block_drivers, mut block_diags) = collect_block_drivers(block.children(), component_analysis, it_context);
                            diagnostics.append(&mut block_diags);
                            (pat.location(), block_drivers)
                        })
                        .collect();

                // Gather all component IDs driven in any arm.
                let mut all_components: IndexSet<ComponentId> = IndexSet::new();
                for (_, ad) in &arm_drivers {
                    all_components.extend(ad.keys().copied());
                }

                // Build a Driver::Match for each driven component and merge into the result.
                for comp_id in all_components {
                    let arms: Vec<(Location, Box<Driver>)> = arm_drivers.iter()
                        .filter_map(|(pat_loc, ad)| {
                            ad.get(&comp_id)?.first().map(|d| (pat_loc.clone(), Box::new(d.clone())))
                        })
                        .collect();

                    let driver_type = arms.first()
                        .map(|(_, d)| d.driver_type())
                        .unwrap_or(DriverType::Continuous);

                    result.entry(comp_id).or_default().push(Driver::Match(DriverMatch {
                        driver_type,
                        subject: subject.location(),
                        arms,
                        else_clause: None,
                    }));
                }
            }
            _ => {}
        }
    }

    (result, diagnostics)
}
