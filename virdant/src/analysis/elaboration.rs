//! Produces the `Elaboration`, the flat signal-level view of a module
//! after inlining submodule and socket instantiations, with each
//! `ElaboratedComponent` carrying its fully-qualified path, type,
//! driver, and optional alias/clock references, indexed by dense
//! `SignalId`s.

use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::{IndexMap, IndexSet};

use crate::analysis::Location;
use crate::analysis::component::{ComponentAnalysis, ComponentId};
use crate::analysis::dependency::{EdgeKind, resolve_it_path_to_owned};
use crate::analysis::drivers::{Driver, DriverAnalysis};
use crate::analysis::memory_image::{memory_image_annotation, MemoryImage};
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType, SocketRole};
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;
use crate::analysis::component::Component;
use crate::common::Flow;
use crate::syntax::ast::AstNode;
use crate::types::substitute::substitute;
use crate::types::typ::Type;

#[derive(Debug)]
pub struct Elaboration {
    components: Vec<ElaboratedComponent>,
    path_to_id: IndexMap<BString, SignalId>,
    #[allow(dead_code)]
    modules: Vec<ElaboratedModule>,
    #[allow(dead_code)]
    sockets: Vec<ElaboratedSocket>,
    /// Flat `(moddef, prefix)` list mirroring `modules`, for external
    /// consumers (e.g. ext-module builtins).
    module_list: Vec<(SymbolId, BString)>,
}

/// A module instance discovered during elaboration.
///
/// Records that a group of components under a common prefix
/// belongs to an instantiation of a particular moddef.
#[derive(Debug)]
pub struct ElaboratedModule {
    /// The module definition symbol (e.g., `Core`).
    #[allow(dead_code)]
    moddef: SymbolId,
    /// Fully-qualified path prefix (e.g., `top.core`).
    #[allow(dead_code)]
    prefix: BString,
}

/// A socket instance discovered during elaboration.
///
/// Records that a group of channel components under a common
/// prefix belongs to an instantiation of a particular socketdef.
#[derive(Debug)]
pub struct ElaboratedSocket {
    /// The socket definition symbol (e.g., `Mem`).
    #[allow(dead_code)]
    socketdef: SymbolId,
    /// Client or Server.
    role: SocketRole,
    /// Fully-qualified path prefix (e.g., `top.core.mem`).
    prefix: BString,
}

impl ElaboratedSocket {
    /// The socket definition symbol (e.g. the `Mem` of a
    /// `socket mem of Mem[...]` instantiation).
    pub fn socketdef(&self) -> SymbolId {
        self.socketdef
    }

    /// The socket instance's role.
    pub fn role(&self) -> SocketRole {
        self.role
    }

    /// The fully-qualified path prefix of the socket instance's
    /// channels (e.g. `top.core.mem`).
    pub fn prefix(&self) -> &BStr {
        self.prefix.as_bstr()
    }
}

#[derive(Debug)]
pub struct ElaboratedComponent {
    id: SignalId,
    path: BString,
    typ: Type,
    component_kind: ComponentKind,
    driver_type: DriverType,
    driver: Option<Driver>,
    /// RHS expression of the component's `initial` statement, if any.
    initial_expr: Option<Location>,
    alias: Option<SignalId>,
    clock: Option<SignalId>,
    /// The mono type arguments of the generic mod def instance this
    /// component belongs to (`[]` when its module is non-generic).  The
    /// simulator substitutes these into the (generic) analysis types
    /// of driver expressions inside the instance.
    env: Vec<Type>,
    /// The scope in which the driver expression's references are
    /// resolved.  For most components this is their own module; for
    /// `incoming` ports it is the parent module; for socket channels
    /// it is the module that declares the socket instance.
    driver_scope: BString,
    /// Where `driver` was written, in module-level terms (see
    /// `DriverSite`).  `None` when the component has no driver.
    driver_site: Option<DriverSite>,
    /// For a `memory` component: the number of words it holds.  The
    /// component's `typ` is the *element* type (a memory introduces no
    /// new type), so the depth lives here.
    depth: Option<u32>,
    /// For a `memory` component: its image annotation (`@image`,
    /// `@image_hex`, or `@image_inline`) resolved to a `MemoryImage` —
    /// its format and the source of its bytes.  `None` otherwise.
    image: Option<MemoryImage>,
}

/// The module-level identity of a component's driver statement: the
/// module definition whose body contains the statement, and the component
/// of that module's `ComponentAnalysis` which the statement targets.
///
/// An elaborated component's driver is not always written in the
/// component's own module (`incoming` ports and parent-driven socket
/// channels are driven from the parent), so the elaboration records the
/// site when it looks the driver up and later uses it to consult that
/// module's per-module analyses — in particular its `DependencyGraph`,
/// whose reference resolution already handles `it` with the statement's
/// it-block context — instead of re-resolving paths itself.
#[derive(Debug, Clone, Copy)]
struct DriverSite {
    moddef: SymbolId,
    target: ComponentId,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SignalId(usize);

impl SignalId {
    /// Dense 0-based index assigned by `build_elaboration`.  Suitable for
    /// indexing into a `Vec` sized to `Elaboration::components().len()`.
    pub fn index(self) -> usize {
        self.0
    }
}

impl Elaboration {
    /// The module instances discovered during elaboration:
    /// `(path prefix, moddef symbol)` pairs.
    pub fn modules(&self) -> &[(SymbolId, BString)] {
        &self.module_list
    }

    pub fn resolve<P: AsRef<BStr>>(&self, path: P) -> Option<&ElaboratedComponent> {
        self.path_to_id
            .get(path.as_ref())
            .map(|id| &self.components[id.0])
    }

    pub fn components(&self) -> &[ElaboratedComponent] {
        &self.components
    }

    /// The socket instances discovered during elaboration.
    pub fn sockets(&self) -> &[ElaboratedSocket] {
        &self.sockets
    }

    pub fn component(
        &self,
        elaborated_component_id: SignalId,
    ) -> &ElaboratedComponent {
        &self.components[elaborated_component_id.0]
    }

    pub fn dump(&self) {
        for component in &self.components {
            let alias_str = if let Some(alias_id) = &component.alias {
                let alias = &self.components[alias_id.0];
                format!(" (alias: {})", &alias.path)
            } else {
                String::new()
            };

            let clock_str = if let Some(clock_id) = &component.clock {
                let clock = &self.components[clock_id.0];
                format!(" (on: {})", &clock.path)
            } else {
                String::new()
            };

            println!("{} : {:?}{alias_str}{clock_str}", component.path, component.typ);
        }
    }
}

impl ElaboratedComponent {
    pub fn id(&self) -> SignalId {
        self.id
    }

    pub fn path(&self) -> &BString {
        &self.path
    }

    pub fn typ(&self) -> Type {
        self.typ.clone()
    }

    /// The mono type arguments of the generic mod def instance this
    /// component belongs to (`[]` when its module is non-generic).
    pub fn env(&self) -> &[Type] {
        &self.env
    }

    pub fn driver_type(&self) -> DriverType {
        self.driver_type
    }

    pub fn driver(&self) -> Option<&Driver> {
        self.driver.as_ref()
    }

    pub fn initial_expr(&self) -> Option<&Location> {
        self.initial_expr.as_ref()
    }

    pub fn is_reg(&self) -> bool {
        self.driver_type == DriverType::Latched
    }

    pub fn clock(&self) -> Option<SignalId> {
        self.clock
    }

    /// The declared depth of a `memory` component, or `None` for any
    /// other kind.
    pub fn depth(&self) -> Option<u32> {
        self.depth
    }

    /// The image annotation of a `memory` component, resolved to its
    /// format and the source of its bytes, or `None`.
    pub fn image(&self) -> Option<&MemoryImage> {
        self.image.as_ref()
    }

    pub fn component_kind(&self) -> ComponentKind {
        self.component_kind
    }

    /// The peer component this one is connected to by a `:=:` socket
    /// connection (the Sink side copies the Source side's value).
    pub fn alias(&self) -> Option<SignalId> {
        self.alias
    }

    /// The scope in which the driver expression's references are
    /// resolved (see the field docs).
    pub fn driver_scope(&self) -> &BStr {
        self.driver_scope.as_bstr()
    }
}

/// Context supplied by the parent module when recursing into a submodule.
/// Needed so that `incoming` ports can look up their driver in the parent scope.
struct ParentCtx {
    /// The parent module definition (the scope its drivers are written in).
    moddef: SymbolId,
    component_analysis: Arc<ComponentAnalysis>,
    driver_analysis: Arc<DriverAnalysis>,
    /// The instance name as it appears in the parent (e.g. `"gcd"`), used to
    /// construct the sub-port lookup path `"{instance}.{port}"`.
    instance_name: String,
}

/// Look up the first driver of the component at module-level `local_path`
/// in `moddef`'s analyses, together with its `DriverSite`.
fn lookup_driver(
    moddef: SymbolId,
    component_analysis: &ComponentAnalysis,
    driver_analysis: &DriverAnalysis,
    local_path: &BStr,
) -> Option<(Driver, DriverSite)> {
    let target = component_analysis.resolve(local_path)?.id();
    let driver = driver_analysis.drivers().get(&target)?.first().cloned()?;
    Some((driver, DriverSite { moddef, target }))
}

/// Look up the driver of a submodule's port or socket channel
/// (`{instance}.{local_path}`) as written in the parent module.
fn lookup_parent_driver(ctx: &ParentCtx, local_path: &BStr) -> Option<(Driver, DriverSite)> {
    let sub_path: BString =
        format!("{}.{}", ctx.instance_name, local_path.to_str_lossy()).into();
    lookup_driver(ctx.moddef, &ctx.component_analysis, &ctx.driver_analysis, sub_path.as_bstr())
}

/// Recursively traverse the module at `moddef`, collecting all fully-qualified
/// component paths into `components`.  `prefix` is the dotted path so far
/// (e.g. `"top"` or `"top.foo"`).  `env` holds the mono type arguments of the
/// generic mod def instance being elaborated (`[]` for a non-generic module);
/// component types are substituted with them, and nested instantiations
/// compose their written arguments with them.
fn elaborate_module(
    builder: &mut Builder,
    moddef: SymbolId,
    prefix: &str,
    components: &mut Vec<ElaboratedComponent>,
    modules: &mut Vec<ElaboratedModule>,
    sockets: &mut Vec<ElaboratedSocket>,
    module_list: &mut Vec<(SymbolId, BString)>,
    connects: &mut Vec<(BString, BString)>,
    parent_ctx: Option<ParentCtx>,
    env: Vec<Type>,
) {
    modules.push(ElaboratedModule {
        moddef,
        prefix: BString::from(prefix),
    });
    module_list.push((moddef, BString::from(prefix)));

    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(moddef);
    let driver_analysis = builder.get_driver_analysis(moddef);

    // Local names already elaborated from the module AST (components by
    // name, submodule/socket instances by instance name); synthesized
    // platform ports not in this set are appended after the walk.
    let mut handled: IndexSet<BString> = IndexSet::new();

    // Implicit-clock rule: if this module declares *exactly one*
    // Clock-typed component, `reg`/`outgoing reg` components may omit
    // their `on <CLOCK>` clause, and it is as if the user had written
    // `on <the_one_clock>`.  Any other number of Clock-typed components
    // (zero or more than one) requires an explicit `on` clause (reported
    // by the package analysis).
    //
    // The clock component may be declared after the reg in the source,
    // so implicit clocks are resolved after this module's statements are
    // walked: `pending_implicit_clocks` holds (component index, clock
    // name) pairs to patch once every component of this module is in
    // `components`.
    let implicit_clock = component_analysis
        .implicit_clock()
        .and_then(|id| component_analysis.component(id))
        .map(|component| component.path());
    let mut pending_implicit_clocks: Vec<(usize, BString)> = vec![];

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let name = parsing.string(component.name);
                handled.insert(name.to_owned());
                let path: BString = format!("{prefix}.{}", name.to_str_lossy()).into();

                let typ_node = stmt.typ().unwrap();
                let typ = builder.get_type_at(typ_node.location()).unwrap();
                // Substitute the def's generic parameters with this
                // instance's type arguments (mono after this point).
                let typ = substitute(&typ, &env);

                let driver_type = match component.kind {
                    ComponentKind::Reg | ComponentKind::OutgoingReg => DriverType::Latched,
                    _ => DriverType::Continuous,
                };

                // `incoming` ports are driven from the parent scope.
                // All other kinds (outgoing, wire, reg) are driven within this module.
                let (driver, driver_site) = if component.kind == ComponentKind::Incoming {
                    parent_ctx.as_ref().and_then(|ctx| lookup_parent_driver(ctx, name))
                } else {
                    lookup_driver(moddef, &component_analysis, &driver_analysis, name)
                }
                .unzip();

                let initial_expr = component_analysis
                    .resolve(name)
                    .and_then(|c| driver_analysis.initials().get(&c.id())?.first().cloned());

                let id = SignalId(components.len());
                let clock = if let Some(clock_node) = stmt.clock() {
                    // Resolve the clock reference (e.g. `clock` in `reg x : T on clock`)
                    // to the ElaboratedComponentId of the already-accumulated clock component
                    // (e.g. `top.clock`).  The incoming clock port is always declared before
                    // any reg in the source, so it is guaranteed to be in `components` already.
                    clock_node.path().and_then(|interned| {
                        let clock_name = parsing.string(interned).to_str_lossy().into_owned();
                        let full_path: BString = format!("{prefix}.{clock_name}").into();
                        components
                            .iter()
                            .position(|c| c.path == full_path)
                            .map(SignalId)
                    })
                } else if (component.kind == ComponentKind::Reg
                    || component.kind == ComponentKind::OutgoingReg
                    || component.kind == ComponentKind::Memory)
                    && let Some(implicit_clock_name) = &implicit_clock
                {
                    // The reg omitted its `on` clause and this module has a
                    // unique implicit clock: defer resolution until every
                    // component of this module has been accumulated.
                    pending_implicit_clocks.push((components.len(), implicit_clock_name.clone()));
                    None
                } else {
                    None
                };
                let driver_scope: BString = if component.kind == ComponentKind::Incoming {
                    // Driven from the parent module's scope: drop the
                    // instance segment (and the module segment) from the path.
                    let last_dot = path.rfind_byte(b'.').expect("elaborated path always has at least one dot");
                    let second_last = path[..last_dot].rfind_byte(b'.').unwrap_or(0);
                    BString::from(path[..second_last].to_vec())
                } else {
                    BString::from(prefix)
                };
                components.push(ElaboratedComponent {
                    id,
                    path,
                    typ,
                    component_kind: component.kind,
                    driver_type,
                    driver,
                    initial_expr,
                    alias: None,
                    clock,
                    env: env.clone(),
                    driver_scope,
                    driver_site,
                    depth: component_analysis
                        .resolve(name)
                        .and_then(|c| c.depth()),
                    image: if component.kind == ComponentKind::Memory {
                        memory_image_annotation(
                            &parsing, &stmt,
                        )
                        .map(|(kind, arg, _)| {
                            MemoryImage::resolve(
                                builder.db().project_root(),
                                kind,
                                arg.as_bstr(),
                            )
                        })
                    } else {
                        None
                    },
                });
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name).to_str_lossy().into_owned();
                handled.insert(instance_name.clone().into());
                let ofness_node = stmt.child(1);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let packages = builder.get_packages();
                let submodule_name = parsing.string(ofness.name);
                let qualifier = ofness
                    .package
                    .and_then(|pkg| packages.id(parsing.string(pkg)));
                let submodule_symbol =
                    match crate::analysis::package::resolve_item_reference(
                        builder,
                        submodule_name,
                        location.package(),
                        qualifier,
                    ) {
                        Some(symbol) => symbol,
                        None => continue,
                    };
                let child_prefix = format!("{prefix}.{instance_name}");
                let ctx = ParentCtx {
                    moddef,
                    component_analysis: component_analysis.clone(),
                    driver_analysis: driver_analysis.clone(),
                    instance_name: instance_name.clone(),
                };
                // Compose this instance's written type arguments with
                // the current (mono) environment, giving the child's
                // mono environment.  A non-generic target has no
                // arguments, so the child environment is empty.
                let child_env: Vec<Type> = crate::types::typedef::instantiation_target(builder, &stmt)
                    .map(|(_symbol, written)| {
                        written
                            .iter()
                            .map(|arg| substitute(arg, &env))
                            .collect()
                    })
                    .unwrap_or_default();
                elaborate_module(
                    builder,
                    submodule_symbol.id,
                    &child_prefix,
                    components,
                    modules,
                    sockets,
                    module_list,
                    connects,
                    Some(ctx),
                    child_env,
                );

                // `:=:` connections written inside the instance's it-block.
                if let Some(it_block) = stmt.children().into_iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It))
                {
                    let block = &it_block.children()[0];
                    collect_connects(
                        &component_analysis,
                        &block.children(),
                        Some(instance_name.as_bytes()),
                        prefix,
                        connects,
                    );
                }
            }
            AstNodePayload::Socket(socket) => {
                let instance_name = parsing.string(socket.name);
                handled.insert(instance_name.to_owned());
                let ofness_node = stmt.child(1);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload()
                else {
                    continue;
                };
                let packages = builder.get_packages();
                let socket_name = parsing.string(ofness.name);
                let qualifier = ofness
                    .package
                    .and_then(|pkg| packages.id(parsing.string(pkg)));
                let socket_symbol = match crate::analysis::package::resolve_item_reference(
                    builder,
                    socket_name,
                    location.package(),
                    qualifier,
                ) {
                    Some(symbol) => symbol,
                    None => continue,
                };

                let socket_prefix: BString = format!(
                    "{prefix}.{}",
                    instance_name.to_str_lossy()
                )
                .into();

                sockets.push(ElaboratedSocket {
                    socketdef: socket_symbol.id,
                    role: socket.role,
                    prefix: socket_prefix.clone(),
                });

                // Each channel of the socketdef becomes an elaborated
                // component at `{prefix}.{instance}.{channel}`.  The
                // side this module drives (client cosi / server soci)
                // takes its driver from this module's driver analysis;
                // the other side is driven by its peer through the
                // `:=:` connection (resolved below via `connects`).
                let socket_args: Vec<Type> =
                    crate::types::typedef::instantiation_target(builder, &stmt)
                        .map(|(_symbol, written)| {
                            written.iter().map(|arg| substitute(arg, &env)).collect()
                        })
                        .unwrap_or_default();

                let socket_def_location = socket_symbol.location();
                let socket_def_parsing = builder.get_parsing(socket_def_location.package());
                let socket_def_ast =
                    socket_def_parsing.ast_node(socket_def_location.ast_node_id());
                for channel_stmt in socket_def_ast.children() {
                    let AstNodePayload::Channel(channel) = channel_stmt.payload() else {
                        continue;
                    };
                    let channel_name = socket_def_parsing.string(channel.name);
                    let path: BString = format!(
                        "{prefix}.{}.{}",
                        instance_name.to_str_lossy(),
                        channel_name.to_str_lossy()
                    )
                    .into();
                    let Ok(typ) = builder.get_type_at(channel_stmt.typ().unwrap().location())
                        else { continue };
                    let typ = substitute(&typ, &socket_args);
                    let local_path: BString = format!("{}.{}",
                        instance_name.to_str_lossy(), channel_name.to_str_lossy()).into();
                    // The channel may be driven by this module's socket
                    // it-block, or (like `incoming` ports) from the parent
                    // module's scope (`sub.socket.channel := ...`).
                    let (found, parent_driven) = match lookup_driver(
                        moddef, &component_analysis, &driver_analysis, local_path.as_bstr(),
                    ) {
                        Some(found) => (Some(found), false),
                        None => (
                            parent_ctx.as_ref().and_then(|ctx| {
                                lookup_parent_driver(ctx, local_path.as_bstr())
                            }),
                            true,
                        ),
                    };
                    let (driver, driver_site) = found.unzip();
                    let id = SignalId(components.len());
                    // Socket-body drivers are written in the module
                    // that declares the socket instance; parent-driven
                    // channels resolve in the parent module's scope (the
                    // current prefix minus its instance segment).
                    let driver_scope: BString = match prefix.rfind('.') {
                        Some(dot) if parent_driven => BString::from(&prefix[..dot]),
                        _ => BString::from(prefix),
                    };
                    components.push(ElaboratedComponent {
                        id,
                        path,
                        typ,
                        component_kind: ComponentKind::Net,
                        driver_type: DriverType::Continuous,
                        driver,
                        initial_expr: None,
                        alias: None,
                        clock: None,
                        env: env.clone(),
                        driver_scope,
                        driver_site,
                        depth: None,
                        image: None,
                    });
                }

                // `:=:` connections written inside the socket instance's
                // it-block.
                if let Some(it_block) = stmt.children().into_iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It))
                {
                    let block = &it_block.children()[0];
                    collect_connects(
                        &component_analysis,
                        &block.children(),
                        Some(instance_name.to_str_lossy().as_bytes()),
                        prefix,
                        connects,
                    );
                }
            }
            AstNodePayload::BidirectionalDriver => {
                collect_connects(
                    &component_analysis,
                    std::slice::from_ref(&stmt),
                    None,
                    prefix,
                    connects,
                );
            }
            _ => {}
        }
    }

    // Resolve deferred implicit clocks now that every component of this
    // module is in `components`, regardless of declaration order.
    for (reg_index, clock_name) in pending_implicit_clocks {
        let full_path: BString = format!("{prefix}.{clock_name}").into();
        if let Some(clock_id) = components
            .iter()
            .position(|c| c.path == full_path)
            .map(SignalId)
        {
            components[reg_index].clock = Some(clock_id);
        }
    }

    // Platform-default synthesis: a module bound to a platform via a
    // `for` clause implicitly declares every platform port.  Ports
    // missing from the module AST were synthesized into the component
    // analysis (outgoing ones driven by the platform's `default
    // <EXPR>`); elaborate them here so the simulation sees them.
    for (local_path, component) in component_analysis.components() {
        if !component.synthetic() || handled.contains(&local_path) {
            continue;
        }
        let component_kind = match component.kind() {
            Some(kind) => kind,
            None => continue,
        };
        let path: BString = format!("{prefix}.{}", local_path.to_str_lossy()).into();
        let typ = component
            .typ()
            .map(|t| substitute(&t, &env))
            .unwrap_or(Type::Bit);
        let (driver, driver_site) = if component_kind == ComponentKind::Incoming {
            // Incoming ports are driven from the parent module's scope.
            parent_ctx.as_ref().and_then(|ctx| lookup_parent_driver(ctx, local_path.as_bstr()))
        } else {
            // Outgoing: driver analysis drives it with the platform's
            // default expression.
            driver_analysis
                .drivers()
                .get(&component.id())
                .and_then(|ds| ds.first().cloned())
                .map(|d| (d, DriverSite { moddef, target: component.id() }))
        }
        .unzip();
        let driver_scope: BString = if component_kind == ComponentKind::Incoming {
            let last_dot = path.rfind_byte(b'.').expect("elaborated path always has at least one dot");
            let second_last = path[..last_dot].rfind_byte(b'.').unwrap_or(0);
            BString::from(path[..second_last].to_vec())
        } else {
            BString::from(prefix)
        };
        let id = SignalId(components.len());
        components.push(ElaboratedComponent {
            id,
            path,
            typ,
            component_kind,
            driver_type: DriverType::Continuous,
            driver,
            initial_expr: None,
            alias: None,
            clock: None,
            env: env.clone(),
            driver_scope,
            driver_site,
            depth: None,
            image: None,
        });
    }
}

pub(crate) fn build_elaboration(builder: &mut Builder, top: SymbolId) -> Arc<Elaboration> {
    let mut components: Vec<ElaboratedComponent> = vec![];
    let mut modules: Vec<ElaboratedModule> = vec![];
    let mut sockets: Vec<ElaboratedSocket> = vec![];
    let mut module_list: Vec<(SymbolId, BString)> = vec![];
    let mut connects: Vec<(BString, BString)> = vec![];
    elaborate_module(
        builder, top, "top",
        &mut components, &mut modules, &mut sockets, &mut module_list, &mut connects,
        None,
        vec![],
    );

    // Build a reverse index: fully-elaborated path -> ElaboratedComponentId.
    let path_to_id: IndexMap<BString, SignalId> = components
        .iter()
        .enumerate()
        .map(|(i, c)| (c.path.clone(), SignalId(i)))
        .collect();

    // Resolve the socket connections (`a :=: b`): each pair routes the
    // Source side's channel value into the Sink side's channel.  The
    // connects carry fully-elaborated paths, recorded during the walk.
    for (source_path, sink_path) in connects.drain(..) {
        match (path_to_id.get(&source_path), path_to_id.get(&sink_path)) {
            (Some(source_id), Some(sink_id)) => {
                components[(*sink_id).index()].alias = Some(*source_id);
            }
            _ => {
                eprintln!("elaboration: unresolved socket connection {source_path:?} :=: {sink_path:?}");
            }
        }
    }

    // Resolve aliases: when a component's driver is a bare reference expression,
    // find the ElaboratedComponentId it names.  (Socket-connection aliases
    // set above take precedence.)
    let aliases: Vec<Option<SignalId>> = components
        .iter()
        .map(|c| resolve_alias(c, &path_to_id, builder))
        .collect();

    for (component, alias) in components.iter_mut().zip(aliases) {
        if component.alias.is_none() {
            component.alias = alias;
        }
    }

    Arc::new(Elaboration { components, path_to_id, modules, sockets, module_list })
}

/// Collect every `:=:` socket connection under `stmts` (which may
/// recurse into submodule/socket it-blocks) and record, for each
/// channel pair, the fully-elaborated `(source_path, sink_path)`.
///
/// Mirrors `collect_bidirectional_assigns` in `verilog/conversion.rs`:
/// `it.*` paths are rewritten against the it-block context, channels
/// are paired by matching suffixes under the two instance paths, and
/// the Flow direction decides which side drives.
fn collect_connects(
    component_analysis: &ComponentAnalysis,
    stmts: &[AstNode<'_>],
    it_context: Option<&[u8]>,
    prefix: &str,
    connects: &mut Vec<(BString, BString)>,
) {
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let children = stmt.children();
                let (Some(lhs_node), Some(rhs_node)) = (children.first(), children.get(1)) else {
                    continue;
                };
                let (Some(lhs_path), Some(rhs_path)) = (lhs_node.path(), rhs_node.path()) else {
                    continue;
                };
                let lhs = resolve_it_path_to_owned(
                    parsing.string(lhs_path),
                    it_context.map(BStr::new),
                );
                let rhs = resolve_it_path_to_owned(
                    parsing.string(rhs_path),
                    it_context.map(BStr::new),
                );

                let lhs_prefix = format!("{}.", lhs.to_str_lossy());
                let rhs_prefix = format!("{}.", rhs.to_str_lossy());

                for (lhs_full_path, lhs_component) in component_analysis.components() {
                    let lhs_path_str = lhs_full_path.to_str_lossy();
                    if !lhs_path_str.starts_with(&lhs_prefix) {
                        continue;
                    }
                    let suffix = &lhs_path_str[lhs_prefix.len()..];
                    let rhs_full_path = BString::from(format!("{}{}", rhs_prefix, suffix).into_bytes());
                    let Some(rhs_component) =
                        component_analysis.resolve(rhs_full_path.as_bstr())
                    else {
                        continue;
                    };

                    // Tristate nets (Bus/Net kinds) would need a
                    // transitive-resolution model; the simulator does not
                    // support tristate buffers.
                    let is_net = |c: &Component| {
                        matches!(c.kind(), Some(ComponentKind::Bus) | Some(ComponentKind::Net))
                    };
                    if is_net(&lhs_component) && is_net(&rhs_component) {
                        continue;
                    }

                    let (source_local, sink_local) =
                        match (lhs_component.flow(), rhs_component.flow()) {
                            (Flow::Source, Flow::Sink) => (lhs_full_path.clone(), rhs_full_path),
                            (Flow::Sink, Flow::Source) => (rhs_full_path, lhs_full_path.clone()),
                            _ => continue,
                        };
                    if source_local == sink_local {
                        continue;
                    }
                    connects.push((
                        BString::from(format!("{prefix}.{source_local}")),
                        BString::from(format!("{prefix}.{sink_local}")),
                    ));
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing.string(submodule.name).to_str_lossy().into_owned();
                recurse_it_block(
                    component_analysis,
                    stmt,
                    &name,
                    it_context,
                    prefix,
                    connects,
                );
            }
            AstNodePayload::Socket(socket) => {
                let name = stmt.parsing.string(socket.name).to_str_lossy().into_owned();
                recurse_it_block(
                    component_analysis,
                    stmt,
                    &name,
                    it_context,
                    prefix,
                    connects,
                );
            }
            _ => {}
        }
    }
}

/// Recurse into a Submodule/Socket statement's it-block, collecting
/// `:=:` connections with the it-context chained by the instance name.
fn recurse_it_block(
    component_analysis: &ComponentAnalysis,
    stmt: &AstNode<'_>,
    instance_name: &str,
    it_context: Option<&[u8]>,
    prefix: &str,
    connects: &mut Vec<(BString, BString)>,
) {
    let Some(it_block) = stmt.children().into_iter()
        .find(|c| matches!(c.payload(), AstNodePayload::It))
    else {
        return;
    };
    let block = &it_block.children()[0];
    let chained: BString = match it_context {
        Some(ctx) => {
            let mut out = BString::from(ctx);
            out.push(b'.');
            out.extend_from_slice(instance_name.as_bytes());
            out
        }
        None => BString::from(instance_name.as_bytes()),
    };
    collect_connects(
        component_analysis,
        &block.children(),
        Some(chained.as_bstr()),
        prefix,
        connects,
    );
}

/// Rewrite `it` / `it.<rest>` against an it-block context (the instance
/// name, dotted for nesting), matching the rewriting performed by
/// driver analysis.
/// Returns the `SignalId` that `component`'s driver references, if the
/// driver is a bare reference (`x := y`, `x := it`, `x := it.y`) to another
/// elaborated component.
///
/// Whether the driver is *syntactically* a bare reference is read from the
/// AST.  *What* that reference names is not re-resolved here: the module's
/// `DependencyGraph` already resolved every reference in the driver
/// statement with the statement's it-block context (so `it` inside
/// `incoming clock : Clock { cpu.clock := it }` is `clock`, and the
/// elaboration agrees with every other analysis on that).  A bare
/// reference has exactly one dependee — its source — whose module-level
/// path is then placed under the driver's scope prefix.
fn resolve_alias(
    component: &ElaboratedComponent,
    path_to_id: &IndexMap<BString, SignalId>,
    builder: &mut Builder,
) -> Option<SignalId> {
    // Only `Driver::Expr` can be a bare reference; `Driver::If` cannot.
    let Driver::Expr(driver_type, loc) = component.driver.as_ref()? else {
        return None;
    };
    // A register's bare `it <= y` is an alias too: the runtime still
    // treats the component as a register (the copied value lands in its
    // pending slot and commits on the clock edge).  Its dependency edge
    // is sequential rather than combinational.
    let edge_kind = match driver_type {
        DriverType::Latched => EdgeKind::Sequential,
        DriverType::Continuous => EdgeKind::Combinational,
    };

    let parsing = builder.get_parsing(loc.package());
    let node = parsing.ast_node(loc.ast_node_id());

    // The driver expression must be a plain reference (not a binop, literal, etc.)
    if !matches!(node.payload(), AstNodePayload::ExprReference) {
        return None;
    }

    // The single dependee of the driver statement, as resolved by the
    // dependency analysis of the module the statement was written in.
    let site = component.driver_site?;
    let graph = builder.get_dependency_graph(site.moddef);
    let mut sources = graph
        .dependencies_of(site.target)?
        .iter()
        .filter(|edge| edge.kind == edge_kind)
        .map(|edge| edge.dependee);
    let source = sources.next()?;
    if sources.next().is_some() {
        // Not a bare reference after all (e.g. a reference shadowed by a
        // `let` whose RHS reads several components).
        return None;
    }
    let source_path = builder
        .get_component_analysis(site.moddef)
        .component(source)?
        .path();

    // The scope in which the driver statement was written was computed
    // during the elaboration walk (`driver_scope`): the component's own
    // module for most kinds, the parent module for `incoming` ports and
    // parent-driven socket channels.
    let full_path: BString = format!(
        "{}.{}",
        component.driver_scope().to_str_lossy(),
        source_path.to_str_lossy()
    )
    .into();
    let target = path_to_id.get(&full_path).copied();
    // Never alias a component to itself.
    if target == Some(component.id()) {
        return None;
    }
    target
}
