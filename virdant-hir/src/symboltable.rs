use paste::paste;
use virdant_tokenizer::TokenKind;
use std::sync::Arc;

use virdant_common::{ChannelDir, ComponentClass, Flow, ItemKind, SocketRole};
use virdant_parser::{Ast, ComponentKind};
use crate::symbols::*;

pub type SymbolTableError = String;

#[derive(Debug)]
pub struct SymbolTable {
    typedefs: Vec<TypeDef>,
    moddefs: Vec<ModDef>,
    fndefs: Vec<FnDef>,
    socketdefs: Vec<SocketDef>,
    components: Vec<Component>,
    submodules: Vec<Submodule>,
    sockets: Vec<Socket>,
    fields: Vec<Field>,
    ctors: Vec<Ctor>,
    enumerants: Vec<Enumerant>,
    channels: Vec<Channel>,

    errors: Vec<SymbolTableError>,
}

macro_rules! symbol_table_methods {
    ($name:ident, $symbol:ident) => {
        paste!{
            fn [<add_ $name >](&mut self, mut $name: $symbol) -> Id<$symbol> {
                let id = Id::new(self.[< $name s >].len());
                $name.set_id(id.clone());
                self.[<$name s>].push($name);
                id
            }

            pub fn $name(&self, id: Id<$symbol>) -> &$symbol {
                &self.[<$name s>][id.to_usize()]
            }
        }
    };
}

impl SymbolTable {
    symbol_table_methods!(typedef, TypeDef);
    symbol_table_methods!(moddef, ModDef);
    symbol_table_methods!(socketdef, SocketDef);
    symbol_table_methods!(fndef, FnDef);

    symbol_table_methods!(component, Component);
    symbol_table_methods!(submodule, Submodule);
    symbol_table_methods!(socket, Socket);

    symbol_table_methods!(field, Field);
    symbol_table_methods!(ctor, Ctor);
    symbol_table_methods!(enumerant, Enumerant);
    symbol_table_methods!(channel, Channel);
}

impl SymbolTable {
    pub fn new(package: &str, ast: Arc<Ast>) -> SymbolTable {
        let mut st = SymbolTable {
                typedefs: vec![],
                moddefs: vec![],
                fndefs: vec![],
                socketdefs: vec![],
                components: vec![],
                submodules: vec![],
                sockets: vec![],
                fields: vec![],
                ctors: vec![],
                enumerants: vec![],
                channels: vec![],
                errors: vec![],
            };
        add_builtins(&mut st);
        add_items(&mut st, package, ast.as_ref());
        add_typedef_symbols(&mut st, package, ast.as_ref());
        add_socketdef_symbols(&mut st, package, ast.as_ref());
        add_moddef_symbols(&mut st, package, ast.as_ref());
        add_moddef_subports(&mut st);
        add_moddef_socketports(&mut st);
        add_moddef_subsocketports(&mut st);
        st
    }

    fn qualify(qualname: &str, in_package: &str) -> String {
        let parts: Vec<&str> = qualname.split("::").collect();
        if parts.len() == 1 {
            format!("{in_package}::{}", parts[0])
        } else if parts.len() == 2 {
            format!("{}::{}", parts[0], parts[1])
        } else {
            unreachable!()
        }
    }

    pub fn resolve_item(&self, qualname: &str, in_package: &str) -> Option<ItemId> {
        if let Some(id) = self.resolve_typedef(qualname, in_package) {
            Some(ItemId::TypeDef(id))
        } else if let Some(id) = self.resolve_moddef(qualname, in_package) {
            Some(ItemId::ModDef(id))
        } else {
            None
        }
    }

    pub fn resolve_typedef(&self, qualname: &str, in_package: &str) -> Option<Id<TypeDef>> {
        for (id, typedef) in self.typedefs.iter().enumerate() {
            if typedef.qualname() == SymbolTable::qualify(&qualname, in_package) {
                return Some(Id::new(id));
            }
        }
        for (id, typedef) in self.typedefs.iter().enumerate() {
            if typedef.qualname() == SymbolTable::qualify(&qualname, "builtin") {
                return Some(Id::new(id));
            }
        }
        None
    }

    pub fn resolve_moddef(&self, qualname: &str, in_package: &str) -> Option<Id<ModDef>> {
        for (id, moddef) in self.moddefs.iter().enumerate() {
            if moddef.qualname() == SymbolTable::qualify(&qualname, in_package) {
                return Some(Id::new(id));
            }
        }
        for (id, moddef) in self.moddefs.iter().enumerate() {
            if moddef.qualname() == SymbolTable::qualify(&qualname, "builtin") {
                return Some(Id::new(id));
            }
        }
        None
    }

    pub fn resolve_socketdef(&self, qualname: &str, in_package: &str) -> Option<Id<SocketDef>> {
        for (id, socketdef) in self.socketdefs.iter().enumerate() {
            if socketdef.qualname() == SymbolTable::qualify(&qualname, in_package) {
                return Some(Id::new(id));
            }
        }
        for (id, socketdef) in self.socketdefs.iter().enumerate() {
            if socketdef.qualname() == SymbolTable::qualify(&qualname, "builtin") {
                return Some(Id::new(id));
            }
        }
        None
    }

    pub fn resolve_component(&self, qualname: &str, in_moddef: &str) -> Option<Id<Component>> {
        for (id, moddef) in self.moddefs.iter().enumerate() {
            if moddef.qualname() == SymbolTable::qualify(&qualname, in_moddef) {
                return Some(Id::new(id));
            }
        }
        None
    }

    fn moddef_ports(&self, moddef: Id<ModDef>) -> Vec<Id<Component>> {
        let mut results = vec![];
        for component in &self.components {
            if component.moddef == moddef && component.class == ComponentClass::Port {
                results.push(component.id());
            }
        }
        results
    }

    fn moddef_sockets(&self, moddef: Id<ModDef>) -> Vec<Id<Socket>> {
        let mut results = vec![];
        for socket in &self.sockets {
            if socket.moddef == moddef {
                results.push(socket.id());
            }
        }
        results
    }

    fn socketdef_channels(&self, socketdef: Id<SocketDef>) -> Vec<Id<Channel>> {
        let mut results = vec![];
        for channel in &self.channels {
            if channel.socketdef == socketdef {
                results.push(channel.id());
            }
        }
        results
    }
}


fn add_builtins(st: &mut SymbolTable) {
    st.add_typedef(TypeDef { id: None, qualname: "builtin::Bit".to_string() });
    st.add_typedef(TypeDef { id: None, qualname: "builtin::Word".to_string() });
    st.add_typedef(TypeDef { id: None, qualname: "builtin::Clock".to_string() });
}

fn add_items(st: &mut SymbolTable, package: &str, ast: &Ast) {
    if let Ast::Package { items, imports: _ } = ast {
        for item in items {
            match item.as_ref() {
                Ast::Item { kind, name, attrs: _, stmts: _ } => {
                    let qualname = format!("{package}::{}", name.as_string());
                    match kind {
                        ItemKind::ModDef => {
                            let moddef = ModDef {
                                id: None,
                                qualname,
                            };
                            st.add_moddef(moddef);
                        },
                        ItemKind::UnionDef |
                        ItemKind::StructDef |
                        ItemKind::EnumDef |
                        ItemKind::BuiltinDef => {
                            let typedef = TypeDef {
                                id: None,
                                qualname,
                            };
                            st.add_typedef(typedef);
                        },
                        ItemKind::FnDef => {
                            let fndef = FnDef {
                                id: None,
                                qualname,
                            };
                            st.add_fndef(fndef);
                        },
                        ItemKind::SocketDef => {
                            let socketdef = SocketDef {
                                id: None,
                                qualname,
                            };
                            st.add_socketdef(socketdef);
                        },
                    }
                },
                _ => (),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Package node");
    }
}

fn add_typedef_symbols(st: &mut SymbolTable, package: &str, ast: &Ast) {
    if let Ast::Package { items, imports: _ } = ast {
        for item in items {
            match item.as_ref() {
                Ast::Item { kind, name, attrs: _, stmts: _ } => {
                    match kind {
                        ItemKind::UnionDef |
                        ItemKind::StructDef |
                        ItemKind::EnumDef |
                        ItemKind::BuiltinDef => {
                            add_typedef_symbols_for(st, package, &name.as_string(), item);
                        },
                        _ => (),
                    }
                },
                _ => (),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Package node");
    }
}

fn add_typedef_symbols_for(st: &mut SymbolTable, package_name: &str, typedef_name: &str, ast: &Ast) {
    if let Ast::Item { stmts, .. } = ast {
        let typedef_qualname = format!("{package_name}::{typedef_name}");
        let typedef_id = st.resolve_typedef(&typedef_qualname, package_name).unwrap();
        for stmt in stmts {
            match stmt.as_ref() {
                Ast::FieldDef(name, _typ) => {
                    let name = name.as_string();
                    let qualname = format!("{package_name}::{typedef_name}::{name}");
                    let field = Field {
                        id: None,
                        qualname,
                        typedef: typedef_id,
                    };
                    st.add_field(field);
                },
                Ast::CtorDef(name, _params) => {
                    let name = name.as_string();
                    let qualname = format!("{package_name}::{typedef_name}::{name}");
                    let ctor = Ctor {
                        id: None,
                        qualname,
                        typedef: typedef_id,
                    };
                    st.add_ctor(ctor);
                },
                Ast::EnumerantDef(name, _value) => {
                    let name = name.as_string();
                    let qualname = format!("{package_name}::{typedef_name}::{name}");
                    let enumerant = Enumerant {
                        id: None,
                        qualname,
                        typedef: typedef_id,
                    };
                    st.add_enumerant(enumerant);
                },
                _ => unreachable!(),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Item node");
    }
}

fn add_socketdef_symbols(st: &mut SymbolTable, package: &str, ast: &Ast) {
    if let Ast::Package { items, imports: _ } = ast {
        for item in items {
            match item.as_ref() {
                Ast::Item { kind, name, attrs: _, stmts: _ } => {
                    match kind {
                        ItemKind::SocketDef => {
                            add_socketdef_symbols_for(st, package, &name.as_string(), item);
                        },
                        _ => (),
                    }
                },
                _ => (),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Package node");
    }
}

fn add_socketdef_symbols_for(st: &mut SymbolTable, package_name: &str, socketdef_name: &str, ast: &Ast) {
    if let Ast::Item { stmts, .. } = ast {
        for stmt in stmts {
            let socketdef_qualname = format!("{package_name}::{socketdef_name}");
            let socketdef_id = st.resolve_socketdef(&socketdef_qualname, package_name).unwrap();
            match stmt.as_ref() {
                Ast::ChannelDef(dir, name, _typ) => {
                    let name = name.as_string();
                    let qualname = format!("{package_name}::{socketdef_name}::{name}");
                    let dir = match dir.kind() {
                        TokenKind::KwMosi => ChannelDir::Mosi,
                        TokenKind::KwMiso => ChannelDir::Miso,
                        _ => unreachable!(),
                    };
                    let channel = Channel {
                        id: None,
                        qualname,
                        name,
                        socketdef: socketdef_id,
                        dir,
                    };
                    st.add_channel(channel);
                },
                _ => unreachable!(),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Item node");
    }
}

fn add_moddef_symbols(st: &mut SymbolTable, package: &str, ast: &Ast) {
    if let Ast::Package { items, imports: _ } = ast {
        for item in items {
            match item.as_ref() {
                Ast::Item { kind, name, attrs: _, stmts: _ } => {
                    match kind {
                        ItemKind::ModDef => {
                            add_moddef_symbols_for(st, package, &name.as_string(), item);
                        },
                        _ => (),
                    }
                },
                _ => (),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Package node");
    }
}

fn add_moddef_symbols_for(st: &mut SymbolTable, package_name: &str, moddef_name: &str, ast: &Ast) {
    if let Ast::Item { stmts, .. } = ast {
        let moddef_qualname = format!("{package_name}::{moddef_name}");
        let moddef_id = st.resolve_moddef(&moddef_qualname, package_name).unwrap();
        for stmt in stmts {
            match stmt.as_ref() {
                Ast::Component { name, kind, typ: _, on: _ } => {
                    let name = name.as_string();
                    let qualname = format!("{package_name}::{moddef_name}::{name}");
                    let flow = match kind {
                        ComponentKind::Incoming => Flow::Sink,
                        ComponentKind::Outgoing => Flow::Source,
                        ComponentKind::Reg => Flow::Duplex,
                        ComponentKind::Wire => Flow::Duplex,
                    };
                    let class = match kind {
                        ComponentKind::Incoming => ComponentClass::Port,
                        ComponentKind::Outgoing => ComponentClass::Port,
                        ComponentKind::Reg => ComponentClass::Reg,
                        ComponentKind::Wire => ComponentClass::Wire,
                    };
                    let component = Component {
                        id: None,
                        qualname,
                        path: vec![name],
                        moddef: moddef_id.clone(),
                        flow,
                        class,
                    };
                    st.add_component(component);
                },
                Ast::Submod { name, moddef } => {
                    let submoddef: String = moddef.into_iter().map(|m| m.as_string()).collect::<Vec<_>>().join("::");
                    let submodule_moddef_id = if let Some(moddef_id) = st.resolve_moddef(&submoddef, package_name) {
                        moddef_id
                    } else {
                        st.errors.push(format!("Could not resolve name: {submoddef}"));
                        continue;
                    };

                    let name = name.as_string();
                    let qualname = format!("{package_name}::{moddef_name}::{name}");
                    let submod = Submodule {
                        id: None,
                        qualname,
                        name,
                        moddef: moddef_id,
                        submodule_moddef: submodule_moddef_id,
                    };
                    st.add_submodule(submod);
                },
                Ast::Socket { name, socketdef, role } => {
                    let socketdef: String = socketdef.into_iter().map(|m| m.as_string()).collect::<Vec<_>>().join("::");
                    let socketdef_id = if let Some(socketdef_id) = st.resolve_socketdef(&socketdef, package_name) {
                        socketdef_id
                    } else {
                        st.errors.push(format!("Could not resolve name: {socketdef}"));
                        continue;
                    };

                    let name = name.as_string();
                    let qualname = format!("{package_name}::{moddef_name}::{name}");
                    let role = match role.kind() {
                        TokenKind::KwMaster => SocketRole::Master,
                        TokenKind::KwSlave => SocketRole::Slave,
                        _ => unreachable!(),
                    };
                    let socket = Socket {
                        id: None,
                        qualname,
                        socketdef: socketdef_id,
                        moddef: moddef_id,
                        name,
                        role,
                    };
                    st.add_socket(socket);
                },
                _ => (),
            }
        }
    } else {
        panic!("make_symbol_table() requires an Ast::Item node");
    }
}

fn add_moddef_subports(st: &mut SymbolTable) {
    let submodules = st.submodules.clone();
    for submodule in submodules {
        add_moddef_subports_for(st, &submodule);
    }
}

fn add_moddef_subports_for(st: &mut SymbolTable, submodule: &Submodule) {
    for port_id in st.moddef_ports(submodule.moddef) {
        let port = &st.component(port_id);
        let port_name = port.path[port.path.len() - 1].to_string();
        let qualname = format!("{}.{port_name}", &submodule.qualname);
        let flow = match port.flow {
            Flow::Source => Flow::Sink,
            Flow::Sink => Flow::Source,
            Flow::Duplex => unreachable!(),
        };
        let class = ComponentClass::SubPort;
        let component = Component {
            id: None,
            qualname,
            path: vec![submodule.name.clone(), port_name],
            moddef: submodule.moddef,
            flow,
            class,
        };
        st.add_component(component);
    }
}

fn add_moddef_socketports(st: &mut SymbolTable) {
    let sockets = st.sockets.clone();
    for socket in sockets {
        add_moddef_socketports_for(st, &socket);
    }
}

fn add_moddef_socketports_for(st: &mut SymbolTable, socket: &Socket) {
    for channel_id in st.socketdef_channels(socket.socketdef) {
        let channel = &st.channel(channel_id);
        let qualname = format!("{}.{}", &socket.qualname, channel.name);
        let flow = match (socket.role, channel.dir) {
            (SocketRole::Master, ChannelDir::Mosi) => Flow::Sink,
            (SocketRole::Master, ChannelDir::Miso) => Flow::Source,
            (SocketRole::Slave, ChannelDir::Mosi) => Flow::Sink,
            (SocketRole::Slave, ChannelDir::Miso) => Flow::Source,
        };
        let class = ComponentClass::SubPort;
        let component = Component {
            id: None,
            qualname,
            path: vec![socket.name.clone(), channel.name.clone()],
            moddef: socket.moddef,
            flow,
            class,
        };
        st.add_component(component);
    }
}

fn add_moddef_subsocketports(st: &mut SymbolTable) {
    let submodules = st.submodules.clone();
    for submodule in submodules {
        let sockets = st.moddef_sockets(submodule.submodule_moddef);
        for socket_id in sockets {
            let socket = st.socket(socket_id).clone();
            add_moddef_subsocketports_for(st, &submodule, &socket);
        }
    }
}

fn add_moddef_subsocketports_for(st: &mut SymbolTable, submodule: &Submodule, socket: &Socket) {
    for channel_id in st.socketdef_channels(socket.socketdef) {
        let channel = &st.channel(channel_id);
        let qualname = format!("{}.{}.{}", &submodule.qualname, socket.name, channel.name);
        let flow = match (socket.role, channel.dir) {
            (SocketRole::Master, ChannelDir::Mosi) => Flow::Sink,
            (SocketRole::Master, ChannelDir::Miso) => Flow::Source,
            (SocketRole::Slave, ChannelDir::Mosi) => Flow::Sink,
            (SocketRole::Slave, ChannelDir::Miso) => Flow::Source,
        };
        let class = ComponentClass::SubPort;
        let component = Component {
            id: None,
            qualname,
            path: vec![socket.name.clone(), channel.name.clone()],
            moddef: socket.moddef,
            flow,
            class,
        };
        st.add_component(component);
    }
}
