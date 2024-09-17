use paste::paste;
use std::sync::Arc;

use virdant_common::ItemKind;
use virdant_parser::Ast;
use crate::symbols::*;

#[derive(Debug)]
pub struct SymbolTable {
    typedefs: Vec<TypeDef>,
    moddefs: Vec<ModDef>,
    fndefs: Vec<FnDef>,
    socketdefs: Vec<SocketDef>,
    components: Vec<Component>,
    submodules: Vec<Submodule>,
    sockets: Vec<Socket>,
}

macro_rules! symbol_table_methods {
    ($name:ident, $symbol:ident) => {
        paste!{
            fn [<add_ $name >](&mut self, mut $name: $symbol) -> Id<$symbol> {
                let id = Id::new(self.typedefs.len());
                $name.set_id(id.clone());
                self.[<$name s>].push($name);
                id
            }

            pub fn $name(&self, id: Id<$symbol>) -> $symbol {
                self.[<$name s>][id.to_usize()].clone()
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
}

impl SymbolTable {
    fn empty() -> SymbolTable {
        SymbolTable {
            typedefs: vec![],
            moddefs: vec![],
            fndefs: vec![],
            socketdefs: vec![],
            components: vec![],
            submodules: vec![],
            sockets: vec![],
        }
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

    pub fn resolve_component(&self, qualname: &str, in_moddef: &str) -> Option<Id<Component>> {
        for (id, moddef) in self.moddefs.iter().enumerate() {
            if moddef.qualname() == SymbolTable::qualify(&qualname, in_moddef) {
                return Some(Id::new(id));
            }
        }
        None
    }

    pub fn new(package: &str, ast: Arc<Ast>) -> SymbolTable {
        let mut st = SymbolTable::empty();
        add_builtins(&mut st);
        add_items(&mut st, package, ast.as_ref());
        add_modef_symbols(&mut st, package, ast.as_ref());
        st
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

fn add_modef_symbols(st: &mut SymbolTable, package: &str, ast: &Ast) {
    if let Ast::Package { items, imports: _ } = ast {
        for item in items {
            match item.as_ref() {
                Ast::Item { kind, name, attrs: _, stmts: _ } => {
                    match kind {
                        ItemKind::ModDef => {
                            add_modef_symbols_for(st, package, &name.as_string(), item);
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

fn add_modef_symbols_for(st: &mut SymbolTable, package: &str, moddef: &str, ast: &Ast) {
    if let Ast::Item { stmts, .. } = ast {
        for stmt in stmts {
            match stmt.as_ref() {
                Ast::Component { name, .. } => {
                    let name = name.as_string();
                    let qualname = format!("{package}::{moddef}::{name}");
                    let component = Component {
                        id: None,
                        qualname,
                    };
                    st.add_component(component);
                },
                Ast::Submod { name, moddef: _ } => {
                    let name = name.as_string();
                    let qualname = format!("{package}::{moddef}::{name}");
                    let submod = Submodule {
                        id: None,
                        qualname,
                    };
                    st.add_submodule(submod);
                },
                Ast::Socket { name, socketdef: _, role: _ } => {
                    let name = name.as_string();
                    let qualname = format!("{package}::{moddef}::{name}");
                    let socket = Socket {
                        id: None,
                        qualname,
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
