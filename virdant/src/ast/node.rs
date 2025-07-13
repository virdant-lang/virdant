use bstr::BString;

use crate::common::{ComponentKind, ItemKind};

use super::*;

macro_rules! ast {
    ($name:ident) => {
        pub struct $name(pub(super) Ast, pub(super) AstNodeId);

        impl $name {
            pub fn ast(&self) -> Ast {
                self.0.clone()
            }
            pub fn ast_node_id(&self) -> AstNodeId {
                self.1.clone()
            }
            pub fn as_ast_node(&self) -> AstNode {
                self.ast().ast_node(self.ast_node_id())
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.as_ast_node())
            }
        }
    };
}

macro_rules! ast_with_payload {
    ($name:ident) => {
        ast!($name);
        impl $name {
            pub fn payload(&self) -> payload::$name {
                if let AstNodePayload::$name(data) = self.as_ast_node().payload() {
                    data
                } else {
                    unreachable!()
                }
            }
        }
    };
}

ast!(Package);
ast!(PackageStmt);
ast_with_payload!(Import);
ast!(Item);
ast_with_payload!(ModDef);
ast_with_payload!(Component);

impl Package {
    pub fn stmts(&self) -> Vec<PackageStmt> {
        let mut result = vec![];
        for child in self.as_ast_node().children() {
            result.push(PackageStmt(self.ast(), child.id()));
        }
        result
    }

    pub fn imports(&self) -> Vec<Import> {
        let mut imports = vec![];
        for child in self.as_ast_node().children() {
            if let AstNodePayload::Import(_) = child.payload() {
                imports.push(Import(self.ast(), child.id()));
            }
        }
        imports
    }

    pub fn items(&self) -> Vec<Item> {
        let mut items = vec![];
        for child in self.as_ast_node().children() {
            if let AstNodePayload::ModDef(_) = child.payload() {
                items.push(Item(self.ast(), child.id()));
            }
        }
        items
    }

    pub fn moddefs(&self) -> Vec<ModDef> {
        let mut results = vec![];
        for item in self.items() {
            if let Some(moddef) = item.as_moddef() {
                results.push(moddef);
            }
        }
        results
    }
}

impl PackageStmt {
    pub fn as_import(&self) -> Option<Import> {
        if let AstNodePayload::Import(_) = self.as_ast_node().payload() {
            Some(Import(self.ast(), self.ast_node_id()))
        } else {
            None
        }
    }

    pub fn as_item(&self) -> Option<Item> {
        match self.as_ast_node().payload() {
            AstNodePayload::ModDef(_) => Some(Item(self.ast(), self.ast_node_id())),
            _ => None
        }
    }
}

impl Import {
    pub fn name(&self) -> InternedString {
        self.payload().package
    }
}

impl Item {
    pub fn kind(&self) -> ItemKind {
        match self.as_ast_node().payload() {
            AstNodePayload::ModDef(_) => ItemKind::ModDef,
            _ => unreachable!(),
        }
    }

    pub fn as_moddef(&self) -> Option<ModDef> {
        let node = self.as_ast_node();
        if let AstNodePayload::ModDef(_) = node.payload() {
            Some(ModDef(self.ast(), node.id()))
        } else {
            None
        }
    }

    pub fn item_references(&self) -> Vec<AstNode> {
        let mut results = vec![];
        for node in self.as_ast_node().walk() {
            if let AstNodePayload::Ofness(_ofness) = node.payload() {
                results.push(node);
            }
        }
        results
    }
}

impl ModDef {
    pub fn as_item(&self) -> Item {
        Item(self.ast(), self.ast_node_id())
    }

    pub fn name(&self) -> BString {
        let stringtable = self.ast().stringtable();
        stringtable.get(&self.payload().name).to_owned()
    }

    pub fn components(&self) -> Vec<Component> {
        let mut results = vec![];
        for child in self.as_ast_node().children() {
            if let AstNodePayload::Component(_) = child.payload() {
                results.push(Component(self.ast(), child.id()));
            }
        }
        results
    }
}

impl Component {
    pub fn name(&self) -> BString {
        let stringtable = self.ast().stringtable();
        stringtable.get(&self.payload().name).to_owned()
    }

    pub fn kind(&self) -> ComponentKind {
        self.payload().kind
    }
}
