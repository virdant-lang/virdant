mod check;
mod infer;

use std::sync::Arc;

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexSet;
use indexmap::IndexMap;

use crate::analysis::symbols::{Symbol, SymbolId};
use crate::common::WordValue;
use crate::common::{BinOp, Flow, UnOp as UnOp, Width};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticLevel};
use crate::analysis::location::Location;
use crate::fqn::PackageFqn;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::{self, AstNodePayload};
use crate::types::context::Referent;

use super::context::TypingContext;
use super::typ::Type;

#[derive(Debug)]
pub struct Typing {
    item: Symbol,
    exprroot: ExprRoot,
    typs: IndexMap<AstNodeId, Type>,
    diagnostics: Vec<Diagnostic>,
    use_locations: IndexMap<BString, Vec<Location>>, // TODO should be use for Referents
    tags: IndexMap<Location, Tag>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}

impl ExprRoot {
    pub fn new(location: Location) -> ExprRoot {
        ExprRoot { location }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn package(&self) -> PackageFqn {
        self.location.package()
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Any,
    All,
    Cast,
    Word,
    Sext,
    Zext,
}


#[derive(Debug, Clone)]
pub enum Tag {
    None,
    SymbolResolution(SymbolId),
    PrimitiveResolution(Primitive),
    ReferentResolution(Referent),
}

impl Tag {
    pub fn symbol_id(&self) -> Option<SymbolId> {
        if let Tag::SymbolResolution(symbol_id) = self {
            Some(symbol_id.clone())
        } else {
            None
        }
    }

    pub fn primitive(&self) -> Option<Primitive> {
        if let Tag::PrimitiveResolution(primitive) = self {
            Some(primitive.clone())
        } else {
            None
        }
    }
}

impl Typing {
    pub fn item(&self) -> &Symbol {
        &self.item
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn tag(&self, location: Location) -> Tag {
        if let Some(tag) = self.tags.get(&location).cloned() {
            tag
        } else {
            Tag::None
        }
    }

    pub fn reference_use_locations(&self) -> &IndexMap<BString, Vec<Location>> {
        &self.use_locations
    }

    pub fn use_component(&mut self, path: &BStr, location: Location) {
        if !self.use_locations.contains_key(path) {
            self.use_locations.insert(path.to_owned(), vec![]);
        }
        self.use_locations.get_mut(path).unwrap().push(location);
    }

    pub(crate) fn type_of_node(&self, id: AstNodeId) -> Option<&Type> {
        self.typs.get(&id)
    }

    fn flag_wrong_type<'p>(&mut self, node: &AstNode<'p>, expected: &Type, actual: &Type) {
        let diag: Diagnostic = diagnostics::WrongType {
            region: node.region(),
            expected: expected.to_string().into(),
            actual: actual.to_string().into(),
        }.into();
        self.diagnostics.push(diag);
    }

    #[allow(unused)]
    fn flag_todo<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Todo {
            region: node.region(),
            message: message.into(),
        }.into());
    }

    fn flag_unknown<'p, S: Into<BString>>(&mut self, node: &AstNode<'p>, message: S) {
        self.diagnostics.push(diagnostics::Unknown {
            region: node.region(),
            message: message.into(),
        }.into());
    }

    fn flag_not_word_type<'p>(&mut self, node: &AstNode<'p>, typ: &Type) {
        self.diagnostics.push(diagnostics::NotWordType {
            region: node.region(),
            typ: typ.to_string().into(),
        }.into());
    }

    fn flag_cant_infer<'p>(&mut self, node: &AstNode<'p>) {
        self.diagnostics.push(diagnostics::CantInfer {
            region: node.region(),
        }.into());
    }

    fn annotate(&mut self, node: &AstNode<'_>, typ: &Type) {
        let previous = self.typs.insert(node.id(), typ.clone());
        if let Some(previous_typ) = previous {
            panic!("Node is already annotated with {previous_typ:?}, can't annotate with {typ:?} at {:?}", node.region());
        }
    }

    // Checks that in a clean typecheck, all expressions in the tree have an annotation.
    pub fn validate(&mut self, builder: &mut Builder) {
        if self.diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
            return;
        }

        let parsing = builder.get_parsing(self.exprroot.package());
        let root_id = self.exprroot.location().ast_node_id();
        let mut queue = vec![root_id];
        while let Some(node_id) = queue.pop() {
            let node = parsing.ast_node(node_id);
            if node.is_expr() && !self.typs.contains_key(&node.id()) {
                self.flag_unknown(&node, "Missing annotation");
            }

            for child in node.children() {
                queue.push(child.id());
            }
        }
    }
}

pub(crate) fn build_type_at(builder: &mut Builder, location: Location) -> Result<Type, Vec<Diagnostic>> {
    let type_index = builder.get_type_index();
    if let Some(typ) = type_index.type_at(location) {
        Ok(typ.clone())
    } else {
        Err(vec![]) // TODO do I actually need diagnostics here?
    }
}

pub(crate) fn typecheck(builder: &mut Builder, symbol_id: SymbolId) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let exprroots = builder.get_exprroots();
    let item = symboltable.symbol(symbol_id);

    let mut use_locations: IndexMap<BString, IndexSet<Location>> = IndexMap::new();

    let parsing = builder.get_parsing(item.package());
    let node: AstNode = parsing.ast_node(item.location().ast_node_id());
    if !node.contains_errors() {
        diagnostics.extend(typecheck_item(builder, item, &exprroots, &mut use_locations));
    }

    if let AstNodePayload::ModDef(moddef) = node.payload() && !moddef.is_ext {
        // Unused varibles and read from sink warnings
        let component_analysis = builder.get_component_analysis(symbol_id);
        for (path, component) in component_analysis.components() {
            if component.can_source() && !use_locations.contains_key(&path) {
                let region = builder.get_location_region(component.location());
                diagnostics.push(diagnostics::UnusedSource {
                    region,
                    path: path.into(),
                }.into());
            } else if component.flow() == Flow::Sink && use_locations.contains_key(&path) {
                for location in &use_locations[&path] {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::ReadFromSink {
                        region,
                        path: path.clone(),
                    }.into());
                }
            }
        }
    }

    diagnostics
}

fn typecheck_item(
    builder: &mut Builder,
    item: Symbol,
    exprroots: &[ExprRoot],
    use_locations: &mut IndexMap<BString, IndexSet<Location>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for exprroot in exprroots {
        if exprroot.package() == item.package() && item_for(builder, exprroot.location()).id() == item.id() {
            let typing = builder.get_typing(exprroot.clone());
            diagnostics.extend(typing.diagnostics());

            let typing_use_locations = typing.reference_use_locations();
            for (path, locations) in typing_use_locations.iter() {
                if !use_locations.contains_key(path.as_bstr()) {
                    use_locations.insert(path.clone(), IndexSet::new());
                }
                let t = use_locations.get_mut(path.as_bstr()).unwrap();
                t.extend(locations.clone());
            }
        }
    }
    diagnostics
}

pub(crate) fn item_for(builder: &mut Builder, location: Location) -> Symbol {
    let symboltable = builder.get_symboltable();
    let parsing = builder.get_parsing(location.package());

    let mut node = parsing.ast_node(location.ast_node_id());
    loop {
        if node.is_item() {
            let name = node.name().unwrap();
            let name_str = parsing.string(name);
            return symboltable
                .resolve_item_in_package(name_str, location.package())
                .unwrap()
                .clone();
        }

        if let Some(parent) = node.parent() {
            node = parsing.ast_node(parent.id());
        } else {
            let region = builder.get_location_region(location.clone());
            panic!("No containing item found for location {location:?} at {region:?}");
        }
    }
}

pub(crate) fn build_typing(builder: &mut Builder, exprroot: ExprRoot) -> Arc<Typing> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());

    let mut current_id = location.ast_node_id();
    // REVIEW walk up until you find the containing item ast node.
    let item_name = loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            break parsing
                .string(current.name().expect("expected containing item to have a name"))
                .to_owned();
        }
        current_id = current
            .parent()
            .expect("expected expr root to be contained in an item")
            .id();
    };
    let symboltable = builder.get_symboltable();
    let item = symboltable
        .resolve_item_in_package(item_name.as_bstr(), location.package())
        .unwrap();
    let context = builder.get_typing_context(item.id());

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = builder.get_expected_type(exprroot.clone());

    let diagnostics = vec![];
    let mut typing = Typing {
        item: item.clone(),
        exprroot: exprroot.clone(),
        typs: IndexMap::new(),
        diagnostics,
        use_locations: IndexMap::new(),
        tags: IndexMap::new(),
    };

    // if there is no expected type, you can't type check the expression.
    if let Some(expected_typ) = expected_typ {
        let _ = typing.check(builder, context, &node, &expected_typ);
    } else {
        if matches!(node.parent().unwrap().payload(), AstNodePayload::Driver(_)) {
            return Arc::new(typing);
        }

        match typing.infer(builder, context, &node) {
            Ok(None) => {
                typing.diagnostics.push(diagnostics::Todo {
                    region: node.region(),
                    message: format!("Can't typecheck expression because we don't know what type it should have").into(),
                }.into());
            }
            Ok(Some(typ)) => {
                typing.typs.insert(node.id(), typ);
            }
            _ => (),
        }
    }

    typing.validate(builder);

    Arc::new(typing)
}
