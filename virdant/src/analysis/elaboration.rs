use std::sync::Arc;

use bstr::{BString, ByteSlice};

use crate::analysis::drivers::Driver;
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

#[derive(Debug)]
pub struct Elaboration {
    components: Vec<ElaboratedComponent>,
}

#[derive(Debug)]
pub struct ElaboratedComponent {
    path: BString,
    typ: Type,
    driver_type: DriverType,
    driver: Option<Driver>,
}

#[derive(Debug, Clone, Copy)]
pub struct ElaboratedComponentId(usize);

impl Elaboration {
    pub fn components(&self) -> &[ElaboratedComponent] {
        &self.components
    }

    pub fn component(&self, elaborated_component_id: ElaboratedComponentId) -> &ElaboratedComponent {
        &self.components[elaborated_component_id.0]
    }

    pub fn dump(&self) {
        for component in &self.components {
            println!("{} : {:?}", component.path, component.typ);
        }
    }
}

impl ElaboratedComponent {
    pub fn path(&self) -> &BString {
        &self.path
    }

    pub fn typ(&self) -> Type {
        self.typ.clone()
    }

    pub fn driver_type(&self) -> DriverType {
        self.driver_type
    }

    pub fn driver(&self) -> Option<&Driver> {
        self.driver.as_ref()
    }
}

/// Recursively traverse the module at `moddef`, collecting all fully-qualified
/// component paths into `components`.  `prefix` is the dotted path so far
/// (e.g. `"top"` or `"top.foo"`).
fn elaborate_module(
    builder: &mut Builder,
    moddef: SymbolId,
    prefix: &str,
    components: &mut Vec<ElaboratedComponent>,
) {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(moddef);
    let driver_analysis = builder.get_driver_analysis(moddef);

    for stmt in item_ast.children() {
        match stmt.payload() {
            AstNodePayload::Component(component) => {
                let name = parsing.string(component.name);
                let path: BString = format!("{prefix}.{}", name.to_str_lossy()).into();

                let typ_node = stmt.typ().unwrap();
                let typ = builder.get_type_at(typ_node.location()).unwrap();

                let driver_type = match component.kind {
                    ComponentKind::Reg => DriverType::Latched,
                    _ => DriverType::Continuous,
                };

                let component = component_analysis.resolve(name).unwrap();
                let driver = driver_analysis.drivers().get(&component.id()).map(|drivers| drivers.first().cloned().unwrap());

                components.push(ElaboratedComponent { path, typ, driver_type, driver });
            }
            AstNodePayload::Submodule(module) => {
                let instance_name = parsing.string(module.name).to_str_lossy().into_owned();
                let ofness_node = stmt.child(0);
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                    continue;
                };
                let submodule_package = ofness
                    .package
                    .map(|pkg| PackageFqn::new(bstr::BString::from(parsing.string(pkg).to_vec())))
                    .unwrap_or_else(|| location.package());
                let submodule_name = parsing.string(ofness.name);
                let symboltable = builder.get_symboltable();
                let submodule_symbol = match symboltable.resolve_item_in_package(submodule_name, submodule_package) {
                    Some(symbol) => symbol.clone(),
                    None => continue,
                };
                let child_prefix = format!("{prefix}.{instance_name}");
                elaborate_module(builder, submodule_symbol.id, &child_prefix, components);
            }
            _ => {}
        }
    }
}

pub(crate) fn build_elaboration(builder: &mut Builder, top: SymbolId) -> Arc<Elaboration> {
    let mut components: Vec<ElaboratedComponent> = vec![];
    elaborate_module(builder, top, "top", &mut components);
    Arc::new(Elaboration { components })
}
