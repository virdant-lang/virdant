use bstr::BString;

use crate::analysis::symbols::SymbolId;
use crate::common::PortDir;
use crate::db::Builder;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Port {
    pub path: BString,
    pub dir: PortDir,
    pub typ: Option<Type>,
}

pub(crate) fn build_ports_of(builder: &mut Builder, symbol_id: SymbolId) -> Vec<Port> {
    let mut ports = vec![];

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    
    // Only ModDef items have ports
    if !matches!(symbol.kind(), crate::analysis::symbols::SymbolKind::ModDef) {
        return ports;
    }

    let component_analysis = builder.get_component_analysis(symbol_id);
    
    for (path, component) in component_analysis.components() {
        use crate::common::Flow;
        use crate::syntax::payload::AstNodePayload;

        // Skip submodule ports by checking the AST node payload
        let location = component.location();
        let parsing = builder.get_parsing(location.package());
        let ast_node = parsing.ast_node(location.ast_node_id());

        // Submodule statements have AstNodePayload::Submodule
        if matches!(ast_node.payload(), AstNodePayload::Submodule(_)) {
            continue;
        }

        // Determine if this component is a port based on its flow
        // From interior perspective: Sink means it can be driven (output port)
        // From interior perspective: Source means it provides values (input port)
        let dir = match component.flow() {
            Flow::Sink => PortDir::Output,
            Flow::Source => PortDir::Input,
            Flow::Duplex => continue, // Duplex components are not ports
        };

        ports.push(Port {
            path,
            dir,
            typ: component.typ(),
        });
    }

    ports
}
