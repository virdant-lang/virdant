use std::sync::Arc;

use bstr::BString;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::{ComponentKind, Flow, PortDir};
use crate::db::Builder;
use crate::types::Type;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug, Clone)]
pub struct Port {
    pub path: BString,
    pub dir: PortDir,
    pub typ: Option<Type>,
}

pub(crate) fn build_ports_of(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Port>> {
    let mut ports = vec![];

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);

    // Only ModDef items have ports
    if !matches!(symbol.kind(), SymbolKind::ModDef) {
        return Arc::new(ports);
    }

    let component_analysis = builder.get_component_analysis(symbol_id);

    for (path, component) in component_analysis.components() {

        // Skip submodule ports by checking the AST node payload
        let location = component.location();
        let parsing = builder.get_parsing(location.package());
        let ast_node = parsing.ast_node(location.ast_node_id());

        // Submodule statements have AstNodePayload::Submodule
        if matches!(ast_node.payload(), AstNodePayload::Submodule(_)) {
            continue;
        }

        // Determine if this component is a port based on its component kind.
        // Outgoing, OutgoingWire, OutgoingReg are all output ports even though
        // OutgoingWire and OutgoingReg have Flow::Duplex internally.
        // Incoming is an input port.
        // Plain Wire and Reg are not ports.
        // Socket channels of the current module have kind=None and flow=Source/Sink.
        let is_port = matches!(component.kind(),
            Some(ComponentKind::Incoming)
            | Some(ComponentKind::Outgoing)
            | Some(ComponentKind::OutgoingWire)
            | Some(ComponentKind::OutgoingReg)
        ) || component.kind().is_none() && matches!(component.flow(), Flow::Source | Flow::Sink);
        if !is_port {
            continue;
        }
        let dir = match component.kind() {
            Some(ComponentKind::Incoming) => PortDir::Input,
            Some(ComponentKind::Outgoing)
            | Some(ComponentKind::OutgoingWire)
            | Some(ComponentKind::OutgoingReg) => PortDir::Output,
            // Socket channels: kind=None, use flow to determine direction.
            None => match component.flow() {
                Flow::Source => PortDir::Input,
                Flow::Sink => PortDir::Output,
                _ => continue,
            },
            _ => continue,
        };

        ports.push(Port {
            path,
            dir,
            typ: component.typ(),
        });
    }

    Arc::new(ports)
}
