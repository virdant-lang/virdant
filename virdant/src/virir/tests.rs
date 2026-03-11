use std::sync::Arc;

use super::*;
use crate::common::PortDir;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Region, Span};
use crate::virir::expr::{Expr, Reference};
use crate::virir::typ::Type;

#[test]
fn test_virir() {
    let region = Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    );

    let virir = Design {
        packages: vec![Package {
            items: vec![Item::ModDef(ModDef {
                region: region.clone(),
                ports: vec![
                    Port {
                        region: region.clone(),
                        name: "inp".to_string(),
                        dir: PortDir::Input,
                        width: 8,
                    },
                    Port {
                        region: region.clone(),
                        name: "out".to_string(),
                        dir: PortDir::Output,
                        width: 8,
                    },
                ],
                drivers: vec![Driver {
                    region: region.clone(),
                    name: "out".to_string(),
                    expr: Arc::new(Expr::Reference(Reference {
                        path: "inp".to_string(),
                        typ: TypeId(0),
                    })),
                }],
            })],
        }],
        types: vec![Arc::new(Type::Word(8))],
    };

    assert_eq!(virir.packages.len(), 1);
    assert_eq!(virir.types.len(), 1);
    match virir.types[0].as_ref() {
        Type::Word(width) => assert_eq!(*width, 8),
        _ => panic!("expected Word[8] type"),
    }

    let Item::ModDef(mod_def) = &virir.packages[0].items[0] else {
        panic!("expected module definition");
    };

    assert_eq!(mod_def.region, region);
    assert_eq!(mod_def.ports.len(), 2);
    assert_eq!(mod_def.ports[0].region, region);
    assert_eq!(mod_def.ports[0].name, "inp");
    assert_eq!(mod_def.ports[0].dir, PortDir::Input);
    assert_eq!(mod_def.ports[0].width, 8);
    assert_eq!(mod_def.ports[1].region, region);
    assert_eq!(mod_def.ports[1].name, "out");
    assert_eq!(mod_def.ports[1].dir, PortDir::Output);
    assert_eq!(mod_def.ports[1].width, 8);

    assert_eq!(mod_def.drivers.len(), 1);
    assert_eq!(mod_def.drivers[0].region, region);
    assert_eq!(mod_def.drivers[0].name, "out");
    let Expr::Reference(reference) = mod_def.drivers[0].expr.as_ref() else {
        panic!("expected reference driver expression");
    };
    assert_eq!(reference.path, "inp");
}
