use std::sync::Arc;

use super::convert_virir_to_verilog;
use crate::common::PortDir;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Region, Span};
use crate::virir::expr::{Expr, Reference};
use crate::virir::typ::Type;
use crate::virir::*;

#[test]
fn test_conversion() {
    let region = Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    );

    let virir = VirIr {
        packages: vec![Package {
            name: "top".to_string(),
            items: vec![
                Item::ModDef(ModDef {
                    region: region.clone(),
                    name: "Top".to_string(),
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
                    instances: vec![Instance {
                        region: region.clone(),
                        name: "passthrough".to_string(),
                        module_path: "top::Passthrough".to_string(),
                    }],
                    drivers: vec![
                        Driver {
                            region: region.clone(),
                            path: "passthrough.inp".to_string(),
                            expr: Arc::new(Expr::Reference(Reference {
                                region: region.clone(),
                                path: "inp".to_string(),
                                typ: TypeId::new(0),
                            })),
                        },
                        Driver {
                            region: region.clone(),
                            path: "out".to_string(),
                            expr: Arc::new(Expr::Reference(Reference {
                                region: region.clone(),
                                path: "passthrough.out".to_string(),
                                typ: TypeId::new(0),
                            })),
                        },
                    ],
                }),
                Item::ModDef(ModDef {
                    region: region.clone(),
                    name: "Passthrough".to_string(),
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
                    instances: vec![],
                    drivers: vec![Driver {
                        region: region.clone(),
                        path: "out".to_string(),
                        expr: Arc::new(Expr::Reference(Reference {
                            region: region.clone(),
                            path: "inp".to_string(),
                            typ: TypeId::new(0),
                        })),
                    }],
                }),
            ],
        }],
        types: vec![Arc::new(Type::Word(8))],
    };

    let verilog = convert_virir_to_verilog(virir);

    assert_eq!(verilog.files.len(), 1);
    assert_eq!(verilog.files[0].name, "top.sv");
    assert_eq!(verilog.files[0].modules.len(), 2);
    assert_eq!(verilog.files[0].modules[0].name, "Top");
    let crate::verilog::Element::Submodule(submodule) = &verilog.files[0].modules[0].elements[0] else {
        panic!("expected first top-level element to be a submodule instance");
    };
    assert_eq!(submodule.name, "passthrough");
    assert_eq!(submodule.submodule_name, "Passthrough");
    assert_eq!(
        submodule.connects,
        vec![
            ("inp".to_string(), "inp".to_string()),
            ("out".to_string(), "out".to_string()),
        ]
    );

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();
}
