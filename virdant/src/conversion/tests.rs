use std::sync::Arc;

use super::convert_virir_to_verilog;
use crate::common::PortDir;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Region, Span};
use crate::virir::expr::{BinOp, Expr, Reference};
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
                    is_export: true,
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
                    wires: vec![],
                    regs: vec![],
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
                    is_export: false,
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
                    wires: vec![],
                    regs: vec![],
                    instances: vec![],
                    drivers: vec![Driver {
                        region: region.clone(),
                        path: "out".to_string(),
                        expr: Arc::new(Expr::BinOp(BinOp {
                            region: region.clone(),
                            typ: TypeId::new(0),
                            op: crate::common::BinOp::Add,
                            lhs: Arc::new(Expr::Reference(Reference {
                                region: region.clone(),
                                path: "inp".to_string(),
                                typ: TypeId::new(0),
                            })),
                            rhs: Arc::new(Expr::Reference(Reference {
                                region: region.clone(),
                                path: "inp".to_string(),
                                typ: TypeId::new(0),
                            })),
                        })),
                    }],
                }),
            ],
        }],
        types: vec![Arc::new(Type::Word(8))],
    };

    let verilog = convert_virir_to_verilog(virir);

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();
}
