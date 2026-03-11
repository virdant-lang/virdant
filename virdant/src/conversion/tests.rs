use crate::virir::*;

#[test]
fn test_conversion() {
    let region = Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    );

    let virir = VirIr {
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
                        region: region.clone(),
                        path: "inp".to_string(),
                        typ: TypeId(0),
                    })),
                }],
            })],
        }],
        types: vec![Arc::new(Type::Word(8))],
    };
}
