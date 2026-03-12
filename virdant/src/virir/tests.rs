use super::*;
use crate::common::BinOp;
use crate::conversion::convert_virir_to_verilog;
use crate::fqn::PackageFqn;
use crate::source::{LineCol, Region, Span};
use crate::virir::typ::Type;

const TEST_VIRIR: &str = "virir {
    package top {
        mod Top {
            incoming inp : builtin::Word[8];
            outgoing out : builtin::Word[8];

            mod passthrough of top::Passthrough;
            passthrough.inp := inp;
            out := passthrough.out;
        }

        mod Passthrough {
            incoming inp : builtin::Word[8];
            outgoing out : builtin::Word[8];
            out := ((inp : builtin::Word[8]) + inp);
        }
    }

    type builtin::Bit;
    type builtin::Word[8];
    type builtin::Clock;
}
";


#[test]
fn test_virir() {
    let virir_text = TEST_VIRIR;
    let virir = parse(virir_text).unwrap();
    let dummy_region = Region::new(
        PackageFqn::new("dummy".into()),
        Span::new(LineCol::new(0, 0), LineCol::new(0, 0)),
    );

    dbg!(&virir);
    assert_eq!(virir.packages.len(), 1);
    assert_eq!(virir.packages[0].name, "top");
    let Item::ModDef(passthrough) = &virir.packages[0].items[1] else {
        panic!("expected second item to be Passthrough");
    };
    let Expr::BinOp(binop) = passthrough.drivers[0].expr.as_ref() else {
        panic!("expected Passthrough driver expression to parse as BinOp");
    };
    assert_eq!(binop.op, BinOp::Add);

    let verilog = convert_virir_to_verilog(virir);
    assert_eq!(verilog.files[0].name, "top.sv");
    assert_eq!(verilog.files[0].modules[0].name, r"\top::Top ");
    assert_eq!(verilog.files[0].modules[1].name, r"\top::Passthrough ");

    let crate::verilog::Element::Submodule(submodule) = &verilog.files[0].modules[0].elements[0] else {
        panic!("expected converted Top module to start with a submodule instance");
    };
    assert_eq!(submodule.name, "passthrough");
    assert_eq!(submodule.submodule_name, r"\top::Passthrough ");
    assert_eq!(
        submodule.connects,
        vec![
            ("inp".to_string(), "inp".to_string()),
            ("out".to_string(), "out".to_string()),
        ]
    );
    let crate::verilog::Element::Assign(assign) = &verilog.files[0].modules[1].elements[0] else {
        panic!("expected Passthrough module to contain an assign");
    };
    let crate::verilog::Expr::BinOp(binop) = &assign.expr else {
        panic!("expected Passthrough assign expression to convert to BinOp");
    };
    assert!(matches!(&*binop.lhs, crate::verilog::Expr::Reference(reference) if reference.name == "inp"));
    assert!(matches!(&*binop.rhs, crate::verilog::Expr::Reference(reference) if reference.name == "inp"));

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();
}
