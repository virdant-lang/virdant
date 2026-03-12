use super::*;
use crate::common::BinOp;
use crate::conversion::convert_virir_to_verilog;

const TEST_VIRIR: &str = include_str!("../../../virir/top.virir");


#[test]
fn test_virir() {
    let virir_text = TEST_VIRIR;
    let virir = parse(virir_text).unwrap();

    dbg!(&virir);
    assert_eq!(virir.packages.len(), 3);
    assert_eq!(virir.packages[0].name, "top");
    assert_eq!(virir.packages[1].name, "passthrough");
    assert_eq!(virir.packages[2].name, "adder");
    let Item::ModDef(top) = &virir.packages[0].items[0] else {
        panic!("expected first item to be Top");
    };
    assert!(top.is_export);
    let Item::ModDef(passthrough) = &virir.packages[1].items[0] else {
        panic!("expected passthrough package to contain Passthrough");
    };
    assert!(!passthrough.is_export);
    let Expr::If(expr_if) = passthrough.drivers[0].expr.as_ref() else {
        panic!("expected Passthrough driver expression to parse as If");
    };
    assert!(matches!(expr_if.cond.as_ref(), Expr::Reference(reference) if reference.path == "sel"));
    assert!(matches!(expr_if.then_expr.as_ref(), Expr::Reference(reference) if reference.path == "inp"));
    let Expr::BinOp(binop) = expr_if.else_expr.as_ref() else {
        panic!("expected else branch to parse as BinOp");
    };
    assert_eq!(binop.op, BinOp::Add);

    let verilog = convert_virir_to_verilog(virir);

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();
}
