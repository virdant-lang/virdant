use super::*;
use crate::common::BinOp;
use crate::conversion::convert_virir_to_verilog;
use crate::transpile::transpile;
use crate::Vir;

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

    let storage_virir = parse(
        r#"virir {
            package storage {
                mod Storage {
                    wire ready : builtin::Bit;
                    reg data : builtin::Word[8];
                }
            }

            type builtin::Bit;
            type builtin::Word[8];
            type builtin::Clock;
        }"#,
    )
    .unwrap();
    let Item::ModDef(storage) = &storage_virir.packages[0].items[0] else {
        panic!("expected first storage item to be a module");
    };
    let wire = &storage.wires[0];
    assert_eq!(wire.name, "ready");
    assert_eq!(wire.typ, TypeId::new(0));
    let reg = &storage.regs[0];
    assert_eq!(reg.name, "data");
    assert_eq!(reg.typ, TypeId::new(1));

    let verilog = convert_virir_to_verilog(virir);

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();

    let mut vir = Vir::new();
    vir.add_package("on_stmts");
    vir.set_package_text("on_stmts", include_str!("../../../examples/on_stmts.vir"));
    let transpiled_virir = transpile(vir.db());
    let transpiled_verilog = convert_virir_to_verilog(transpiled_virir);

    println!("{transpiled_verilog:#?}");
    transpiled_verilog.write_to_stdout().unwrap();

    let mut vir = Vir::new();
    vir.add_package("basic");
    vir.set_package_text("basic", include_str!("../../../examples/basic.vir"));
    let transpiled_virir = transpile(vir.db());
    let transpiled_verilog = convert_virir_to_verilog(transpiled_virir);

    println!("{transpiled_verilog:#?}");
    transpiled_verilog.write_to_stdout().unwrap();

    let virir = parse(
        r#"virir {
            package myext {
                mod Top {
                    incoming hi : builtin::Word[7];
                    incoming lo : builtin::Bit;
                    outgoing w : builtin::Word[8];
                    outgoing z : builtin::Word[8];
                    outgoing s : builtin::Word[8];

                    w := (word((hi : builtin::Word[7]), (lo : builtin::Bit)) : builtin::Word[8]);
                    z := (zext((lo : builtin::Bit)) : builtin::Word[8]);
                    s := (sext((lo : builtin::Bit)) : builtin::Word[8]);
                }
            }

            type builtin::Bit;
            type builtin::Word[7];
            type builtin::Word[8];
            type builtin::Clock;
        }"#,
    )
    .unwrap();
    let roundtrip_text = virir.to_text();
    parse(&roundtrip_text).unwrap();

    let mut vir = Vir::new();
    vir.add_package("extensions_inline");
    vir.set_package_text(
        "extensions_inline",
        r#"mod Top {
            incoming hi : Word[7];
            incoming lo : Bit;

            outgoing w : Word[8];
            w := word(hi, lo);

            outgoing z : Word[8];
            z := zext(lo);

            outgoing s : Word[8];
            s := sext(lo);
        }"#,
    );
    let transpiled_virir = transpile(vir.db());
    let transpiled_verilog = convert_virir_to_verilog(transpiled_virir);

    println!("{transpiled_verilog:#?}");
    transpiled_verilog.write_to_stdout().unwrap();
}
