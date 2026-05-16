use std::sync::Arc;

use bstr::BStr;

use super::*;

#[test]
#[rustfmt::skip]
fn test_sim() {
    let db = crate::util::db_from_file_with_lib("../examples/lfsr.vir", "../lib");
    if let Err(diagnostics) = crate::util::check_db(&db) {
        for diag in diagnostics.iter() {
            eprintln!("{diag:?}");
        }
        assert!(false);
    }
    let symboltable = db.get_symboltable();
    let top = symboltable.resolve(b"lfsr::Lfsr".into()).unwrap();

    let mut sim = Sim::new(Arc::new(db), top.id());
    let clock_id = sim.signal(BStr::new(b"top.clock"));
    let reset_id = sim.signal(BStr::new(b"top.reset"));
    let out_id = sim.signal(BStr::new(b"top.out"));

    println!("--------------------------------------------------------------------------------");
    println!("Set reset = true and flow");
    sim.set(reset_id, Value::Bit(true));
    sim.flow();
    sim.dump();
    println!("--------------------------------------------------------------------------------");
    println!("tick clock");
    sim.tick(clock_id);
    sim.dump();
    println!("--------------------------------------------------------------------------------");
    println!("set reset to false and flow");
    sim.set(reset_id, Value::Bit(false));
    sim.flow();

    /*
     This is a classic 8-bit LFSR with:
       - State r initialized to 255 (0xFF)
       - Feedback bit = MSB (r[7])
       - Shift left by 1 (dropping MSB, inserting 0 at LSB)
       - If MSB was 1, XOR with taps = 29 = 0x1D = 0b00011101
    */
    const VALUES: &[crate::common::WordValue] = &[
        255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174, 65, 130,
        25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178,
        121, 242, 249, 239, 195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244,
        245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27,
        54, 108, 216, 173, 71, 142, 1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135,
        19, 38, 76, 152, 45, 90,
    ];

    for (cycle, expected) in VALUES.into_iter().enumerate() {
        let expected = Value::Word(8, *expected);
        let actual = sim.get(out_id);
        assert_eq!(expected, actual, "top.out had incorrect value on cycle {cycle}");
        sim.tick(clock_id);
    }
}
