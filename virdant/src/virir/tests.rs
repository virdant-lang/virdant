use super::*;
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
            passthrough.in := in;
            out := passthrough.out;
        }

        mod Passthrough {
            incoming inp : builtin::Word[8];
            outgoing out : builtin::Word[8];
            out := (in : builtin::Word[8]);
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

    let verilog = convert_virir_to_verilog(virir);

    println!("{verilog:#?}");
    verilog.write_to_stdout().unwrap();
}
