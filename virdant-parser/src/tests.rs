use paste::paste;
use std::sync::LazyLock;
use std::time::Instant;

use virdant_common::text::Text;
use crate::parse;

const CHECK: char = '✅';
const BATSU: char = '❌';

const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap());

macro_rules! test_example {
    ($filename:ident) => {
        paste!(
            #[test]
            fn [< test_parse_ $filename >]() {
                let filename = format!("{}.vir", stringify!($filename));
                let filepath = EXAMPLES_DIR.join(&filename);
                let text: Text = Text::from(std::fs::read_to_string(&filepath).unwrap());

                let start = Instant::now();
                let ast = parse(text);
                let stop = Instant::now();

                let duration = stop - start;
                //eprintln!("{ast:?}");

                if ast.has_errors() {
                    eprintln!("    {BATSU} {filename:<20}{:>6} us", duration.as_micros());
                } else {
                    eprintln!("    {CHECK} {filename:<20}{:>6} us", duration.as_micros());
                }
            }
        );
    }
}

test_example!(blink);
test_example!(buffer);
test_example!(delay);
test_example!(echo);
test_example!(edge);
test_example!(enums);
test_example!(extensions);
test_example!(fns);
test_example!(gcd);
test_example!(lfsr);
test_example!(lights);
test_example!(literals);
test_example!(matches);
test_example!(passthrough);
test_example!(queue);
test_example!(random);
test_example!(resetter);
test_example!(rf);
test_example!(sockets);
test_example!(structs);
test_example!(top);
test_example!(uart);
