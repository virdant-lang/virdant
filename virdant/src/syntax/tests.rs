use paste::paste;
use std::sync::LazyLock;
use std::time::Instant;
use std::io::Read;

use crate::common::source::Source;
use crate::syntax::parsing::parse;

const CHECK: char = '✅';
const BATSU: char = '❌';

const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap()
});

macro_rules! test_example {
    ($filename:ident) => {
        paste!(
            #[test]
            fn [< test_parse_ $filename >]() {
                let filename = format!("{}.vir", stringify!($filename));
                let filepath = EXAMPLES_DIR.join(&filename);
                let mut file = std::fs::File::open(filepath).unwrap();
                let mut text = vec![];
                file.read_to_end(&mut text).unwrap();

                let source = Source::new(crate::fqn::PackageFqn::new("top".into()), text.into());

                let start = Instant::now();
                let parsing = parse(&source);
                let stop = Instant::now();

                let duration = stop - start;

                let errors = parsing.errors();
                if errors.len() > 0 {
                    eprintln!("    {BATSU} {filename:<20}{:>6} us", duration.as_micros());
                    eprintln!("        ERRORS:");
                    for error in &errors {
                        eprintln!("            {:?}  {}", error, error.region());
                    }
                } else {
                    eprintln!("    {CHECK} {filename:<20}{:>6} us", duration.as_micros());
                }

                assert!(errors.is_empty());
            }
        );
    }
}

test_example!(basic);
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
test_example!(it);
test_example!(conditional_statements);
