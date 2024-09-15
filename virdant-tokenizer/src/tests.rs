use paste::paste;
use std::sync::LazyLock;
use std::time::Instant;

use virdant_common::text::Text;
use crate::tokenize;

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
                let tokenization = tokenize(text);
                let stop = Instant::now();

                let duration = stop - start;

                if tokenization.has_errors() {
                    eprintln!("    {BATSU} {filename:<20}{:>6} us", duration.as_micros());

                    for pos in tokenization.error_positions() {
                        eprintln!("        ERROR: {pos:?}");
                    }
                    panic!("{} errors while parsing {filepath:?}", tokenization.error_positions().len());

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


#[test]
fn location() {
    let text: Text =
"mod Foo {
    incoming clock : Clock;
}
".into();
    let tokenization = tokenize(text.clone());

    assert!(!tokenization.has_errors());

    let positions = tokenization.positions();
    assert_eq!(positions.len(), 10); // Don't forget Eof

    let linecols = [
        (1, 1), (1, 5), (1, 9),
        (2, 5), (2, 14), (2, 20), (2, 22), (2, 27),
        (3, 1),
        (4, 1),
    ];

    for i in 0..tokenization.len() {
        let token_pos = tokenization.positions()[i];
        let token_linecol = tokenization.linecol(token_pos);
        let (line, col) = linecols[i];
        assert_eq!(token_linecol.line(), line, "Token {i} is not on the correct line");
        assert_eq!(token_linecol.col(), col, "Token {i} is not in the correct col");
    }
}

#[test]
fn span() {
    let text: Text =
"mod Foo {
    incoming clock : Clock;
}
".into();
    let tokenization = tokenize(text.clone());

    assert!(!tokenization.has_errors());

    let positions = tokenization.positions();
    assert_eq!(positions.len(), 10);

    let linecols = [
        ((1,  1), (1,  4)), // mod
        ((1,  5), (1,  8)), // Foo
        ((1,  9), (1, 10)), // {
        ((2,  5), (2, 13)), // incoming
        ((2, 14), (2, 19)), // clock
        ((2, 20), (2, 21)), // :
        ((2, 22), (2, 27)), // Clock
        ((2, 27), (2, 28)), // ;
        ((3,  1), (3,  2)), // }
        ((4,  1), (4,  1)), // Eof
    ];

    for i in 0..tokenization.len() {
        let token_pos = tokenization.positions()[i];
        let span = tokenization.span(token_pos);
        let start = span.start();
        let end = span.end();

        let ((start_line, start_col), (end_line, end_col)) = linecols[i];

        assert_eq!(start.line(), start_line, "Token {i} start is not on the correct line");
        assert_eq!(start.col(), start_col, "Token {i} start is not in the correct col");
        assert_eq!(end.line(), end_line, "Token {i} end is not on the correct line");
        assert_eq!(end.col(), end_col, "Token {i} end is not in the correct col");
    }
}
