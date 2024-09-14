use virdant::parser::ParseError;
use virdant::Virdant;
use virdant::tokenizer::Token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args[1] == "--parse" {
        let top_path: String = args[2].clone().into();
        dump_parse(&top_path);
    } else if args[1] == "--tokenize" {
        let top_path: String = args[2].clone().into();
        dump_tokenize(&top_path);
    } else {
        let top_path: String = args[1].clone().into();
        let mut virdant = Virdant::new(&top_path);

        match virdant.check() {
            Err(errors) => {
                eprintln!("{virdant:?}");
                eprintln!("ERRORS:");
                for error in errors.into_iter() {
                    eprintln!("    {error:?}");
                }
                std::process::exit(1);
            },
            Ok(_) => println!("{virdant:?}"),
        }
    }
}

fn dump_parse(filename: &str) {
    let text = std::fs::read_to_string(filename).unwrap();
    let results = virdant::parser::parse(&text);
    println!();
    match results {
        Err(errors) => {
            for error in errors {
                let (lineno, col) = token_to_linecol(&text, error.token());
                eprint!("ERROR: line {lineno} col {col}: ");
                match error {
                    ParseError::Unexpected(token) => eprintln!("Unexpected token: {:?}", token.kind()),
                    ParseError::Expected(expected_item_kind, token) => eprintln!("Expected {:?} but found {:?}", expected_item_kind, token.kind()),
                    ParseError::ExpectedItemStart(token) => eprintln!("Expected the start of an item, but found{:?}", token.kind()),
                    ParseError::Unknown(token) => eprintln!("UNKNOWN error near {token:?}"),
                }
            }
        },
        Ok(ast) => {
            dbg!(&ast);
        },
    }
}

fn token_to_linecol(text: &str, token: Token) -> (usize, usize) {
    let pos = usize::from(token.pos());

    let mut lineno = 1;
    let mut col = 0;

    for (i, ch) in text.as_bytes().iter().copied().enumerate() {
        if pos == i {
            break;
        }
        if ch == b'\n' {
            col = 1;
            lineno += 1;
        } else {
            col += 1;
        }
    }

    (lineno, col)
}

fn dump_tokenize(filename: &str) {
    let text = std::fs::read_to_string(filename).unwrap();
    virdant::tokenizer::pretty_token_debug(&text);
}
