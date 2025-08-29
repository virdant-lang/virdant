use std::path::PathBuf;

use bstr::BStr;
use clap::Parser;
use virdant::source::Source;
use virdant::fqn::PackageFqn;
use virdant::token::tokenize;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    filepath: String,
}

pub fn main() {
    let args = Args::parse();
    let filepath = PathBuf::from(&args.filepath);

    let Some(package) = filepath.file_stem().map(|os_str| os_str.as_encoded_bytes()) else {
        panic!()
    };

    let package = PackageFqn::new(package.into());
    let text = std::fs::read(&args.filepath).unwrap_or_else(|_e| {
        panic!();
    });
    let source = Source::new(package, &text);
    let tokens = tokenize(BStr::new(text.as_slice()));

    for maybe_token in tokens {
        let (ll, token, rr) = maybe_token.unwrap();
        let span = source.to_region(ll, rr).span();
        let span_str = format!("{span}");
        let token_str = format!("{token:?}");
        let token_text = BStr::new(&source[span]);
        eprintln!("{token_str:<13} {span_str:<12} {token_text}");
    }
}
