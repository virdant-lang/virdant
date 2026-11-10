//! Build script that invokes `lalrpop` to generate the parser from the
//! ideal grammar, emitting the grammar text while stripping grammar
//! positions and errors — mirroring the `virdant` crate's build script,
//! so `IDEAL_GRAMMAR.txt` renders in exactly the same style as
//! `GRAMMAR.txt`.

fn main() {
    lalrpop::Configuration::new()
        .emit_grammar(true)
        .strip_grammar_positions(true)
        .strip_grammar_errors(true)
        .process()
        .unwrap();
}
