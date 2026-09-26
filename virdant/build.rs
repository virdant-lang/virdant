//! Build script that invokes `lalrpop` to generate the parser from the
//! grammar, emitting the grammar text while stripping grammar positions
//! and errors.

fn main() {
    lalrpop::Configuration::new()
        .emit_grammar(true)
        .strip_grammar_positions(true)
        .strip_grammar_errors(true)
        .process()
        .unwrap();
}
