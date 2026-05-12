fn main() {
    lalrpop::Configuration::new()
        .emit_grammar(true)
        .strip_grammar_positions(true)
        .strip_grammar_errors(true)
        .process()
        .unwrap();
}
