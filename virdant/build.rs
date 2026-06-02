fn main() {
    // Configure LALRPOP
    lalrpop::Configuration::new()
        .emit_grammar(true)
        .strip_grammar_positions(true)
        .strip_grammar_errors(true)
        .process()
        .unwrap();

    // Configure virdant-db
    let output_dir = std::env::var_os("OUT_DIR").unwrap();
    let db_rs_path = std::path::Path::new(&output_dir).join("db.rs");

    virdant_db::Configuration::new()
        .input_dir("src")              // Scan all .rs files in src/
        .output_file(db_rs_path)      // Generate db.rs in OUT_DIR
        .db_name("Db")                 // Name of generated struct
        .builder_name("Builder")       // Name of builder struct
        .query_enum_name("Query")      // Name of query enum
        .enable_tracing(true)         // Optional: trace logging
        .enable_graphviz(true)         // Optional: graphviz export
        .process()
        .unwrap();
}
