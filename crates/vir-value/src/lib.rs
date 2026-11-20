//! The `vir-value` library: standalone value encode/decode for
//! Virdant projects.
//!
//! ```text
//! encode "@Valid(0) : Valid[Word[8]]"           # value -> bit pattern
//! decode "0b0000000010 Valid[Word[8]]"          # bit pattern -> value
//! ```
//!
//! Types are fully-qualified (`pkg::Name[...]`) except names from the
//! `builtin` package, which are bare (`Bit`, `Word[8]`,
//! `Valid[Word[8]]`).  Non-pub types resolve the same way: lookup is
//! by fully-qualified name, so visibility plays no role.
//!
//! This is the WASM-clean core of the `vir-value` binary: every
//! public entry point is a pure function of its arguments (a `Db`
//! plus a string), with no file or process access.  Build a `Db`
//! with [`db_from_sources`] (in memory — the WASM seam) or from
//! files on disk in the host binary.

mod ast;
mod decode;
mod encode;
mod lexer;
mod resolve;

// Generated from `grammar.lalrpop` by the build script (into OUT_DIR).
lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/grammar.rs"
);

use virdant::db::Db;
use virdant::sim::BitPat;
use virdant::typelayout::TypeLayoutIndex;
use virdant::types::typ::Type;

use bstr::BString;
use resolve::Resolver;

/// Build a `Db` from in-memory package sources: a list of
/// `(package name, source text)` pairs.  This is the seam the
/// `vir-value` CLI and the WASM front-end share: the native binary
/// loads sources from the project on disk and calls this; a WASM
/// module receives the sources from the embedder.
///
/// No builtin library is added automatically; pass `("builtin",
/// BUILTIN_VIR)` yourself (the `BUILTIN_VIR` constant has it).
pub fn db_from_sources(sources: Vec<(&str, String)>) -> Db {
    let mut db = Db::new();
    let names: Vec<BString> = sources.iter().map(|(name, _)| BString::from(*name)).collect();
    db.set_packages(virdant::package::PackageTable::new(names));
    let packages = db.get_packages();
    for (name, text) in sources {
        let package = packages.id(bstr::BStr::new(name)).unwrap();
        db.set_source(package, virdant::common::source::Source::new(package, text.into()));
    }
    db
}

/// The builtin library (`lib/builtin.vir`), embedded at compile time
/// so WASM builds do not need filesystem access.
pub const BUILTIN_VIR: &str = include_str!("../../../lib/builtin.vir");

/// Parse and resolve one type spelling against the project `Db`.
///
/// The input is a type exactly as it appears after the pattern in a
/// `decode` invocation (`Valid[Word[8]]`, `pkg::Name[...]`, bare for
/// builtins), parsed by the same grammar and resolved by the same
/// resolver.  Callers that need a semantic [`Type`] without a value
/// in hand (rendering decisions, layout queries) use this instead of
/// round-tripping through a dummy decode.
pub fn resolve_type(db: &Db, input: &str) -> Result<Type, String> {
    // The grammar has no standalone type production; the `decode`
    // one is `PatternLit VType`, so a leading `0` (a `Nat` pattern
    // literal) is parsed and discarded.
    let toks = lexer::lex(&format!("0 {input}")).map_err(|e| {
        format!(
            "{} at byte {}: {}",
            e.message,
            e.offset,
            render_context(input, e.offset),
        )
    })?;
    let (_, vt) = grammar::DecodeParser::new()
        .parse(toks)
        .map_err(|e| format!("parse error: {e}"))?;
    Resolver::new(db).resolve_type(&vt)
}

/// Parse, resolve, and run one encode/decode invocation against the
/// project `Db`.
pub fn run(db: &Db, subcommand: &str, input: &str) -> Result<String, String> {
    let toks = lexer::lex(input).map_err(|e| {
        format!(
            "{} at byte {}: {}",
            e.message, e.offset, render_context(input, e.offset)
        )
    })?;

    let resolver = Resolver::new(db);
    match subcommand {
        "encode" => {
            let expr = grammar::EncodeParser::new()
                .parse(toks)
                .map_err(|e| format!("parse error: {e}"))?;
            // Pass 1: infer the top-level type.  With no expected type
            // available anywhere, that is the `Can't infer type` error.
            let expected = encode::infer_type(&resolver, &expr)?
                .ok_or_else(|| "Can't infer type".to_string())?;
            // Pass 2: encode against the inferred type.
            let layouts = TypeLayoutIndex::for_types(db, vec![expected.clone()]);
            let bits = encode::encode(&resolver, &layouts, &expected, &expr)?;
            Ok(format!("0b{}", bits.to_bit_string()))
        }
        "decode" => {
            let (pattern, vt) = grammar::DecodeParser::new()
                .parse(toks)
                .map_err(|e| format!("parse error: {e}"))?;
            let typ: Type = resolver.resolve_type(&vt)?;
            let layouts = TypeLayoutIndex::for_types(db, vec![typ.clone()]);
            let width = layouts.type_width(&typ);
            let bits: BitPat = decode::pattern_bits(&pattern, width)?;
            decode::decode(&resolver, &layouts, &typ, &bits)
        }
        _ => unreachable!("subcommand dispatch"),
    }
}

/// A one-line excerpt of `input` around `offset`, for lex errors.
fn render_context(input: &str, offset: usize) -> String {
    let start = input[..offset.min(input.len())]
        .rfind('\n')
        .map(|p| p + 1)
        .unwrap_or(0);
    let end = input[start..]
        .find('\n')
        .map(|p| start + p)
        .unwrap_or(input.len());
    let line = &input[start..end];
    let col = offset.min(input.len()) - start;
    format!("{line}\n{}^", " ".repeat(col))
}

#[cfg(test)]
mod tests {
    use super::*;
    use virdant::types::render::render_type_fqn;

    /// Build a `Db` over `builtin.vir` plus a small scratch package
    /// (the same pattern as `virdant/src/verilog/conversion.rs`'s
    /// unit tests).
    fn scratch_db() -> Db {
        let scratch = r#"
            union type Scratch[T : Type] {
                Empty
                Full(data : T)
            }
        "#;
        let builtin = BUILTIN_VIR;
        db_from_sources(vec![("builtin", builtin.to_string()), ("scratch", scratch.to_string())])
    }

    fn run_ok(input: &str) -> String {
        let db = scratch_db();
        run(&db, "encode", input).unwrap_or_else(|e| panic!("encode '{input}' failed: {e}"))
    }

    fn run_err(subcommand: &str, input: &str) -> String {
        let db = scratch_db();
        let err = run(&db, subcommand, input)
            .expect_err(&format!("{subcommand} '{input}' should fail"));
        err
    }

    #[test]
    fn resolve_type_examples() {
        let db = scratch_db();
        let t = resolve_type(&db, "scratch::Scratch[Word[4]]").unwrap();
        // render_type_fqn qualifies builtins too; only the spelling round-trips.
        assert_eq!(render_type_fqn(&db, &t), "scratch::Scratch[builtin::Word[4]]");
        assert!(resolve_type(&db, "Word[8]").is_ok());
        // Unknown names resolve like decode's do, with the same hint.
        assert!(resolve_type(&db, "Nope").unwrap_err().contains("unknown type 'Nope'"));
    }

    #[test]
    fn ctor_and_enumerant_spellings() {
        // `#` (poke-style) is accepted as an alias for `@` (Virdant
        // source syntax) for both union variants and enum
        // enumerants; the decode output always round-trips.
        assert_eq!(run_ok("#Valid(0w8)"), run_ok("@Valid(0w8)"));
        assert_eq!(run_ok("#Invalid : Valid[Word[8]]"), "0b0000000001");
    }

    #[test]
    fn encode_examples() {
        // The plan's examples (with the sim-canonical tag encoding).
        assert_eq!(run_ok("@Valid(0) : Valid[Word[8]]"), "0b0000000010");
        assert_eq!(run_ok("@Valid(0w8)"), "0b0000000010");
    }

    #[test]
    fn encode_cant_infer() {
        assert_eq!(run_err("encode", "@Valid(0)"), "Can't infer type");
    }

    #[test]
    fn encode_words_and_bits() {
        assert_eq!(run_ok("0w8"), "0b00000000");
        assert_eq!(run_ok("0b1010 : Word[4]"), "0b1010");
        assert_eq!(run_ok("true"), "0b1");
        assert_eq!(run_ok("0b1 : Clock"), "0b1");
    }

    #[test]
    fn encode_roundtrip_union() {
        // Decode the encoded pattern; re-render round-trips.
        let db = scratch_db();
        let bits = run(&db, "encode", "@Full(0xA7w8)").unwrap();
        assert_eq!(bits, "0b1010011110");
        let value = run(&db, "decode", "0b1010011110 scratch::Scratch[Word[8]]").unwrap();
        assert_eq!(value, "@Full(167)");
    }

    #[test]
    fn decode_spellings_union_at_and_enum_pound() {
        // Types with payload-carrying members decode with
        // `@Name(...)` for *every* member (parens even when a
        // member has no payload); pure enums (all members
        // zero-param) decode with `#Name`.  At the top level and
        // at every nesting level.
        let db = scratch_db();
        assert_eq!(
            run(&db, "decode", "0b0000000010 Valid[Word[8]]").unwrap(),
            "@Valid(0)"
        );
        assert_eq!(
            run(&db, "decode", "0b1010011110 scratch::Scratch[Word[8]]").unwrap(),
            "@Full(167)"
        );

        // Nested variants: `@` on the outer and the inner
        // (both are unions).
        // Scratch[Scratch[Word[4]]] is 12 bits: outer tag (2) +
        // inner Full pattern (payload 1010 + tag 10 -> 101010).
        let bits = run(&db, "encode", "@Full(@Full(0b1010)) : scratch::Scratch[scratch::Scratch[Word[4]]]")
            .unwrap_or_else(|e| panic!("nested encode failed: {e}"));
        assert_eq!(bits, "0b10101010");
        assert_eq!(
            run(&db, "decode", "0b10101010 scratch::Scratch[scratch::Scratch[Word[4]]]").unwrap(),
            "@Full(@Full(10))"
        );
    }

    #[test]
    fn decode_roundtrip() {
        let db = scratch_db();
        let out = run(&db, "decode", "0b0000000010 Valid[Word[8]]").unwrap();
        // Valid[T]'s members carry payloads, so every member
        // decodes to the `@Name(...)` spelling.
        assert_eq!(out, "@Valid(0)");
        let out = run(&db, "decode", "0b0000000001 Valid[Word[8]]").unwrap();
        assert_eq!(out, "@Invalid()");
    }

    #[test]
    fn decode_errors() {
        // Wrong width: a Word literal's width must match exactly (a
        // Nat literal zero-extends to the type width).
        assert!(run_err("decode", "0b101w3 Valid[Word[8]]").contains("wide"));
        // Unknown tag (one-hot violated).
        let e = run_err("decode", "0b0000000011 Valid[Word[8]]");
        assert!(e.contains("unknown tag"), "{e}");
        // Non-zero padding: `Empty` has no payload, so the 8 bits above
        // the tag must be zero.
        assert!(
            run_err("decode", "0b1111111101 scratch::Scratch[Word[8]]").contains("padding")
        );
    }

    #[test]
    fn encode_ctor_arity_error() {
        // Arity is checked during constructor lookup.
        assert!(run_err("encode", "@Empty(0)").contains("no constructor named '@Empty'"));
    }

    #[test]
    fn encode_errors() {
        assert!(
            run_err("encode", "@Valid(0 : Word[8]) : Valid[Word[4]]").contains("does not match")
        );
        assert!(
            run_err("encode", "@Valid(0w4) : Valid[Word[8]]").contains("4 bits wide, but expected type is 8")
        );
        assert!(run_err("encode", "0w4 : Valid[Word[8]]").contains("got a word literal"));
        assert!(run_err("encode", "@Nope(0w8)").contains("no constructor"));
        assert!(run_err("encode", "0 : NotAType[3]").contains("unknown type"));
        assert!(run_err("encode", "@Valid(0) : Valid[Word[8], Bit]").contains("expects 1 parameter"));
    }

    #[test]
    fn encode_qualified_and_inference() {
        // Inference from a Word payload through a qualified union.
        assert_eq!(run_ok("@Full(0w8)"), "0b0000000010");
        // Without inference: a Nat payload can't determine the width.
        assert!(run_err("encode", "@Full(0)").contains("Can't infer type"));
        // Ascription inside determines it.
        assert_eq!(run_ok("@Full(0 : Word[8])"), "0b0000000010");
        // A Word payload infers its own width (tag 2 + 4 payload bits).
        assert_eq!(run_ok("@Valid(0w4)"), "0b000010");
    }

    #[test]
    fn decode_requires_qualified_names() {
        assert!(run_err("decode", "0b1 Scratch[Word[8]]").contains("fully qualified"));
    }

    #[test]
    fn lexer_and_parse_errors_are_reported() {
        assert!(run_err("encode", "0b1010 + 1").contains("unexpected character"));
        assert!(run_err("encode", "@Valid(0").contains("parse error"));
    }
}
