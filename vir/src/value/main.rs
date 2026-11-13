//! The `vir-value` binary: standalone value encode/decode for
//! Virdant projects (see plans/vir-value/PLAN.md).
//!
//! `vir value` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.
//!
//! ```
//! vir value encode @Valid(0) : Valid[Word[8]]   # value -> bit pattern
//! vir value decode 0b0000000010 Valid[Word[8]]  # bit pattern -> value
//! ```
//!
//! Types are fully-qualified (`pkg::Name[...]`) except names from the
//! `builtin` package, which are bare (`Bit`, `Word[8]`,
//! `Valid[Word[8]]`).  Non-pub types resolve the same way: lookup is
//! by fully-qualified name, so visibility plays no role.

mod ast;
mod decode;
mod encode;
mod lexer;
mod resolve;

// Generated from `grammar.lalrpop` by the build script (into OUT_DIR).
lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/value/grammar.rs"
);

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::ProjectLocation;
use virdant::db::Db;
use virdant::sim::BitPat;
use virdant::typelayout::TypeLayoutIndex;
use virdant::types::typ::Type;


use resolve::Resolver;

/// Assemble the `vir-value` command line interface.
fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-value")
        .about("Encode and decode Virdant values")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            clap::Command::new("encode")
                .about("Encode a value to a bit pattern")
                .arg(
                    clap::Arg::new("parts")
                        .value_name("VALUE [: TYPE]")
                        .num_args(1..)
                        .required(true)
                        .help("Value expression, optionally with a `: Type` ascription"),
                ),
        )
        .subcommand(
            clap::Command::new("decode")
                .about("Decode a bit pattern to a value")
                .arg(
                    clap::Arg::new("parts")
                        .value_name("PATTERN TYPE")
                        .num_args(2..)
                        .required(true)
                        .help("Bit pattern literal and the type to decode it as"),
                ),
        );
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    help_json::maybe_dump(|| command());
    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);

    let (subcommand, parts): (&str, Vec<String>) = match matches.subcommand() {
        Some(("encode", m)) => (
            "encode",
            m.get_many::<String>("parts")
                .map(|vs| vs.cloned().collect())
                .unwrap_or_default(),
        ),
        Some(("decode", m)) => (
            "decode",
            m.get_many::<String>("parts")
                .map(|vs| vs.cloned().collect())
                .unwrap_or_default(),
        ),
        _ => unreachable!("subcommand_required"),
    };

    // The CLI is shell-friendly: the positional arguments are joined
    // with spaces and parsed as one string, so both
    // `encode @Valid(0) : Valid[Word[8]]` and
    // `encode '@Valid(0):Valid[Word[8]]'` work.
    let input = parts.join(" ");

    let db = location.project_db();

    match run(&db, subcommand, &input) {
        Ok(output) => println!("{output}"),
        Err(message) => {
            eprintln!("ERROR: {message}");
            std::process::exit(1);
        }
    }
}

#[path = "../common/help_json.rs"]
mod help_json;

/// Parse, resolve, and run one encode/decode invocation against the
/// project `Db`.
fn run(db: &Db, subcommand: &str, input: &str) -> Result<String, String> {
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

    /// Build a `Db` over `builtin.vir` plus a small scratch package
    /// (the same pattern as `virdant/src/verilog/conversion.rs`'s
    /// unit tests).
    fn scratch_db() -> Db {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let builtin = std::fs::read_to_string(root.join("../lib/builtin.vir")).unwrap();
        let scratch = r#"
            union type Scratch[T : Type] {
                Empty
                Full(data : T)
            }
        "#;
        use bstr::BString;
        let mut db = Db::new();
        let names: Vec<BString> = vec![BString::from("builtin"), BString::from("scratch")];
        db.set_packages(virdant::package::PackageTable::new(names));
        let packages = db.get_packages();
        for (name, text) in [("builtin", builtin.as_str()), ("scratch", scratch)] {
            let package = packages
                .id(bstr::BStr::new(name))
                .unwrap();
            db.set_source(package, virdant::common::source::Source::new(package, text.to_string().into()));
        }
        db
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
    fn ctor_prefix_spellings() {
        // `#` (poke-style) is accepted as an alias for `@` (Virdant
        // source syntax); the decode output is always `@`.
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
    fn decode_ctors_prefixed_with_at() {
        // Payload constructors (union variants and payload-carrying
        // enum enumerants) decode with `@Name(...)` — the Virdant
        // source syntax — even though `#` is accepted on input.  At
        // the top level and at every nesting level.
        let db = scratch_db();
        assert_eq!(
            run(&db, "decode", "0b0000000010 Valid[Word[8]]").unwrap(),
            "@Valid(0)"
        );
        assert_eq!(
            run(&db, "decode", "0b1010011110 scratch::Scratch[Word[8]]").unwrap(),
            "@Full(167)"
        );

        // Nested constructors: `@` on the outer and the inner.
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
        assert_eq!(out, "@Valid(0)");
        let out = run(&db, "decode", "0b0000000001 Valid[Word[8]]").unwrap();
        // Zero-payload enum enumerants decode to the `#` spelling.
        assert_eq!(out, "#Invalid");
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
