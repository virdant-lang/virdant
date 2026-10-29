//! Compatibility shim: `vir-filecheck <check-file>` behaves exactly like
//! `vir-virtue --filecheck <check-file>`.  Kept for direct invocations
//! of the matcher (and `make install`).

#[path = "../help_json.rs"]
mod help_json;

#[path = "../virtue/filecheck.rs"]
mod filecheck;

fn main() {
    help_json::maybe_dump(|| {
        clap::Command::new("vir-filecheck")
            .about("Standalone FileCheck matcher")
            .long_about(
                "Matches CHECK directives in <check-file> against stdin. \
                 Prints errors to stderr and exits 1 on failure.",
            )
            .arg(
                clap::Arg::new("check-file")
                    .value_name("CHECK_FILE")
                    .help("File containing the CHECK directives")
                    .required(true)
                    .value_hint(clap::ValueHint::FilePath),
            )
    });

    filecheck::cli_main(std::env::args().nth(1));
}
