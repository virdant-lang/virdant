//! Constraint emission: turns a platform item's `@pin`/`@period_ns`
//! metadata into the PCF file nextpnr-ice40 consumes, or the XDC file
//! nextpnr-xilinx consumes.

use std::fmt::Write;

use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexSet;

use crate::analysis::platform::{platform_clocks, platform_pins};
use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::build::toolchain::Toolchain;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};

/// Emits constraints for a platform item in the toolchain's format,
/// returning the build directory file extension together with the
/// content: PCF for ice40, XDC for xilinx.  The toolchain runners read
/// the same `<project>.<extension>` name from the build directory.
pub fn emit_constraints(
    builder: &mut Builder,
    platform_id: SymbolId,
    toolchain: Toolchain,
) -> Result<(&'static str, String), Vec<Diagnostic>> {
    match toolchain {
        Toolchain::Ice40 => emit_pcf(builder, platform_id).map(|text| ("pcf", text)),
        Toolchain::Xilinx => emit_xdc(builder, platform_id).map(|text| ("xdc", text)),
    }
}

/// Diagnostics for every platform port without a `@pin`.  Both
/// emitters refuse to emit an incomplete constraints file: a missing
/// pin would otherwise leave the port unconstrained at place-and-route
/// time (Decision 13: point-of-use errors).
fn require_pins(builder: &mut Builder, platform_id: SymbolId) -> Result<(), Vec<Diagnostic>> {
    let pins = platform_pins(builder, platform_id);

    let pinned: IndexSet<BString> = pins.iter().map(|(port, _)| port.clone()).collect();
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();
    for slot in symboltable.slots(platform_id) {
        if slot.kind() != SymbolKind::Component {
            continue;
        }
        if !pinned.contains(slot.name()) {
            let region = builder.get_location_region(slot.location());
            diagnostics.push(Diagnostic::new(
                region,
                diagnostics::Unknown {
                    message: format!(
                        "Platform port {} is missing a @pin annotation",
                        slot.name().to_str_lossy(),
                    ).into(),
                },
            ));
        }
    }
    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(diagnostics)
    }
}

/// Emits the PCF for a platform item: one `set_io` line per pinned
/// port, one `set_frequency` line per clock port (platforms with
/// several clocks simply get several lines; Decision 13 forbids
/// cardinality constraints).  A platform port with no `@pin` yields
/// an `Unknown` diagnostic naming the port, and no file content is
/// produced — the PCF would otherwise be silently incomplete
/// (Decision 13: point-of-use errors).
pub fn emit_pcf(builder: &mut Builder, platform_id: SymbolId) -> Result<String, Vec<Diagnostic>> {
    require_pins(builder, platform_id)?;
    let pins = platform_pins(builder, platform_id);
    let clocks = platform_clocks(builder, platform_id);

    let mut out = String::new();
    for (port, pin) in &pins {
        let _ = writeln!(out, "set_io {port} {pin}");
    }
    for (port, _pin, period_ns) in &clocks {
        let mhz = (1000.0 / period_ns * 10.0).round() / 10.0;
        let _ = writeln!(out, "set_frequency {port} {mhz:.1}");
    }
    Ok(out)
}

/// Emits the XDC for a platform item: one `set_property LOC` and
/// `set_property IOSTANDARD LVCMOS33` pair per pinned port, and one
/// `create_clock` per clock port.  nextpnr-xilinx reads the `LOC`
/// property (not `PACKAGE_PIN`), and its `create_clock` target lookup
/// does not strip `{}` braces the way `set_property`'s does, so port
/// names are written bare.  The I/O standard is hardcoded `LVCMOS33`:
/// the Arty A7's banks are 3.3 V, and a platform on other-voltage
/// banks would need the standard spelled out in its port
/// annotations, which do not exist yet.
pub fn emit_xdc(builder: &mut Builder, platform_id: SymbolId) -> Result<String, Vec<Diagnostic>> {
    require_pins(builder, platform_id)?;
    let pins = platform_pins(builder, platform_id);
    let clocks = platform_clocks(builder, platform_id);

    let mut out = String::new();
    for (port, pin) in &pins {
        let _ = writeln!(out, "set_property LOC {pin} [get_ports {port}]");
        let _ = writeln!(out, "set_property IOSTANDARD LVCMOS33 [get_ports {port}]");
    }
    for (port, _pin, period_ns) in &clocks {
        let _ = writeln!(out, "create_clock -period {period_ns} [get_ports {port}]");
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use bstr::BString;

    use crate::common::source::Source;
    use crate::db::Db;
    use crate::package::PackageTable;

    use super::*;

    fn db_from_pairs(files: &[(&str, String)]) -> Db {
        let mut db = Db::new();
        let names: Vec<BString> = files.iter().map(|(name, _)| BString::from(*name)).collect();
        db.set_packages(PackageTable::new(names));
        let packages = db.get_packages();
        for (name, text) in files {
            let package = packages.id(name.as_bytes().into()).unwrap();
            db.set_source(package, Source::new(package, text.clone().into()));
        }
        db
    }

    fn platform_symbol_id(db: &Db) -> SymbolId {
        let symboltable = db.get_symboltable();
        let platform_symbol = symboltable
            .items()
            .into_iter()
            .find(|s| s.kind() == SymbolKind::Platform)
            .unwrap();
        platform_symbol.id()
    }

    fn builder(db: &Db) -> crate::db::Builder {
        crate::db::Builder::new(db)
    }

    #[test]
    fn pcf_pins_and_clock() {
        let design = r#"platform IceSugar {
    @period_ns("83.33")
    @pin(35)
    incoming clock : Clock

    @pin(39)
    outgoing led_red : Bit

    @pin("E3")
    incoming switch0 : Bit
}
"#.to_string();
        let mut db = db_from_pairs(&[("builtin", builtin_source()), ("design", design)]);
        let platform_id = platform_symbol_id(&db);
        let mut builder = builder(&db);
        let pcf = emit_pcf(&mut builder, platform_id).unwrap();
        assert_eq!(
            pcf,
            "set_io clock 35\n\
             set_io led_red 39\n\
             set_io switch0 E3\n\
             set_frequency clock 12.0\n"
        );
    }

    #[test]
    fn pcf_multiple_clocks() {
        let design = r#"platform IceSugar {
    @period_ns("83.33")
    @pin(35)
    incoming clk1 : Clock

    @period_ns(10)
    @pin(36)
    incoming clk2 : Clock
}
"#.to_string();
        let mut db = db_from_pairs(&[("builtin", builtin_source()), ("design", design)]);
        let platform_id = platform_symbol_id(&db);
        let mut builder = builder(&db);
        let pcf = emit_pcf(&mut builder, platform_id).unwrap();
        assert_eq!(
            pcf,
            "set_io clk1 35\n\
             set_io clk2 36\n\
             set_frequency clk1 12.0\n\
             set_frequency clk2 100.0\n"
        );
    }

    #[test]
    fn xdc_pins_and_clock() {
        let design = r#"platform ArtyA7 {
    @period_ns("10")
    @pin("E3")
    incoming clock : Clock

    @pin("H5")
    outgoing led0 : Bit

    @pin("A8")
    incoming switch0 : Bit
}
"#.to_string();
        let mut db = db_from_pairs(&[("builtin", builtin_source()), ("design", design)]);
        let platform_id = platform_symbol_id(&db);
        let mut builder = builder(&db);
        let xdc = emit_xdc(&mut builder, platform_id).unwrap();
        assert_eq!(
            xdc,
            "set_property LOC E3 [get_ports clock]\n\
             set_property IOSTANDARD LVCMOS33 [get_ports clock]\n\
             set_property LOC H5 [get_ports led0]\n\
             set_property IOSTANDARD LVCMOS33 [get_ports led0]\n\
             set_property LOC A8 [get_ports switch0]\n\
             set_property IOSTANDARD LVCMOS33 [get_ports switch0]\n\
             create_clock -period 10 [get_ports clock]\n"
        );
    }

    #[test]
    fn xdc_missing_pin_is_unknown_diagnostic() {
        let design = r#"platform ArtyA7 {
    @period_ns("10")
    @pin("E3")
    incoming clock : Clock

    outgoing no_pin : Bit
}
"#.to_string();
        let mut db = db_from_pairs(&[("builtin", builtin_source()), ("design", design)]);
        let platform_id = platform_symbol_id(&db);
        let mut builder = builder(&db);
        let Err(diagnostics) = emit_xdc(&mut builder, platform_id) else {
            panic!("expected missing-pin diagnostics");
        };
        assert_eq!(diagnostics.len(), 1);
        let message = match &diagnostics[0].payload {
            crate::diagnostics::DiagnosticPayload::Unknown(unknown) => {
                unknown.message.to_string()
            }
            payload => panic!("expected Unknown diagnostic, got {payload:?}"),
        };
        assert!(message.contains("no_pin"), "message: {message}");
        assert!(message.contains("missing a @pin"), "message: {message}");
    }

    #[test]
    fn pcf_missing_pin_is_unknown_diagnostic() {
        let design = r#"platform IceSugar {
    @period_ns("83.33")
    @pin(35)
    incoming clock : Clock

    outgoing no_pin : Bit
}
"#.to_string();
        let mut db = db_from_pairs(&[("builtin", builtin_source()), ("design", design)]);
        let platform_id = platform_symbol_id(&db);
        let mut builder = builder(&db);
        let Err(diagnostics) = emit_pcf(&mut builder, platform_id) else {
            panic!("expected missing-pin diagnostics");
        };
        assert_eq!(diagnostics.len(), 1);
        let message = match &diagnostics[0].payload {
            crate::diagnostics::DiagnosticPayload::Unknown(unknown) => {
                unknown.message.to_string()
            }
            payload => panic!("expected Unknown diagnostic, got {payload:?}"),
        };
        assert!(message.contains("no_pin"), "message: {message}");
        assert!(message.contains("missing a @pin"), "message: {message}");
    }

    fn builtin_source() -> String {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        std::fs::read_to_string(root.join("../lib/builtin.vir")).unwrap()
    }
}
