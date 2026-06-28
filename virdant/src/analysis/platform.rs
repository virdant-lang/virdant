use bstr::{BStr, ByteSlice};
use std::sync::Arc;

use crate::common::ComponentKind;
use crate::common::PortDir;
use crate::db::Builder;
use crate::diagnostics::{Diagnostic};
use crate::fqn::PackageFqn;
use crate::common::source::Region;
use crate::syntax::ast::{item_children, AstNode};
use crate::syntax::payload::AstNodePayload;

/// Strip the surrounding double quotes from a `Str`-literal span and unescape
/// the trivial escapes.  The lexer matches the quotes, so an annotation
/// `@fpga("ice40")` interns the bytes `"ice40"` (with quotes).
fn unquote_str(s: &BStr) -> &BStr {
    let bytes = s.as_bytes();
    if bytes.len() >= 2 && bytes[0] == b'"' && bytes[bytes.len() - 1] == b'"' {
        BStr::new(&bytes[1..bytes.len() - 1])
    } else {
        s
    }
}

#[derive(Debug)]
pub struct PlatformAnalysis {
    pub name: bstr::BString,
    pub fpga: Option<bstr::BString>,
    pub part: Option<bstr::BString>,
    pub constraints_file: Option<bstr::BString>,
    pub clock: Option<ClockSource>,
    pub pins: Vec<PinBinding>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct ClockSource {
    pub port: bstr::BString,
    pub pin: PinLocator,
    pub period_ns: f64,
}

#[derive(Debug, Clone)]
pub struct PinBinding {
    pub port: bstr::BString,
    pub pin: PinLocator,
    pub direction: PortDir,
}

#[derive(Debug, Clone)]
pub enum PinLocator {
    Numeric(u64),
    Named(bstr::BString),
}

const VALID_FPGAS: &[&str] = &["ice40", "ecp5", "nexus", "gowin", "artix7"];

pub(crate) fn build_platform_analysis(
    builder: &mut Builder,
    package: PackageFqn,
) -> Arc<PlatformAnalysis> {
    let parsing = builder.get_parsing(package.clone());
    let package_analysis = builder.get_package_analysis(package.clone());
    let mut diagnostics = Vec::new();

    let Some(platform_id) = package_analysis.platform() else {
        diagnostics.push(
            crate::diagnostics::PlatformMissing { package: package.clone() }.into()
        );
        return Arc::new(PlatformAnalysis {
            name: package.as_ref().to_owned(),
            fpga: None, part: None, constraints_file: None,
            clock: None, pins: Vec::new(), diagnostics,
        });
    };

    let node = parsing.ast_node(platform_id);
    let AstNodePayload::Platform(p) = node.payload() else { unreachable!() };
    let name = parsing.string(p.name).to_owned();

    let platform_region = Region::new(package.clone(), node.span());

    let mut fpga = None;
    let mut part = None;
    let mut constraints_file = None;
    for anno in node.annotations() {
        let Some(an) = anno.annotation_name() else { continue };
        let key = parsing.string(an);
        if key == "fpga" {
            fpga = anno.annotation_string()
                .map(|s| unquote_str(parsing.string(s)).to_owned());
        } else if key == "part" {
            part = anno.annotation_string()
                .map(|s| unquote_str(parsing.string(s)).to_owned());
        } else if key == "constraints" {
            constraints_file = anno.annotation_string()
                .map(|s| unquote_str(parsing.string(s)).to_owned());
        }
    }

    if let Some(ref f) = fpga {
        if !VALID_FPGAS.iter().any(|v| f == *v) {
            diagnostics.push(
                crate::diagnostics::PlatformUnknownFpga {
                    fpga: f.clone(),
                    region: platform_region.clone(),
                }.into()
            );
        }
    }
    let mut pins = Vec::new();
    let mut clock: Option<ClockSource> = None;
    let mut seen: indexmap::IndexMap<bstr::BString, Region> = indexmap::IndexMap::new();

    for child in item_children(&node) {
        let AstNodePayload::Component(comp) = child.payload() else { continue };
        if !matches!(comp.kind, ComponentKind::Incoming | ComponentKind::Outgoing) {
            continue;
        }
        let port_name = parsing.string(comp.name).to_owned();
        let port_region = Region::new(package.clone(), child.span());

        if seen.contains_key(&port_name) {
            diagnostics.push(
                crate::diagnostics::DuplicateSlot {
                    item: name.clone().into(),
                    region: port_region,
                    slot: port_name.clone(),
                }.into()
            );
            continue;
        }
        seen.insert(port_name.clone(), port_region.clone());

        let mut pin_locator: Option<PinLocator> = None;
        let mut period_ns: Option<f64> = None;
        let mut pin_count = 0u32;
        for anno in child.annotations() {
            let Some(an) = anno.annotation_name() else { continue };
            let key = parsing.string(an);
            if key == "pin" {
                pin_count += 1;
                if let Some(s) = anno.annotation_string() {
                    pin_locator = Some(PinLocator::Named(
                        unquote_str(parsing.string(s)).to_owned()));
                } else if let Some(n) = anno.annotation_natural() {
                    pin_locator = Some(PinLocator::Numeric(n));
                }
            } else if key == "period_ns" {
                if let Some(s) = anno.annotation_string() {
                    period_ns = std::str::from_utf8(unquote_str(parsing.string(s)))
                        .ok().and_then(|s| s.parse::<f64>().ok());
                } else if let Some(n) = anno.annotation_natural() {
                    period_ns = Some(n as f64);
                }
            }
        }

        if pin_count == 0 {
            diagnostics.push(
                crate::diagnostics::PlatformMissingPin {
                    port: port_name.clone(),
                    region: port_region.clone(),
                }.into()
            );
            continue;
        } else if pin_count > 1 {
            diagnostics.push(
                crate::diagnostics::PlatformMultiplePins {
                    port: port_name.clone(),
                    region: port_region.clone(),
                }.into()
            );
            continue;
        }

        let direction = match comp.kind {
            ComponentKind::Incoming => PortDir::Input,
            ComponentKind::Outgoing => PortDir::Output,
            _ => unreachable!(),
        };
        let pin = pin_locator.unwrap();
        pins.push(PinBinding {
            port: port_name.clone(),
            pin: pin.clone(),
            direction,
        });

        if is_clock_type(&parsing, &child) {
            if let Some(period) = period_ns {
                if clock.is_some() {
                    diagnostics.push(
                        crate::diagnostics::PlatformMultipleClocks {
                            region: port_region.clone(),
                        }.into()
                    );
                } else {
                    clock = Some(ClockSource {
                        port: port_name.clone(),
                        pin,
                        period_ns: period,
                    });
                }
            } else {
                diagnostics.push(
                    crate::diagnostics::PlatformMissingPeriodNs {
                        port: port_name.clone(),
                        region: port_region.clone(),
                    }.into()
                );
            }
        }
    }

    if clock.is_none() {
        diagnostics.push(
            crate::diagnostics::PlatformMissingClock { region: platform_region }.into()
        );
    }

    Arc::new(PlatformAnalysis {
        name, fpga, part, constraints_file, clock, pins, diagnostics,
    })
}

fn is_clock_type(parsing: &crate::syntax::parsing::Parsing, node: &AstNode<'_>) -> bool {
    // A Component node's structural children are [Type, ...] (after the
    // Annotations wrapper is filtered out by item_children).
    // The Type wraps an Ofness whose name is the type name.
    for child in item_children(node) {
        if let AstNodePayload::Type(_) = child.payload() {
            let inner = child.child(0);
            if let AstNodePayload::Ofness(ofness) = inner.payload() {
                return parsing.string(ofness.name) == "Clock";
            }
        }
    }
    false
}

impl PinLocator {
    pub fn to_string(&self) -> String {
        match self {
            PinLocator::Numeric(n) => n.to_string(),
            PinLocator::Named(s) => s.to_string(),
        }
    }
}

impl PinBinding {
    pub fn port_str(&self) -> &BStr {
        self.port.as_bstr()
    }
}
