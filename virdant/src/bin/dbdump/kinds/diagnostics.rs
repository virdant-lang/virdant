//! Renders diagnostic-array query results as HTML cards.
//!
//! All six diagnostic-style kinds share one array-of-diagnostics shape.
//! Messages mirror `Diagnostic::message()` in `virdant/src/diagnostics.rs`.

use serde_json::Value as JsonValue;

use super::common::{badges, escape_html, muted, pre_json, region_string, Ctx};

pub fn render_check(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

pub fn render_check_drivers(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

pub fn render_combinational_cycle_check(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

pub fn render_match_coverage(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

pub fn render_syntax_errors(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

pub fn render_type_check(ctx: &Ctx, result: &JsonValue) -> String {
    render_diagnostics(ctx, result)
}

fn render_diagnostics(ctx: &Ctx, result: &JsonValue) -> String {
    let items = match result.as_array() {
        Some(items) => items,
        None => return pre_json(result),
    };
    if items.is_empty() {
        return muted("No diagnostics.");
    }

    let mut num_errors = 0usize;
    let mut num_warnings = 0usize;
    let mut num_infos = 0usize;
    let mut sorted: Vec<&JsonValue> = items.iter().collect();
    for diag in &sorted {
        let level = level_of(&diag["payload"]);
        match level {
            0 => num_errors += 1,
            1 => num_warnings += 1,
            _ => num_infos += 1,
        }
    }
    sorted.sort_by_key(|diag| level_of(&diag["payload"]));

    let mut body = String::new();
    body.push_str(&badges(num_errors, num_warnings, num_infos));
    for diag in &sorted {
        let payload = &diag["payload"];
        let level = level_of(payload);
        let region = region_string(ctx, &diag["region"]);
        let message = match payload_known_message(payload) {
            Some(message) => escape_html(&message),
            None => {
                let variant = payload_variant(payload);
                format!(
                    "{} {}",
                    escape_html(&variant),
                    pre_json(&payload[variant.as_str()]),
                )
            }
        };
        body.push_str(&format!(
            "<p><span class=\"badge {class}\">{name}</span> <code>{region}</code> {message}</p>\n",
            class = level_class(level),
            name = level_name(level),
            region = escape_html(&region),
            message = message,
        ));
    }
    body
}

fn level_of(payload: &JsonValue) -> u8 {
    match payload_variant(payload).as_str() {
        "NoRegDrivers"
        | "NotIt"
        | "UnusedSource"
        | "ReadFromSink"
        | "UnfilledHole"
        | "EmptyDriverBlock"
        | "RedundantDriver" => 1,
        "Todo" => 2,
        _ => 0,
    }
}

fn level_class(level: u8) -> &'static str {
    match level {
        0 => "error",
        1 => "warning",
        _ => "muted",
    }
}

fn level_name(level: u8) -> &'static str {
    match level {
        0 => "Error",
        1 => "Warning",
        _ => "Info",
    }
}

fn payload_variant(payload: &JsonValue) -> String {
    payload
        .as_object()
        .and_then(|map| map.keys().next().cloned())
        .unwrap_or_else(|| "?".to_string())
}

fn payload_known_message(payload: &JsonValue) -> Option<String> {
    let variant = payload_variant(payload);
    let data = &payload[variant.as_str()];
    match variant.as_str() {
        "SyntaxError" => {
            Some(format!("Syntax Error: {}", field_str(data, "message")))
        }
        "ImportNotAtTopError" => Some("Import not at top of file".to_string()),
        "ImportCycle" => {
            let cycle = str_list(data, "package_cycle");
            if cycle.len() > 1 {
                Some(format!("Import cycle: {}", cycle.join(" ")))
            } else if cycle.len() == 1 {
                Some(format!("Package imports itself: {}", cycle[0]))
            } else {
                Some("Import cycle: ?".to_string())
            }
        }
        "UnresolvedImportError" => {
            Some(format!("Unresolved import: {}", field_str(data, "imported_package")))
        }
        "DuplicateImport" => Some("Duplicate import".to_string()),
        "DuplicateItem" => Some("Duplicate Item".to_string()),
        "DuplicateSlot" => Some("Duplicate slot".to_string()),
        "UnresolvedPackage" => {
            Some(format!("Unresolved package {}", field_str(data, "package")))
        }
        "UnresolvedItem" => Some(format!("Unresolved item {}", field_str(data, "item"))),
        "UnresolvedType" => Some(format!("Unresolved type {}", field_str(data, "typ"))),
        "MissingOnClause" => {
            Some(format!("Missing on clause for reg {}", field_str(data, "component")))
        }
        "UnexpectedOnClause" => {
            Some(format!("Unexpected on clause {}", field_str(data, "component")))
        }
        "WrongDriverType" => {
            let driver_type_str = match data["expected_driver_type"].as_str() {
                Some("Continuous") => ":=",
                Some("Latched") => "<=",
                _ => "?",
            };
            Some(format!(
                "Wrong driver type for {}, expected {driver_type_str}",
                field_str(data, "target"),
            ))
        }
        "NoRegDrivers" | "NoDrivers" => {
            Some(format!("No drivers for {}", field_str(data, "target")))
        }
        "MultipleDrivers" => {
            Some(format!("Multiple drivers for {}", field_str(data, "target")))
        }
        "DriverForSink" => {
            Some(format!("Driver for sink {}", field_str(data, "target")))
        }
        "UnresolvedComponent" => {
            Some(format!("Unresolved component {}", field_str(data, "path")))
        }
        "ItNotInItBlock" => Some("'it' used outside of an it block".to_string()),
        "NotIt" => {
            Some(format!(
                "'{}' should be written as 'it' inside of this driver block",
                field_str(data, "component"),
            ))
        }
        "UnresolvedCtor" => {
            Some(format!("Unresolved constructor {}", field_str(data, "ctor")))
        }
        "UnusedSource" => Some(format!("Unused signal {}", field_str(data, "path"))),
        "ReadFromSink" => Some(format!("Read from sink {}", field_str(data, "path"))),
        "UnfilledHole" => {
            let name = data["name"].as_str().unwrap_or("?");
            match data["typ"].as_str() {
                Some(typ) => Some(format!("Unfilled hole: {name} : {typ}")),
                None => Some(format!("Unfilled hole: {name}")),
            }
        }
        "ModuleCycle" => {
            Some(format!("Module cycle: {}", str_list(data, "module_cycle").join(", ")))
        }
        "UnresolvedMethod" => {
            Some(format!(
                "Unresovled method: {} on type {}",
                field_str(data, "method"),
                field_str(data, "subject_typ"),
            ))
        }
        "WrongType" => {
            Some(format!(
                "Wrong type: Expected {} but found {}",
                field_str(data, "expected"),
                field_str(data, "actual"),
            ))
        }
        "Unknown" => Some(field_str(data, "message")),
        "DoesntFit" => {
            Some(format!(
                "Value doesn't fit: {} is a {}-bit value, but literal is type builtin::Word[{}]",
                field_num(data, "value"),
                field_num(data, "minwidth"),
                field_num(data, "width"),
            ))
        }
        "NotWordType" => {
            Some(format!(
                "Expected type {} which is not a Word type",
                field_str(data, "typ"),
            ))
        }
        "CantInfer" => Some("Can't infer".to_string()),
        "WrongArgCount" => Some("Wrong arg count".to_string()),
        "CantTruncate" => {
            Some(format!(
                "Cannot truncate Word[{}] to the larger type Word[{}]",
                field_num(data, "source_width"),
                field_num(data, "target_width"),
            ))
        }
        "Todo" => Some(format!("TODO: {}", field_str(data, "message"))),
        "MatchNotExhaustive" => {
            Some(format!(
                "Non-exhaustive match on {}: not covered: {}",
                field_str(data, "subject_typ"),
                field_str(data, "missing"),
            ))
        }
        "MatchOverlappingArm" => {
            Some(format!("Overlapping match arm: {}", field_str(data, "overlap")))
        }
        "MatchRedundantElse" => {
            Some("Redundant else arm: all values are already covered".to_string())
        }
        "MatchMultipleElse" => Some("Multiple else arms in match".to_string()),
        "MatchElseNotLast" => {
            Some("else arm must be the last arm of the match".to_string())
        }
        "InvalidDocstring" => {
            Some(format!(
                "Invalid docstring: content must start with a space, got \"{}\"",
                field_str(data, "content"),
            ))
        }
        "EnumUnknownWidth" => {
            Some(
                "Enum type's first enumerant does not have an inferrable width. \
                 Add an explicit width, e.g. `= 0wN`."
                    .to_string(),
            )
        }
        "DuplicateEnumValue" => {
            Some(format!(
                "Duplicate enum value: enumerants of {} share value {}",
                field_str(data, "enum_name"),
                field_num(data, "value"),
            ))
        }
        "InvalidWordIndexWidth" => {
            let array_width = field_num(data, "array_width");
            let index_width = field_num(data, "index_width");
            Some(format!(
                "Invalid word index width: Word[{array_width}] indexed by Word[{index_width}] requires {array_width} == 2^{index_width}",
            ))
        }
        "IndexOutOfBounds" => {
            Some(format!(
                "Bit index {} out of bound for Word[{}]",
                field_num(data, "index"),
                field_num(data, "array_width"),
            ))
        }
        "IndexRangeOutOfBounds" => {
            Some(format!(
                "Bit range {}..{} out of bounds for Word[{}]",
                field_num(data, "index_hi"),
                field_num(data, "index_lo"),
                field_num(data, "array_width"),
            ))
        }
        "InvalidIndexRange" => {
            Some(format!(
                "Invalid bit range {}..{}: upper bound must be greater than or equal to lower bound",
                field_num(data, "index_hi"),
                field_num(data, "index_lo"),
            ))
        }
        "IndexNotWordType" => {
            Some(format!("Expected a Word type: {}", field_str(data, "typ")))
        }
        "EmptyDriverBlock" => Some("Empty driver block".to_string()),
        "RedundantUnused" => {
            Some(format!("Redundant `unused`: {}", field_str(data, "path")))
        }
        "RedundantDriver" => {
            Some(format!("Redundant driver: {}", field_str(data, "path")))
        }
        "CombinationalLoop" => {
            Some(format!(
                "Combinational loop detected: {}",
                str_list(data, "components").join(" -> "),
            ))
        }
        _ => None,
    }
}

#[allow(dead_code)]
fn payload_message(payload: &JsonValue) -> String {
    match payload_known_message(payload) {
        Some(message) => message,
        None => {
            let variant = payload_variant(payload);
            format!("{variant} {}", pre_json(&payload[variant.as_str()]))
        }
    }
}

fn field_str(data: &JsonValue, field: &str) -> String {
    data[field].as_str().unwrap_or("?").to_string()
}

fn field_num(data: &JsonValue, field: &str) -> String {
    match data[field].as_u64() {
        Some(value) => value.to_string(),
        None => field_str(data, field),
    }
}

fn str_list(data: &JsonValue, field: &str) -> Vec<String> {
    data[field]
        .as_array()
        .map(|items| {
            items
                .iter()
                .map(|item| item.as_str().unwrap_or("?").to_string())
                .collect()
        })
        .unwrap_or_default()
}
