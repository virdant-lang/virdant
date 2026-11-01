//! Semantic comparison of VCD dumps.
//!
//! `trace = true` compares the VCD written by `vir run --trace`
//! against `trace.golden.vcd`.  A byte-for-byte diff would break on
//! any change to how the VCD is *emitted* — identifier codes, scope
//! layout, variable ordering, header text, no-op timestamps — so the
//! comparison is over the *meaning* of the dump: the value history of
//! every variable.
//!
//! `parse` extracts, for each variable (its full hierarchical path and
//! width), the sequence of times at which its value changed and the
//! value it changed to.  `equal` then checks that both dumps describe
//! the same variables with the same histories.

use std::collections::BTreeMap;

/// One variable's history: (time, value) change points.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct History {
    pub width: u64,
    /// Changes in time order; consecutive equal values already
    /// removed by `parse`.
    pub changes: Vec<(u64, String)>,
}

/// Every variable in a dump, keyed by hierarchical path
/// (`top.counter.count`).  BTreeMap for stable diagnostics.
pub type Dump = BTreeMap<String, History>;

/// Parse a VCD dump into per-variable histories.
///
/// Understands the subset of VCD the sim writer produces (plus the
/// constructs other writers commonly emit): `$scope`/`$upscope`,
/// `$var` declarations (id codes mapped to hierarchical names),
/// scalar (`0!`) and vector (`b1010 !`) value changes, `$dumpvars`
/// snapshots, and `#time` markers.  `$date`, `$version`, `$comment`,
/// `$timescale`, and `$enddefinitions` are skipped; the timescale is
/// not compared, since time values are compared symbolically across
/// the two dumps (both come from the same writer).
///
/// Returns Err on structurally invalid input.
pub fn parse(text: &str) -> Result<Dump, String> {
    let mut words = Tokens::new(text);

    let mut dump: BTreeMap<String, History> = BTreeMap::new();
    // id code -> (hierarchical path, width)
    let mut vars: BTreeMap<String, (String, u64)> = BTreeMap::new();
    // id code -> last value; changes are recorded only when the value
    // differs from the last one seen for the variable.
    let mut last: BTreeMap<String, String> = BTreeMap::new();
    let mut scopes: Vec<String> = Vec::new();
    let mut time: u64 = 0;
    let mut in_dumpvars = false;

    while let Some(w) = words.next() {
        match w {
            "$scope" => {
                // $scope <type> <name> $end
                let kind = words.next().ok_or("VCD: truncated $scope")?;
                let name = words.next().ok_or("VCD: truncated $scope")?;
                words.expect_end("$scope")?;
                let _ = kind;
                scopes.push(name.to_string());
            }
            "$upscope" => {
                words.expect_end("$upscope")?;
                scopes.pop();
            }
            "$var" => {
                // $var <type> <width> <id> <name> [<bit-range>] $end
                let _kind = words.next().ok_or("VCD: truncated $var")?;
                let width: u64 = words
                    .next()
                    .ok_or("VCD: truncated $var")?
                    .parse()
                    .map_err(|_| "VCD: bad width in $var")?;
                let id = words.next().ok_or("VCD: truncated $var")?.to_string();
                let name = words.next().ok_or("VCD: truncated $var")?.to_string();
                // Optional `[msb:lsb]` range before $end.
                loop {
                    let tok = words.next().ok_or("VCD: truncated $var")?;
                    if tok == "$end" {
                        break;
                    }
                }
                let mut path = scopes.clone();
                path.push(name);
                vars.insert(id, (path.join("."), width));
            }
            "$dumpvars" | "$dumpall" => {
                in_dumpvars = true;
            }
            "$end" => {
                in_dumpvars = false;
            }
            _ => {
                if let Some(rest) = w.strip_prefix('#') {
                    time = rest.parse().map_err(|_| format!("VCD: bad time `{w}`"))?;
                    continue;
                }
                if in_dumpvars {
                    // Initial snapshot taken at the dump time; treat
                    // like any other change.
                }
                // A value change: scalar `0!`/`1!`/`x!`/`z!`, or a
                // vector token `b1010 !`/`B...` whose id is the next
                // word.
                let (value, id) = if w.starts_with('b') || w.starts_with('B') {
                    let id = words.next().ok_or("VCD: vector change without id")?;
                    (w[1..].to_string(), id.to_string())
                } else if w.starts_with('r') || w.starts_with('R') {
                    let id = words.next().ok_or("VCD: real change without id")?;
                    (w[1..].to_string(), id.to_string())
                } else if w.chars().count() >= 2 {
                    (w[..w.chars().count() - 1].to_string(), w[w.chars().count() - 1..].to_string())
                } else {
                    return Err(format!("VCD: malformed value change `{w}`"));
                };
                let Some((path, width)) = vars.get(&id) else {
                    // Ids the dump never declared (shouldn't happen);
                    // skip rather than fail.
                    continue;
                };
                let path = path.clone();
                let width = *width;
                if last.get(&id).map(|v| v == &value).unwrap_or(false) {
                    continue;
                }
                last.insert(id, value.clone());
                let entry = dump.entry(path).or_insert_with(|| History {
                    width,
                    changes: Vec::new(),
                });
                // A later change within the same timestep overwrites
                // an earlier one.
                if entry.changes.last().map(|(t, _)| *t) == Some(time) {
                    entry.changes.pop();
                }
                entry.changes.push((time, value));
            }
        }
    }
    Ok(dump)
}

/// Compare two dumps; Err describes the first difference found.
/// `a` is the actual dump, `b` the golden one; diagnostics speak of
/// the two by those names.
pub fn diff(a: &Dump, b: &Dump) -> Result<(), String> {
        let a_names: Vec<&String> = a.keys().collect();
        let b_names: Vec<&String> = b.keys().collect();
        for name in &a_names {
            if !b.contains_key(*name) {
                return Err(format!("variable `{name}` present in actual but not in golden"));
            }
        }
        for name in &b_names {
            if !a.contains_key(*name) {
                return Err(format!("variable `{name}` present in golden but not in actual"));
            }
        }
        for name in a_names {
            let (ha, hb) = (&a[name], &b[name]);
            if ha.width != hb.width {
                return Err(format!(
                    "variable `{name}` has width {} in actual but {} in golden",
                    ha.width, hb.width
                ));
            }
            for (i, (ca, cb)) in ha.changes.iter().zip(hb.changes.iter()).enumerate() {
                if ca != cb {
                    return Err(format!(
                        "variable `{name}` change #{} differs:\n  golden: t={} value={}\n  actual: t={} value={}",
                        i + 1,
                        ca.0,
                        ca.1,
                        cb.0,
                        cb.1
                    ));
                }
            }
            if ha.changes.len() != hb.changes.len() {
                let (longer, which) = if ha.changes.len() > hb.changes.len() {
                    (&ha.changes, "actual")
                } else {
                    (&hb.changes, "golden")
                };
                let extra = &longer[hb.changes.len().min(ha.changes.len())];
                return Err(format!(
                    "variable `{name}`: {} has {} changes but {} has {} (first extra: t={} value={})",
                    which,
                    longer.len(),
                    if which == "actual" { "golden" } else { "actual" },
                    hb.changes.len().min(ha.changes.len()),
                    extra.0,
                    extra.1
                ));
            }
        }
        Ok(())
}

/// Whitespace tokenizer that keeps `$end` significant.
struct Tokens<'a> {
    words: std::str::SplitWhitespace<'a>,
}

impl<'a> Tokens<'a> {
    fn new(text: &'a str) -> Tokens<'a> {
        Tokens { words: text.split_whitespace() }
    }
    fn next(&mut self) -> Option<&'a str> {
        self.words.next()
    }
    fn expect_end(&mut self, what: &str) -> Result<(), String> {
        match self.next() {
            Some("$end") => Ok(()),
            other => Err(format!("VCD: expected $end after {what}, got {other:?}")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "\
$timescale 1ps $end
$scope module top $end
$var wire 4 ! count $end
$var wire 8 \" exit $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
b0000 !
b11111111 \"
$end
#1000
b0001 !
#15000
b11111111 \"
#16000
b1111 !
";

    #[test]
    fn parses_histories() {
        let dump = parse(SAMPLE).unwrap();
        assert_eq!(dump.len(), 2);
        let count = &dump["top.count"];
        assert_eq!(count.width, 4);
        assert_eq!(
            count.changes,
            vec![(0, "0000".into()), (1000, "0001".into()), (16000, "1111".into())]
        );
        // The change at t=15000 re-writes the value already held since
        // t=0; the parser drops it as a no-op, so the history holds a
        // single change point.
        let exit = &dump["top.exit"];
        assert_eq!(exit.changes, vec![(0, "11111111".into())]);
    }

    #[test]
    fn equal_dumps_pass() {
        let a = parse(SAMPLE).unwrap();
        let b = parse(SAMPLE).unwrap();
        diff(&a, &b).unwrap();
    }

    #[test]
    fn different_id_codes_and_ordering_still_equal() {
        // Same semantics, different emission: ids swapped, vars in
        // reverse order, extra header text, a no-op timestamp.
        let other = "\
$date 2024
$version test
$timescale 1ps $end
$scope module top $end
$var wire 8 \" exit $end
$var wire 4 ! count $end
$upscope $end
$enddefinitions $end
#0
$dumpvars
b0000 !
b11111111 \"
$end
#1000
b0001 !
#5000
#15000
b11111111 \"
#16000
b1111 !
";
        let a = parse(SAMPLE).unwrap();
        let b = parse(other).unwrap();
        diff(&a, &b).unwrap();
    }

    #[test]
    fn value_difference_is_reported() {
        let a = parse(SAMPLE).unwrap();
        let mutated = SAMPLE.replace("b0001 !", "b0010 !");
        let b = parse(&mutated).unwrap();
        assert!(diff(&a, &b).is_err());
    }
}
