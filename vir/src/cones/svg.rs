//! Renders the cone network of an elaborated design as a standalone SVG.
//!
//! One rounded box per register and per boundary port of the top module;
//! one arrow per cone edge, pointing in the direction of data flow:
//! `A -> B` when `B` (a register input or a boundary output port) is
//! derived from `A`'s value combinationally, i.e. when `B` is in `A`'s
//! forward cone.  Combinational wires between registers are not drawn as
//! nodes — cone edges span them.
//!
//! The layout is a simple layered placement, top to bottom: every node
//! sits in the row after its latest predecessor (longest-path layering,
//! with combinational cycles broken by ignoring edges that close a loop
//! during the walk).  The horizontal axis is organized by module: every
//! module instance owns a fixed band of slots, nested inside its
//! parent's band, so sibling modules never interleave and their group
//! perimeters never overlap.
//!
//! Routing is hierarchical, so an arrow never crosses a module perimeter
//! unless it genuinely connects an inside component to an outside one:
//!
//! * An edge whose endpoints share a module stays inside that module:
//!   it runs in the free side-margins of the module's own slots and in
//!   the row-gaps between the module's own rows, both of which its
//!   perimeter encloses.
//! * An edge whose endpoints are in different modules descends in the
//!   source's slot margin, crossing only the bottom edges of the
//!   modules that contain the source but not the target; travels along
//!   the *bottom strip* of the deepest common ancestor (a reserved
//!   horizontal lane inside that ancestor, below everything it
//!   contains); and climbs into the target's module, crossing only the
//!   bottom edges of the modules that contain the target but not the
//!   source.
//!
//! Every route is drawn with rounded corners, and shared runs are
//! de-overlapped by assigning each edge its own lane: a distinct
//! departure/arrival point along the box edge, a distinct height within
//! a row-gap, a distinct x within a slot margin, and a distinct y within
//! a strip.  Where more lanes are needed than fit at `MIN_LANE`
//! spacing, the geometry grows instead of the spacing shrinking.

use std::fmt::Write as _;

use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use virdant::analysis::cones::Cones;
use virdant::analysis::elaboration::{Elaboration, SignalId};
use virdant::common::ComponentKind;

const MARGIN: f64 = 20.0;
const TITLE_H: f64 = 30.0;
const LEGEND_H: f64 = 30.0;
const NODE_H: f64 = 26.0;
const CHAR_W: f64 = 6.6;
/// Minimum spacing between parallel edge runs (lanes).  When a row-gap,
/// slot margin, box edge, or strip needs more lanes than fit at this
/// spacing, the geometry grows to provide them instead of the spacing
/// shrinking — this caps the density of the drawing.
const MIN_LANE: f64 = 7.0;

/// Minimum horizontal gap between adjacent slots; each slot's free side
/// margin (used for vertical corridor runs) is half of it.  Grows with
/// the margin lane count.
const COL_GAP: f64 = 44.0;
/// Minimum vertical gap between rows; each row-gap hosts the horizontal
/// shift runs of the edges leaving the row above and arriving at the row
/// below.  Grows with the gap lane count.
const LAYER_GAP: f64 = 44.0;
/// Height of the bottom strip each module reserves inside its own
/// perimeter for its cross-module edges' horizontal travel.
const STRIP_MIN: f64 = 16.0;
/// Corner radius for routed edges.
const CORNER_R: f64 = 9.0;
const FONT_SIZE: f64 = 11.0;
const EDGE_COLOR: &str = "#777777";
/// Module-group perimeters: no fill, just a light-blue stroke that
/// contrasts with the gray dependency arrows and the pastel node fills.
const GROUP_COLOR: &str = "#5b9bd5";
/// Module perimeter stroke width.
const GROUP_STROKE_W: f64 = 7.5;
/// A module's perimeter is inset from its slot band by this much, so
/// adjacent sibling perimeters keep a visible gap and the slot-boundary
/// cracks between them stay outside every perimeter.
const GROUP_INSET: f64 = 6.0;
const GROUP_PAD_TOP: f64 = 24.0;
const GROUP_PAD_BOTTOM: f64 = 12.0;
/// Forced clearance between a module perimeter and the perimeters of
/// modules nested inside it; parent boxes grow to provide it.  Sized to
/// keep a child's perimeter clear of the parent's label text, which
/// sits in the strip just inside the parent's top-left corner.
const GROUP_NEST_MARGIN: f64 = 24.0;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Register,
    Source,
    Sink,
}

impl Kind {
    fn fill(self) -> &'static str {
        match self {
            Kind::Register => "#a9c7e8",
            Kind::Source => "#b5e0b0",
            Kind::Sink => "#f4d38a",
        }
    }

    fn legend_label(self) -> &'static str {
        match self {
            Kind::Register => "register",
            Kind::Source => "incoming port",
            Kind::Sink => "outgoing port",
        }
    }
}

struct Node {
    label: String,
    kind: Kind,
    layer: usize,
    /// Slot index within the node's module band (global slot grid).
    slot: usize,
    x: f64,
    cy: f64,
    w: f64,
}

impl Node {
    fn cx(&self) -> f64 {
        self.x + self.w / 2.0
    }

    fn top(&self) -> f64 {
        self.cy - NODE_H / 2.0
    }

    fn bottom(&self) -> f64 {
        self.cy + NODE_H / 2.0
    }
}

/// The module instance a signal path belongs to: the path minus its
/// last segment (`top.soc.cpu.state` belongs to `top.soc.cpu`).
fn module_of(label: &str) -> &str {
    &label[..label.rfind('.').expect("signal paths always have a dot")]
}

/// The deepest module instance that contains both `a` and `b`.
fn lca(a: &str, b: &str) -> String {
    let mut common = String::new();
    for (x, y) in a.split('.').zip(b.split('.')) {
        if x != y {
            break;
        }
        if !common.is_empty() {
            common.push('.');
        }
        common.push_str(x);
    }
    common
}

/// The k-th of m parallel runs, spread symmetrically within ±`half`:
/// lane spacing shrinks as m grows, but no lane ever leaves the
/// reserved corridor.
fn spread(m: usize, k: usize, half: f64) -> f64 {
    if m <= 1 {
        return 0.0;
    }
    -half + (k as f64 + 1.0) * (2.0 * half) / (m as f64 + 1.0)
}

/// A polyline through `points` with corners rounded to `CORNER_R`
/// (shrunk to fit short segments).  Each rounded corner is inscribed in
/// the original corner, so the curve never leaves the corner's box-free
/// neighborhood and the box-free guarantee of the routing is preserved.
fn rounded_path(points: &[(f64, f64)]) -> String {
    let fmt = |p: (f64, f64)| format!("{:.1} {:.1}", p.0, p.1);
    if points.len() < 3 {
        return format!("M {} L {}", fmt(points[0]), fmt(points[points.len() - 1]));
    }
    let mut d = format!("M {}", fmt(points[0]));
    for i in 1..points.len() - 1 {
        let p = points[i];
        let a = points[i - 1];
        let b = points[i + 1];
        let (ux, uy) = (p.0 - a.0, p.1 - a.1);
        let (vx, vy) = (b.0 - p.0, b.1 - p.1);
        let len_in = (ux * ux + uy * uy).sqrt();
        let len_out = (vx * vx + vy * vy).sqrt();
        let r = CORNER_R.min(len_in / 2.0).min(len_out / 2.0);
        let p1 = (p.0 - ux / len_in * r, p.1 - uy / len_in * r);
        let p2 = (p.0 + vx / len_out * r, p.1 + vy / len_out * r);
        d += &format!(" L {} Q {} {}", fmt(p1), fmt(p), fmt(p2));
    }
    d += &format!(" L {}", fmt(points[points.len() - 1]));
    d
}

/// Longest-path layering over predecessors.  A node on the current
/// recursion path (a combinational cycle closing here) contributes no
/// additional depth, which breaks the cycle; memoization keeps this
/// linear.
fn compute_layer(
    v: usize,
    preds: &[Vec<usize>],
    memo: &mut [Option<usize>],
    in_stack: &mut [bool],
) -> usize {
    if let Some(layer) = memo[v] {
        return layer;
    }
    if in_stack[v] {
        return 0;
    }
    in_stack[v] = true;
    let mut layer = 0;
    for &u in &preds[v] {
        layer = layer.max(compute_layer(u, preds, memo, in_stack) + 1);
    }
    in_stack[v] = false;
    memo[v] = Some(layer);
    layer
}

/// Slot width of a module's band: its widest row of direct
/// registers/ports, plus the bands of all its child modules.
fn subtree_width(
    m: &str,
    children: &IndexMap<String, Vec<String>>,
    max_direct: &IndexMap<String, usize>,
    widths: &mut IndexMap<String, usize>,
) -> usize {
    if let Some(&w) = widths.get(m) {
        return w;
    }
    let mut w = max_direct[m];
    if let Some(kids) = children.get(m) {
        for c in kids {
            w += subtree_width(c, children, max_direct, widths);
        }
    }
    widths.insert(m.to_owned(), w);
    w
}

/// Assign `[start, start + width)` as module `m`'s band: its direct
/// nodes occupy the first `max_direct[m]` slots, then each child
/// module's band in order.
fn assign_band(
    m: &str,
    start: usize,
    children: &IndexMap<String, Vec<String>>,
    max_direct: &IndexMap<String, usize>,
    widths: &mut IndexMap<String, usize>,
    band_start: &mut IndexMap<String, usize>,
) {
    band_start.insert(m.to_owned(), start);
    let mut pos = start + max_direct[m];
    if let Some(kids) = children.get(m) {
        for c in kids {
            assign_band(c, pos, children, max_direct, widths, band_start);
            pos += widths[c];
        }
    }
}

/// Escape the characters that are special in SVG text content.  Signal
/// paths are alphanumerics and dots, so this is a formality.
fn escape(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
}

pub fn render(elaboration: &Elaboration, cones: &Cones, top: &str) -> String {
    // --- nodes: registers and boundary ports, in elaboration order ---
    let mut index_of: IndexMap<SignalId, usize> = IndexMap::new();
    let mut nodes: Vec<Node> = Vec::new();
    for id in cones.signals() {
        let component = elaboration.component(*id);
        let kind = if cones.is_register(*id) {
            Kind::Register
        } else {
            match component.component_kind() {
                ComponentKind::Incoming => Kind::Source,
                _ => Kind::Sink,
            }
        };
        let label = component.path().to_str_lossy().into_owned();
        let w = (label.chars().count() as f64 * CHAR_W).ceil() + 14.0;
        index_of.insert(*id, nodes.len());
        nodes.push(Node {
            label,
            kind,
            layer: 0,
            slot: 0,
            x: 0.0,
            cy: 0.0,
            w,
        });
    }

    // --- edges in data-flow direction: A -> B when B is in A's forward
    // cone (B's input is derived from A's output combinationally) ---
    let mut edges: IndexSet<(usize, usize)> = IndexSet::new();
    for id in cones.signals() {
        let u = index_of[id];
        if let Some(cone) = cones.forward(*id) {
            for target in cone {
                edges.insert((u, index_of[target]));
            }
        }
    }

    // --- layering: longest path over predecessors, with cycles broken by
    // ignoring edges into nodes on the current recursion path (the +1 is
    // still applied, so every node gets a finite layer).  The recursion
    // depth is the depth of the cone DAG; fine for realistic designs. ---
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nodes.len()];
    for &(u, v) in &edges {
        preds[v].push(u);
    }
    let mut memo: Vec<Option<usize>> = vec![None; nodes.len()];
    let mut in_stack: Vec<bool> = vec![false; nodes.len()];
    for v in 0..nodes.len() {
        let layer = compute_layer(v, &preds, &mut memo, &mut in_stack);
        nodes[v].layer = layer;
    }
    let max_layer = nodes.iter().map(|n| n.layer).max().unwrap_or(0);

    // --- route classification: an edge whose endpoints share a module
    // instance routes entirely inside that module; an edge between
    // different modules routes via the bottom strip of their deepest
    // common ancestor, crossing only the perimeters it must. ---
    #[derive(Clone, Copy, PartialEq, Debug)]
    enum RKind {
        SelfLoop,
        Internal,
        Cross,
    }
    let routes: Vec<(RKind, usize, usize, String)> = edges
        .iter()
        .map(|&(u, v)| {
            let mu = module_of(&nodes[u].label).to_owned();
            let mv = module_of(&nodes[v].label).to_owned();
            if u == v {
                (RKind::SelfLoop, u, v, mu)
            } else if mu == mv {
                (RKind::Internal, u, v, mu)
            } else {
                (RKind::Cross, u, v, lca(&mu, &mv))
            }
        })
        .collect();

    // --- module bands: every module instance gets a fixed slot range in
    // the global grid, nested inside its parent's range, so sibling
    // modules occupy disjoint slot intervals in every row and their
    // group perimeters can never overlap, however the nodes interleave.
    // A module's band reserves room for its own direct registers/ports
    // (its widest row) followed by its children's bands, in elaboration
    // order; slots left empty in a row stay empty. ---
    let mut modules: IndexSet<String> = IndexSet::new();
    for node in &nodes {
        let mut m = module_of(&node.label).to_owned();
        loop {
            if !modules.insert(m.clone()) {
                break; // already present, and so are its ancestors
            }
            let Some(dot) = m.rfind('.') else { break }; // root reached
            m.truncate(dot);
        }
    }
    let mut children: IndexMap<String, Vec<String>> = IndexMap::new();
    for m in &modules {
        if let Some(dot) = m.rfind('.') {
            children.entry(m[..dot].to_owned()).or_default().push(m.clone());
        }
    }
    let mut direct: IndexMap<(String, usize), usize> = IndexMap::new();
    for node in &nodes {
        *direct
            .entry((module_of(&node.label).to_owned(), node.layer))
            .or_default() += 1;
    }
    let max_direct: IndexMap<String, usize> = modules
        .iter()
        .map(|m| {
            let md = (0..=max_layer)
                .map(|r| direct.get(&(m.clone(), r)).copied().unwrap_or(0))
                .max()
                .unwrap_or(0);
            (m.clone(), md)
        })
        .collect();
    let mut widths: IndexMap<String, usize> = IndexMap::new();
    for m in modules.iter() {
        subtree_width(m, &children, &max_direct, &mut widths);
    }
    let mut band_start: IndexMap<String, usize> = IndexMap::new();
    if let Some(root) = modules.iter().find(|m| !m.contains('.')) {
        assign_band(root, 0, &children, &max_direct, &mut widths, &mut band_start);
    }
    let mut next_direct: IndexMap<(String, usize), usize> = IndexMap::new();
    for node in &mut nodes {
        let m = module_of(&node.label);
        let k = next_direct.entry((m.to_owned(), node.layer)).or_default();
        node.slot = band_start[m] + *k;
        *k += 1;
    }

    // --- stub allocation: which box edge each route departs from and
    // arrives at.  Departures run down (from the bottom edge) unless the
    // target sits above the source within the same module, in which case
    // the route runs up (from the top edge); arrivals mirror this. ---
    let mut bottom_stubs: IndexMap<usize, Vec<(usize, usize)>> = IndexMap::new();
    let mut top_stubs: IndexMap<usize, Vec<(usize, usize)>> = IndexMap::new();
    for (ei, r) in routes.iter().enumerate() {
        let (lu, lv) = (nodes[r.1].layer, nodes[r.2].layer);
        match r.0 {
            RKind::SelfLoop => {
                bottom_stubs.entry(r.1).or_default().push((ei, 0));
                bottom_stubs.entry(r.1).or_default().push((ei, 1));
            }
            RKind::Cross => {
                // Cross-module routes always dive to the common
                // ancestor's bottom strip.
                bottom_stubs.entry(r.1).or_default().push((ei, 0));
                bottom_stubs.entry(r.2).or_default().push((ei, 0));
            }
            RKind::Internal => match lu.cmp(&lv) {
                std::cmp::Ordering::Less => {
                    bottom_stubs.entry(r.1).or_default().push((ei, 0));
                    top_stubs.entry(r.2).or_default().push((ei, 0));
                }
                std::cmp::Ordering::Greater => {
                    // Depart from the source's top; arrive at the
                    // target's bottom (the route climbs from below it).
                    top_stubs.entry(r.1).or_default().push((ei, 0));
                    bottom_stubs.entry(r.2).or_default().push((ei, 0));
                }
                std::cmp::Ordering::Equal => {
                    bottom_stubs.entry(r.1).or_default().push((ei, 0));
                    bottom_stubs.entry(r.2).or_default().push((ei, 0));
                }
            },
        }
    }

    // --- band lanes: one horizontal run per (row-gap, anchoring module),
    // keyed by the physical gap index (band g sits between rows g-1 and
    // g; band 0 is the reserve above row 0).  Runs from different modules
    // never overlap in x (each is confined to its own module's slots), so
    // they may share a lane row; runs from the same module get distinct
    // lanes. ---
    let mut band_lanes: IndexMap<(usize, String, bool), Vec<usize>> = IndexMap::new();
    for (ei, r) in routes.iter().enumerate() {
        let (lu, lv) = (nodes[r.1].layer, nodes[r.2].layer);
        let mu = module_of(&nodes[r.1].label).to_owned();
        let mv = module_of(&nodes[r.2].label).to_owned();
        match r.0 {
            RKind::SelfLoop => {
                band_lanes.entry((lu + 1, mu, true)).or_default().push(ei);
            }
            RKind::Internal => {
                if lv > lu {
                    band_lanes.entry((lu + 1, mu.clone(), true)).or_default().push(ei);
                    if lv > lu + 1 {
                        band_lanes.entry((lv, mu, false)).or_default().push(ei);
                    }
                } else if lv == lu {
                    band_lanes.entry((lu + 1, mu, true)).or_default().push(ei);
                } else {
                    band_lanes.entry((lu, mu.clone(), false)).or_default().push(ei);
                    if lv < lu - 1 {
                        band_lanes.entry((lv + 1, mu, true)).or_default().push(ei);
                    }
                }
            }
            RKind::Cross => {
                band_lanes.entry((lu + 1, mu, true)).or_default().push(ei);
                band_lanes.entry((lv + 1, mv, true)).or_default().push(ei);
            }
        }
    }

    // --- corridor lanes: vertical runs in the free side margins of the
    // slots.  Every corridor at a slot belongs to that slot's owning
    // module, so the run stays inside its module's perimeter. ---
    let mut corridor_lanes: IndexMap<(usize, bool), Vec<usize>> = IndexMap::new();
    let mut corridor_use: IndexMap<usize, (usize, usize)> = IndexMap::new();
    // Per-edge corridor registry: which slot margin each vertical run
    // uses (internal multi-row edges use one, on the source's slot;
    // cross edges use two, on the source's and the target's slots).
    let mut dep_corridor: IndexMap<usize, (usize, bool)> = IndexMap::new();
    let mut arr_corridor: IndexMap<usize, (usize, bool)> = IndexMap::new();
    for (ei, r) in routes.iter().enumerate() {
        let mut needs: Vec<usize> = Vec::new();
        match r.0 {
            RKind::SelfLoop => {}
            RKind::Internal => {
                let (lu, lv) = (nodes[r.1].layer, nodes[r.2].layer);
                if lu.abs_diff(lv) >= 2 {
                    needs.push(nodes[r.1].slot);
                }
            }
            RKind::Cross => {
                needs.push(nodes[r.1].slot);
                needs.push(nodes[r.2].slot);
            }
        }
        for slot in needs {
            let use_e = corridor_use.entry(slot).or_default();
            let left = use_e.0 <= use_e.1;
            if left {
                use_e.0 += 1;
            } else {
                use_e.1 += 1;
            }
            corridor_lanes.entry((slot, left)).or_default().push(ei);
            let arrival = r.0 == RKind::Cross && slot == nodes[r.2].slot;
            if arrival {
                arr_corridor.insert(ei, (slot, left));
            } else {
                dep_corridor.insert(ei, (slot, left));
            }
        }
    }

    // --- strips: each common ancestor reserves a horizontal lane at the
    // bottom of its own perimeter, below everything it contains, for its
    // cross-module edges' horizontal travel. ---
    let mut strip_lanes: IndexMap<String, Vec<usize>> = IndexMap::new();
    for (ei, r) in routes.iter().enumerate() {
        if r.0 == RKind::Cross {
            strip_lanes.entry(r.3.clone()).or_default().push(ei);
        }
    }

    // --- sizing: the geometry grows to keep every lane at least
    // MIN_LANE away from its neighbors ---
    let layer_gap = band_lanes
        .values()
        .map(|l| (l.len() + 1) as f64 * MIN_LANE + 12.0)
        .fold(LAYER_GAP, f64::max);
    let col_gap = corridor_lanes
        .values()
        .map(|l| (l.len() + 1) as f64 * 2.0 * MIN_LANE + 8.0)
        .fold(COL_GAP, f64::max);
    for i in 0..nodes.len() {
        let stubs = bottom_stubs
            .get(&i)
            .map(|v| v.len())
            .unwrap_or(0)
            .max(top_stubs.get(&i).map(|v| v.len()).unwrap_or(0));
        let needed = (stubs + 1) as f64 * MIN_LANE + 16.0;
        nodes[i].w = nodes[i].w.max(needed);
    }
    let strip_h: IndexMap<String, f64> = strip_lanes
        .iter()
        .map(|(m, l)| {
            (
                m.clone(),
                ((l.len() + 1) as f64 * MIN_LANE + 8.0).max(STRIP_MIN),
            )
        })
        .collect();

    // --- geometry ---
    let slot_w = nodes.iter().map(|n| n.w).fold(40.0, f64::max) + col_gap;
    let n_slots = modules
        .iter()
        .find(|m| !m.contains('.'))
        .and_then(|root| widths.get(root))
        .copied()
        .unwrap_or(0);
    let content_w = n_slots as f64 * slot_w;
    let plot_top = MARGIN + TITLE_H + LEGEND_H;
    let row_top = |l: usize| plot_top + layer_gap + (l as f64) * (NODE_H + layer_gap);
    // Top of physical row-gap `g` (band 0 is the reserve above row 0).
    let band_top = |g: usize| if g == 0 { plot_top } else { row_top(g - 1) + NODE_H };
    let mut total_h = (row_top(max_layer) + NODE_H + layer_gap + MARGIN).max(120.0);

    for node in &mut nodes {
        let cx = MARGIN + (node.slot as f64 + 0.5) * slot_w;
        node.x = cx - node.w / 2.0;
        node.cy = row_top(node.layer) + NODE_H / 2.0;
    }
    let mut total_w = (content_w + 2.0 * MARGIN).max(200.0);

    // --- module group boxes: the x range is the module's slot band
    // (inset from its edges, so adjacent sibling perimeters keep a
    // visible gap and the cracks between bands stay outside every
    // perimeter); the y range is a recursive enclosure of the module's
    // own nodes and its children's boxes, extended at the bottom by the
    // module's routing strip. ---
    let mut groups: IndexMap<String, Vec<usize>> = IndexMap::new();
    for (i, node) in nodes.iter().enumerate() {
        groups
            .entry(module_of(&node.label).to_owned())
            .or_default()
            .push(i);
    }
    // Intermediate modules with no direct registers/ports of their own
    // (e.g. a module that only instantiates submodules) still get a
    // perimeter around their children.
    for m in modules.iter() {
        groups.entry(m.clone()).or_default();
    }
    // Y ranges, computed deepest-first so parents can enclose children.
    let mut depth_sorted: Vec<&String> = modules.iter().collect();
    depth_sorted.sort_by_key(|m| std::cmp::Reverse(m.split('.').count()));
    let mut box_y: IndexMap<String, (f64, f64)> = IndexMap::new();
    for m in depth_sorted {
        let mut y0 = f64::MAX;
        let mut y1 = f64::MIN;
        if let Some(members) = groups.get(m) {
            for &i in members {
                y0 = y0.min(nodes[i].top());
                y1 = y1.max(nodes[i].bottom());
            }
        }
        if y0.is_finite() {
            y0 -= GROUP_PAD_TOP;
            y1 += GROUP_PAD_BOTTOM;
        }
        if let Some(kids) = children.get(m) {
            for c in kids {
                let (cy0, cy1) = box_y[c];
                y0 = y0.min(cy0 - GROUP_NEST_MARGIN);
                y1 = y1.max(cy1 + GROUP_NEST_MARGIN);
            }
        }
        let sh = strip_h.get(m).copied().unwrap_or(0.0);
        let y1 = if y1.is_finite() { y1 + sh } else { y1 };
        box_y.insert(m.clone(), (y0, y1));
    }
    let boxes: Vec<(String, f64, f64, f64, f64)> = modules
        .iter()
        .filter_map(|m| {
            let bs = *band_start.get(m)?;
            let w = *widths.get(m)?;
            let (y0, y1) = box_y.get(m)?;
            if !y0.is_finite() {
                return None;
            }
            Some((
                m.clone(),
                MARGIN + bs as f64 * slot_w + GROUP_INSET,
                *y0,
                MARGIN + (bs + w) as f64 * slot_w - GROUP_INSET,
                *y1,
            ))
        })
        .collect();
    // Expanded group boxes may need more canvas than the nodes alone.
    for &(_, _, _, gx1, gy1) in &boxes {
        total_w = total_w.max(gx1 + MARGIN);
        total_h = total_h.max(gy1 + MARGIN);
    }

    // --- lane value helpers ---
    let band_lane_y = |g: usize, m: &str, upper: bool, ei: usize| -> f64 {
        let list = match band_lanes.get(&(g, m.to_owned(), upper)) {
            Some(l) => l,
            None => panic!("missing band lane {g}/{m}/{upper} for edge {ei}"),
        };
        let k = list.iter().position(|&e| e == ei).unwrap();
        let center = band_top(g) + layer_gap / 2.0;
        let y = center + spread(list.len(), k, (layer_gap - 12.0) / 2.0);
        if upper {
            // Stay inside the anchoring module's bottom edge.
            let (_, y1) = box_y[m];
            y.min(y1 - 6.0)
        } else {
            // Stay inside the anchoring module's top edge.
            let (y0, _) = box_y[m];
            y.max(y0 + 6.0)
        }
    };
    let strip_lane_y = |m: &str, ei: usize| -> f64 {
        let list = &strip_lanes[m];
        let k = list.iter().position(|&e| e == ei)
            .unwrap_or_else(|| panic!("strip lane miss m={m} ei={ei} list={list:?}"));
        let h = strip_h[m];
        let (_, by1) = box_y[m];
        by1 - h / 2.0 + spread(list.len(), k, (h - 6.0) / 2.0)
    };
    let corridor_lane_x = |slot: usize, left: bool, ei: usize, m: &str| -> f64 {
        let list = &corridor_lanes[&(slot, left)];
        let k = list.iter().position(|&e| e == ei)
            .unwrap_or_else(|| panic!("corridor lane miss slot={slot} left={left} ei={ei} list={list:?} dep={dep_corridor:?} arr={arr_corridor:?} groups={corridor_lanes:?}"));
        let slot_left = MARGIN + slot as f64 * slot_w;
        let center = if left {
            slot_left + col_gap / 4.0
        } else {
            slot_left + slot_w - col_gap / 4.0
        };
        let x = center + spread(list.len(), k, (col_gap / 2.0 - 8.0) / 2.0);
        // Keep clear of the owning module's inset perimeter stroke.
        let bs = band_start[m];
        let w = widths[m];
        let lo = MARGIN + bs as f64 * slot_w + GROUP_INSET + GROUP_STROKE_W / 2.0 + 2.0;
        let hi = MARGIN + (bs + w) as f64 * slot_w - GROUP_INSET - GROUP_STROKE_W / 2.0 - 2.0;
        x.clamp(lo, hi)
    };
    let stub_dx = |stubs: &IndexMap<usize, Vec<(usize, usize)>>,
                   node: usize,
                   ei: usize,
                   occ: usize|
                   -> f64 {
        let list = match stubs.get(&node) {
            Some(l) => l,
            None => panic!("missing stub map node={node} ei={ei} occ={occ}"),
        };
        let k = list.iter().position(|&(e, o)| e == ei && o == occ)
            .unwrap_or_else(|| panic!("missing stub lane node={node} ei={ei} occ={occ} in {list:?}"));
        spread(list.len(), k, nodes[node].w / 2.0 - 8.0)
    };

    // --- routes (geometry) ---
    let mut out = String::new();
    writeln!(out, r##"<?xml version="1.0" encoding="UTF-8"?>"##).unwrap();
    writeln!(
        out,
        r##"<svg xmlns="http://www.w3.org/2000/svg" width="{total_w}" height="{total_h}" viewBox="0 0 {total_w} {total_h}">"##
    )
    .unwrap();
    writeln!(
        out,
        r##"<rect x="0" y="0" width="{total_w}" height="{total_h}" fill="#ffffff"/>"##
    )
    .unwrap();
    writeln!(
        out,
        r##"<defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#555555"/></marker></defs>"##
    )
    .unwrap();

    writeln!(
        out,
        r##"<text x="{MARGIN}" y="{}" font-family="sans-serif" font-size="14" font-weight="bold" fill="#222222">Cones of '{}'</text>"##,
        MARGIN + 14.0,
        escape(top),
    )
    .unwrap();

    let legend_y = MARGIN + TITLE_H;
    let mut lx = MARGIN;
    for kind in [Kind::Register, Kind::Source, Kind::Sink] {
        writeln!(
            out,
            r##"<rect x="{lx}" y="{legend_y}" width="13" height="13" fill="{}" stroke="#555555"/>"##,
            kind.fill(),
        )
        .unwrap();
        let label = kind.legend_label();
        writeln!(
            out,
            r##"<text x="{}" y="{}" font-family="sans-serif" font-size="{FONT_SIZE}" fill="#222222">{label}</text>"##,
            lx + 18.0,
            legend_y + 11.0,
        )
        .unwrap();
        lx += 18.0 + label.len() as f64 * 6.0 + 18.0;
    }

    // Module group perimeters (drawn in the background; the labels go on
    // top of everything, after the nodes):
    for &(_, gx, gy, gx1, gy1) in &boxes {
        let gw = gx1 - gx;
        let gh = gy1 - gy;
        writeln!(
            out,
            r##"  <rect x="{gx}" y="{gy}" width="{gw}" height="{gh}" rx="8" fill="none" stroke="{GROUP_COLOR}" stroke-width="{GROUP_STROKE_W}"/>"##
        )
        .unwrap();
    }

    // Edges first (under the boxes).
    for (ei, r) in routes.iter().enumerate() {
        let (u, v) = (r.1, r.2);
        let (nu, nv) = (&nodes[u], &nodes[v]);
        let (lu, lv) = (nu.layer, nv.layer);
        let pts: Vec<(f64, f64)> = match r.0 {
            RKind::SelfLoop => {
                let mu = module_of(&nu.label);
                let y = band_lane_y(lu + 1, mu, true, ei);
                let dx1 = stub_dx(&bottom_stubs, u, ei, 0);
                let dx2 = stub_dx(&bottom_stubs, u, ei, 1);
                vec![
                    (nu.cx() + dx1, nu.bottom()),
                    (nu.cx() + dx1, y),
                    (nu.cx() + dx2, y),
                    (nu.cx() + dx2, nu.bottom()),
                ]
            }
            RKind::Internal => {
                let mu = module_of(&nu.label);
                if lv > lu {
                    // Forward inside the module: down to the row-gap
                    // below the source, across, and into the target's
                    // top.
                    let dep_x = nu.cx() + stub_dx(&bottom_stubs, u, ei, 0);
                    let y_d = band_lane_y(lu + 1, mu, true, ei);
                    let arr_x = nv.cx() + stub_dx(&top_stubs, v, ei, 0);
                    if lv == lu + 1 {
                        vec![
                            (dep_x, nu.bottom()),
                            (dep_x, y_d),
                            (arr_x, y_d),
                            (arr_x, nv.top()),
                        ]
                    } else {
                        let (c_slot, c_left) = dep_corridor[&ei];
                        let x_c = corridor_lane_x(c_slot, c_left, ei, mu);
                        let y_a = band_lane_y(lv, mu, false, ei);
                        vec![
                            (dep_x, nu.bottom()),
                            (dep_x, y_d),
                            (x_c, y_d),
                            (x_c, y_a),
                            (arr_x, y_a),
                            (arr_x, nv.top()),
                        ]
                    }
                } else if lv == lu {
                    // Same row: through the row-gap below, up into the
                    // target's bottom.
                    let dep_x = nu.cx() + stub_dx(&bottom_stubs, u, ei, 0);
                    let y_d = band_lane_y(lu + 1, mu, true, ei);
                    let arr_x = nv.cx() + stub_dx(&bottom_stubs, v, ei, 0);
                    vec![
                        (dep_x, nu.bottom()),
                        (dep_x, y_d),
                        (arr_x, y_d),
                        (arr_x, nv.bottom()),
                    ]
                } else {
                    // Back edge inside the module: up from the source's
                    // top, across, and down into the target's top.
                    let dep_x = nu.cx() + stub_dx(&top_stubs, u, ei, 0);
                    let y_d = band_lane_y(lu, mu, false, ei);
                    let arr_x = nv.cx() + stub_dx(&bottom_stubs, v, ei, 0);
                    if lv == lu - 1 {
                        vec![
                            (dep_x, nu.top()),
                            (dep_x, y_d),
                            (arr_x, y_d),
                            (arr_x, nv.bottom()),
                        ]
                    } else {
                        let (c_slot, c_left) = dep_corridor[&ei];
                        let x_c = corridor_lane_x(c_slot, c_left, ei, mu);
                        let y_a = band_lane_y(lv + 1, mu, true, ei);
                        vec![
                            (dep_x, nu.top()),
                            (dep_x, y_d),
                            (x_c, y_d),
                            (x_c, y_a),
                            (arr_x, y_a),
                            (arr_x, nv.bottom()),
                        ]
                    }
                }
            }
            RKind::Cross => {
                // Out of the source's module (down through its bottom
                // edge), along the common ancestor's strip, and up into
                // the target's module (through its bottom edge).
                let mu = module_of(&nu.label);
                let mv = module_of(&nv.label);
                let dep_x = nu.cx() + stub_dx(&bottom_stubs, u, ei, 0);
                let y_d = band_lane_y(lu + 1, mu, true, ei);
                let (du_slot, du_left) = dep_corridor[&ei];
                let x_u = corridor_lane_x(du_slot, du_left, ei, mu);
                let y_s = strip_lane_y(&r.3, ei);
                let (dv_slot, dv_left) = arr_corridor[&ei];
                let x_v = corridor_lane_x(dv_slot, dv_left, ei, mv);
                let y_a = band_lane_y(lv + 1, mv, true, ei);
                let arr_x = nv.cx() + stub_dx(&bottom_stubs, v, ei, 0);
                vec![
                    (dep_x, nu.bottom()),
                    (dep_x, y_d),
                    (x_u, y_d),
                    (x_u, y_s),
                    (x_v, y_s),
                    (x_v, y_a),
                    (arr_x, y_a),
                    (arr_x, nv.bottom()),
                ]
            }
        };
        let d = rounded_path(&pts);
        writeln!(
            out,
            r##"  <path d="{d}" fill="none" stroke="{EDGE_COLOR}" stroke-linejoin="round" marker-end="url(#arrow)"/>"##
        )
        .unwrap();
    }

    // Node boxes.
    for node in &nodes {
        writeln!(
            out,
            r##"  <rect x="{}" y="{}" width="{}" height="{NODE_H}" rx="4" fill="{}" stroke="#444444"/>"##,
            node.x,
            node.top(),
            node.w,
            node.kind.fill(),
        )
        .unwrap();
        writeln!(
            out,
            r##"  <text x="{}" y="{}" font-family="monospace" font-size="{FONT_SIZE}" text-anchor="middle" dominant-baseline="central" fill="#111111">{}</text>"##,
            node.cx(),
            node.cy,
            escape(&node.label),
        )
        .unwrap();
    }

    // Module labels go last, on top of everything, each with a small
    // opaque white halo so that no edge, box, or nested perimeter ever
    // strikes through the text.
    for (name, gx, gy, _, _) in &boxes {
        let label = escape(name);
        let halo_w = name.len() as f64 * CHAR_W + 10.0;
        writeln!(
            out,
            r##"  <rect x="{}" y="{}" width="{halo_w}" height="15" fill="#ffffff"/>"##,
            gx + 4.0,
            gy + 2.0,
        )
        .unwrap();
        writeln!(
            out,
            r##"  <text x="{}" y="{}" font-family="monospace" font-size="{FONT_SIZE}" fill="#111111">{}</text>"##,
            gx + 8.0,
            gy + 13.0,
            label,
        )
        .unwrap();
    }

    writeln!(out, "</svg>").unwrap();
    out
}
